;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :clim-internals)

;;; Note: in the methods defined on output streams, I often use
;;;	  the sheet's medium as the argument to the draw-* routines.
;;;	  This is so that they don't get recorded if the stream also
;;;	  happens to be an output-recording-stream. - MikeMac 1/7/99

;;; Standard-Output-Stream class

(defclass standard-output-stream (fundamental-character-output-stream)
  ()
  )

(defmethod stream-recording-p ((stream t)) nil)

(defmethod stream-drawing-p ((stream t)) t)

#+ignore(defmethod stream-write-char ((stream standard-output-stream) char)
  (multiple-value-bind (cx cy) (stream-cursor-position stream)
    (cond
     ((eq char #\Newline)
      (setf (stream-cursor-position stream)
            (value 0
                   (+ cy
                      (stream-line-height stream)
                      (stream-vertical-spacing stream)))))
     (t
      (draw-text* (sheet-medium stream) char cx (+ cy (stream-baseline stream)))
      (setf (stream-cursor-position stream)
            (values (+ cx (stream-character-width stream char)) cy))))))


;;; Cursor class

(defgeneric cursor-sheet (cursor))

(defgeneric cursor-position (cursor))

(defgeneric* (setf cursor-position) (x y cursor))

(defgeneric cursor-active (cursor))
(defgeneric (setf cursor-active) (value cursor))

(defgeneric cursor-state (cursor))
(defgeneric (setf cursor-state) (value cursor))

(defgeneric cursor-focus (cursor))

(defgeneric cursor-visibility (cursor))
(defgeneric (setf cursor-visibility) (visibility cursor))

;;; Cursor-Mixin class
(defclass cursor-mixin ()
  ((sheet :initarg :sheet
	  :reader cursor-sheet)
   (x :initform 0 :initarg :x-position)
   (y :initform 0 :initarg :y-position)
   (width :initform 8)
   (appearance :type (member :solid :hollow) 
               :initarg :appearance :initform :hollow
               :accessor cursor-appearance)
   ;; XXX what does "cursor is active" mean?
   ;; It means that the sheet (stream) updates the cursor, though
   ;; currently the cursor appears to be always updated after stream
   ;; text operations. -- moore
   (cursor-active :initform nil
                  :accessor cursor-active)
   (cursor-state :initform nil
                 :accessor cursor-state)))

(defgeneric cursor-height (cursor))

(defmethod print-object ((cursor cursor-mixin) stream)
  (with-slots (x y) cursor
    (print-unreadable-object (cursor stream :type t :identity t)
      (format stream "~D ~D " x y))))

;;; XXX What to do when we can't draw the cursor immediately (like,
;;; we're not drawing?) The whole flip-screen-cursor idea breaks down.

(defmethod (setf cursor-state) :around (state (cursor cursor-mixin))
  (unless (eq state (slot-value cursor 'cursor-state))
    (flip-screen-cursor cursor))
  (call-next-method))

(defun decode-cursor-visibility (visibility)
  "Given :on, :off, or nil, returns the needed active and state attributes for the cursor."
  (ecase visibility
    ((:on t) (values t t))
    (:off    (values t nil))
    ((nil)   (values nil nil))))

(defmethod cursor-visibility ((cursor cursor-mixin))
  (let ((a (cursor-active cursor))
        (s (cursor-state cursor)))
    (cond ((and a s) :on)
          ((and a (not s)) :off)
          (t nil))))

(defmethod (setf cursor-visibility) (nv (cursor cursor-mixin))
  (multiple-value-bind (active state)
      (decode-cursor-visibility nv)
    (setf (cursor-state cursor)  state
          (cursor-active cursor) active)))

(defmethod cursor-position ((cursor cursor-mixin))
  (with-slots (x y) cursor
    (values x y)))

(defmethod* (setf cursor-position) (nx ny (cursor cursor-mixin))
  (with-slots (x y) cursor
    (letf (((cursor-state cursor) nil))
      (multiple-value-prog1
	  (setf (values x y) (values nx ny))))
    (when (and (cursor-active cursor)
	       (output-recording-stream-p (cursor-sheet cursor)))
      (stream-close-text-output-record (cursor-sheet cursor)))))

(defmethod flip-screen-cursor ((cursor cursor-mixin))
  (when (stream-drawing-p (cursor-sheet cursor))
    (with-slots (x y sheet width) cursor
      (let ((height (cursor-height cursor)))
	(draw-rectangle* (sheet-medium (cursor-sheet cursor))
			 x y
			 (+ x width) (+ y height)
			 :filled (ecase (cursor-appearance cursor)
                                   (:solid t) (:hollow nil))
			 :ink +flipping-ink+)))))

(defmethod display-cursor ((cursor cursor-mixin) state)
  (unless (stream-drawing-p (cursor-sheet cursor))
    (return-from display-cursor nil))
  (with-slots (x y sheet width) cursor
    (let ((height (cursor-height cursor)))
      (case state
	(:draw (draw-rectangle* (sheet-medium (cursor-sheet cursor))
				x y
				(+ x width) (+ y height)
				:filled (ecase (cursor-appearance cursor)
                                   (:solid t) (:hollow nil))
				:ink +foreground-ink+
				))       
        (:erase
               ;; This is how I'd like this to work, as painting over with the background
               ;; ink is repugnant. I leave this disabled because I'm concerned about
               ;; infinite recursion if replay-output-record calls here (which Goatee
               ;; does currently).  --Hefner
                #+nil (repaint-sheet (cursor-sheet cursor)
                                     (make-bounding-rectangle x y (+ 1 x width)
                                                              (+ 1 y height)))
                (draw-rectangle* (sheet-medium (cursor-sheet cursor))
                                 x y
                                 (+ x width) (+ y height)
                                 :filled (ecase (cursor-appearance cursor)
                                   (:solid t) (:hollow nil))
                                 :ink +background-ink+))))))

;;; Standard-Text-Cursor class

(defclass standard-text-cursor (cursor-mixin cursor)
  ())

(defmethod cursor-height ((cursor standard-text-cursor))
  (slot-value (cursor-sheet cursor) 'height))


;;; Extended-Output-Stream class

;;; Stream text cursor protocol
(defgeneric stream-text-cursor (stream))
(defgeneric (setf stream-text-cursor) (cursor stream))

(defgeneric stream-cursor-position (stream))
(defgeneric* (setf stream-cursor-position) (x y stream))

(defgeneric stream-increment-cursor-position (stream dx dy))

;;; Text protocol
(defgeneric stream-character-width (stream character &key text-style))

(defgeneric stream-string-width (stream character &key start end text-style))

(defgeneric stream-text-margin (stream))
(defgeneric (setf stream-text-margin) (margin stream))

(defgeneric stream-line-height (stream &key text-style))

(defgeneric stream-vertical-spacing (stream))

(defgeneric stream-baseline (stream))

(defgeneric stream-end-of-line-action (stream))
(defgeneric (setf stream-end-of-line-action) (action stream))

(defgeneric stream-end-of-page-action (stream))
(defgeneric (setf stream-end-of-page-action) (action stream))

;;; Standard-Extended-Output-Stream class

(defclass standard-extended-output-stream (extended-output-stream
                                           standard-output-stream)
  ((cursor :accessor stream-text-cursor)
   (foreground :initarg :foreground :reader stream-foreground)
   (background :initarg :background :reader stream-background)
   (text-style :initarg :text-style :reader stream-text-style)
   (vspace :initarg :vertical-spacing :reader stream-vertical-spacing)
   (margin :initarg :text-margin :writer (setf stream-text-margin))
   (eol :initarg :end-of-line-action :accessor stream-end-of-line-action)
   (eop :initarg :end-of-page-action :accessor stream-end-of-page-action)
   (view :initarg :default-view :accessor stream-default-view)
   (baseline :initform 0 :reader stream-baseline)
   ;; What is this? --GB
   (height :initform 0)
   ;; When the stream takes part in the space alloction protocol, this
   ;; remembers our demand:
   (seos-current-width  :initform 0)
   (seos-current-height :initform 0))
  (:default-initargs
   :foreground +black+ :background +white+ :text-style *default-text-style*
   :vertical-spacing 2 :text-margin nil :end-of-line-action :wrap
   :end-of-page-action :scroll :default-view +textual-view+))

(defmethod stream-force-output :after ((stream
					standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-force-output medium)))

(defmethod compose-space ((pane standard-extended-output-stream) &key width height)
  (declare (ignorable width height))
  
  (with-slots (seos-current-width seos-current-height) pane
    (make-space-requirement :width seos-current-width
                            :height seos-current-height)))

(defmethod initialize-instance :after
    ((stream standard-extended-output-stream) &rest args)
  (declare (ignore args))
  (setf (stream-text-cursor stream)
	(make-instance 'standard-text-cursor :sheet stream))
  (setf (cursor-active (stream-text-cursor stream)) t))


(defmethod stream-cursor-position ((stream standard-extended-output-stream))
  (cursor-position (stream-text-cursor stream)))

(defmethod* (setf stream-cursor-position) (x y (stream standard-extended-output-stream))
  (setf (cursor-position (stream-text-cursor stream)) (values x y)))

(defun stream-set-cursor-position (stream x y)
  (setf (stream-cursor-position stream) (values x y)))

(defmethod stream-increment-cursor-position ((stream standard-extended-output-stream) dx dy)
  (multiple-value-bind (x y) (cursor-position (stream-text-cursor stream))
    (let ((dx (or dx 0))
	  (dy (or dy 0)))
    (setf (cursor-position (stream-text-cursor stream)) (values (+ x dx) (+ y dy))))))



;;;

(defmethod handle-repaint :around ((stream standard-extended-output-stream)
                                   region)
  (declare (ignorable region))
  (let ((cursor (stream-text-cursor stream)))
    (if (cursor-state cursor)
	;; Erase the cursor so that the subsequent flip operation will make a
	;; cursor, whether or not the next-method erases the location of the
	;; cursor.
	;; XXX clip to region?  No one else seems to...
	;; Sure clip to region! --GB
	(letf (((cursor-state cursor) nil))
	  (call-next-method))
	(call-next-method))))

(defmethod scroll-vertical ((stream standard-extended-output-stream) dy)
  (multiple-value-bind (tx ty) (bounding-rectangle-position (sheet-region stream))
    (scroll-extent stream tx (+ ty dy))))

(defmethod scroll-horizontal ((stream standard-extended-output-stream) dx)
  (multiple-value-bind (tx ty) (bounding-rectangle-position (sheet-region stream))
    (scroll-extent stream (+ tx dx) ty)))

(defmacro with-cursor-off (stream &body body)
  `(letf (((cursor-visibility (stream-text-cursor ,stream)) nil))
     ,@body))

(defmethod stream-wrap-line ((stream standard-extended-output-stream))
  (let ((margin (stream-text-margin stream)))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (declare (ignore cx))
      (draw-rectangle* (sheet-medium stream) margin cy (+ margin 4) (+ cy (slot-value stream 'height))
		       :ink +foreground-ink+
		       :filled t)))
  (stream-write-char stream #\newline))



(defun seos-write-string (stream string &optional (start 0) end)
  (let* ((medium       (sheet-medium stream))
	 (text-style   (medium-text-style medium))
	 (new-baseline (text-style-ascent text-style medium))
	 (new-height   (text-style-height text-style medium))
	 (margin       (stream-text-margin stream))
	 (end          (or end (length string))))
    (flet ((find-split (delta)  ;; FIXME: This can be done smarter.
	     (loop for i from (1+ start) upto end
	       as sub-width = (stream-string-width stream string
						   :start start :end i
						   :text-style text-style)
	       while (<= sub-width delta)
	       finally (return (1- i)))))
  
      (with-slots (baseline height vspace) stream
	(multiple-value-bind (cx cy) (stream-cursor-position stream)
	  (when (> new-baseline baseline)
	    ;;(when (or (> baseline 0)
	    ;;          (> height 0))
	    ;;  (scroll-vertical stream (- new-baseline baseline))
	    ;;  ) ; the beginning of the line should be moved down, but not the whole stream -- APD, 2002-06-18
	    (setq baseline new-baseline))
	  (if (> new-height height)
	      (setq height new-height))
	  (let ((width (stream-string-width stream string
					    :start start :end end
					    :text-style text-style))
		(split end))
	    (when (>= (+ cx width) margin)
	      (ecase (stream-end-of-line-action stream)
		(:wrap
		 (setq split (find-split (- margin cx))))
		(:scroll
		 (scroll-horizontal stream width))
		(:allow)))
	    (unless (= start split)
	      (stream-write-output stream
				   string
                                   nil
				   start split)
	      (setq cx (+ cx width))	      
          (with-slots (x y) (stream-text-cursor stream)
                (setf x cx y cy)))
	    (when (/= split end)
	      (let ((current-baseline baseline))
		(setf baseline current-baseline))		
;		(stream-wrap-line stream)
;		(multiple-value-bind (new-cx new-cy) (stream-cursor-position stream)
;		  (setf cx new-cx
;			cy new-cy			
;			baseline current-baseline)
;		  (setf (stream-cursor-position stream) (values cx cy))))
	      (stream-wrap-line stream)
	      (seos-write-string stream string split end))
	    ))))))

(defun seos-write-newline (stream)
  (let ((medium       (sheet-medium stream))
	(%view-height (bounding-rectangle-height
		       (or (pane-viewport stream)
			   stream)))
	(view-height  (bounding-rectangle-height stream)))    
    (with-slots (baseline height vspace) stream
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
	(setf height (max height (text-style-height (medium-text-style medium) medium)))
	(setf cx 0
	      cy (+ cy height vspace))
	(when (> (+ cy height) view-height)
	  (ecase (stream-end-of-page-action stream)
	    ((:scroll :allow)
             (change-space-requirements stream
                                        :width  (bounding-rectangle-width stream)
                                        :height (+ cy height))
	     ;;(scroll-vertical stream (+ height vspace))
	     )
	    (:wrap
	     (setq cy 0))))
	(unless (eq :allow (stream-end-of-page-action stream))
	  (scroll-extent stream 0 (max 0 (- (+ cy height) %view-height))))
	
	;; mikemac says that this "erase the new line" behavior is
	;; required by the stream text protocol, but I don't see
	;; it.  I'm happy to put this back in again, but in the
	;; meantime it makes debugging of updating-output a bit easier
	;; not to have "extra" records laying around.  If/When it goes
	;; back in... the draw-rectangle has to happen on the stream,
	;; not the medium. -- moore
	#+nil(draw-rectangle* medium cx cy (+ margin 4) (+ cy height)
			      :ink +background-ink+
			      :filled t)
	(setq baseline 0
	      height   0)
	(setf (stream-cursor-position stream) (values cx cy))))))




(defgeneric stream-write-output (stream line string-width &optional start end)
  (:documentation
   "Writes the character or string LINE to STREAM. This function produces no
more than one line of output i.e., doesn't wrap. If STRING-WIDTH is
non-nil, that is used as the width where needed; otherwise
STREAM-STRING-WIDTH will be called."))

;;; The cursor is in stream coordinates.
(defmethod stream-write-output (stream line string-width
				&optional (start 0) end)
  (declare (ignore string-width))
  (with-slots (baseline vspace) stream
     (multiple-value-bind (cx cy) (stream-cursor-position stream)
       (draw-text* (sheet-medium stream) line
                   cx (+ cy baseline)
		   :transformation +identity-transformation+
		   :start start :end end))))

(defmethod stream-write-char ((stream standard-extended-output-stream) char)
  (with-cursor-off stream  
   (if (char= #\Newline char)
       (seos-write-newline stream)
     (seos-write-string stream (string char)))))

;;; I added the (subseq string seg-start ...) forms. Under ACL, there is some
;;; wierd interaction with FORMAT. This shows up as overwritten text in the
;;; pointer documentation and in menus. It acts like a shared buffer is being corrupted
;;; but I can't narrow it down. Using SUBSEQ does fix this interaction that's been
;;; here since 4/16/03 - Mikemac 12/6/2003
(defmethod stream-write-string ((stream standard-extended-output-stream) string
				&optional (start 0) end)
  (let ((seg-start start)
	(end (or end (length string))))
    (with-cursor-off stream
     (loop for i from start below end do
       (when (char= #\Newline
		    (char string i))
	 (seos-write-string stream (subseq string seg-start i))
	 (seos-write-newline stream)
	 (setq seg-start (1+ i))))
     (seos-write-string stream (subseq string seg-start end)))))

;(defmethod stream-write-string ((stream standard-extended-output-stream) string
;				&optional (start 0) end)
;  (if (null end)
;      (setq end (length string)))
;  (with-room-for-line
;      (loop for i from start below end
;	    for char = (aref string i)
;	    do (do-char))))

(defmethod stream-character-width ((stream standard-extended-output-stream) char &key (text-style nil))
  (with-sheet-medium (medium stream)
    (text-style-character-width (or text-style (medium-text-style medium))
                                medium
                                char)))

(defmethod stream-string-width ((stream standard-extended-output-stream) string
				&key (start 0) (end nil) (text-style nil))
  (with-sheet-medium (medium stream)
    (if (null text-style)
        (setq text-style (medium-text-style (sheet-medium stream))))
    (multiple-value-bind (total-width total-height final-x final-y baseline)
        (text-size medium string :text-style text-style
                   :start start :end end)
      (declare (ignore total-height final-y baseline))
      (values final-x total-width))))

(defmethod stream-text-margin ((stream standard-extended-output-stream))
  (with-slots (margin) stream
    (or margin
	(- (bounding-rectangle-width (or (pane-viewport stream)
                                         stream))
	   (text-size stream "O")))))

(defmethod stream-line-height ((stream standard-extended-output-stream)
                               &key (text-style nil))
  (+ (text-style-height (or text-style (medium-text-style (sheet-medium stream)))
			(sheet-medium stream))
     (stream-vertical-spacing stream)))

(defmethod stream-line-column ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (floor x (stream-string-width stream " "))))

(defmethod stream-start-line-p ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (zerop x)))

(defmacro with-room-for-graphics ((&optional (stream t)
                                   &rest arguments
				   &key (first-quadrant t)
                                        height
                                        (move-cursor t)
                                        (record-type ''standard-sequence-output-record))
                                  &body body)
  (declare (ignore first-quadrant height move-cursor record-type))
  (let ((cont (gensym "CONT."))
        (stream (stream-designator-symbol stream '*standard-output*)))
    `(labels ((,cont (,stream)
               ,@body))
      (declare (dynamic-extent #',cont))
      (invoke-with-room-for-graphics #',cont ,stream ,@arguments))))

(defmacro with-end-of-line-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-line-action ,stream) ,action))
     ,@body))

(defmacro with-end-of-page-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-page-action ,stream) ,action))
     ,@body))

(defmethod beep (&optional medium)
  (if medium
      (medium-beep medium)
      (when (sheetp *standard-output*)
        (medium-beep (sheet-medium *standard-output*)))))
  
(defmethod scroll-quantum ((sheet standard-extended-output-stream))
  (stream-line-height sheet))
