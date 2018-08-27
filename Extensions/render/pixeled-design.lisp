(in-package :mcclim-render-internals)

(deftype pixeled-design-fn () '(function (fixnum fixnum) (values octet octet octet octet)))

(defparameter *pixeled-foreground-design* +black+)
(defparameter *pixeled-background-design* +white+)

;;;
;;; Pixeled Design
;;;
(defclass pixeled-design ()
  ((region :initarg :region :initform +everywhere+ :type region
           :accessor pixeled-design-region)))

(defgeneric pixeled-rgba-fn (design))
(defgeneric pixeled-rgba-unsafe-fn (design))

(defmethod pixeled-rgba-fn :around (design)
  (with-slots (region)
      design
    (if (region-equal region +everywhere+)
        (pixeled-rgba-unsafe-fn design)
        (call-next-method))))

(defmethod climi::%pattern-rgba-value ((pattern pixeled-design) x y)
  (multiple-value-bind (red green blue alpha) (funcall (pixeled-rgba-fn pattern) x y)
    (dpb red (byte 8 24)
         (dpb green (byte 8 16)
              (dpb blue (byte 8 8)
                   (or alpha 255))))))

(defmethod pixeled-design-region ((object pattern))
  (make-bounding-rectangle 0 0 (pattern-width object) (pattern-height object)))

;;;
;;; Functiona Design
;;;
(defclass pixeled-functional-design (pixeled-design)
  ((color-fn :initarg :color-fn :type pixeled-design-fn)))

(defun make-pixeled-functional-design (&key color-fn (region +everywhere+))
  (make-instance 'pixeled-functional-design :color-fn color-fn :region region))

(defmethod pixeled-rgba-fn ((design pixeled-functional-design))
  (with-slots (color-fn region)
      design
    (declare (type pixeled-design-fn color-fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (clim:region-contains-position-p region x y)
          (funcall color-fn x y)
          (values 0 0 0 0)))))

(defmethod pixeled-rgba-unsafe-fn ((design pixeled-functional-design))
  (with-slots (color-fn region)
      design
    (declare (type pixeled-design-fn color-fn))
    color-fn))

;;;
;;; Flippend Design
;;;
(defclass pixeled-flipping-design (pixeled-functional-design)
  ())

(defun make-pixeled-flipping-design (&key color-fn (region +everywhere+))
  (make-instance 'pixeled-flipping-design :color-fn color-fn :region region))

;;;
;;; Image Design
;;;
(defclass pixeled-image-design (pixeled-design)
  ((image :initarg :image :initform nil
          :accessor pixeled-image-design-image)
   (dx :initarg :dx :initform 0 :type fixnum
       :accessor pixeled-image-design-dx)
   (dy :initarg :dy :initform 0 :type fixnum
       :accessor pixeled-image-design-dy)))

(defun make-pixeled-image-design (&key (image nil))
  (make-instance 'pixeled-image-design
                 :image image
                 :region (make-rectangle* 0 0
                                          (1- (pattern-width image))
                                          (1- (pattern-height image)))))

(defmethod  pixeled-rgba-fn ((design pixeled-image-design))
  (with-slots (image dx dy region)
      design
    (image-rgba-get-fn image :dx dx :dy dy :region region)))

(defmethod  pixeled-rgba-unsafe-fn ((design pixeled-image-design))
  (with-slots (image dx dy region)
      design
    (image-rgba-get-fn image :dx dx :dy dy :region nil)))

;;;
;;; Make a pixeled design
;;;
(defgeneric %make-pixeled-design (design))

(defun make-pixeled-design (design &key foreground background)
  (let ((*pixeled-foreground-design* (or foreground *pixeled-foreground-design*))
        (*pixeled-background-design* (or background *pixeled-background-design*)))
    (typecase design
      ((or color opacity climi::uniform-compositum) design)
      (climi::indirect-ink
       (cond ((eql design +foreground-ink+) *pixeled-foreground-design*)
             ((eql design +background-ink+) *pixeled-background-design*)
             (T design)))
      (otherwise (%make-pixeled-design design)))))

(defmethod %make-pixeled-design (ink)
  (error "unknow how to make an rgba design of the ~A" ink))

(defun make-flipping-fn (design1 design2)
  (declare (ignore design1 design2))
  +dark-red+)

(defmethod %make-pixeled-design ((ink (eql +flipping-ink+)))
  (make-flipping-fn *pixeled-background-design* *pixeled-foreground-design*))

(defmethod %make-pixeled-design ((ink standard-flipping-ink))
  (with-slots (climi::design1 climi::design2) ink
    (make-flipping-fn climi::design1 climi::design2)))

(defmethod %make-pixeled-design ((ink indexed-pattern))
  (make-pixeled-image-design :image (climi::%collapse-pattern ink)))

(defmethod %make-pixeled-design ((ink rectangular-tile))
  (let ((design (climi::%collapse-pattern ink)))
    (make-pixeled-functional-design
     :color-fn (lambda (x y)
                 (%rgba->vals
                  (climi::%pattern-rgba-value design
                                              (mod x (pattern-width design))
                                              (mod y (pattern-height design))))))))

(defgeneric %transform-design (design transformation)
  (:method (design transformation)
    (let ((design-fn (pixeled-rgba-fn design)))
      (declare (type pixeled-design-fn design-fn))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
                   (declare (type fixnum x y))
                   (with-transformed-position (transformation x y)
                     (funcall design-fn (round x) (round y)))))))
  (:method ((design pixeled-image-design) (transformation climi::standard-translation))
    (with-slots (dx dy region)
        design
      (multiple-value-bind (x0 y0)
          (transform-position transformation dx dy)
        (with-bounding-rectangle* (x1 y1 x2 y2)
            (transform-region (invert-transformation transformation) region)
          (setf dx (round x0))
          (setf dy (round y0))
          (setf region (make-rectangle* (round x1) (round y1) (round x2) (round y2))))))
    design))

(defmethod %make-pixeled-design ((ink transformed-design))
  (let ((design (%make-pixeled-design (transformed-design-design ink)))
        (transformation (invert-transformation (transformed-design-transformation ink))))
    (%transform-design design transformation)))

(defgeneric compose-in-rgba-design (ink mask)
  (:method ((ink pixeled-design) (mask pixeled-design))
    (let ((mask-fn (pixeled-rgba-fn mask))
	  (ink-fn (pixeled-rgba-fn ink)))
      (declare (type pixeled-design-fn ink-fn mask-fn))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
		   (multiple-value-bind (r1 g1 b1 a1)
		       (funcall ink-fn x y)
		     (multiple-value-bind (r2 g2 b2 a2)
			 (funcall mask-fn x y)
		       (declare (ignore r2 g2 b2))
		       (values r1 g1 b1 (octet-mult a1 a2)))))
       :region (region-intersection (pixeled-design-region ink)
                                    (pixeled-design-region mask))))))

(defgeneric compose-out-rgba-design (ink mask)
  (:method ((ink pixeled-design) (mask pixeled-design))
    (let ((mask-fn (pixeled-rgba-fn mask))
	  (ink-fn (pixeled-rgba-fn ink)))
      (declare (type pixeled-design-fn ink-fn mask-fn))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
		   (multiple-value-bind (r1 g1 b1 a1)
		       (funcall ink-fn x y)
		     (multiple-value-bind (r2 g2 b2 a2)
			 (funcall mask-fn x y)
		       (declare (ignore r2 g2 b2))
		       (values r1 g1 b1 (octet-mult a1 (- 255 a2))))))
       :region (pixeled-design-region ink)))))

(defgeneric compose-over-rgba-design (fore back)
  (:method ((fore pixeled-design) (back pixeled-design))
    (let ((fore-fn (pixeled-rgba-fn fore))
	  (back-fn (pixeled-rgba-fn back)))
      (declare (type pixeled-design-fn fore-fn back-fn))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
                   (multiple-value-bind (r1 g1 b1 a1)
		       (funcall fore-fn x y)
		     (multiple-value-bind (r2 g2 b2 a2)
			 (funcall back-fn x y)
		         (multiple-value-bind (red green blue alpha)
			     (octet-blend-function
			      r2 g2 b2 a2 r1 g1 b1 a1)
			   (values red green blue alpha)))))))))

(defmethod %make-pixeled-design ((ink in-compositum))
  (let ((c-ink (make-pixeled-design (compositum-ink ink)))
	(c-mask (make-pixeled-design (compositum-mask ink))))
    (compose-in-rgba-design c-ink c-mask)))

(defmethod %make-pixeled-design ((ink out-compositum))
  (let ((c-ink (make-pixeled-design (compositum-ink ink)))
	(c-mask (make-pixeled-design (compositum-mask ink))))
    (compose-out-rgba-design c-ink c-mask)))

(defmethod %make-pixeled-design ((ink over-compositum))
  (let ((c-fore (make-pixeled-design (compositum-foreground ink)))
	(c-back (make-pixeled-design (compositum-background ink))))
    (compose-over-rgba-design c-fore c-back)))

(defmethod %make-pixeled-design ((ink image-design))
  (let* ((img (slot-value ink 'image)))
    (make-pixeled-image-design :image img)))

(defmethod %make-pixeled-design ((ink climi::%rgba-pattern))
  (make-instance 'pixeled-image-design
                 :image ink
                 :region (make-rectangle* 0 0
                                          (1- (pattern-width ink))
                                          (1- (pattern-height ink)))))
