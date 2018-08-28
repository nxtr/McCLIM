(in-package :mcclim-render-internals)

(deftype pixeled-design-fn () '(function (fixnum fixnum) (values octet octet octet octet)))

(defparameter *pixeled-foreground-design* +black+)
(defparameter *pixeled-background-design* +white+)

;;;
;;; Pixeled Design
;;;
(defclass pixeled-design ()
  ((region :initarg :region :initform +everywhere+ :type region
           :accessor pixeled-design-region)
   (color-fn :initarg :color-fn :type pixeled-design-fn :reader pixeled-rgba-fn)))

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

(defun make-pixeled-functional-design (&key color-fn (region +everywhere+))
  (make-instance 'pixeled-design :color-fn color-fn :region region))

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
  (let ((design (climi::%collapse-pattern ink)))
    (make-pixeled-functional-design
     :color-fn (lambda (x y)
                 (%rgba->vals (climi::%pattern-rgba-value design x y))))))

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
                     (funcall design-fn (round x) (round y))))))))

(defmethod %make-pixeled-design ((ink transformed-design))
  (let ((design (%make-pixeled-design (transformed-design-design ink)))
        (transformation (invert-transformation (transformed-design-transformation ink))))
    (%transform-design design transformation)))

(defgeneric compose-in-rgba-design (ink mask)
  (:method (ink mask)
    (let ((region (region-intersection (pixeled-design-region ink)
                                       (pixeled-design-region mask))))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
                   (if (clim:region-contains-position-p region x y)
                       (multiple-value-bind (r1 g1 b1 a1)
                           (%rgba->vals (climi::%pattern-rgba-value ink x y))
                         (multiple-value-bind (r2 g2 b2 a2)
                             (%rgba->vals (climi::%pattern-rgba-value mask x y))
                           (declare (ignore r2 g2 b2))
                           (values r1 g1 b1 (octet-mult a1 a2))))
                       (values 0 0 0 0)))
       :region region))))

(defgeneric compose-out-rgba-design (ink mask)
  (:method (ink mask)
    (let ((region (pixeled-design-region ink)))
      (make-pixeled-functional-design
       :color-fn (lambda (x y)
                   (if (clim:region-contains-position-p region x y)
                       (multiple-value-bind (r1 g1 b1 a1)
                           (%rgba->vals (climi::%pattern-rgba-value ink x y))
                         (multiple-value-bind (r2 g2 b2 a2)
                             (%rgba->vals (climi::%pattern-rgba-value mask x y))
                           (declare (ignore r2 g2 b2))
                           (values r1 g1 b1 (octet-mult a1 (- 255 a2)))))
                       (values 0 0 0 0)))
       :region region))))

(defgeneric compose-over-rgba-design (fore back)
  (:method (fore back)
    (make-pixeled-functional-design
     :color-fn (lambda (x y)
                 (multiple-value-bind (r1 g1 b1 a1)
                     (%rgba->vals (climi::%pattern-rgba-value fore x y))
                   (multiple-value-bind (r2 g2 b2 a2)
                       (%rgba->vals (climi::%pattern-rgba-value back x y))
                     (multiple-value-bind (red green blue alpha)
                         (octet-blend-function
                          r2 g2 b2 a2 r1 g1 b1 a1)
                       (values red green blue alpha))))))))

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

(defmethod %make-pixeled-design ((ink climi::%rgba-pattern))
  (make-pixeled-functional-design
   :color-fn (lambda (x y)
               (%rgba->vals (climi::%pattern-rgba-value ink x y)))))
