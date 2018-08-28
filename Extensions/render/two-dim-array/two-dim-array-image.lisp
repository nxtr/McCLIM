(in-package :mcclim-render-internals)

;;;
;;; Two dimensional array of pixels
;;;
(defclass two-dim-array-image (image)
  ())

;;;
;;; RGBA
;;;
(defclass rgba-image (two-dim-array-image image-mixin)
  ())

(defmethod image-rgba-set-fn ((image rgba-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type (simple-array (unsigned-byte 32) 2) pixels))
    (lambda (x y red green blue &optional (alpha 255))
      (declare (type fixnum x y red green blue alpha))
      (setf (aref pixels (+ y dy) (+ x dx))
            (dpb red (byte 8 24)
                 (dpb green (byte 8 16)
                      (dpb blue (byte 8 8) (dpb alpha (byte 8 0) 0))))))))

(defmethod image-rgba-blend-fn ((image rgba-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type (simple-array (unsigned-byte 32) 2) pixels))
    (lambda (x y red green blue &optional (alpha 255))
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r2 g2 b2 a2)
          (let ((p (aref pixels (+ y dy) (+ x dx))))
            (values (ldb (byte 8 24) p) (ldb (byte 8 16) p) (ldb (byte 8 8) p)
                    (ldb (byte 8 0) p)))
        (multiple-value-bind (red green blue alpha)
            (octet-rgba-blend-function red green blue alpha r2 g2 b2 a2)
          (setf (aref pixels (+ y dy) (+ x dx))
                (dpb red (byte 8 24)
                     (dpb green (byte 8 16)
                          (dpb blue (byte 8 8)
                               (dpb alpha (byte 8 0) 0))))))))))

(defmethod image-rgba-xor-blend-fn ((image rgba-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type (simple-array (unsigned-byte 32) 2) pixels))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r2 g2 b2 a2)
          (let ((p (aref pixels (+ y dy) (+ x dx))))
            (values (ldb (byte 8 24) p) (ldb (byte 8 16) p) (ldb (byte 8 8) p)
                    (ldb (byte 8 0) p)))
        (multiple-value-bind (red green blue alpha)
            (octet-rgba-blend-function (color-octet-xor r2 red)
                                       (color-octet-xor g2 green)
                                       (color-octet-xor b2 blue)
                                       (color-octet-xor a2 alpha) r2 g2 b2 a2)
          (setf (aref pixels (+ y dy) (+ x dx))
                (dpb red (byte 8 24)
                     (dpb green (byte 8 16)
                          (dpb blue (byte 8 8)
                               (dpb alpha (byte 8 0) 0))))))))))


(defmethod image-gray-set-fn ((image rgba-image) &key (dx 0) (dy 0))
   (let ((pixels (image-pixels image)))
     (lambda (x y gray)
       (declare (type fixnum x y gray))
       (setf (aref pixels (+ y dy) (+ x dx)) gray))))
