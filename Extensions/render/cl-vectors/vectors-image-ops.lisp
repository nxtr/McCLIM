(in-package :mcclim-render-internals)

;;;
;;; aa render functions
;;;

(defun aa-render-draw-fn (image clip-region design)
  (LET ((SET-FN (IMAGE-RGBA-SET-FN IMAGE))
        (BLEND-FN (IMAGE-RGBA-BLEND-FN IMAGE)))
    (declare (type image-rgba-blend-fn blend-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y)
               (type fixnum alpha))
      (when
          (or (null clip-region) (region-contains-position-p clip-region x y))
        (setf alpha (min (abs alpha) 255))
        (when (plusp alpha)
          (multiple-value-bind (r.fg g.fg b.fg a.fg)
              (%rgba->vals (climi::%pattern-rgba-value design x y))
            (if (> (octet-mult a.fg alpha) 250)
                (funcall set-fn x y r.fg g.fg b.fg a.fg)
                (funcall blend-fn x y r.fg g.fg b.fg
                         (octet-mult a.fg ALPHA)))))))))

(defun aa-render-draw-span-fn (image clip-region design)
  (let ((set-fn (image-rgba-set-fn image))
        (blend-fn (image-rgba-blend-fn image)))
    (declare (type image-rgba-set-fn set-fn)
             (type image-rgba-blend-fn blend-fn))
    (lambda (x1 x2 y alpha)
      (declare (type fixnum x1 x2 y alpha))
      (loop for x from x1 below x2
         do (when (or (null clip-region)
                      (region-contains-position-p clip-region x y))
              (setf alpha (min (abs alpha) 255))
              (when (plusp alpha)
                (multiple-value-bind (r.fg g.fg b.fg a.fg)
                    (%rgba->vals (climi::%pattern-rgba-value design x y))
                  (if (> (octet-mult a.fg alpha) 250)
                      (funcall set-fn x y r.fg g.fg b.fg #xff)
                      (funcall blend-fn x y r.fg g.fg b.fg
                               (octet-mult a.fg alpha))))))))))

(defun aa-render-xor-draw-fn (image clip-region design)
  (let ((blend-fn (image-rgba-xor-blend-fn image)))
    (declare (type image-rgba-xor-blend-fn blend-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (when (or (null clip-region)
                (region-contains-position-p clip-region x y))
        (setf alpha (min (abs alpha) 255))
        (when (plusp alpha)
          (multiple-value-bind (r.fg g.fg b.fg a.fg)
              (%rgba->vals (climi::%pattern-rgba-value design x y))
            (funcall blend-fn x y r.fg g.fg b.fg
                     (octet-mult a.fg alpha))))))))

(defun aa-render-xor-draw-span-fn (image clip-region design)
  (let ((blend-fn (image-rgba-xor-blend-fn image)))
    (declare (type image-rgba-xor-blend-fn blend-fn))
    (lambda (x1 x2 y alpha)
      (declare (type fixnum x1 x2 y alpha))
      (loop for x from x1 below x2
         do (when (or (null clip-region)
                      (region-contains-position-p clip-region x y))
              (setf alpha (min (abs alpha) 255))
              (when (plusp alpha)
                (multiple-value-bind (r.fg g.fg b.fg a.fg)
                    (%rgba->vals (climi::%pattern-rgba-value design x y))
                  (funcall blend-fn x y r.fg g.fg b.fg
                           (octet-mult a.fg alpha)))))))))

(defun aa-render-alpha-draw-fn (image clip-region)
  (let ((set-fn (image-gray-set-fn image)))
    (declare (type image-gray-set-fn set-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (when (or (null clip-region)
                (region-contains-position-p clip-region x y))
        (setf alpha (min (abs alpha) 255))
        (when (plusp alpha) (funcall set-fn x y alpha))))))

(defun aa-render-alpha-draw-span-fn (image clip-region)
  (let ((set-fn (image-gray-set-fn image)))
    (declare (type image-gray-set-fn set-fn))
    (lambda (x1 x2 y alpha)
      (declare (type fixnum x1 x2 y alpha))
      (loop for x from x1 below x2
         do (when (or (null clip-region)
                      (region-contains-position-p clip-region x y))
              (setf alpha (min (abs alpha) 255))
              (when (plusp alpha) (funcall set-fn x y alpha)))))))
