;;;
;;; Copyright (c) 2016 Daniel Kochmański
;;;

(in-package :clim-internals)

(defmacro define-opticl-reader (name opticl-reader)
  `(defun ,name (image-pathname)
     (let* ((img (handler-case
                     (,opticl-reader image-pathname)
                   (error ()
                     (error 'unsupported-bitmap-format)))))
       (convert-opticl-img img))))

(define-opticl-reader opticl-read-bitmap-file opticl:read-image-file)
(define-opticl-reader opticl-read-gif-file opticl:read-gif-file)
(define-opticl-reader opticl-read-jpg-file opticl:read-jpeg-file)
(define-opticl-reader opticl-read-pbm-file opticl:read-pbm-file)
(define-opticl-reader opticl-read-pgm-file opticl:read-pgm-file)
(define-opticl-reader opticl-read-png-file opticl:read-png-file)
(define-opticl-reader opticl-read-pnm-file opticl:read-pnm-file)
(define-opticl-reader opticl-read-ppm-file opticl:read-ppm-file)
(define-opticl-reader opticl-read-tiff-file opticl:read-tiff-file)

#+ (or)
(defmacro define-opticl-image-file-writer (format fn)
  `(define-image-file-writer ,format (image destination)
     (let ((img (coerce-image image 'opticl-rgb-image)))
       (if ,fn
           (if (streamp destination)
               (funcall ,fn destination (image-pixels img))
               (opticl:write-image-file destination (image-pixels img)))
           (error "Cannot write image to: ~S" destination)))))

#+ (or)
(progn
  (define-opticl-image-file-writer :tiff #'opticl:write-tiff-stream)
  (define-opticl-image-file-writer :tif #'opticl:write-tiff-stream)
  (define-opticl-image-file-writer :jpeg #'opticl:write-jpeg-stream)
  (define-opticl-image-file-writer :jpg #'opticl:write-jpeg-stream)
  (define-opticl-image-file-writer :png #'opticl:write-png-stream)
  (define-opticl-image-file-writer :pbm #'opticl:write-pbm-stream)
  (define-opticl-image-file-writer :pgm #'opticl:write-pgm-stream)
  (define-opticl-image-file-writer :gif #'opticl:write-gif-stream))


(setf (gethash :fallback climi::*bitmap-file-readers*)
      #'opticl-read-bitmap-file)

(defun convert-opticl-img (img)
  "Converts opticl image format to RGBA array."
  (let* ((height (array-dimension img 0))
         (width (array-dimension img 1))
         (array (make-array (list height width)
                            :element-type '(unsigned-byte 32))))
    (etypecase img
      (opticl:gray-image
       (opticl:do-pixels (y x) img
         (let ((v (aref img y x)))
           (setf (aref array y x)
                 (dpb v (byte 8 24)
                      (dpb v (byte 8 16)
                           (dpb v (byte 8 8)
                                #xff)))))))

      (opticl:gray-alpha-image
       (opticl:do-pixels (y x) img
         (let ((v (aref img y x 0)))
           (setf (aref array y x)
                 (dpb v (byte 8 24)
                      (dpb v (byte 8 16)
                           (dpb v (byte 8 8)
                                (aref img y x 1))))))))

      (opticl:rgb-image
       (opticl:do-pixels (y x) img
         (setf (aref array y x)
               (dpb (aref img y x 0) (byte 8 24)
                    (dpb (aref img y x 1) (byte 8 16)
                         (dpb (aref img y x 2) (byte 8 8)
                              #xff))))))

      (opticl:rgba-image
       (opticl:do-pixels (y x) img
         (setf (aref array y x)
               (dpb (aref img y x 0) (byte 8 24)
                    (dpb (aref img y x 1) (byte 8 16)
                         (dpb (aref img y x 2) (byte 8 8)
                              (aref img y x 3))))))))
    array))


;;; Bitmap images
;;;
;;; Based on CLIM 2.2, with an extension permitting the definition of
;;; new image formats by the user.

(define-bitmap-file-reader :xpm (pathname)
  (xpm-parse-file pathname))

(define-bitmap-file-reader :pixmap (pathname)
  (read-bitmap-file pathname :format :xpm))

(define-bitmap-file-reader :pixmap-3 (pathname)
  (read-bitmap-file pathname :format :xpm))

(define-bitmap-file-reader :gif (pathname)
  (opticl-read-gif-file pathname))

(define-bitmap-file-reader :jpg (pathname)
  (opticl-read-jpg-file pathname))

(define-bitmap-file-reader :jpeg (pathname)
  (opticl-read-jpg-file pathname))

(define-bitmap-file-reader :pbm (pathname)
  (opticl-read-pbm-file pathname))

(define-bitmap-file-reader :pgm (pathname)
  (opticl-read-pgm-file pathname))

(define-bitmap-file-reader :png (pathname)
  (opticl-read-png-file pathname))

(define-bitmap-file-reader :pnm (pathname)
  (opticl-read-pnm-file pathname))

(define-bitmap-file-reader :ppm (pathname)
  (opticl-read-ppm-file pathname))

(define-bitmap-file-reader :tiff (pathname)
  (opticl-read-tiff-file pathname))
