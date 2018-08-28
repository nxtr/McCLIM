(in-package :mcclim-render-internals)

;; rgba
(deftype image-rgba-set-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgba-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgba-xor-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-gray-set-fn () '(function (fixnum fixnum octet octet)))

;; alpha
(defgeneric image-rgba-set-fn (image &key dx dy))
(defgeneric image-rgba-blend-fn (image &key dx dy))
(defgeneric image-rgba-xor-blend-fn (image &key dx dy))
(defgeneric image-gray-set-fn (image &key dx dy))
