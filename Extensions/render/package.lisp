

(defpackage :mcclim-render
  (:nicknames #:clim-render)
  (:use)
  (:export
   ;; colors
   #:color->octets
   ;; image
   #:image
   #:draw-image*
   #:medium-draw-image*
   #:make-image-design
   #:image-pixels
   #:image-rgba-get-fn
   #:image-rgba-set-fn
   #:image-rgba-blend-fn
   #:image-gray-get-fn
   #:image-gray-set-fn
   #:image-gray-blend-fn
   #:image-gray-alpha-get-fn
   #:image-alpha-get-fn
   #:image-alpha-set-fn
   #:image-alpha-blend-fn
   ;; design
   #:make-pixeled-design
   ;; image ops
   #:make-image
   #:clone-image
   #:copy-image
   #:clone-alpha-channel
   #:copy-alpha-channel
   #:blend-image
   #:crop-image
   #:fill-image
   ;; two dimensional array image
   #:two-dim-array-image
   #:rgba-image
   #:gray-image))

(defpackage :mcclim-render-extensions
  (:use)
  (:export
   ;; colors
   #:octet
   #:color-octet-xor
   #:octet-mult
   #:octet-blend-function
   #:octet-rgba-blend-function
   #:octet-rgb-blend-function
   #:octet-gray-blend-function
   #:octet-alpha-blend-function
   #:color-value->octet
   #:color-octet->value
   #:rgba->rgb
   #:rgba->gray
   #:rgba->gray-alpha
   #:rgba->alpha
   #:rgb->rgba
   #:rgb->gray
   #:rgb->alpha
   #:gray->rgba
   #:gray->rgb
   #:gray->alpha
   ;; image
   #:image-design
   #:image-rgba-get-code
   #:image-rgba-set-code
   #:image-rgba-blend-code
   #:image-gray-get-code
   #:image-gray-set-code
   #:image-gray-alpha-get-code
   #:image-gray-blend-code
   #:image-alpha-get-code
   #:image-alpha-set-code
   #:image-alpha-blend-code
   ;; design
   #:pixeled-design
   #:pixeled-uniform-design
   #:pixeled-uniform-design-red
   #:pixeled-uniform-design-green
   #:pixeled-uniform-design-blue
   #:pixeled-uniform-design-alpha
   #:pixeled-flipping-design
   #:pixeled-rgba-fn
   #:pixeled-design-fn
   #:%make-pixeled-design))

(defpackage :mcclim-render-internals
  (:use #:clim #:clim-lisp #:mcclim-render #:mcclim-render-extensions)
  (:import-from :clim-internals
                #:standard-color
                #:named-color
                #:standard-flipping-ink
                #:%transparent-ink
                #:standard-opacity
                #:opacity-value
                #:pattern
                #:indexed-pattern
                #:rectangular-tile
                #:rectangular-tile-design
                #:transformed-design
                #:transformed-design-design
                #:transformed-design-transformation
                #:with-transformed-position
                #:in-compositum
                #:out-compositum
                #:over-compositum
                #:compositum-ink
                #:compositum-mask
                #:compositum-foreground
                #:compositum-background
                #:def-grecording
                #:defmethod*
                #:output-record-position
                #:defrecord-predicate
                #:with-standard-rectangle*
                #:coordinate=
                #:if-supplied
                ;; backend
                #:destroy-mirror
                #:realize-mirror
                #:mirrored-pixmap
                #:port-register-mirror
                #:port-lookup-mirror
                #:port-lookup-sheet
                #:pixmap-mirror
                #:pixmap-medium)
  (:import-from :mcclim-truetype
                #:truetype-font-size
                #:truetype-font-face
                #:glyph-pixarray
                #:ensure-gethash
                #:invoke-with-truetype-path-restart
                #:*truetype-font-path*
                #:*family-names*
                #:zpb-ttf-font-loader
                #:*zpb-font-lock*
                #:*fontconfig-faces*
                #:*families/faces*
                #:truetype-device-font-name
                #:fontconfig-font-name
                #:make-truetype-device-font-name
                #:make-fontconfig-font-name
                #:truetype-font-family
                #:truetype-font
                #:truetype-face
                #:truetype-font-size
                #:truetype-font-ascent
                #:truetype-font-descent
                #:zpb-ttf-font-units->pixels)
  (:import-from :clim-backend
                #:port-set-mirror-region
                #:port-set-mirror-transformation)
  (:export))
