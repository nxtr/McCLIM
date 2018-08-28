(cl:in-package #:asdf-user)

(defsystem #:mcclim-render
  :description "Support for raster images McCLIM."
  :depends-on (#:mcclim-render/core))

(defsystem #:mcclim-render/core
  :depends-on (#:clim-basic
               #:mcclim-image
               #:mcclim-fonts/truetype)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "color")
               (:file "image")
               (:file "pixeled-design")))

(defsystem #:mcclim-render/render
    :depends-on (#:mcclim-render/core #:cl-vectors)
    :components
    ((:module "render"
              :serial t
              :components
              ((:file "prim-arc")
               (:file "prim-text")))))

(defsystem #:mcclim-render/cl-vectors
  :depends-on (#:clim-basic
               #:mcclim-fonts/truetype
               #:mcclim-render/render)
    :components
    ((:module "cl-vectors"
              :serial t
              :components
              ((:file "vectors")
               (:file "vectors-image-ops")))))

(defsystem #:mcclim-render/backend
    :depends-on (#:mcclim-render/cl-vectors)
    :components
    ((:module "backend"
              :serial t
              :components
              ((:file "mirror")
               (:file "mirrored-sheet")
               (:file "pixmap")
               (:file "medium")
               (:file "fonts")
               (:file "port")))))

(defsystem #:mcclim-render/clx
    :depends-on (#:mcclim-clx
                 #:mcclim-render/backend))
