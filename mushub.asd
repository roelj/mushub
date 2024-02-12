;;;; mushub.asd

(asdf:defsystem    #:mushub
  :description     "Musician's Hub"
  :author          "Roel Janssen <rrejanssen@gmail.com>"
  :license         "AGPLv3+"
  :version         "0.0.1"
  :serial          t
  :depends-on      (#:cffi
                    #:cl-css
                    #:cl-json
                    #:cl-svg
                    #:easy-routes
                    #:hunchentoot
                    #:log4cl
                    #:parenscript
                    #:spinneret
                    #:unix-opts
                    #:uuid
                    #:ironclad)
  :components      ((:module "src"
                     :components
                     ((:file "package")
                      (:file "utils")
                      (:file "images")
                      (:file "javascript")
                      (:file "fonts")
                      (:file "persistence")
                      (:file "sndfile")
                      (:file "stylesheet")
                      (:file "track-visualisation")
                      (:file "web-server")
                      (:file "ui"))))
  :build-operation "program-op"
  :build-pathname  "mushub"
  :entry-point     "mushub:main")
