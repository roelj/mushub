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
                    #:uuid)
  :components      ((:module "src"
                     :components
                     ((:file "package")
                      (:file "utils")
                      (:file "fonts")
                      (:file "images")
                      (:file "javascript")
                      (:file "persistence")
                      (:file "sndfile")
                      (:file "stylesheet")
                      (:file "track-visualisation")
                      (:file "ui")
                      (:file "web-server"))))
  :build-operation "program-op"
  :build-pathname  "mushub"
  :entry-point     "mushub:main")
