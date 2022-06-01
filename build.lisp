(require :ASDF)

(setf asdf:*central-registry*
      (list* '*default-pathname-defaults*
             (truename #P".")
             asdf:*central-registry*))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:make :mushub)

;; (require :mushub)
;; (sb-ext:save-lisp-and-die (merge-pathnames (truename #P".") #P"mushub")
;;   :toplevel    #'mushub:main     ; The function to execute.
;;   :executable  t                 ; Make a stand-alone executable.
;;   :compression 9                 ; Highest compression level.
;;   :purify      t                 ; Do an extra GC round before dumping.
;;   :save-runtime-options t)       ; Don't process SBCL's command-line arguments.
