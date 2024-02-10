;;;; ui.lisp

(in-package :mushub)

(defun show-usage (usage-of)
  "Show a usage message for USAGE-OF."
  (if (eq usage-of nil)
      (progn
	(format t "~a~%~%~a~%~%Available subcommands:~%  initialize~%  web~%~%~a~%"
		(concatenate 'string "mushub " *version*)
		"Usage: mushub [initialize|web] [-h|--help] [-v|--version]"
		"Report bugs to rrejanssen@gmail.com"))
      (opts:describe
       :prefix   (concatenate 'string "mushub " *version*)
       :suffix   "Report bugs to rrejanssen@gmail.com"
       :usage-of (concatenate 'string "mushub " usage-of))))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro with-option-handling (options free-args body)
  `(multiple-value-bind (,options ,free-args)
       (handler-case
           (handler-bind ((opts:unknown-option #'unknown-option))
             (opts:get-opts))

         ;; Message for a missing argument.
         (opts:missing-arg (condition)
           (format *error-output*
                   "Error: option ~s needs an argument.~%"
                   (opts:option condition)))

         ;; Message for a faulty argument.
         (opts:arg-parser-failed (condition)
           (format *error-output*
                   "Error: cannot parse ~s as argument of ~s~%"
                   (opts:raw-arg condition)
                   (opts:option condition)))

         ;; Message for missing a required option.
         (opts:missing-required-option (con)
           (format *error-output*
                   "Error: ~a~%" con)
           (opts:exit 1)))
     ,body))

(defun main-real ()
  "The function at which the program starts."

  (opts:define-opts
    (:name :help
     :description "This help text."
     :short #\h
     :long "help")
    (:name :version
     :description "Version information."
     :short #\v
     :long "version"))

  (let ((subcommand (cadr (opts:argv))))
    (cond
      ((string= subcommand "web")         (web-main))
      ((string= subcommand "initialize")  (initialize-main))
      (t
       (with-option-handling options free-args
         (progn
           (when (or (null options)
                     (getf options :help))
             (show-usage nil)
             (opts:exit 0))

           (when (getf options :version)
             (format *standard-output* "mushub ~a.~%" *version*)
             (opts:exit 0))))))))

(defun initialize-main ()
  "The function handling the 'initialize' subcommand options."

  (opts:define-opts
    (:name :help
     :description "This help text."
     :short #\h
     :long "help")
    (:name :test
     :description "Test option."
     :short #\t
     :long "test"))

  (with-option-handling options free-args
    (progn
      (when (or (null options)
                (getf options :help))
        (show-usage "initialize")
        (opts:exit 0))

      (when (getf options :test)
        (format *standard-output* "Argv: ~a~%" (opts:argv))))))

(defun web-main ()
  "The function handling the 'web' subcommand options."

  (opts:define-opts
    (:name :help
     :description "This help text."
     :short #\h
     :long "help")
    (:name :port
     :description "Port to start the server on."
     :short #\p
     :long "port"
     :arg-parser #'parse-integer
     :default 9000)
    (:name :storage-root
     :description "The storage location for uploaded files and metadata."
     :short #\d
     :long "storage-root"
     :arg-parser #'identity
     :default (truename #P".")))

  (with-option-handling options free-args
    (progn
      (when (getf options :help)
        (show-usage "web")
        (opts:exit 0))

      (log:info "Starting server.")
      (start-instance (getf options :port)
                      ;; (getf options :document-root)
                      )
      (loop (sleep 1)))))

(defun main ()
  "Wrapper for the main function that handles various signals."

  (handler-case (main-real)
    (#+allegro   excl:interrupt-signal
     #+ccl       ccl:interrupt-signal-condition
     #+clisp     system::simple-interrupt-condition
     #+ecl       ext:interactive-interrupt
     #+sbcl      sb-sys:interactive-interrupt
     #+lispworks mp:process-interrupt
     () (progn
          (format *standard-output* "~%")
          (log:info "Shutting down server.")
          (opts:exit 1)))))
