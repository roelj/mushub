;;;; mushub.lisp

(in-package :mushub)

(defvar *days*   '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defvar *months* '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defvar *server-start-date*
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p timezone)
	    (get-decoded-time)
    (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT"
            (nth day-of-week *days*)
            date (nth (1- month) *months*) year
            (+ hour timezone (if dst-p -1 0)) minute second)))

(defmacro enable-cached-response ()
  '(progn
    (setf (hunchentoot:header-out :Last-Modified hunchentoot:*reply*) *server-start-date*)
    (setf (hunchentoot:header-out :Cache-Control hunchentoot:*reply*) "max-age=31536000")))

(defmacro html-handler (procedure uri body)
  `(easy-routes:defroute ,procedure (,uri) ()
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
     (setf (hunchentoot:header-out :server) *server-name*)
     (with-template-page ,body)))

(defmacro with-template-page (content)
  (let ((*print-pretty* t))
    `(spinneret:with-html-string
       (:doctype)
       (:html
        (:head (:title "Music Hub")
               (:link  :rel "icon"
                       :type "image/svg+xml"
                       :href "/static/images/logo-dark.svg")
               (:meta  :name "description" :content "Musician's Hub")
               (:meta  :name "keywords"    :content "mushub")
               (:meta  :name "author"      :content "Roel Janssen")
               (:link :rel "stylesheet" :type "text/css" :href "/css/main.css")
               (:style :type "text/css"
                       (:raw (cl-css:css *stylesheet*)))
               (:script :src "/scripts/jquery-3.7.1.min.js")
               (:script :src "/scripts/file-uploader.js")
        (:body
         (:div#wrapper
          (:div#header
           (:a :href "/" (:img :src "/static/images/logo.svg" :alt "Logo"))
           (:div#subheader
            (:form :action "/project" :method "post"
             (:input#song-code :type "text" :name "project-code")
             (:input#submit-btn :type "submit" :value "GO"))))
          (:div#content ,content)))))))

(defun respond-400 (message)
  (setf (hunchentoot:return-code*) 400)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:header-out :server) *server-name*)
  (json:encode-json-to-string `(("message" . ,message))))

(defun respond-500 (message)
  (setf (hunchentoot:return-code*) 500)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:header-out :server) *server-name*)
  (json:encode-json-to-string `(("message" . ,message))))

;; ERROR HANDLERS
;; ----------------------------------------------------------------------------

(defmacro define-error-handler (status-code body)
  `(defmethod hunchentoot:acceptor-status-message
       (acceptor (http-status-code (eql ,status-code)) &key)
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
     (setf (hunchentoot:header-out :server) *server-name*)
     (let ((*print-pretty* nil))
       (with-template-page ,body))))

(defun start-instance (port)
  "Returns a HUNCHENTOOT:EASY-ACCEPTOR instance."
  (let* ((access-log   (merge-pathnames "mushub-access.log"
                                        (user-homedir-pathname)))
         (messages-log (merge-pathnames "mushub-messages.log"
                                        (user-homedir-pathname)))
         (server  (make-instance
                   'easy-routes:easy-routes-acceptor
                   :port                    port
                   ;; :access-log-destination  access-log
                   ;; :message-log-destination messages-log 
                   )))

    (log:info "Starting web server at ~a" port)
    (hunchentoot:start server)

    ;; ERROR HANDLERS
    ;; ------------------------------------------------------------------------
    (define-error-handler 404
        (:div#content-wrapper
         (:h1 "Not found")
         (:p "Oops! This page cannot be found.")))

    (define-error-handler 405
        (:div#content-wrapper
         (:h1 "Method not allowed")
         (:p "This HTTP method is not allowed.")))

    (define-error-handler 403
      (:div#content-wrapper
       (:h1 "Not allowed")
       (:p "This action is not allowed.")))

    ;; IMAGES
    ;; ------------------------------------------------------------------------

    (easy-routes:defroute logo-svg ("/static/images/logo.svg") ()
      (setf (hunchentoot:content-type*) "image/svg+xml; charset=utf-8")
      (enable-cached-response)
      *logo-svg*)

    (easy-routes:defroute logo-dark-svg ("/static/images/logo-dark.svg") ()
      (setf (hunchentoot:content-type*) "image/svg+xml; charset=utf-8")
      (enable-cached-response)
      *logo-dark-svg*)

    (easy-routes:defroute logo-png ("/static/images/logo.png") ()
      (setf (hunchentoot:content-type*) "image/png")
      (enable-cached-response)
      *logo-png*)

    (easy-routes:defroute logo-dark-png ("/static/images/logo-dark.png") ()
      (setf (hunchentoot:content-type*) "image/png")
      (enable-cached-response)
      *logo-dark-png*)

    ;; FONTS
    ;; ------------------------------------------------------------------------

    (easy-routes:defroute firacode-medium-ttf ("/static/fonts/FiraCode-Medium.ttf") ()
      (setf (hunchentoot:content-type*) "font/ttf")
      (enable-cached-response)
      *firacode-medium-ttf*)

    (easy-routes:defroute firacode-medium-woff2 ("/static/fonts/FiraCode-Medium.woff2") ()
      (setf (hunchentoot:content-type*) "font/woff2")
      (enable-cached-response)
      *firacode-medium-woff2*)

    (easy-routes:defroute mavenpro-medium-ttf ("/static/fonts/MavenPro-Medium.ttf") ()
      (setf (hunchentoot:content-type*) "font/ttf")
      (enable-cached-response)
      *mavenpro-medium-ttf*)

    (easy-routes:defroute mavenpro-medium-woff2 ("/static/fonts/MavenPro-Medium.woff2") ()
      (setf (hunchentoot:content-type*) "font/woff2")
      (enable-cached-response)
      *mavenpro-medium-woff2*)

    ;; JAVASCRIPT DEPENDENCIES
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute jquery-3-7-1-min-js ("/scripts/jquery-3.7.1.min.js") ()
      (setf (hunchentoot:content-type*) "application/javascript; charset=utf-8")
      (enable-cached-response)
      *jquery-3-7-1-min-js*)

    ;; STYLESHEET
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute main-css ("/css/main.css") ()
      (setf (hunchentoot:content-type*) "text/css; charset=utf-8")
      (enable-cached-response)
      (cl-css:css *stylesheet*))

    ;; JAVASCRIPT FOR FILE UPLOADER
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute file-uploader ("/scripts/file-uploader.js") ()
      (setf (hunchentoot:content-type*) "application/javascript; charset=utf-8")
      (enable-cached-response)
      *file-uploader-js*)

    ;; MAIN PAGE
    ;; ------------------------------------------------------------------------
    (html-handler web-root "/"
     (list (:h1 "Music Hub")
           (:p "Welcome to MusicHub.")
           (:div.center
            (:form :action "/project" :method "post"
              (:input :type "hidden" :name "project-code" :value "")
              (:input.action-button :type "submit" :value "New song")))))

    ;; PROJECT REDIRECTOR
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute project ("/project" :method :post) ()
      (let ((code (hunchentoot:post-parameter "project-code")))
        (log:info "Client: ~a:~d"
                  (hunchentoot:real-remote-addr *request*)
                  (hunchentoot:remote-port *request*))
        (hunchentoot:redirect
         (format nil "/project/~a" (if (string= code "")
                                       (generate-identifier)
                                       code))
         :code 302
         :protocol :https)))

    ;; PROJECT PAGE
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute project-code ("/project/:code") ()
      (with-template-page
          (list (:h1 code)
                (:div#file-upload-field :class "upload-wrapper record-type-field"
                  (:input#file :type "file"
                               :name "file"
                               :multiple t
                               :aria-label "Upload file")
                  (:div#file-upload :class "upload-container"
                    (:h4 "Drag audio file(s) here")
                    (:p "Or click to open a file dialog."))))))

    ;; UPLOAD TRACK
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute project-upload-track ("/project/:code/upload-track"
                                                :method :post) ()
      (let* ((file-spec (hunchentoot:post-parameter "file")))
        (log:info "Parameters: ~a~%" file-spec))
      (respond-500 "Not implemented."))

    server))

(defun stop-instance (server)
  (hunchentoot:stop server))
