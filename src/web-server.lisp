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
    (setf (hunchentoot:header-out :Server) *server-name*)
    (setf (hunchentoot:header-out :Last-Modified hunchentoot:*reply*) *server-start-date*)
    (setf (hunchentoot:header-out :X-Content-Type-Options hunchentoot:*reply*) "nosniff")
    (setf (hunchentoot:header-out :Cache-Control hunchentoot:*reply*) "max-age=31536000, immutable")))

(defmacro disable-cached-response ()
  '(progn
    (setf (hunchentoot:header-out :Server) *server-name*)
    (setf (hunchentoot:header-out :X-Content-Type-Options hunchentoot:*reply*) "nosniff")
    (setf (hunchentoot:header-out :Cache-Control hunchentoot:*reply*) "no-cache")))

(defmacro html-handler (procedure uri body)
  `(easy-routes:defroute ,procedure (,uri) ()
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
     (setf (hunchentoot:header-out :Server) *server-name*)
     (with-template-page ,body)))

(defmacro with-template-page (content &optional scripts)
  (let ((*print-pretty* t))
    `(spinneret:with-html-string
       (:doctype)
       (:html :lang "en"
        (:head (:title "Music Hub")
               (:meta  :name "viewport"
                       :content "width=700")
               (:link  :rel "icon"
                       :type "image/svg+xml"
                       :href "/static/images/logo-dark.svg")
               (:meta  :name "description" :content "Musician's Hub")
               (:meta  :name "keywords"    :content "mushub")
               (:meta  :name "author"      :content "Roel Janssen")
               (:link :rel "stylesheet" :type "text/css" :href "/css/main.css")
               ,@(unless (null (position 'jquery scripts))
                   '((:script :src "/scripts/jquery-3.7.1.min.js")))
               ,@(unless (null (position 'file-uploader scripts))
                   '((:script :src "/scripts/file-uploader.js")))
               ,@(unless (null (position 'project scripts))
                   '((:script :src "/scripts/project.js"))))
        (:body
         (:div#wrapper
          (:div#header
           (:a :href "/" (:img :src "/static/images/logo.svg" :alt "Logo"))
           (:div#subheader
            (:form :action "/project" :method "post"
              (:input#song-code :type "text" :name "project-code"
                                :title "Project code"
                                :placeholder "")
              (:input#submit-btn :type "submit" :value "GO"))))
          (:div#content ,content)
          (:div#footer (:p ""))))))))

(defun respond-with-code (message code)
  (cond
    ((equal code 204)
     (setf (hunchentoot:return-code*) 204)
     (setf (hunchentoot:content-type*) nil)
     (setf (hunchentoot:header-out :Server) *server-name*)
     nil)
    (t
     (setf (hunchentoot:return-code*) code)
     (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
     (setf (hunchentoot:header-out :Server) *server-name*)
     (json:encode-json-to-string `(("message" . ,message))))))

(defun respond-400 (message) (respond-with-code message 400))
(defun respond-500 (message) (respond-with-code message 500))
(defun respond-406 (message) (respond-with-code message 406))
(defun respond-405 (message) (respond-with-code message 405))
(defun respond-404 (message) (respond-with-code message 404))

;; ERROR HANDLERS
;; ----------------------------------------------------------------------------

(defmacro define-error-handler (status-code body)
  `(defmethod hunchentoot:acceptor-status-message
       (acceptor (http-status-code (eql ,status-code)) &key)
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
     (setf (hunchentoot:header-out :server) *server-name*)
     (with-template-page ,body)))

(defun metadata-for-project (metadata-directory code)
  (let* ((filename (merge-pathnames (concatenate 'string code ".lisp")
                                    metadata-directory)))
    (handler-case
        (eval
         (with-open-file (handle filename
                                 :direction :input
                                 :if-does-not-exist :error)
           (read handle)))
      (file-error (error)
        (log:error error)
        nil))))

(defun page-project-code (metadata-directory code)
  (let ((metadata (metadata-for-project metadata-directory code)))
    (with-template-page
      (:form :id "project-form"
             (:div#last-modified (:p "Unsaved"))
             (:input :type "text"
                     :name "title"
                     :id "title"
                     :placeholder "Untitled"
                     :value (if (typep metadata 'project) (project-title metadata) ""))
             (:p (:strong :class "no-select" "Project identifier:")
                 (:code :id "project-uuid" code))
             (:p "")
             (:div#file-upload-field :class "upload-wrapper record-type-field"
              (:input#file :type "file"
                           :name "file"
                           :multiple t
                           :aria-label "Upload file")
              (:div#file-upload :class "upload-container no-select"
                (:h4 "Drag file(s) here")
                (:p "Or click to open a file dialog.")))
             (:div#tracks))
      (jquery file-uploader project))))

(defun page-project (metadata-directory)
  (let ((code (string-trim '(#\Space #\Tab #\Newline)
                           (hunchentoot:post-parameter "project-code"))))
    (if (string= code "")
        (let* ((metadata   (make-instance 'project))
               (identifier (project-uuid metadata))
               (filename   (merge-pathnames
                            (concatenate 'string identifier ".lisp")
                            metadata-directory)))
        (log:info "Creating project metadata file ~s." filename)
        (handler-case
            (progn
              (with-open-file (handle filename
                                      :direction :output
                                      :if-does-not-exist :create)
                (write (make-load-form metadata) :stream handle))
              (hunchentoot:redirect
               (concatenate 'string "/project/" identifier) :code 302))
          (file-error (error)
            (log:error error)
            (respond-500 (format nil "~a" error)))))
      (progn
        (log:info "Client: ~a:~d"
                  (hunchentoot:real-remote-addr *request*)
                  (hunchentoot:remote-port *request*))
        (hunchentoot:redirect (concatenate 'string "/project/" code)
                              :code 302)))))

(defun page-credits ()
  (with-template-page
    (list (:h1 "Software used to build this")
          (:table :id "credits-table"
           (:thead
            (:tr
             (:th "Software")
             (:th "Purpose")))
           (:tbody
            (:tr
             (:td (:a :href "https://lisp-lang.org/" "Common Lisp"))
             (:td "Programming environment."))
            (:tr
             (:td (:a :href "https://edicl.github.io/hunchentoot/" "hunchentoot"))
             (:td "Web server in Common Lisp."))
            (:tr
             (:td (:a :href "https://cffi.common-lisp.dev/" "CFFI"))
             (:td "Access libsndfile's C API."))
            (:tr
             (:td (:a :href "https://libsndfile.github.io/libsndfile/" "libsndfile"))
             (:td "Reading audio files for both metadata and audio signal."))))
          (:p ""))))

(defun api-update-project-metadata (metadata-directory code)
  "Implements the automatic form saving of the project page."
  (let ((method (hunchentoot:request-method hunchentoot:*request*)))
    (cond
      ((eq method :put)
       (let* ((post-data (json:decode-json-from-string
                          (hunchentoot:raw-post-data :force-text t)))
              (metadata  (project-from-disk metadata-directory code)))
         (if metadata
             (progn
               (set-project-title (assoc-ref :title post-data) metadata)
               (project-to-disk metadata-directory metadata)
               (respond-with-code nil 204))
             (let ((error-message (format nil "Could not find project ~a on disk" code)))
               (log:error error-message)
               (respond-500 error-message)))))
      (t
       (respond-405 "Only POST is allowed here.")))))

(defun api-project-tracks (metadata-directory code)
  (let* ((metadata (project-from-disk metadata-directory code)))
    (if metadata
        (json:encode-json-to-string
         (mapcar #'(lambda (instance) (track->alist (eval instance)))
                 (project-tracks metadata)))
        (let ((error-message (format nil "Could not find project ~a on disk" code)))
          (log:error error-message)
          (respond-500 error-message)))))

(defun download-track (tracks-directory uuid)
  (let ((filename (merge-pathnames uuid tracks-directory)))
    (setf (hunchentoot:header-out :Content-Disposition)
          (format nil "attachment; filename=~s" (concatenate 'string uuid ".wav")))
    (hunchentoot:handle-static-file filename)))

(defun page-upload-track (metadata-directory tracks-directory code)
  (let* ((file-spec         (hunchentoot:post-parameter "file"))
         (project-metadata  (project-from-disk metadata-directory code)))
    (cond
      ((not project-metadata)
            (log:info "Attempting to upload track to a non-existing project.")
            (respond-404 "The project this track would be part of does not exist."))
      ((and (typep file-spec 'list)
            (typep (nth 2 file-spec) 'string)
            (string= (subseq (nth 2 file-spec) 0 5) "audio"))
       (multiple-value-bind (handle metadata)
           (track-metadata (car file-spec))
         (let* ((uuid          (track-uuid metadata))
                (data-filename (merge-pathnames uuid tracks-directory))
                (svg-filename  (merge-pathnames (concatenate 'string uuid ".svg")
                                                tracks-directory))
                (data          (track-data metadata handle)))
           (set-track-filename (cadr file-spec) metadata)
           (project-cons-track project-metadata metadata)
           (project-to-disk metadata-directory project-metadata)
           (uiop:copy-file (car file-spec) data-filename)
           (with-open-file (svg-handle svg-filename
                                       :direction :output
                                       :if-exists :supersede)
             (log:info "Writing SVG to ~s" svg-filename)
             (waveform-svg data svg-handle))
           (json:encode-json-to-string
            (cons `(,:visual-uri . ,(format nil "/track/~a/preview.svg" uuid))
                  (track->alist metadata))))))
      ((and (typep file-spec 'list)
            (typep (nth 2 file-spec) 'string))
       (let ((filetype (nth 2 file-spec)))
         (log:info "User uploaded ~s." filetype)
         (respond-400 (format nil "'~a' is not a supported file type." filetype))))
      (t
       (log:info "Woops, file-spec set to: ~a." file-spec)
       (respond-406 "Please submit a 'multipart/form-data request'.")))))

(defun start-instance (port &optional (storage-root (user-homedir-pathname)))
  "Returns a HUNCHENTOOT:EASY-ACCEPTOR instance."
  (let* ((logs-directory     (merge-pathnames "logs/" storage-root))
         (tracks-directory   (merge-pathnames "tracks/" storage-root))
         (metadata-directory (merge-pathnames "metadata/" storage-root))
         (access-log         (merge-pathnames "access.log" logs-directory))
         (messages-log       (merge-pathnames "messages.log" logs-directory))
         (server             (make-instance
                              'easy-routes:easy-routes-acceptor
                              :address                 "0.0.0.0"
                              :port                    port
                              :access-log-destination  access-log
                              :message-log-destination messages-log)))
    (mapcar #'ensure-directories-exist
            (list tracks-directory
                  metadata-directory
                  logs-directory))
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

    ;; JAVASCRIPT
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute jquery-3-7-1-min-js ("/scripts/jquery-3.7.1.min.js") ()
      (setf (hunchentoot:content-type*) "application/javascript; charset=utf-8")
      (enable-cached-response)
      *jquery-3-7-1-min-js*)

    (easy-routes:defroute file-uploader ("/scripts/file-uploader.js") ()
      (setf (hunchentoot:content-type*) "application/javascript; charset=utf-8")
      (enable-cached-response)
      *file-uploader-js*)

    (easy-routes:defroute project-form-js ("/scripts/project.js") ()
      (setf (hunchentoot:content-type*) "application/javascript; charset=utf-8")
      (enable-cached-response)
      *project-form-js*)

    ;; STYLESHEET
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute main-css ("/css/main.css") ()
      (setf (hunchentoot:content-type*) "text/css; charset=utf-8")
      (enable-cached-response)
      (cl-css:css *stylesheet*))

    ;; MAIN PAGE
    ;; ------------------------------------------------------------------------
    (html-handler web-root "/"
     (list (:h1 "Music Hub")
           (:p "Welcome to this Music Hub.")
           (:p "This hub will use a " (:strong "sample rate of 48kHz") " and a "
               (:strong "sample format of 24-bits integers.")
               "Audio tracks with other sample rates or sample formats "
               "will be converted.")
           (:div.center
            (:form :action "/project" :method "post"
                   (:input :type "hidden" :name "project-code" :value ""
                           :title "Project code"
                           :placeholder "")
              (:input.action-button :type "submit" :value "New song")))))

    (easy-routes:defroute credits ("/credits") ()
      (page-credits))

    ;; PROJECT
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute project ("/project" :method :post) ()
      (disable-cached-response)
      (page-project metadata-directory))

    (easy-routes:defroute project-code ("/project/:code") ()
      (disable-cached-response)
      (page-project-code metadata-directory code))

    (easy-routes:defroute api-project-metadata ("/api/v1/project/:code" :method :put) ()
      (disable-cached-response)
      (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
      (api-update-project-metadata metadata-directory code))

    (easy-routes:defroute api-project-tracks-sym ("/api/v1/:code/tracks" :method :get) ()
      (disable-cached-response)
      (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
      (api-project-tracks metadata-directory code))

    ;; UPLOAD TRACK
    ;; ------------------------------------------------------------------------
    (easy-routes:defroute project-upload-track ("/project/:code/upload-track"
                                                :method :post) ()
      (disable-cached-response)
      (page-upload-track metadata-directory tracks-directory code))

    (easy-routes:defroute download-track-sym ("/track/:track-uuid/download"
                                              :method :get) ()
      (download-track tracks-directory track-uuid))

    (easy-routes:defroute project-track-svg ("/track/:track-uuid/preview.svg"
                                             :method :get) ()
      (setf (hunchentoot:content-type*) "image/svg+xml; charset=utf-8")
      (enable-cached-response)
      (alexandria:read-file-into-string
       (merge-pathnames (concatenate 'string track-uuid ".svg")
                        tracks-directory)))
    server))

(defun stop-instance (server)
  (hunchentoot:stop server))
