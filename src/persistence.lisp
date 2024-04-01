;;;; persistence.lisp

(in-package :mushub)

;; ----------------------------------------------------------------------------
;; PROJECT
;; ----------------------------------------------------------------------------

(defclass project ()
  ((uuid            :initarg   :uuid
                    :type      string
                    :initform  (generate-identifier)
                    :reader    project-uuid)
   (title           :initarg   :title
                    :type      string
                    :initform  ""
                    :reader    project-title
                    :writer    set-project-title)
   (last-modified   :initarg   :last-modified
                    :type      integer
                    :initform  (get-universal-time)
                    :reader    project-last-modified
                    :writer    set-project-last-modified)
   (created         :initarg   :created
                    :type      integer
                    :initform  (get-universal-time)
                    :reader    project-created)
   (tracks          :initarg   :tracks
                    :initform  nil
                    :reader    project-tracks
                    :writer    set-project-tracks))
  (:documentation
   "The basic structure for a MusHub project."))

(defmethod make-load-form ((instance project) &optional environment)
  (declare (ignore environment))
  `(make-instance 'project
                  :uuid           ,(project-uuid          instance)
                  :title          ,(project-title         instance)
                  :last-modified  ,(project-last-modified instance)
                  :created        ,(project-created       instance)
                  :tracks        ',(project-tracks        instance)))

;; ----------------------------------------------------------------------------
;; TRACK
;; ----------------------------------------------------------------------------

(defclass track ()
  ((uuid            :initarg   :uuid
                    :type      string
                    :initform  (generate-identifier)
                    :reader    track-uuid)
   (filename        :initarg   :filename
                    :type      string
                    :initform  ""
                    :reader    track-filename
                    :writer    set-track-filename)
   (instrument      :initarg   :instrument
                    :type      string
                    :initform  ""
                    :reader    track-instrument
                    :writer    set-track-instrument)
   (state           :initarg   :state
                    :type      string
                    :initform  ""
                    :reader    track-state
                    :writer    set-track-state)
   (last-modified   :initarg   :last-modified
                    :type      integer
                    :initform  (get-universal-time)
                    :reader    track-last-modified
                    :writer    set-track-last-modified)
   (created         :initarg   :created
                    :type      integer
                    :initform  (get-universal-time)
                    :reader    track-created)

   ;; Properties gathered through libsndfile.
   ;; -------------------------------------------------------------------------
   (frames          :initarg   :frames
                    :type      integer
                    :initform  0
                    :reader    track-frames
                    :writer    set-track-frames)
   (sample-rate     :initarg   :sample-rate
                    :type      integer
                    :initform  0
                    :reader    track-sample-rate
                    :writer    set-track-sample-rate)
   (channels        :initarg   :channels
                    :type      integer
                    :initform  0
                    :reader    track-channels
                    :writer    set-track-channels)
   (format          :initarg   :format
                    :type      string
                    :initform  ""
                    :reader    track-format
                    :writer    set-track-format)
   (sections        :initarg   :sections
                    :type      integer
                    :initform  0
                    :reader    track-sections
                    :writer    set-track-sections)
   (seekable        :initarg   :seekable
                    :type      boolean
                    :initform  nil
                    :reader    track-seekable
                    :writer    set-track-seekable)
   (title           :initarg   :title
                    :type      string
                    :initform  ""
                    :reader    track-title
                    :writer    set-track-title)
   (copyright       :initarg   :copyright
                    :type      string
                    :initform  ""
                    :reader    track-copyright
                    :writer    set-track-copyright)
   (software        :initarg   :software
                    :type      string
                    :initform  ""
                    :reader    track-software
                    :writer    set-track-software)
   (artist          :initarg   :artist
                    :type      string
                    :initform  ""
                    :reader    track-artist
                    :writer    set-track-artist)
   (comment         :initarg   :comment
                    :type      string
                    :initform  ""
                    :reader    track-comment
                    :writer    set-track-comment)
   (date            :initarg   :date
                    :type      string
                    :initform  ""
                    :reader    track-date
                    :writer    set-track-date)
   (album           :initarg   :album
                    :type      string
                    :initform  ""
                    :reader    track-album
                    :writer    set-track-album)
   (license         :initarg   :license
                    :type      string
                    :initform  ""
                    :reader    track-license
                    :writer    set-track-license)
   (track-number    :initarg   :track-number
                    :type      integer
                    :initform  0
                    :reader    track-track-number
                    :writer    set-track-track-number)
   (genre           :initarg   :genre
                    :type      string
                    :initform  ""
                    :reader    track-genre
                    :writer    set-track-genre)

   ;; Broadcast metadata (also through libsndfile)
   ;; -------------------------------------------------------------------------
   (description     :initarg   :description
                    :type      string
                    :initform  ""
                    :reader    track-description
                    :writer    set-track-description)
   (originator      :initarg   :originator
                    :type      string
                    :initform  ""
                    :reader    track-originator
                    :writer    set-track-originator)
   (origin-ref      :initarg   :origin-ref
                    :type      string
                    :initform  ""
                    :reader    track-origin-ref
                    :writer    set-track-origin-ref)
   (origin-date     :initarg   :origin-date
                    :type      string
                    :initform  ""
                    :reader    track-origin-date
                    :writer    set-track-origin-date)
   (origin-time     :initarg   :origin-time
                    :type      string
                    :initform  ""
                    :reader    track-origin-time
                    :writer    set-track-origin-time)
   (time-ref-low    :initarg   :time-ref-low
                    :type      string
                    :initform  ""
                    :reader    track-time-ref-low
                    :writer    set-track-time-ref-low)
   (time-ref-high   :initarg   :time-ref-high
                    :type      string
                    :initform  ""
                    :reader    track-time-ref-high
                    :writer    set-track-time-ref-high)
   (version         :initarg   :version
                    :type      string
                    :initform  ""
                    :reader    track-version
                    :writer    set-track-version)
   (umid            :initarg   :umid
                    :type      string
                    :initform  ""
                    :reader    track-umid
                    :writer    set-track-umid))
  (:documentation
   "The basic structure for a track inside a MusHub project."))

(defmethod make-load-form ((instance track) &optional environment)
  (declare (ignore environment))
  `(make-instance 'track
                  :uuid           ,(track-uuid          instance)
                  :filename       ,(track-filename      instance)
                  :instrument     ,(track-instrument    instance)
                  :state          ,(track-state         instance)
                  :uuid           ,(track-uuid          instance)
                  :last-modified  ,(track-last-modified instance)
                  :created        ,(track-created       instance)
                  :frames         ,(track-frames        instance)
                  :sample-rate    ,(track-sample-rate   instance)
                  :channels       ,(track-channels      instance)
                  :format         ,(track-format        instance)
                  :sections       ,(track-sections      instance)
                  :seekable       ,(track-seekable      instance)
                  :title          ,(track-title         instance)
                  :copyright      ,(track-copyright     instance)
                  :software       ,(track-software      instance)
                  :artist         ,(track-artist        instance)
                  :comment        ,(track-comment       instance)
                  :date           ,(track-date          instance)
                  :album          ,(track-album         instance)
                  :license        ,(track-license       instance)
                  :track-number   ,(track-track-number  instance)
                  :genre          ,(track-genre         instance)
                  :description    ,(track-description   instance)
                  :originator     ,(track-originator    instance)
                  :origin-ref     ,(track-origin-ref    instance)
                  :origin-date    ,(track-origin-date   instance)
                  :origin-time    ,(track-origin-time   instance)
                  :time-ref-low   ,(track-time-ref-low  instance)
                  :time-ref-high  ,(track-time-ref-high instance)
                  :version        ,(track-version       instance)
                  :umid           ,(track-umid          instance)))

(defun project-cons-track (instance track)
  (let ((tracks (project-tracks instance)))
    (set-project-tracks (cons track tracks) instance)))

(defun track-metadata-from-disk (base-pathname identifier)
  (let ((filename (merge-pathnames (concatenate 'string identifier ".lisp")
                                   base-pathname)))
    (with-open-file (handle filename :direction :input
                                     :if-does-not-exist nil)
      (if handle
          (eval (read handle))
          nil))))

(defun project-from-disk (base-pathname project-uuid)
  (let ((filename  (merge-pathnames (concatenate 'string project-uuid ".lisp")
                                    base-pathname)))
    (with-open-file (handle filename
                            :direction :input
                            :if-does-not-exist nil)
      (if handle
          (let ((metadata (eval (read handle))))
            (set-project-tracks (mapcar #'(lambda (track)
                                            (track-metadata-from-disk
                                             base-pathname track))
                                        (project-tracks metadata))
                                metadata)
            metadata)
          nil))))

(defun project-to-disk (base-pathname metadata)
  (with-open-file (handle (merge-pathnames (concatenate 'string
                                             (project-uuid metadata)
                                             ".lisp")
                                           base-pathname)
                          :direction :output
                          :if-exists :supersede)
    (let ((tracks (project-tracks metadata)))
      (set-project-tracks
       (mapcar #'track-uuid (project-tracks metadata)) metadata)
      (write (make-load-form metadata) :stream handle)
      (set-project-tracks tracks metadata))))

(defun track-metadata-to-disk (base-pathname metadata)
  (with-open-file (handle (merge-pathnames (concatenate 'string
                                             (track-uuid metadata)
                                             ".lisp")
                                           base-pathname)
                          :direction :output
                          :if-exists :supersede)
    (write (make-load-form metadata) :stream handle)))

(defun track->alist (instance)
  "Transform an INSTANCE of a TRACK into a association list."
  `((:uuid           . ,(track-uuid          instance))
    (:filename       . ,(track-filename      instance))
    (:instrument     . ,(track-instrument    instance))
    (:state          . ,(track-state         instance))
    (:uuid           . ,(track-uuid          instance))
    (:last-modified  . ,(track-last-modified instance))
    (:created        . ,(track-created       instance))
    (:frames         . ,(track-frames        instance))
    (:sample-rate    . ,(track-sample-rate   instance))
    (:channels       . ,(track-channels      instance))
    (:format         . ,(track-format        instance))
    (:sections       . ,(track-sections      instance))
    (:seekable       . ,(track-seekable      instance))
    (:title          . ,(track-title         instance))
    (:copyright      . ,(track-copyright     instance))
    (:software       . ,(track-software      instance))
    (:artist         . ,(track-artist        instance))
    (:comment        . ,(track-comment       instance))
    (:date           . ,(track-date          instance))
    (:album          . ,(track-album         instance))
    (:license        . ,(track-license       instance))
    (:track-number   . ,(track-track-number  instance))
    (:genre          . ,(track-genre         instance))
    (:description    . ,(track-description   instance))
    (:originator     . ,(track-originator    instance))
    (:origin-ref     . ,(track-origin-ref    instance))
    (:origin-date    . ,(track-origin-date   instance))
    (:origin-time    . ,(track-origin-time   instance))
    (:time-ref-low   . ,(track-time-ref-low  instance))
    (:time-ref-high  . ,(track-time-ref-high instance))
    (:version        . ,(track-version       instance))
    (:umid           . ,(track-umid          instance))))
