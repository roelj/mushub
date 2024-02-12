;;;; package.lisp

(defpackage :mushub
  (:import-from :hunchentoot  :*request*
                              :content-type*
                              :define-easy-handler
                              :easy-acceptor
                              :header-out
                              :raw-post-data
                              :request-method
                              :return-code*
                              :start
                              :stop)
  (:import-from :cl-css       :css)
  (:import-from :cl-json      :encode-json-to-string)
  (:import-from :cl-svg       :make-svg-toplevel :make-group :draw*)
  (:import-from :spinneret    :with-html-string)
  (:import-from :log4cl       :info)
  (:import-from :easy-routes  :easy-routes-acceptor)
  (:import-from :uuid         :print-bytes :make-v4-uuid)
  (:import-from :parenscript  :chain :create :FALSE :ps :new)
  (:use :cl :cffi)
  (:export :main :start-instance :stop-instance :*version*))
