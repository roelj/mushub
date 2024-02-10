;;;; utils.lisp

(in-package :mushub)

(defun generate-identifier ()
  (string-downcase
   (format nil "~a" (uuid:make-v4-uuid))))

(defmacro assoc-ref (key alist)
  `(cdr (assoc ,key ,alist)))

(defvar *version* "0.0.1")
(defvar *server-name* "mushub")
