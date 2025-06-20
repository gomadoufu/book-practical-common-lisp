(in-package :cl-user)
(defpackage :jp.ac.dendai.im.cps.yoshiki.spam
            (:use :common-lisp :jp.ac.dendai.im.cps.yoshiki.pathnames))

(in-package :jp.ac.dendai.im.cps.yoshiki.spam)

(defun classify (text)
  (classification (score (extract-features text))))
