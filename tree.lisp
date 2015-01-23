(in-package :com.github.flpa.cl-tree)

;; TODO: should be uppercased?
(defparameter *show-hidden* nil)
(defconstant +indent+ 4)

; test needs general way of getting folder or filename: pathname-name only returns
; file names?

(let ((indent (- 0 +indent+))) ;; TODO: hacky or good?
  (walk-directory "/tmp/a"
			 #'(lambda (x)
			     (if (directory-pathname-p x)
				 (progn
				   (incf indent 4)
				   (format t "~vTdas dir ~a~%" indent x))
				 (format t "~vT├── ~a~%"
					 indent
					 (pathname-name x)
					 )))
			 :directories t))
