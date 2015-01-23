(in-package :com.github.flpa.cl-tree)

(defparameter *show-hidden* nil)

; test needs general way of getting folder or filename: pathname-name only returns
; file names?
(walk-directory "/tmp/a"
			 #'(lambda (x)
			     (if (directory-pathname-p x)
				 (format t "das dir ~a~%" x)
				 (format t "├── ~a~%"
				     (pathname-name x)
				     )))
			 :directories t)
