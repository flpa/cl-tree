(in-package :com.github.flpa.cl-tree)

;; TODO: should be uppercased?
(defparameter *show-hidden* nil)
(defconstant +indent+ 4)
(defconstant +line-middle+ "├── ")
(defconstant +line-end+ "└── ")

; test needs general way of getting folder or filename: pathname-name only returns
; file names?


;; how to peek for correctly printing last

;;missing file ext?

;; push dirs on stack ? does that help?

(let ((indent (- 0 +indent+));; TODO: hacky or good?
      (next-print nil)      ;; next name to print 
      (dir "/tmp/a") ;; will be param
      (isfirst t))
  (fresh-line)
  (flet ((print-entry ()
	   (when next-print (format t "~vT~a~a~%" indent
				    +line-middle+ next-print))))
    (walk-directory dir
		    #'(lambda (x)
			(if isfirst 
			    (or (format t ".~%")
				 (setf isfirst nil))
			    (progn
			      (print-entry)
			      (setf next-print 
				    (or (and (directory-pathname-p x)
					     (incf indent +indent+)
					     (car (last (pathname-directory x))))
					(pathname-name x))))))
		    :directories t)
    (print-entry)))
