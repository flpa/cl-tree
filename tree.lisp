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

(defun base-name (p)
  (or (and (directory-pathname-p p)
	   (car (last (pathname-directory p))))
      (pathname-name p)))

(let ((dir "/tmp/a") ;; will be param
      (isfirst t)
      (top-length nil))
  (fresh-line)
  (walk-directory dir
		  #'(lambda (x)
		      (if isfirst 
			  (or (format t " .~%")
			      (setf isfirst nil)
			      (setf top-length (length (pathname-directory x))))
			  (progn
			    (format t "~vT~a~a~%"
				    (* +indent+ (- (length (pathname-directory x))
						   top-length
						   (if (directory-pathname-p x) 1 0)))
				    +line-middle+
				    (base-name x)))))
		    :directories t
		    :test #'(lambda (x) (or *show-hidden*
					    (not (char-equal #\.
							     (aref (base-name x) 0)))))
    ))
