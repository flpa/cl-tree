;; cl-tree: Reimplementing Linux 'tree' command in Common Lisp
;; Copyright (C) 2015 Florian Patzl 
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :com.github.flpa.cl-tree)

(defparameter *show-hidden* nil)
(defparameter *line-middle* "├── ")
(defparameter *line-straight* "│   ")
(defparameter *line-end* "└── ")

;; TODO ... why do I need to do this?
(defun base-name (p)
  (or (and (directory-pathname-p p)
	   (car (last (pathname-directory p))))
      (if (pathname-type p)
	  (concatenate 'string (pathname-name p) "." (pathname-type p))
	  (pathname-name p))))

;; TODO: hidden handling

;; cmdline options?
;; http://www.cliki.net/unix-options
;; http://www.cliki.net/getopt

(defun walk-directory2 (dirname) 
  ;;   - root dir
  (let ((dircount -1) (filecount 0))
    (labels
	((walk (name prefixes)
	   (format t "~{~a~}~a~%"
		   prefixes
		   (if prefixes (base-name name) "."))
	   (if (directory-pathname-p name)
	       (incf dircount)
	       (incf filecount))
	   (when (directory-pathname-p name)
	     (let ((new-prefixes (if prefixes
				     (append `(,*line-straight*) prefixes)
				     `(,*line-middle*+)))
		   (children (list-sorted name)))
	       (when children
		 (dolist (x (butlast children)) (walk x new-prefixes))
		 (walk (car (last children))
		       (append (butlast new-prefixes) `(,*line-end*))))))))
      (fresh-line)
      (walk (pathname-as-directory dirname) nil)
      (format t "~%~a directories, ~a files" dircount filecount))))

;; TODO: by removing dots we blur the line between name and ending
(defun list-sorted (dirname)
  (sort (list-directory dirname) #'string< 
	:key #'(lambda (x) 
		 (remove-if #'(lambda(x) (char-equal #\. x))
			    (base-name x)))))

(walk-directory2 "/tmp/a")

(defun is-hidden (pathname)
  (or *show-hidden*
      (not (char-equal #\.
		       (aref (base-name pathname) 0)))))

