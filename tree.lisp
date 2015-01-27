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

(defparameter *line-middle* "├── ")
(defparameter *line-straight* "│   ")
(defparameter *line-end* "└── ")

(defparameter *show-hidden* nil)
(defparameter *no-report* nil)

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
	       (progn
		 (incf dircount)
		 (let ((new-prefixes (if prefixes
					 (append `(,*line-straight*) prefixes)
					 `(,*line-middle*+)))
		       (children (sort-with-hidden (list-directory name))))
		   (when children
		     (dolist (x (butlast children)) (walk x new-prefixes))
		     (walk (car (last children))
			   (append (butlast new-prefixes) `(,*line-end*))))))
	       (incf filecount))))
      (fresh-line)
      (walk (pathname-as-directory dirname) nil)
      (unless *no-report*
	;; TODO: singular/plural; might even add translations
	(format t "~%~a directories, ~a files" dircount filecount)))))

;; TODO: actually rather generic?
(defun filter-pathnames (pathnames predicates)
  (remove-if-not #'(lambda (item)
		     (every #'(lambda (p) (funcall p item)) predicates))
		 pathnames))

(defun sort-with-hidden (files)
  "Sorts a list of files, ignoring leading dots."
  (sort files #'string< 
	:key #'(lambda(x) (remove-leading-dots (base-name x)))))

(walk-directory2 "/tmp/a")

(defun is-hidden (pathname) 
  (not (char-equal #\.
		   (aref (base-name pathname) 0))))

;; TODO: this is veery non-lispy... Also, there should be a library for this?
(defun remove-leading-dots (input)
  (loop with string = input
     while (and (> (length string) 0)
		(char-equal (aref string 0)
			    #\.))
     do (setf string (subseq string 1))
     finally (return string)))
