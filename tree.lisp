;;;; cl-tree: Reimplementing Unix 'tree' command in Common Lisp
;;;; Copyright (C) 2015 Florian Patzl 
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;;; -----------------------------------------------------------------------------------------------

(in-package :cl-user)
(defpackage #:com.github.flpa.cl-tree
  (:use :common-lisp)
  (:import-from :uiop/pathname
		:directory-pathname-p
		:hidden-pathname-p)
  (:import-from :uiop/filesystem
		:directory-files)
  (:export :tree))

(in-package :com.github.flpa.cl-tree)

(defparameter *line-middle* "├── ")
(defparameter *line-straight* "│   ")
(defparameter *line-end* "└── ")

;;;; ---------------
;;;; Public API
;;;; ---------------

(defun tree (root-dir &key (noreport nil)
			(show-hidden nil)
			(directories-only nil)
			(prune-empty nil)
			(dirs '()))
  (let ((dircount -1) ; -1 to exclude root directory from count.
	(filecount 0)
	(predicates (build-predicates show-hidden
				      directories-only
				      prune-empty)))
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
					 `(,*line-middle*)))
		       (children (sort-with-hidden
				  (filter-pathnames (directory-files name) predicates))))
		   (when children
		     (dolist (x (butlast children)) (walk x new-prefixes))
		     (walk (car (last children))
			   (append (butlast new-prefixes) `(,*line-end*))))))
	       (incf filecount))))
      (fresh-line)
      ;; walk dirs, if no dirs use root-dir
      (walk root-dir '())
      (unless noreport
	(print-report dircount filecount))
      (fresh-line))))

;;;; ---------------
;;;; Internals
;;;; ---------------

;; TODO: macro of better readability of parameter<>predicate mapping?
(defun build-predicates (show-hidden directories-only prune-empty)
  (loop for x in `((,(not show-hidden) ,#'visible-p)
		   (,directories-only ,#'directory-pathname-p)
		   (,prune-empty ,#'file-or-non-empty-dir-p))
     if (car x) collect (cadr x)))

;; TODO ... why do I need to do this?
(defun base-name (p)
  "Determines the base name of the `pathname' P, i.e. the directory name for directories or the file
name, including the extension, for files."
  (if (directory-pathname-p p)
      (car (last (pathname-directory p)))
      (if (pathname-type p)
	  (concatenate 'string (pathname-name p) "." (pathname-type p))
	  (pathname-name p))))

;; TODO: actually rather generic?
(defun filter-pathnames (pathnames predicates)
  (remove-if-not #'(lambda (item)
		     (every #'(lambda (p) (funcall p item)) predicates))
		 pathnames))

(defun sort-with-hidden (pathnames)
  "Sorts a list of PATHNAMES, ignoring leading dots."
  (sort pathnames #'string< 
	:key #'(lambda(x) (remove-leading-dots (base-name x)))))

(defun remove-leading-dots (input)
  (string-left-trim (list #\.) input))

(defun print-report (dircount filecount)
  "Prints the number of directories and files, also taking taking care of pluralization.
  e.g.: '5 directories, 1 file'.
  Output is preceded by a blank line."
  (format t "~&~%~a director~:@P, ~a file~:P" dircount filecount))

;;;; ---------------
;;;; Predicates
;;;; ---------------

(defun visible-p (pathname)
  (not (char-equal #\.
		   (aref (base-name pathname) 0))))

(defun file-or-non-empty-dir-p (pathname)
  (if (directory-pathname-p pathname)
      (directory-files pathname) ; TODO: is it ok to return the list?
      t))
