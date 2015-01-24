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

;; TODO: real tree sorts files alphabetically ignoring leading dots?

;; TODO: should be uppercased?
(defparameter *show-hidden* nil)
(defconstant +indent+ 4)
(defconstant +line-middle+ "├── ")
(defconstant +line-straight+ "│   ")
(defconstant +line-end+ "└── ")

(defun base-name (p)
  (or (and (directory-pathname-p p)
	   (car (last (pathname-directory p))))
      (pathname-name p)))

(defun walk-directory2 (dirname) 
  (labels
      ((walk (name prefixes)
	 (dolist (prefix prefixes) (princ prefix))
	 (princ (base-name name))
	 (terpri)
	 (when (directory-pathname-p name)
	   (let ((new-prefixes (if prefixes
				   (append `(,+line-straight+) prefixes)
				   `(,+line-middle+))))
	    (dolist (x (list-directory name))
	      (walk x new-prefixes))))))
    (fresh-line)
    (walk (pathname-as-directory dirname) nil)))

(walk-directory2 "/tmp/a")

(defun is-hidden (pathname)
  (or *show-hidden*
      (not (char-equal #\.
		       (aref (base-name pathname) 0)))))

