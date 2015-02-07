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
(defpackage #:com.github.flpa.cl-tree.core
  (:use :common-lisp)
  (:import-from :uiop/pathname
                :directory-pathname-p
                :hidden-pathname-p)
  (:import-from :uiop/filesystem
                :directory-files)
  (:export :tree))

(in-package :com.github.flpa.cl-tree.core)

(defparameter *line-middle*   "├── ")
(defparameter *line-straight* "│   ")
(defparameter *line-end*      "└── ")

;;;; ---------------
;;;; Public API
;;;; ---------------

;; TODO: not satisfied with the function signature
(defun tree (directories &key (noreport nil)
                         (show-hidden nil)
                         (directories-only nil)
                         (prune-empty nil))
  "TREE traverses a list of DIRECTORIES, represented as pathnames, while printing their contents 
   according to the specified parameters."
  (fresh-line) ; We always want to start on a fresh line.
  (walk-tree 
    directories 
    '() 
    (build-predicates show-hidden
                      directories-only
                      prune-empty)
    0
    0
    (not noreport)))

;; pros of flat list: convert to tail call recursion, direct passing of counts, convert to list
;; cons: how to do things before and after folders? (indentation, xml?)

(defun walk-tree (frontier prefixes predicates dircount filecount print-report)
  "A recursive implementation of the tree functionality, inspired by depth-first-search as 
   showcased in AIMA by Norvig.
   FRONTIER is the LIFO list of pathnames elements to be handled, the list of PREDICATES controls
   which elements are printed (and explored, in the case of directories). 
   PREFIXES is the list of strings to be printed before the current file. DIRCOUNT and FILECOUNT
   are numbers maintained for a final report, which is printed if PRINT-REPORT is true."
  (if frontier
    (let ((current (first frontier)))
      (if (equal current :closedir) 
        (walk-tree (rest frontier) (rest prefixes) predicates dircount filecount print-report)
        (progn 
          (format t "~{~a~}~a~%" 
                  (if (equal :closedir (first (rest frontier)))
                    (append (butlast prefixes) (list *line-end*)) 
                    prefixes)
                  (base-name current))
          (if (directory-pathname-p current)
            (walk-tree (append (sort-with-hidden
                                 (filter-items (directory-files current) predicates))
                               (list :closedir)
                               (rest frontier))
                       (cons (if prefixes *line-straight* *line-middle*) prefixes)
                       predicates
                       (if prefixes (1+ dircount) dircount)
                       filecount
                       print-report)
            (walk-tree (rest frontier) prefixes predicates dircount (1+ filecount) print-report)))))
    (progn 
      (when print-report
        (print-report dircount filecount))
      (fresh-line))))

;;;; ---------------
;;;; Internals
;;;; ---------------

(defun build-predicates (show-hidden directories-only prune-empty)
  "Builds a list of predicates to be applied to every folder and file based on the parameter.
   This effectively defines the mapping between parameters and predicate-functions."
  ;; TODO: macro for better readability of parameter<>predicate mapping?
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

(defun filter-items (items predicates)
  "Returns the filtered sequence of ITEMS that satisfy all of the sequence of PREDICATES."
  (remove-if #'(lambda (item)
                 (notevery #'(lambda (p) (funcall p item)) predicates))
             items))

(defun sort-with-hidden (pathnames)
  "Sorts a list of PATHNAMES, ignoring leading dots."
  (sort pathnames #'string-lessp
        :key #'(lambda(x) (remove-leading-dots (base-name x)))))

(defun remove-leading-dots (input)
  "Removes any number of leading dots from the `string' INPUT."
  (string-left-trim (list #\.) input))

(defun print-report (dircount filecount)
  "Prints the number of directories and files, also taking taking care of pluralization.
   e.g.: '5 directories, 1 file'.
   Output is preceded by a blank line."
  (format t "~&~%~a director~:@P, ~a file~:P" dircount filecount))

(defun visible-p (pathname)
  "Determines whether a given PATHNAME is visible, i.e. not hidden, by Unix conventions:
   Hidden files and directories start with a dot (.)"
  (not (char-equal #\.
                   (aref (base-name pathname) 0))))

(defun file-or-non-empty-dir-p (pathname)
  "Checks whether a given PATHNAME denotes a file or an empty directory."
  (if (directory-pathname-p pathname)
    (not (endp (directory-files pathname)))
    t))
;; improve with OR?

(defun file-or-dir-within-limit-p (limit)
  "Returns a predicate function that only accepts files, and directories with no more than LIMIT children."
  (lambda (pathname)
    (if (directory-pathname-p pathname) 
      (<= (length (directory-files pathname)) limit)
      t)))
