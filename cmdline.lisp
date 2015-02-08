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
(defpackage #:com.github.flpa.cl-tree.cmdline
  (:documentation "This package encapsulates interaction with the command-line.
                   It parses arguments and passes them to the TREE core function.")
  (:use :common-lisp
	:com.github.flpa.cl-tree.core
	:unix-options)
  (:import-from :uiop/pathname
		:merge-pathnames*
		:ensure-directory-pathname)
  (:import-from :uiop/os
		:getcwd)
  (:export :tree-cmd))

(in-package :com.github.flpa.cl-tree.cmdline)

(defun tree-cmd ()
  "TREE-CMD is the main command-line entry point."
  (with-cli-options () (a
			d
			noreport
			prune
                        &parameters
                        filelimit
			&free directories)
    (tree (collect-directory-pathnames directories)
	  :show-hidden a
	  :directories-only d
	  :noreport noreport
	  :prune-empty prune
          :file-limit (when filelimit (parse-integer filelimit)))))

(defun collect-directory-pathnames (paths)
  "Collects the complete pathnames for a list of PATHS specified as strings.
   That means that relative paths are merged with the current working directory path, while
   absolute paths remaing untouched.
   In case the list of PATHS is empty, the current working directory pathname is returned, merged
   with \".\" to achieve the output of the original tree command."
  (mapcar
	   #'(lambda (dir)
	       (merge-pathnames* (ensure-directory-pathname (pathname dir))
				 (getcwd)))
	   (if paths paths '("."))))

