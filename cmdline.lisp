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
  (:use :common-lisp
	:com.github.flpa.cl-tree
	:unix-options)
  (:export :tree-cmd))

(in-package :com.github.flpa.cl-tree.cmdline)

(defun tree-cmd ()
  (with-cli-options () (a
			d
			noreport
			prune
			&free directories)
    (tree (uiop/os:getcwd)
	  :show-hidden a
	  :directories-only d
	  :noreport noreport
	  :prune-empty prune)))
