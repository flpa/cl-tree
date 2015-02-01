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
(defpackage #:com.github.flpa.cl-tree.util
  (:use :common-lisp)
  (:import-from :uiop/stream
		:temporary-directory)
  (:import-from :uiop/filesystem
		:delete-directory-tree))

(in-package :com.github.flpa.cl-tree.util)

;; TODO: contribute
(defmacro with-temporary-directory (&body body)
  `(let ((directory
	  (ensure-directories-exist
	   (merge-pathnames (pathname (format nil "lisp~36R/" (random (ash 1 32))))
			    (temporary-directory)))))
     (unwind-protect
	  (progn
	    ,@body)
       (delete-directory-tree directory :validate t))))

