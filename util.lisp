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
                :delete-directory-tree)
  (:import-from :uiop/pathname
                :merge-pathnames*
                :ensure-directory-pathname)
  (:export :with-temporary-directory))

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

;;; Drafts for a compact DSL for creating test file-structures.

;; drafting syntax for convenient file structure creation
;(:a (:b (:file "hallo") (:.hidden "test")))

(defstruct simple-file name content)
(defstruct simple-directory name sub-directories files)

(defun create-file (file dir-pathname)
  (with-open-file (s (merge-pathnames (pathname (simple-file-name file)) dir-pathname )
                     :if-does-not-exist :create 
                     :direction :output
                     :if-exists :supersede)
    (write-line (simple-file-content file) s)))

(defun create-directory (directory root-dir-pathname)
  (let ((own-pathname (ensure-directory-pathname 
                        (merge-pathnames (pathname (simple-directory-name directory)) 
                                         root-dir-pathname))))
   (ensure-directories-exist own-pathname)
   (mapc #'(lambda (file) (create-file file own-pathname)) (simple-directory-files directory))
   (mapc #'(lambda (sub-directory) (create-directory sub-directory own-pathname)) (simple-directory-sub-directories directory))))
