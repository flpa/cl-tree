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
(defpackage #:com.github.flpa.cl-tree.test.util
  (:use :common-lisp
        :5am
        :com.github.flpa.cl-tree.test.suites)
  (:import-from :com.github.flpa.cl-tree.util
                ::make-simple-file
                ::make-simple-directory
                ::create-file
                ::create-directory
                ::with-temporary-directory)
  (:import-from :uiop/stream
                :temporary-directory)
  (:import-from :uiop/filesystem
                :directory-exists-p
                :file-exists-p))

(in-package :com.github.flpa.cl-tree.test.util)

;;; See suites.lisp

(def-suite util :in all)

(def-suite test-create-file :in util)
(in-suite test-create-file)

(test creates-and-writes-content
  (create-file (make-simple-file :name "testfile.txt" :content "content") (temporary-directory))
  (is (equal "content" 
             (with-open-file (s (merge-pathnames (pathname "testfile.txt") (temporary-directory)))
               (read-line s)))))

(def-suite test-create-directory :in util)
(in-suite test-create-directory)

(test creates-dirs-and-files
  (with-temporary-directory 
    (create-directory 
      (make-simple-directory :name "a" 
                             :sub-directories 
                             (list
                               (make-simple-directory :name "b")
                               (make-simple-directory :name "c" 
                                                      :files (list (make-simple-file 
                                                                     :name "d" 
                                                                     :content "e"))))) directory)
    (is-true (directory-exists-p (merge-pathnames #P"a/b/" directory)))
    ;; only testing existence here since content is covered in specific tests for create-file
    (is-true (file-exists-p (merge-pathnames #P"a/c/d" directory)))))
