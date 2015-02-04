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
(defpackage #:com.github.flpa.cl-tree.test.full
  (:use :common-lisp
        :5am
        :com.github.flpa.cl-tree.test.suites)
  (:import-from :com.github.flpa.cl-tree.core
                :tree)
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

(in-package :com.github.flpa.cl-tree.test.full)

;;; See suites.lisp

(def-suite util :in all)

(def-suite test-full :in all)
(in-suite test-full)

(test creates-and-writes-content
  (create-file (make-simple-file :name "testfile.txt" :content "content") (temporary-directory))
  (is (equal "content" 
             (with-open-file (s (merge-pathnames (pathname "testfile.txt") (temporary-directory)))
               (read-line s)))))

(def-suite test-create-directory :in util)
(in-suite test-create-directory)


;;; Tests to capture the current status quo, hopefully useful during refactoring

(test full-tests
  (with-temporary-directory 
    (create-directory 
      (make-simple-directory :name "root" 
                             :files (list (make-simple-file :name ".hidden" :content "secret")
                                          (make-simple-file :name "DEPS" :content "unix-options")
                                          (make-simple-file :name "the-file" :content "test"))
                             :sub-directories 
                             (list
                               (make-simple-directory :name "empty")
                               (make-simple-directory :name ".hidden-dir" 
                                                      :files (list (make-simple-file 
                                                                     :name "nested" 
                                                                     :content "tested")))
                               (make-simple-directory :name "withfiles" 
                                                      :files (list (make-simple-file 
                                                                     :name "file.lisp" 
                                                                     :content "nil"))))) directory)
    (princ (with-output-to-string (*standard-output*) (tree (list (merge-pathnames "./" directory)))))))
