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

;;; ---------------------------------------------
;;; See suites.lisp for explanations of the
;;; approach used in these tests.
;;; ---------------------------------------------

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


;;; These tests run some typical invocations of TREE and capture all output, comparing it to
;;; the expected output. They're not supposed to provide full test coverage of the function but
;;; are meant as a way to find changes that break the output in a fundamental way.

(test full-tests
  (with-temporary-directory 
    (create-directory 
      (make-simple-directory :name "root" 
                             :files (list (make-simple-file :name ".hidden" :content "secret")
                                          (make-simple-file :name "DEPS" :content "unix-options")
                                          (make-simple-file :name "the-file" :content "test"))
                             :sub-directories 
                             (list
                               (make-simple-directory :name "dir-empty")
                               (make-simple-directory :name ".dir-hidden" 
                                                      :files (list (make-simple-file 
                                                                     :name "nested" 
                                                                     :content "tested")))
                               (make-simple-directory :name "dir-withfiles" 
                                                      :files (list (make-simple-file 
                                                                     :name "file.lisp" 
                                                                     :content "nil"))))) directory)
    ;; no options
    (is (equal (format nil ".~@
                            └── root~@
                            │   ├── DEPS~@
                            │   ├── dir-empty~@
                            │   ├── dir-withfiles~@
                            │   │   └── file.lisp~@
                            │   └── the-file~@
                            ~@
                            3 directories, 3 files~@
                            "
                       )
               (with-output-to-string (*standard-output*) (tree (list (merge-pathnames "./" directory))))))
    ;;directories only
    (is (equal (format nil ".~@
                            └── root~@
                            │   ├── dir-empty~@
                            │   └── dir-withfiles~@
                            ~@
                            3 directories, 0 files~@
                            ")
               (with-output-to-string (*standard-output*) (tree (list (merge-pathnames "./" directory))
                                                                :directories-only t))))
      ;; noreport
      (is (equal (format nil ".~@
                              └── root~@
                              │   ├── DEPS~@
                              │   ├── dir-empty~@
                              │   ├── dir-withfiles~@
                              │   │   └── file.lisp~@
                              │   └── the-file~@
                              ")
                 (with-output-to-string (*standard-output*) (tree (list (merge-pathnames "./" directory))
                                                                  :noreport t))))))
