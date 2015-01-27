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


;; TODO: separate package?
(in-package :com.github.flpa.cl-tree)

(asdf:load-system 'lisp-unit)
(use-package :lisp-unit)

(define-test test-base-name
  (assert-equal "home" (base-name #p"/home/"))
  (assert-equal "dir" (base-name #p"/home/dir/"))
  (assert-equal "dir" (base-name #p"/home/is/great/and/a/dir/"))
  (assert-equal "file" (base-name #p"/home/file"))
  (assert-equal "file" (base-name #p"/home/has/folders/file"))
  (assert-equal "file.c" (base-name #p"/home/has/folders/file.c"))
  )

(define-test test-remove-leading-dots
  (assert-equal "plain" (remove-leading-dots "plain"))
  (assert-equal "main.c" (remove-leading-dots "main.c"))
  (assert-equal "hidden" (remove-leading-dots ".hidden"))
  (assert-equal "hidden.asd" (remove-leading-dots ".hidden.asd"))
  (assert-equal "" (remove-leading-dots ""))
  (assert-equal "" (remove-leading-dots "."))
  (assert-equal "" (remove-leading-dots ".."))
  )

(define-test test-sort-with-hidden
  (assert-equal `("a" "b" "c") (sort-with-hidden `("c" "a" "b")))
  (assert-equal `("a" ".b" "c") (sort-with-hidden `("c" "a" ".b")))
  (assert-equal `(".aha" "alpha" ".arm") (sort-with-hidden `("alpha" ".aha" ".arm")))
  )
