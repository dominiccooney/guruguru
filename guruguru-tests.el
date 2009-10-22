;; Copyright 2009 Dominic Cooney. All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Tests for guruguru.el

(require 'el-expectations "third_party/el-expectations.el")

(require 'guruguru)

(setq max-lisp-eval-depth 3000)

(defmacro with-test-buffer (text &rest body)
  "Creates a temporary buffer containing TEXT and executes BODY."
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(defmacro eval-with-text (text pattern &rest body)
  "Evaluates PATTERN in a temporary buffer containing TEXT, then executes BODY."
  `(with-test-buffer ,text
     (let ((gg-bindings ()))
       (gg-eval ,pattern)
       ,@body)))

(defun digitp (ch)
  (and (<= ?0 ch) (<= ch ?9)))

(defun greaterp (ch)
  (< prev ch))

(expectations
 (desc "guruguru")

 ; gg-any at a character should succeed
 (expect '(t . ?a)
   (eval-with-text "a" (gg-any)))

 ; gg-any at end-of-buffer should fail
 (expect nil
   (eval-with-text "" (gg-any)))

 ; gg-seq should succeed if all parsers succeed
 (expect '(t . ?b)
   (eval-with-text "ab" (gg-seq (gg-any) (gg-any))))

 ; gg-seq should fail if any parser fails
 (expect nil
   (eval-with-text "a" (gg-seq (gg-any) (gg-any))))

 ; gg-seq should rewind on failure
 (expect 1
   (eval-with-text "a"
     (gg-seq (gg-any) (gg-any))
     (point)))

 ; gg-if should fail if the parser fails
 (expect nil
   (eval-with-text "a" (gg-if nil (gg-seq (gg-any) (gg-any)))))

 ; gg-if should fail if the predicate fails
 (expect nil
   (eval-with-text "a" (gg-if 'digitp (gg-any))))

 ; gg-if should succeed if the parser and the predicate succeed
 (expect '(t . ?1)
   (eval-with-text "1" (gg-if 'digitp (gg-any))))

 ; bindings should be in scope in gg-if predicate
 (expect '(t . ?b)
   (eval-with-text "ab"
     (gg-seq
      (gg-bind 'prev (gg-any))
      (gg-if 'greaterp (gg-any)))))

 ; gg-alt should succeed with the first parser that succeeds
 (expect '(t . ?a)
   (eval-with-text "a"
     (gg-alt
      (gg-seq (gg-any) (gg-any))  ; text too short
      (gg-if 'digitp (gg-any))    ; predicate fails
      (gg-any))))

 ; gg-alt should fail if all of the alternatives fail
 (expect nil
   (eval-with-text "a"
     (gg-alt
      (gg-seq (gg-any) (gg-any))
      (gg-if 'digitp (gg-any)))))

 ; gg-act should invoke action if the parser succeeds
 (expect '(t . (1 . 2))
   (eval-with-text "a"
     (gg-act (lambda () (cons gg-start gg-end)) (gg-any))))

 ; gg-bind should introduce bindings visible to gg-act's action
 (expect '(t . "b97")
   (eval-with-text "ab"
     (gg-act
      (lambda () (format "%c%d" y x))
      (gg-seq (gg-bind 'x (gg-any)) (gg-bind 'y (gg-any))))))

 ; gg-end should fail if there's still content in the buffer
 (expect nil
   (eval-with-text "a" (gg-end)))

 ; gg-end should succeed if there's no content left in the buffer
 (expect '(t . nil)
   (eval-with-text "a" (gg-seq (gg-any) (gg-end))))

 ; really parse a small, right-recursive grammar
 (expect '(t . 7)
   (with-test-buffer "aaaaaaa"  ; seven "a"s
    ; xs := . xs / (end)
    (let* ((xs (gg-alt
                (gg-act (lambda () (1+ len))
                        (gg-seq (gg-any) (gg-bind 'len (gg-call 'tail))))
                (gg-act (lambda () 0) (gg-end))))
           (gg-bindings ())
           (gg-grammar (make-gg-grammar :rules `((tail . ,xs)))))
      (gg-eval (gg-call 'tail)))))

 ; parse a small, right-recursive grammar specified in abstract syntax
 (expect '(t . 9)
   (with-test-buffer "bbbbba"   ; 5 * 2 - 1 = 9
    (gg-parse
     (gg-grammar
      (start x := expr eof -> x)
      (expr
         ?b x := expr -> (+ 2 x)
       | ?a x := expr -> (- x 1)
       | -> 0)))))
)
