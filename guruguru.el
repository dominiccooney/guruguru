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

;;;; guruguru - a parsing expression grammar (PEG) combinator library

;; guruguru is a Parsing Expression Grammar (PEG) [1] combinator
;; library for emacs lisp. guruguru is a Packrat parser which supports
;; left-recursive rules [2].

;;; References

;; [1] Ford, Bryan (2004, January 14-16) "Parsing Expression Grammars:
;;     A Recognition-Based Syntactic Foundation" Symposium on
;;     Principles of Programming Languages, Venice, Italy.
;;     http://pdos.csail.mit.edu/~baford/packrat/popl04

;; [2] Warth, Alessandro, Douglass, James R., and Millstein, Todd
;;     (2008, January 7-8) "Packrat Parsers Can Support Left
;;     Recursion" Workshop on Partial Evaluation and Program
;;     Manipulation, San Francisco, USA.
;;     http://portal.acm.org/citation.cfm?id=1328408.1328424

(eval-when-compile (require 'cl))

(defstruct gg-grammar
  "A grammar.
RULES is an assoc list of symbols to parsing expressions."
  rules)

(defun gg-grammar-start (grammar)
  "Gets the name of the start rule of GRAMMAR."
  (caar (gg-grammar-rules grammar)))

(defun gg-pattern-for-rule (grammar sym)
  "Looks up the pattern of the rule named SYM in GRAMMAR."
  (cdr (assq sym (gg-grammar-rules grammar))))

(defun gg-eps ()
  "The empty expression: succeeds, and does not consume any input."
  '(gg-eps))

(defun gg-any ()
  "Reads the character at `point'.
`gg-any' fails if the point is at the end of the current buffer."
  '(gg-any))

(defun gg-re (re)
  "Matches the regular expression at `point'."
  (cons 'gg-re re))

(defun gg-seq (&rest args)
  "Parses the patterns in ARGS sequentially.
`gg-seq' produces the result of the last pattern as its result."
  (if (null args)
      (gg-eps)
    (let ((fst (car args))
          (snd (apply 'gg-seq (cdr args))))
      (cond
       ((eq 'gg-eps (car fst)) snd)
       ((eq 'gg-eps (car snd)) fst)
       (t `(gg-seq ,fst ,snd))))))

(defun gg-alt (&rest args)
  "Tries the patterns in ARGS until one succeeds."
  (cond ((and (consp args) (cdr args))
         `(gg-alt ,(car args) ,(apply 'gg-alt (cdr args))))
        ((consp args)
         (car args))
        (t
         (error "no alternatives"))))

(defun gg-negative-lookahead (pattern)
  "Succeeds if PATTERN does not come next.
`gg-negative-lookahead' does not consume any input."
  `(gg-negative-lookahead ,pattern))

(defun gg-if (predicate pattern)
  "Filters the result of PATTERN with PREDICATE.
PREDICATE should be a pure function with one argument, the result
of PATTERN.

`gg-if' just determines whether the result of PATTERN should be
accepted; to transform results use `gg-bind' and `gg-act'."
  `(gg-if ,predicate ,pattern))

(defmacro gg-eq-p (a)
  "Creates a `lambda' which tests whether its argument is A using `eq'.

`gg-eq-p' creates a lexical environment for A, so it is useful for
making predicates for `gg-if'."
  `(lambda (b) (eq ,(eval a) b)))

(defun gg-bind (symbol pattern)
  "Binds the result of PATTERN to SYMBOL.
The symbol is observable in subsequent `gg-if' and `gg-act'
expressions."
  `(gg-bind ,symbol ,pattern))

(defun gg-act (action pattern)
  "Calls ACTION to produce the result of PATTERN.
ACTION should be a pure function, because ACTION could be invoked
as part of a larger expression that backtracks over PATTERN. In
that case the result of ACTION may be cached and reused, without
invoking ACTION a second time."
  `(gg-act ,action ,pattern))

(defmacro gg-action-lambda (body)
  "Embeds BODY in a lambda expression suitable for use with `gg-act'."
  `(lambda () ,(eval body)))

; TODO: parameterized rules
; TODO: higher-ordered rules?
(defun gg-call (rule)
  "Looks up RULE in the grammar and activates it."
  `(gg-call ,rule))

(defmacro gg-dynamic-let (bindings &rest body)
  "Evaluates BODY with BINDINGS in scope.
BINDINGS is a list of bindings, ((sym1 val1) (sym2 val2) ...),
like the first argument to `let'."
  (let* ((bindings (eval bindings))
         (symbols (mapcar 'car bindings))
         (values (mapcar 'gg-dynamic-let-quote-value (mapcar 'cdr bindings))))
    `(funcall (lambda ,symbols ,@body) ,@values)))

(defun gg-dynamic-let-quote-value (value)
  "Adds a level of quoting to VALUE."
  `',value)

(defmacro gg-try-parse (&rest body)
  "Evaluates BODY but resets `point' and bindings if BODY fails."
  `(let* ((gg-try-parse-start (point))
          (gg-try-parse-bindings gg-bindings)
          (gg-try-parse-result (progn ,@body)))
     (unless gg-try-parse-result (gg-retry))
     gg-try-parse-result))

(defun gg-retry ()
  "Resets `point' and bindings in a `gg-try-parse'."
  (goto-char gg-try-parse-start)
  (setq gg-bindings gg-try-parse-bindings))

(defun gg-eval (pattern)
  "Interprets PATTERN at `point' and returns the result."
  (case (car pattern)
    ('gg-eps
     '(t . nil))

    ('gg-any
     (unless (eobp)
       (goto-char (1+ (point)))
       `(t . ,(char-before))))

    ('gg-re
     (gg-try-parse
      (if (and (search-forward-regexp (cdr pattern) nil t)
               (= gg-try-parse-start (match-beginning 0)))
          `(t . ,(match-string 0))
        (progn
          (gg-retry)
          nil))))

    ('gg-seq
     (gg-try-parse
      (let ((p (cadr pattern))
            (q (caddr pattern)))
        (and (gg-eval p) (gg-eval q)))))

    ('gg-alt
     (gg-try-parse
      (let ((p (cadr pattern))
            (q (caddr pattern)))
        (or (gg-eval p)
            (progn (gg-retry) (gg-eval q))))))

    ('gg-negative-lookahead
     (gg-try-parse
      (let* ((p (cadr pattern))
             (result (gg-eval p)))
        (gg-retry)  ; rewind, no matter what
        (if result nil '(t)))))

    ('gg-if
     (gg-try-parse
      (let* ((pred (cadr pattern))
             (pat (caddr pattern))
             (result (gg-eval pat)))
        (and result
             (gg-dynamic-let gg-bindings (funcall pred (cdr result)))
             result))))

    ('gg-bind
     (let* ((symbol (cadr pattern))
            (pat (caddr pattern))
            (result (gg-eval pat)))
       (when result
         (setq gg-bindings (cons (cons symbol (cdr result)) gg-bindings))
         result)))

    ('gg-act
     (let* ((action (cadr pattern))
            (pat (caddr pattern))
            (gg-start (point))
            (gg-bindings ())
            (result (gg-eval pat))
            (gg-end (point)))
       (and result
            (cons t  ; success
                  (gg-dynamic-let gg-bindings
                                  (funcall action))))))

    ('gg-call
     (let* ((rule (cadr pattern))
            (pat (gg-pattern-for-rule gg-grammar rule)))
       (if pat
           (gg-apply-rule rule pat)
         (error "Invalid rule %s" rule))))

    (t
     (error "Invalid pattern %s" pattern))))

(defstruct gg-memo
  "The memoized result of applying a rule.
RESULT is the parse result; see `gg-parse'. END is the character
position after parsing the result."
  result
  end)

(defstruct gg-lr
  "A stack of active rules, for determining when left-recursion starts.
SEED is the result parsed so far. RULE is the rule active at this
level on the stack. HEAD is `gg-head' of the first recursively
entered rule. NEXT is the previous activation on the stack."
  seed
  rule
  head
  next)

(defstruct (gg-head
  (:constructor nil)
  (:constructor make-gg-head
                (&key rule
                      (eval-set (make-hash-table :test 'equal))
                      (involved-set (make-hash-table :test 'equal)))))
  "The set of rules involved in a left-recursive loop."
  rule
  eval-set
  involved-set)

(defun gg-grow-lr (rule pos memo head)
  "Parses during left recursion by repeatedly invoking the head rule."
  (let ((done nil))
    (puthash pos head gg-heads)  ; left recursion growth is starting
    (while (not done)
      (goto-char pos)
      (setf (gg-head-eval-set head)
            (copy-hash-table (gg-head-involved-set head)))
      (let ((result (gg-eval (gg-pattern-for-rule gg-grammar rule))))
        (if (or (not result) (<= (point) (gg-memo-end memo)))
            (setq done t)
          (progn
            (setf (gg-memo-result memo) result)
            (setf (gg-memo-end memo) (point))))))
    (puthash pos nil gg-heads)
    (goto-char (gg-memo-end memo))
    (gg-memo-result memo)))

(defun gg-set-up-lr (rule lr)
  "Walks the LR stack and builds LR's set of involved rules."
  (unless (gg-lr-head lr)
    (setf (gg-lr-head lr) (make-gg-head :rule rule)))
  (let ((s gg-lr-stack)
        (head (gg-lr-head lr)))
    (while (not (eq (gg-lr-head s) head))
      (setf (gg-lr-head s) head)
      (puthash (gg-lr-rule s) t (gg-head-involved-set head))
      (setq s (gg-lr-next s)))))

(defun gg-lr-answer (rule pos memo)
  "Computes the latest result for RULE involved in left recursion at POS.
MEMO is the `gg-memo' to update with the memoized result."
  (let* ((lr (gg-memo-result memo))
         (head (gg-lr-head lr)))
    (if (not (equal rule (gg-head-rule head)))
        (gg-lr-seed lr)
      (progn
        (setf (gg-memo-result memo) (gg-lr-seed lr))
        (if (gg-memo-result memo)
            (gg-grow-lr rule pos memo head)
          nil)))))

(defun gg-recall (rule pos)
  "Gets the memoized entry, if any, for invoking RULE at POS.
Memoized entries are stored in the dynamically scoped variable
GG-MEMO-TABLE."
  (let ((memo (gethash (cons rule pos) gg-memo-table))
        (head (gethash pos gg-heads)))
    (cond
     ((not head)
      ; not growing a seed parse
      memo)

     ((and (not memo)
           (not (equal (gg-head-rule head) rule))
           (not (gethash rule (gg-head-involved-set head))))
      ; the rule isn't involved in the recursion, so fail
      (make-gg-memo :result nil :end pos))

     ((gethash rule (gg-head-eval-set head))
      ; evaluate the rule if it is involved, but only once
      (puthash rule nil (gg-head-eval-set head))
      (let ((result (gg-eval (gg-call rule))))
        (setf (gg-memo-result memo) result)
        (setf (gg-memo-end memo) (point)))
      memo)
     (t memo))))

(defun gg-apply-rule (rule pattern)
  "Applies the rule named RULE with body PATTERN at point."
  (let* ((gg-bindings ())
         (start-pos (point))
         (key (cons rule start-pos))
         (memo (gg-recall rule start-pos)))
    (if (not memo)
        ; no memoized result, so parse
        (let ((lr (make-gg-lr :head nil :seed nil :rule rule
                              :next gg-lr-stack)))
          ; push the rule activation stack
          (setq gg-lr-stack lr)
          ; record the left recursion marker as a temporary result
          (setq memo (make-gg-memo :result lr :end start-pos))
          (puthash key memo gg-memo-table)
          ; evaluate the rule
          (let ((result (gg-eval pattern)))
            ; pop the rule activation stack
            (setq gg-lr-stack (gg-lr-next gg-lr-stack))
            (setf (gg-memo-end memo) (point))
            (if (gg-lr-head lr)
                (progn
                  (setf (gg-lr-seed lr) result)
                  (gg-lr-answer rule start-pos memo))
              (setf (gg-memo-result memo) result))))
      (progn
        ; a memoized result, so skip to the end of the result
        (goto-char (gg-memo-end memo))
        (if (gg-lr-p (gg-memo-result memo))
            ; memoized result is a left-recursion marker so start
            ; recursive parsing
            (let ((lr (gg-memo-result memo)))
              (gg-set-up-lr rule lr)
              (gg-lr-seed lr))
          ; ordinary result is memoized, so done
          (gg-memo-result memo))))))

(defun gg-end ()
  "Succeeds if `point' is at the end of the buffer.
`gg-end' is used as the last symbol of a top-level production to
ensure that a parser consumed all available input."
  (gg-negative-lookahead (gg-any)))

; TODO: gg-lookahead could arguably introduce bindings for an action
(defun gg-lookahead (pattern)
  "Succeeds if PATTERN comes next.
`gg-negative-lookahead' does not consume any input."
  (gg-negative-lookahead (gg-negative-lookahead pattern)))

(defun gg-rule (name &rest alternatives)
  "Translates a rule named NAME into a parsing expression.
`gg-rule' returns a cons cell with NAME in the car and the
expression in the cdr.

ALTERNATIVES into a list of alternatives (there may be just one)
separated by |.

Alternatives have the form

  expression -> action

where expression is a parsing expression and action is an
s-expression. The expression may be empty, so

  -> 'mu

is a valid alternative that consumes no input and produces 'mu.

Of course, empty expressions only really make sense as the last
alternative in a PEG: because the empty expression always
succeeds, no subsequent alternative will ever be activated.

An action is optional, in which case the reduction is the value
of the rightmost term in the expression. If the expression is
empty and there is no action, the reduction is nil.

An expression e, can:

    ?a  Match a specific character.

  \"a*\"  Match a regular expression.

   eof  Succeed if at end-of-buffer.

     x  Evaluate a named rule.

 e1 e2  Be a sequence of expressions.

   ~ e  Look ahead to check that a given expression is not matched.

   & e  Look ahead to check that a given expression is matched.

x := e  Bind the reduction of an expression, e, to a variable,
        x. The variable is visible in predicates and the action,
        in the lexical scope of the alternative.

e if p  Use a predicate p to determine the success of the
        expression e.

A predicate may be a symbol, in which case it's called as a
function and passed the reduction of e; or an s-expression in
which case it's evaluated.

Operator precedence:

1: :=
0: ~ & sequence if
"
  `(,name . ,(gg-translate-alts alternatives)))

(defun gg-translate-alts (alts)
  "Translates the symbols in ALTS into a parsing expression."
  (let* ((alt-rest (gg-translate-alt alts 0 (gg-eps)))
         (alt (car alt-rest))
         (rest (cdr alt-rest)))
    (if (eq '| (car rest))
        ; there's more to come
        (gg-alt alt (gg-translate-alts (cdr rest)))
      alt)))

(defun gg-translate-alt (alt prec expr)
  "Translates symbols in ALT, at precedence PREC, to continue building EXPR.
See `gg-rule' for the syntax of ALT. PREC controls how early to
stop consuming symbols. EXPR is the partial expression already built."
  (if (eq (cadr alt) ':=)
      (let* ((sym (car alt))
             (rvalue-rest (gg-translate-alt (cddr alt) 1 (gg-eps)))
             (rvalue (car rvalue-rest))
             (rest (cdr rvalue-rest)))
        (gg-translate-alt rest prec (gg-seq expr (gg-bind sym rvalue))))

    (case (car alt)
      ('->  ; action
       (if (= 1 prec)
           (cons expr alt)
         (let ((act (cadr alt))
               (rest (cddr alt)))
           (cons (gg-act (gg-action-lambda act) expr) rest))))

      ('|   ; another alternative, so this one is done
       (cons expr alt))

      ('if  ; filter
       (if (= 1 prec)
           (cons expr alt)
         (let ((pred (cadr alt))
               (rest (cddr alt)))
           (gg-translate-alt rest prec (gg-if pred expr)))))

      ('~   ; negative lookahead
       (if (= 1 prec)
           (cons expr alt)
         (let ((lookahead-rest (gg-translate-alt (cdr alt) prec (gg-eps))))
           (gg-translate-alt
            (cdr lookahead-rest)
            prec
            (gg-seq expr (gg-negative-lookahead (car lookahead-rest)))))))

      ('&   ; lookahead
       (if (= 1 prec)
           (cons expr alt)
         (let ((lookahead-rest (gg-translate-alt (cdr alt) prec (gg-eps))))
           (gg-translate-alt
            (cdr lookahead-rest)
            prec
            (gg-seq expr (gg-lookahead (car lookahead-rest)))))))

      ('eof
       (if (= 1 prec)
           (cons expr alt)
         (gg-translate-alt (cdr alt) prec (gg-seq expr (gg-end)))))

      (t
       (cond
        ((null alt)
         (cons expr nil))

        ((symbolp (car alt))
         (let ((expr (gg-seq expr (gg-call (car alt))))
               (rest (cdr alt)))
           (if (= 0 prec)
               (gg-translate-alt rest prec expr)
             (cons expr rest))))

        ((numberp (car alt))
         (let ((expr (gg-seq expr (gg-if (gg-eq-p (car alt)) (gg-any))))
               (rest (cdr alt)))
           (if (= 0 prec)
               (gg-translate-alt rest prec expr)
             (cons expr rest))))

        ((stringp (car alt))
         (let ((expr (gg-seq expr (gg-re (car alt))))
               (rest (cdr alt)))
           (if (= 0 prec)
               (gg-translate-alt rest prec expr)
             (cons expr rest))))

        (t
         (error "Invalid expression %s" alt)))))))

(defun gg-translate-rules (rules)
  "Internal.

Translates a list of rules, RULES, through repeated application of `gg-rule'."
  (mapcar (lambda (rule) (apply 'gg-rule rule)) rules))

(defmacro gg-grammar (&rest rules)
  "Translates RULES into a grammar.
See `gg-rule' for the syntax of rules. The first is the start rule."
  `(make-gg-grammar :rules '(,@(gg-translate-rules rules))))

(defun gg-parse (gg-grammar)
  "Parses GG-GRAMMAR from the current point.
On success, `gg-parse' returns (t . result) and point is moved to
the end of the successful parse. On failure, `gg-parse' returns
nil and point is not moved."
  (let ((gg-memo-table (make-hash-table :test 'equal))
        (gg-heads (make-hash-table :test 'eq))
        (gg-lr-stack nil))
    (gg-eval (gg-call (gg-grammar-start gg-grammar)))))

(provide 'guruguru)
