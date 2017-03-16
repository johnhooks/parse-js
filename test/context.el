;;;; test/context.el --- Tests for parse-js.el context stack.

;;; Copyright (C) 2016 John Hooks

;; This file is part of parse-js.el
;;
;; parse-js.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; parse-js.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with parse-js.el.  If not, see <http://www.gnu.org/licenses/>.

;;; The Code:

(require 'ert)
(require 'parse-js)

(defun parse-js-test-env (body)
  `(with-temp-buffer
     (js-mode)
     (parse-js-config)
     ,@body))

(defmacro parse-js-deftest (name &rest body)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "parse-js-test-ctx-%s" name)) ()
     ,(parse-js-test-env body)))

(parse-js-deftest brace-is-block-colon-b-stat
  (let ((parse-js--ctx-stack (list parse-js-B-STAT)))
    (should (eq t (parse-js--brace-is-block-p parse-js-COLON)))))

(parse-js-deftest brace-is-block-colon-b-expr
  (let ((parse-js--ctx-stack (list parse-js-B-EXPR)))
    (should (eq nil (parse-js--brace-is-block-p parse-js-COLON)))))

(parse-js-deftest brace-is-block-return
  (insert "return {}")
  (goto-char 1)
  (parse-js-get-token)                        ; move over return
  (parse-js--skip-whitespace-and-comments)
  (should (eq nil (parse-js--brace-is-block-p parse-js-RETURN))))

(parse-js-deftest brace-is-block-return-newline
  (insert "return \n {}")
  (goto-char 1)
  (parse-js-get-token)                        ; move over return
  (parse-js--skip-whitespace-and-comments)
  (should (eq t (parse-js--brace-is-block-p parse-js-RETURN))))

(parse-js-deftest brace-is-block-else
  (should (eq t (parse-js--brace-is-block-p parse-js-ELSE))))

(parse-js-deftest brace-is-block-semi
  (should (eq t (parse-js--brace-is-block-p parse-js-SEMI))))

(parse-js-deftest brace-is-block-eof
  (should (eq t (parse-js--brace-is-block-p parse-js-EOF))))

(parse-js-deftest brace-is-block-paren-r
  (should (eq t (parse-js--brace-is-block-p parse-js-PAREN-R))))

(parse-js-deftest brace-is-block-brace-l-b-stat
  (let ((parse-js--ctx-stack (list parse-js-B-STAT)))
    (should (eq t (parse-js--brace-is-block-p parse-js-BRACE-L)))))

(parse-js-deftest brace-is-block-expr-allowed
  ;; The argument to `parse-js--brace-is-block-p' is just a place holder.
  (let ((parse-js--expr-allowed t))
    (should (eq nil (parse-js--brace-is-block-p parse-js-ASSIGN)))))

(parse-js-deftest brace-is-block-expr-not-allowed
  ;; The argument to `parse-js--brace-is-block-p' is just a place holder.
  (let ((parse-js--expr-allowed nil))
    (should (eq t (parse-js--brace-is-block-p parse-js-BRACE-R)))))

;;; Update Context Functions

;; Note: The following Token Types have their own update-ctx functions:
;; - parse-js-BRACE-L
;; - parse-js-BRACE-R
;; - parse-js-PAREN-L
;; - parse-js-PAREN-R
;; - parse-js-DOLLAR-BRACE-L
;; - parse-js-BACKQUOTE
;; - parse-js-INC-DEC
;; - parse-js-FUNCTION

;;; Update Context parse-js-BRACE-L

(parse-js-deftest update-brace-l
  ;; Whether or not brace is a block, `parse-js--expr-allowed' should be t
  (let ((parse-js--type parse-js-BRACE-L))
    (parse-js--update-ctx nil)
    (should (eq t parse-js--expr-allowed))))

(parse-js-deftest stack-brace-l-b-stat
  ;; When `parse-js--brace-is-block-p' returns t, `parse-js--update-ctx' pushs `parse-js-B-STAT'
  ;; on to the top of `parse-js--ctx-stack'.
  (let ((parse-js--type parse-js-BRACE-L)
        (parse-js--expr-allowed nil))           ; `parse-js--brace-is-block-p' returns t
    (parse-js--update-ctx nil)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT
                         parse-js-B-STAT)))))

(parse-js-deftest stack-brace-l-b-expr
  ;; When `parse-js--brace-is-block-p' returns nil, `parse-js--update-ctx' pushs `parse-js-B-EXPR'
  ;; on to the top of `parse-js--ctx-stack'.
  (let ((parse-js--type parse-js-BRACE-L)
        (parse-js--expr-allowed t))             ; `parse-js--brace-is-block-p' returns nil
    (parse-js--update-ctx nil)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-EXPR
                         parse-js-B-STAT)))))

;;; Update Context parse-js-BRACE-R
;; Note: parse-js-PAREN-R uses the exact same function as well.

(parse-js-deftest update-brace-r
  (let ((parse-js--type parse-js-BRACE-R))
    (parse-js--update-ctx nil)
    (should (eq t parse-js--expr-allowed))))

(parse-js-deftest update-brace-r-f-expr
  (let ((parse-js--type parse-js-BRACE-R))
    (push parse-js-F-EXPR parse-js--ctx-stack)
    (push parse-js-B-STAT parse-js--ctx-stack)
    (parse-js--update-ctx nil)
    (should (eq nil parse-js--expr-allowed))))

(parse-js-deftest update-brace-r-b-tmpl
  (let ((parse-js--type parse-js-BRACE-R))
    (push parse-js-B-TMPL parse-js--ctx-stack)
    (parse-js--update-ctx nil)
    (should (eq t parse-js--expr-allowed))))

(parse-js-deftest update-brace-r-b-expr
  (let ((parse-js--type parse-js-BRACE-R))
    (push parse-js-B-EXPR parse-js--ctx-stack)
    (parse-js--update-ctx nil)
    (should (eq nil parse-js--expr-allowed))))

(parse-js-deftest update-brace-r-b-stat
  (let ((parse-js--type parse-js-BRACE-R))
    (push parse-js-B-STAT parse-js--ctx-stack)
    (parse-js--update-ctx nil)
    (should (eq t parse-js--expr-allowed))))

(parse-js-deftest stack-brace-r-f-expr
  (let ((parse-js--type parse-js-BRACE-R))
    (push parse-js-F-EXPR parse-js--ctx-stack)
    (push parse-js-B-STAT parse-js--ctx-stack)
    (parse-js--update-ctx nil)                  ; Should pop parse-js-B-STAT and parse-js-F-EXPR
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

(parse-js-deftest stack-brace-r-b-tmpl
  (let ((parse-js--type parse-js-BRACE-R))
    (push parse-js-B-TMPL parse-js--ctx-stack)
    (parse-js--update-ctx nil)                  ; Should pop parse-js-B-TMPL
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

(parse-js-deftest stack-brace-r-b-expr
  (let ((parse-js--type parse-js-BRACE-R))
    (push parse-js-B-EXPR parse-js--ctx-stack)
    (parse-js--update-ctx nil)                  ; Should pop parse-js-B-EXPR
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

(parse-js-deftest stack-brace-r-b-stat
  (let ((parse-js--type parse-js-BRACE-R))
    (push parse-js-B-STAT parse-js--ctx-stack)
    (parse-js--update-ctx nil)                  ; Should pop parse-js-B-STAT
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

;;; Update Context parse-js-PAREN-L

(parse-js-deftest update-paren-l
  ;; `parse-js--expr-allowed' should be allows be switched to t
  (let ((parse-js--type parse-js-PAREN-L))
    (parse-js--update-ctx nil)
    (should (eq t parse-js--expr-allowed))))

(parse-js-deftest stack-paren-l-if
  (let ((parse-js--type parse-js-PAREN-L))
    (parse-js--update-ctx parse-js-IF)
    (should (equal parse-js--ctx-stack
                   (list parse-js-P-STAT
                         parse-js-B-STAT)))))

(parse-js-deftest stack-paren-l-for
  (let ((parse-js--type parse-js-PAREN-L))
    (parse-js--update-ctx parse-js-FOR)
    (should (equal parse-js--ctx-stack
                   (list parse-js-P-STAT
                         parse-js-B-STAT)))))

(parse-js-deftest stack-paren-l-with
  (let ((parse-js--type parse-js-PAREN-L))
    (parse-js--update-ctx parse-js-WITH)
    (should (equal parse-js--ctx-stack
                   (list parse-js-P-STAT
                         parse-js-B-STAT)))))

(parse-js-deftest stack-paren-l-while
  (let ((parse-js--type parse-js-PAREN-L))
    (parse-js--update-ctx parse-js-WHILE)
    (should (equal parse-js--ctx-stack
                   (list parse-js-P-STAT
                         parse-js-B-STAT)))))

(parse-js-deftest stack-paren-l-p-expr
  (let ((parse-js--type parse-js-PAREN-L))
    (parse-js--update-ctx nil)                  ; Anything else should push parse-js-P-EXPR
    (should (equal parse-js--ctx-stack
                   (list parse-js-P-EXPR
                         parse-js-B-STAT)))))

;;; Update Context parse-js-DOLLAR-BRACE-L

(parse-js-deftest update-dollar-brace-l
  ;; `parse-js--expr-allowed' should be always be switched to t
  (let ((parse-js--type parse-js-DOLLAR-BRACE-L))
    (parse-js--update-ctx nil)
    (should (eq t parse-js--expr-allowed))))


(parse-js-deftest stack-dollar-brace-l
  (let ((parse-js--type parse-js-DOLLAR-BRACE-L))
    (parse-js--update-ctx nil)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-TMPL
                         parse-js-B-STAT)))))

;;; Update Context parse-js-BACKQUOTE

(parse-js-deftest update-backquote
  (let ((parse-js--type parse-js-BACKQUOTE))
    (parse-js--update-ctx nil)
    (should (eq nil parse-js--expr-allowed))))

(parse-js-deftest stack-backquote-start
  (let ((parse-js--type parse-js-BACKQUOTE))
    (parse-js--update-ctx nil)
    (should (equal parse-js--ctx-stack
                   (list parse-js-Q-TMPL
                         parse-js-B-STAT)))))

(parse-js-deftest stack-backquote-end
  (let ((parse-js--type parse-js-BACKQUOTE))
    (push parse-js-Q-TMPL parse-js--ctx-stack)
    (parse-js--update-ctx nil)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

;;; Update Context parse-js-INC-DEC
;; Note: `parse-js--expr-allowed' should be left alone

(parse-js-deftest update-inc-dec-previous-nil
  (let ((parse-js--type parse-js-INC-DEC)
        (parse-js--expr-allowed nil))
    (parse-js--update-ctx nil)
    (should (eq nil parse-js--expr-allowed))))

(parse-js-deftest update-inc-dec-previous-t
  (let ((parse-js--type parse-js-INC-DEC)
        (parse-js--expr-allowed t))
    (parse-js--update-ctx nil)
    (should (eq t parse-js--expr-allowed))))

;;; Update Context parse-js-FUNCTION
;;  Note: `parse-js--expr-allowed' should always be switched to nil

(parse-js-deftest update-function
  (let ((parse-js--type parse-js-FUNCTION))
    (parse-js--update-ctx nil)
    (should (eq nil parse-js--expr-allowed))))

(parse-js-deftest stack-function-prev-tt-not-before-expr
  (let ((parse-js--type parse-js-FUNCTION)
        (parse-js--expr-allowed nil))
    (parse-js--update-ctx nil)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

(parse-js-deftest stack-function-prev-tt-before-expr-default
  ;; Note: parse-js-B-STAT also has to be parse-js--current-ctx
  (let ((parse-js--type parse-js-FUNCTION)
        (parse-js--expr-allowed t))
    (parse-js--update-ctx '((before-expr t)))
    (should (equal parse-js--ctx-stack
                   (list parse-js-F-EXPR
                         parse-js-B-STAT)))))

(parse-js-deftest stack-function-prev-tt-before-expr-semi
  (let ((parse-js--type parse-js-FUNCTION)
        (parse-js--expr-allowed t))
    (parse-js--update-ctx parse-js-SEMI)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

(parse-js-deftest stack-function-prev-tt-before-expr-else
  (let ((parse-js--type parse-js-FUNCTION)
        (parse-js--expr-allowed t))
    (parse-js--update-ctx parse-js-ELSE)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

(parse-js-deftest stack-function-prev-tt-before-expr-colon
  (let ((parse-js--type parse-js-FUNCTION)
        (parse-js--expr-allowed t))
    (parse-js--update-ctx parse-js-COLON)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

(parse-js-deftest stack-function-prev-tt-before-expr-brace-l
  (let ((parse-js--type parse-js-FUNCTION)
        (parse-js--expr-allowed t))
    (parse-js--update-ctx parse-js-BRACE-L)
    (should (equal parse-js--ctx-stack
                   (list parse-js-B-STAT)))))

;; For the remaining Token Types, the presence or absence of the
;; alist label 'before-expr is used to update the `parse-js--ctx-stack'.
(mapc #'(lambda (tt)
          (eval
           `(parse-js-deftest
              ,(intern (format "before-expr-%s" (symbol-name tt)))
              (let ((parse-js--type ,(intern (format "parse-js-%s" (symbol-name tt)))))
                (parse-js--update-ctx parse-js-EOF)
                (should (eq t parse-js--expr-allowed))))))
      '(;; BRACKET-L                       ; Has own update-ctx function.
        ;; BRACE-L                         ; Has own update-ctx function.
        ;; PAREN-L                         ; Has own update-ctx function.
        COMMA
        SEMI
        COLON
        QUESTION
        ARROW
        ELLIPSIS
        ;; DOLLOR-BRACE-L                  ; Has own update-ctx function.
        EQ
        ASSIGN
        PREFIX
        LOGICAL-OR
        LOGICAL-AND
        BITWISE-OR
        BITWISE-XOR
        BITWISE-AND
        EQUALITY
        RELATIONAL
        BITSHIFT
        PLUS-MIN
        MODULO
        STAR
        STARSTAR
        SLASH
        CASE
        DO
        ELSE
        RETURN
        THROW
        NEW
        EXTENDS
        IN
        INSTANCEOF
        TYPEOF
        VOID
        DELETE))


(mapc #'(lambda (tt)
          (eval
           `(parse-js-deftest
              ,(intern (format "not-before-expr-%s" (symbol-name tt)))
              (let ((parse-js--type ,(intern (format "parse-js-%s" (symbol-name tt)))))
                (parse-js--update-ctx parse-js-EOF)
                (should (eq nil parse-js--expr-allowed))))))
      '(NUM
        REGEXP
        STRING
        NAME
        EOF
        ;; BRACKET-R                       ; Has own update-ctx function.
        DOT
        TEMPLATE
        ;; BACKQUOTE                       ; Has own update-ctx function.
        ;; INC-DEC                         ; Has own update-ctx function.
        BREAK
        CATCH
        DEFAULT
        CONTINUE
        DEBUGGER
        FINALLY
        FOR
        FUNCTION
        IF
        SWITCH
        TRY
        VAR
        CONST
        WHILE
        WITH
        THIS
        SUPER
        CLASS
        EXPORT
        IMPORT
        NULL
        TRUE
        FALSE))
