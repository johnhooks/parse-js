;;;; test/lexer.el --- Tests for parse-js.el lexer.

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

(defun parse-js-test-assq (key one two)
  (should (equal (cadr (assq key one))
                 (cadr (assq key two)))))

(defun parse-js-test-lexer (depth test-token)
  (let (token
        (index 0))
    (while (< index depth)
      (setq token (parse-js-get-token)
            index (1+ index)))
    (should (equal (assq 'type token)
                   (assq 'type test-token)))
    (should (equal (assq 'start token)
                   (assq 'start test-token)))
    (should (equal (assq 'end token)
                   (assq 'end test-token)))
    (if (assq 'value test-token)
        (should (equal (assq 'value token)
                       (assq 'value test-token))))))

(defun parse-js-test-lexer-err ()
  (while (not (eq parse-js-EOF (cadr (assq 'type (parse-js-get-token)))))))

(defun parse-js-test-lexer-warn (start end)
  (while (not (eq parse-js-EOF (cadr (assq 'type (parse-js-get-token))))))
  (let ((warning (pop parse-js--warnings)))
    (should (equal start (car warning)))
    (should (equal end (cadr warning)))))

(defun parse-js-test-lexer-ctx (depth expr-allowed)
  (let (token
        (index 0))
    (while (< index depth)
      (setq token (parse-js-get-token)
            index (1+ index)))
    (should (equal parse-js--expr-allowed expr-allowed))))

(cl-defmacro parse-js-test-env (content bind func)
  ;; The test environment.
  `(let ,(append '()                    ; Put any default variables here.
                 bind)
     (with-temp-buffer
       (insert ,content)
       (js-mode)
       (parse-js-config)
       (funcall ,func))))

(cl-defmacro parse-js-deftest-lexer (name content type &key bind value length (start 1) (depth 1))
  (declare (indent defun))
  `(ert-deftest ,(intern (format "parse-js-test-lex-%s" name)) ()
     (parse-js-test-env ,content ,bind
                #'(lambda ()
                    (parse-js-test-lexer ,depth (list (list 'type ,type)
                                              (list 'start ,start)
                                              (list 'end ,(+ start length))
                                              ,(when value
                                                 `(list 'value ,value))))))))

(cl-defmacro parse-js-deftest-lexer-err (name content expected &key bind)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "parse-js-test-lex-error-%s" name)) ()
     (parse-js-test-env ,content ,bind
                #'(lambda ()
                    (should-error
                     (parse-js-test-lexer-err)
                     :type ,expected)))))

(cl-defmacro parse-js-deftest-lexer-warning (name content &key start end bind)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "parse-js-test-lex-warning-%s" name)) ()
     (parse-js-test-env ,content ,bind
                #'(lambda ()
                    (parse-js-test-lexer-warn ,start ,end)))))

;;; Punctuation

(parse-js-deftest-lexer punc-brace-l
  "{" parse-js-BRACE-L :length 1)

(parse-js-deftest-lexer punc-brace-r
  "}" parse-js-BRACE-R :length 1)

(parse-js-deftest-lexer punc-paren-l
  "(" parse-js-PAREN-L :length 1)

(parse-js-deftest-lexer punc-paren-r
  ")" parse-js-PAREN-R :length 1)

(parse-js-deftest-lexer punc-bracket-l
  "[" parse-js-BRACKET-L :length 1)

(parse-js-deftest-lexer punc-bracket-r
  "]" parse-js-BRACKET-R :length 1)

(parse-js-deftest-lexer punc-comma
  "," parse-js-COMMA :length 1)

(parse-js-deftest-lexer punc-semi
  ";" parse-js-SEMI :length 1)

(parse-js-deftest-lexer punc-colon
  ":" parse-js-COLON :length 1)

(parse-js-deftest-lexer punc-dot
  "." parse-js-DOT :length 1)

(parse-js-deftest-lexer punc-question
  "?" parse-js-QUESTION :length 1)

(parse-js-deftest-lexer punc-arrow
  "=>" parse-js-ARROW :length 2)

(parse-js-deftest-lexer punc-ellipsis
  "..." parse-js-ELLIPSIS :length 3)

(parse-js-deftest-lexer punc-dollar-brace-l
  ;; An empty `parse-js-TEMPLATE' is read before the `parse-js-DOLLAR-BRACE-L',
  ;; hence the :depth of 3.
  "`${" parse-js-DOLLAR-BRACE-L :length 2 :start 2 :depth 3)

(parse-js-deftest-lexer punc-backquote
  "`" parse-js-BACKQUOTE :length 1)

;;; Operators

(parse-js-deftest-lexer op-slash
  "4/5" parse-js-SLASH :start 2 :length 1 :depth 2)

(parse-js-deftest-lexer op-slash-after-single-line-comment
  "// foo bar \n 4/4" parse-js-SLASH :start 15 :length 1 :depth 3)

(parse-js-deftest-lexer op-assign-division
  "foo/=10" parse-js-ASSIGN :start 4 :length 2 :depth 2)

(parse-js-deftest-lexer op-star
  "*" parse-js-STAR :length 1)

(parse-js-deftest-lexer op-star-assign
  "*=" parse-js-ASSIGN :length 2)

(parse-js-deftest-lexer op-starstar
  "**" parse-js-STARSTAR :length 2)

(parse-js-deftest-lexer op-starstar-assign
  "**=" parse-js-ASSIGN :length 3)

(parse-js-deftest-lexer op-modulo
  "%" parse-js-MODULO :length 1)

(parse-js-deftest-lexer op-modulo-assign
  "%=" parse-js-ASSIGN :length 2)

(parse-js-deftest-lexer op-logical-and
  "&&" parse-js-LOGICAL-AND :length 2)

(parse-js-deftest-lexer op-logical-or
  "||" parse-js-LOGICAL-OR :length 2)

(parse-js-deftest-lexer op-bitwise-and
  "&" parse-js-BITWISE-AND :length 1)

(parse-js-deftest-lexer op-bitwise-and-assign
  "&=" parse-js-ASSIGN :length 2)

(parse-js-deftest-lexer op-bitwise-or
  "|" parse-js-BITWISE-OR :length 1)

(parse-js-deftest-lexer op-bitwise-or-assign
  "|=" parse-js-ASSIGN :length 2)

(parse-js-deftest-lexer op-bitwise-xor
  "^" parse-js-BITWISE-XOR :length 1)

(parse-js-deftest-lexer op-bitwise-xor-assign
  "^=" parse-js-ASSIGN :length 2)

(parse-js-deftest-lexer op-increment
  "++" parse-js-INC-DEC :length 2)

(parse-js-deftest-lexer op-decrement
  "--" parse-js-INC-DEC :length 2)

(parse-js-deftest-lexer op-plus
  "+" parse-js-PLUS-MIN :length 1)

(parse-js-deftest-lexer op-plus-assign
  "+=" parse-js-ASSIGN :length 2)

(parse-js-deftest-lexer op-minus
  "-" parse-js-PLUS-MIN :length 1)

(parse-js-deftest-lexer op-minus-assign
  "-=" parse-js-ASSIGN :length 2)

(parse-js-deftest-lexer op-greater-than
  ">" parse-js-RELATIONAL :length 1)

(parse-js-deftest-lexer op-greater-than-or-equal
  ">=" parse-js-RELATIONAL :length 2)

(parse-js-deftest-lexer op-less-than
  "<" parse-js-RELATIONAL :length 1)

(parse-js-deftest-lexer op-less-than-or-equal
  "<=" parse-js-RELATIONAL :length 2)

(parse-js-deftest-lexer op-bitshift-left
  "<<" parse-js-BITSHIFT :length 2)

(parse-js-deftest-lexer op-bitshift-left-assign
  "<<=" parse-js-ASSIGN :length 3)

(parse-js-deftest-lexer op-bitshift-right
  ">>" parse-js-BITSHIFT :length 2)

(parse-js-deftest-lexer op-bitshift-right-assign
  ">>=" parse-js-ASSIGN :length 3)

(parse-js-deftest-lexer op-bitshift-right-zero
  ">>>" parse-js-BITSHIFT :length 3)

(parse-js-deftest-lexer op-bitshift-right-zero-assign
  ">>>=" parse-js-ASSIGN :length 4)

(parse-js-deftest-lexer op-eq
  "=" parse-js-EQ :length 1)

(parse-js-deftest-lexer op-negation
  "!" parse-js-PREFIX :length 1)

(parse-js-deftest-lexer op-equal
  "==" parse-js-EQUALITY :length 2)

(parse-js-deftest-lexer op-equal-strict
  "===" parse-js-EQUALITY :length 3)

(parse-js-deftest-lexer op-not-equal
  "!=" parse-js-EQUALITY :length 2)

(parse-js-deftest-lexer op-not-equal-strict
  "!==" parse-js-EQUALITY :length 3)

;;; Names & Identifiers

(parse-js-deftest-lexer name
  "foo" parse-js-NAME :length 3)

(parse-js-deftest-lexer name-dollar
  "foo$" parse-js-NAME :length 4)

(parse-js-deftest-lexer name-dollar-start
  "$foo" parse-js-NAME :length 4)

(parse-js-deftest-lexer name-underscore
  "foo_bar" parse-js-NAME :length 7)

(parse-js-deftest-lexer name-underscore-start
  "_foo" parse-js-NAME :length 4)

(parse-js-deftest-lexer name-number-end
  "foo256" parse-js-NAME :length 6)

(parse-js-deftest-lexer name-unicode-escape
  "\\u0066oo" parse-js-NAME :length 8 :value "foo")

(parse-js-deftest-lexer name-unicode-code-point
  "\\u{62}ar" parse-js-NAME :length 8 :value "bar")

(parse-js-deftest-lexer keyword-unicode-escape
  "\\u0073witch" parse-js-SWITCH :length 11)

;; (parse-js-deftest-lexer no-keyword-after-dot
;;   ".switch" parse-js-NAME :start 2 :length 6 :depth 2 :value "switch")

(parse-js-deftest-lexer no-keyword-escapes-ecma-5-and-below
  "\\u0073witch" parse-js-NAME :length 11 :value "switch"
  :bind ((parse-js-ecma-version 5)))

;;; Name & Identifier Errors

(parse-js-deftest-lexer-err unexpected-character-error
  "foo @" 'parse-js-unexpected-character-error)

;;; Name & Identifier Warnings

(parse-js-deftest-lexer-warning identifier-invalid-escape
  "foo\\x0035" :start 4 :end 6)

(parse-js-deftest-lexer-warning identifier-invalid-start
  "\\u0010foo" :start 1 :end 7)

(parse-js-deftest-lexer-warning identifier-invalid-part
  "foo\\u0010" :start 4 :end 10)

;;; Numbers

(parse-js-deftest-lexer integer-zero
  "0" parse-js-NUM :length 1)

(parse-js-deftest-lexer float-zero
  "0.0" parse-js-NUM :length 3)

(parse-js-deftest-lexer integer-base-two
  "0b10000000000000000" parse-js-NUM :length 19)

(parse-js-deftest-lexer integer-base-two-upper
  "0B10000000000000000" parse-js-NUM :length 19)

(parse-js-deftest-lexer integer-base-eight
  "0o200000" parse-js-NUM :length 8)

(parse-js-deftest-lexer integer-base-eight-upper
  "0O200000" parse-js-NUM :length 8)

(parse-js-deftest-lexer integer-base-eight-leading-zero
  "0200000" parse-js-NUM :length 7)

(parse-js-deftest-lexer integer-base-ten
  "65536" parse-js-NUM :length 5)

(parse-js-deftest-lexer integer-base-twelve
  "0x10000" parse-js-NUM :length 7)

(parse-js-deftest-lexer integer-base-twelve-upper
  "0X10000" parse-js-NUM :length 7)

(parse-js-deftest-lexer float-basic
  "65536.0" parse-js-NUM :length 7)

(parse-js-deftest-lexer float-exponent
  "6.5536e4" parse-js-NUM :length 8)

(parse-js-deftest-lexer float-exponent-positive
  "6.5536e+4" parse-js-NUM :length 9)

(parse-js-deftest-lexer float-exponent-negative
  "655360000e-4" parse-js-NUM :length 12)

(parse-js-deftest-lexer float-exponent-capital
  "6.5536E+4" parse-js-NUM :length 9)

(parse-js-deftest-lexer float-start-with-dot
  ".65536e5" parse-js-NUM :length 8)

;;; Number Warnings

(parse-js-deftest-lexer-warning float-missing-exponent
  "6.5536e foo" :start 1 :end 8)

(parse-js-deftest-lexer-warning float-missing-exponent-with-sign
  "6.5536e- foo" :start 1 :end 9)

(parse-js-deftest-lexer-warning identifier-after-number
  "65536foo" :start 6 :end 9)

;;; Strings

(parse-js-deftest-lexer string-single-quotes
  "'hello foo'" parse-js-STRING :length 11)

(parse-js-deftest-lexer string-single-quotes-escape-single-quote
  "'hello \\\'foo\\\''" parse-js-STRING :length 15)

(parse-js-deftest-lexer string-double-quotes
  "\"hello foo\"" parse-js-STRING :length 11)

(parse-js-deftest-lexer string-double-quotes-escape-double-quote
  "\"hello \\\"foo\\\"\"" parse-js-STRING :length 15)

(parse-js-deftest-lexer string-hex-escape
  "\"fo\\x6f bar\"" parse-js-STRING :length 12)

(parse-js-deftest-lexer string-escape-sequence
  "'foo \\u0620ar'" parse-js-STRING :length 14)

(parse-js-deftest-lexer string-code-point
  "'foo \\u{62}ar'" parse-js-STRING :length 14)

(parse-js-deftest-lexer string-allow-octal-in-non-strict
  "\"foo \\42ar\"" parse-js-STRING :length 11
  :bind ((parse-js--strict nil)))

;;; String Warnings

(parse-js-deftest-lexer-warning string-missing-delimiter
  "\"foo bar baz" :start 1 :end 13)

(parse-js-deftest-lexer-warning string-octal-in-strict
  "\"foo \\42ar\"" :start 6 :end 9
  :bind ((parse-js--strict t)))

(parse-js-deftest-lexer-warning string-invalid-hex-escape
  "\"foo \\x6g\"" :start 6 :end 9)

;;; Template Strings

(parse-js-deftest-lexer template-basic
  "`foo bar`" parse-js-TEMPLATE :length 7 :start 2 :depth 2)

(parse-js-deftest-lexer template-hex-escape
  "`fo\\x6f bar`" parse-js-TEMPLATE :length 10 :start 2 :depth 2)

(parse-js-deftest-lexer template-escape-sequence
  "`foo \\u0062ar`" parse-js-TEMPLATE :length 12 :start 2 :depth 2)

(parse-js-deftest-lexer template-code-point
  "`foo \\u{62}ar`" parse-js-TEMPLATE :length 12 :start 2 :depth 2)

;;; Template String Warnings

;; (parse-js-deftest-lexer-err template-invalid-below-ecma-6
;;   "`foo bar`" 'parse-js-unexpected-character-error
;;   :bind ((parse-js-ecma-version 5)))

(parse-js-deftest-lexer-warning template-invalid-octal
  "`foo \\42ar`" :start 6 :end 9)

;;; Regular Expressions

(parse-js-deftest-lexer regexp-no-flags
  "/foo ba[rR]/" parse-js-REGEXP :length 12)

(parse-js-deftest-lexer regexp-with-flags
  "/foo ba[rR]/gi" parse-js-REGEXP :length 14)

(parse-js-deftest-lexer regexp-escape-closing-delimiter
  "/foo\\/bar/" parse-js-REGEXP :length 10)

(parse-js-deftest-lexer regexp-forward-slash-in-class
  "/foo[/]bar/" parse-js-REGEXP :length 11)

(parse-js-deftest-lexer regexp-escape-closing-class-delimiter
  "/foo[\\]]bar/" parse-js-REGEXP :length 12)

(parse-js-deftest-lexer regexp-with-octal-escape
  "/f[oO]*\\142a[rR]/" parse-js-REGEXP :length 17 :bind ((parse-js--strict nil)))

(parse-js-deftest-lexer regexp-with-octal-escape-in-class
  "/f[oO]*ba[\\162R]/" parse-js-REGEXP :length 17 :bind ((parse-js--strict nil)))

(parse-js-deftest-lexer regexp-with-hex-escape
  "/f[oO]*\\x62a[rR]/" parse-js-REGEXP :length 17)

(parse-js-deftest-lexer regexp-with-hex-escape-in-class
  "/f[oO]*ba[\\x72R]/" parse-js-REGEXP :length 17)

(parse-js-deftest-lexer regexp-with-escape-sequence
  "/f[oO]*\\u0062a[rR]/" parse-js-REGEXP :length 19)

(parse-js-deftest-lexer regexp-with-escape-sequence-in-class
  "/f[oO]*ba[\\u0072R]/" parse-js-REGEXP :length 19)

(parse-js-deftest-lexer regexp-with-code-point
  "/f[oO]*\\u{62}a[rR]/" parse-js-REGEXP :length 19)

(parse-js-deftest-lexer regexp-with-code-point-in-class
  "/f[oO]*ba[\\u{72}R]/" parse-js-REGEXP :length 19)

;;; Regular Expression Warnings

(parse-js-deftest-lexer-warning regexp-missing-delimiter
  "/ba[rz]" :start 1 :end 8)

(parse-js-deftest-lexer-warning regexp-missing-delimiter-class
  "/ba[rz/" :start 1 :end 8)

;;; Keywords

(mapc #'(lambda (tt)
          (eval
           `(parse-js-deftest-lexer
              ,(intern (format "keyword-%s" (symbol-name tt)))
              ,(downcase (symbol-name tt))
              ,(intern (format "parse-js-%s" (symbol-name tt)))
              :length ,(length (symbol-name tt)))))
      '(BREAK
        CASE
        CATCH
        CONTINUE
        DEBUGGER
        DEFAULT
        DO
        ELSE
        FINALLY
        FOR
        ;; FUNCTION             ; FIXME: The addition of the update-ctx function
        IF                      ; has messed up the reference in the hash table.
        RETURN
        SWITCH
        THROW
        TRY
        VAR
        CONST
        WHILE
        WITH
        NEW
        THIS
        SUPER
        CLASS
        EXTENDS
        EXPORT
        IMPORT
        NULL
        TRUE
        FALSE
        IN
        INSTANCEOF
        TYPEOF
        VOID
        DELETE))

;; Comments

(parse-js-deftest-lexer single-line-comment
  "foo // bar\n" parse-js-COMMENT :length 7 :start 5 :depth 2)

(parse-js-deftest-lexer single-line-comment-eof
  "foo // bar" parse-js-COMMENT :length 6 :start 5 :depth 2)

(parse-js-deftest-lexer multi-line-comment
  "foo /*\n  bar  \n*/\n" parse-js-COMMENT :length 13 :start 5 :depth 2)

(parse-js-deftest-lexer multi-line-comment-eof
  "foo /*\n  bar  \n*/" parse-js-COMMENT :length 13 :start 5 :depth 2)

(parse-js-deftest-lexer multi-line-on-single-line-comment
  "foo /*  bar  */\n" parse-js-COMMENT :length 11 :start 5 :depth 2)

;;; Comment Errors

(parse-js-deftest-lexer-warning comment-missing-delimiter
  "foo /* bar \n qux" :start 5 :end 17)
