;;;; parse-js.el --- A Simple JavaScript Parser -*- lexical-binding: t; -*-

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

;;; Commentary:

;; The name of the project and the parser are inspired by parse-js
;; by Marijn Haverbeke, github.com/marijnh/parse-js.
;; The tokenizer is based on Acorn, github.com/ternjs/acorn.

;;; Code:

(defgroup parse-js-mode nil
  "A JavaScript parser minor-mode."
  :group 'languages)

(defcustom parse-js-ecma-version 6
  "ECMAScript version used to parse."
  :type 'number
  :group 'parse-js-mode)

(defcustom parse-js-always-strict t
  "Parse all JavaScript as module code."
  :type 'boolean
  :group 'parse-js-mode)

;;; Error Master

(define-error 'parse-js-error "A parse-js error")

;;; Errors Tokenizer

(define-error 'parse-js-token-error
  "A parse-js tokenizer error" 'parse-js-error)

(define-error 'parse-js-unexpected-character-error
  "Unexpected character" 'parse-js-token-error)

(define-error 'parse-js-identifier-error
  "Unable to parse identifier" 'parse-js-token-error)
(define-error 'parse-js-identifier-start-error
  "Invalid identifier starting character" 'parse-js-identifier-error)
(define-error 'parse-js-identifier-part-error
  "Invalid identifier character" 'parse-js-identifier-error)
(define-error 'parse-js-unexpected-identifier-error
  "Unexpected identifier" 'parse-js-identifier-error)

(define-error 'parse-js-number-error
  "Error while reading number" 'parse-js-token-error)
(define-error 'parse-js-number-invalid-error
  "Invalid number" 'parse-js-number-error)

(define-error 'parse-js-string-error
  "Error while reading string" 'parse-js-token-error)
(define-error 'parse-js-string-delimiter-error
  "Missing string closing delimiter" 'parse-js-string-error)
(define-error 'parse-js-string-hex-escape-error
  "Invalid hexadecimal escape sequence" 'parse-js-string-error)
(define-error 'parse-js-string-octal-strict-error
  "Invalid octal literal in strict mode" 'parse-js-string-error)

(define-error 'parse-js-template-error
  "Error while reading template string" 'parse-js-token-error)
(define-error 'parse-js-template-delimiter-errror
  "Missing template closing delimiter" 'parse-js-template-error)
(define-error 'parse-js-template-octal-error
  "Invalid octal literal in template string" 'parse-js-template-error)

(define-error 'parse-js-code-point-error
  "Error while reading Unicode code point" 'parse-js-token-error)
(define-error 'parse-js-code-point-escape-error
  "Invalid code point escape" 'parse-js-code-point-error)
(define-error 'parse-js-code-point-bound-error
  "Code point out of bounds" 'parse-js-code-point-error)
(define-error 'parse-js-code-point-delimiter-error
  "Missing code point closing delimiter" 'parse-js-code-point-error)

(define-error 'parse-js-escape-sequence-error   ; Should it be code-point-error?
  "Invalid escape sequence" 'parse-js-code-point-error)

(define-error 'parse-js-regexp-error
  "Error while reading regular expression" 'parse-js-token-error)
(define-error 'parse-js-regexp-delimiter-error
  "Missing regex closing delimiter" 'parse-js-regexp-error)
(define-error 'parse-js-regexp-flags-error
  "Invalid regexp flags" 'parse-js-regexp-error)

;;; Errors Parser

(define-error 'parse-js-parse-error
  "A parse-js parsing error" 'parse-js-error)

;;; Reserved Words Global Map

(defvar parse-js-reserve-words (make-hash-table :test 'equal)
  "Hash table to map reserved words to different environments.")

(puthash 3 '("abstract" "boolean" "byte" "char" "class" "double"
             "enum" "export" "extends" "final" "float" "goto"
             "implements" "import" "int" "interface" "long" "native"
             "package" "private" "protected" "public" "short" "static"
             "super" "synchronized" "throws" "transient" "volatile")
         parse-js-reserve-words)

(puthash 5 '("class" "enum" "extends" "super" "const" "export" "import")
         parse-js-reserve-words)

(puthash 6 '("enum")
         parse-js-reserve-words)

(puthash 'strict '("implements" "interface" "let" "package" "private"
                   "protected" "public" "static" "yield")
         parse-js-reserve-words)

(puthash 'strict-bind '("eval" "arguments")
         parse-js-reserve-words)

;;; Token Types

;; Quoted from acorn/src/tokentype.js

;;   "The assignment of fine-grained, information-carrying type objects
;;    allows the tokenizer to store the information it has about a
;;    token in a way that is very cheap for the parser to look up.

;;    All token type variables start with an underscore, to make them
;;    easy to recognize.

;;    The `beforeExpr` property is used to disambiguate between regular
;;    expressions and divisions. It is set on all token types that can
;;    be followed by an expression (thus, a slash after them would be a
;;    regular expression).
;;
;;    The `startsExpr` property is used to check if the token ends a
;;    `yield` expression. It is set on all token types that either can
;;    directly start an expression (like a quotation mark) or can
;;    continue an expression (like the body of a string).
;;
;;    `isLoop` marks a keyword as starting a loop, which is important
;;    to know when parsing a label, in order to allow or disallow
;;    continue jumps to that label."

(defvar parse-js-keywords (make-hash-table :test 'equal)
  "Hash table to map keyword names to token types.")

;; Perhaps just make token type a regular list...
(defun parse-js--make-tt (label &rest options)
  "Return token type alist with LABEL and token type OPTIONS."
  `(,@(mapcar (lambda (option)
                (if (integerp option)
                    `(binop ,option)
                  `(,option t)))
              options)
    (label, label)))

(defun parse-js--make-binop (label prec)
  "Return binary operation token type with LABEL and PREC precedence."
  (parse-js--make-tt label prec 'before-expr))

(defun parse-js--make-keyword (label &rest options)
  "Return keyword token type with LABEL and token type OPTIONS.
Also stores a reference to the token type in `parse-js-keywords'."
  (puthash (intern label)
           (apply #'parse-js--make-tt `(,label ,@options keyword))
           parse-js-keywords))

(defvar parse-js-NUM (parse-js--make-tt "num" 'starts-expr))
(defvar parse-js-REGEXP (parse-js--make-tt "regexp" 'starts-expr))
(defvar parse-js-STRING (parse-js--make-tt "string" 'starts-expr))
(defvar parse-js-NAME (parse-js--make-tt "name" 'starts-expr))
(defvar parse-js-EOF (parse-js--make-tt "eof"))

;;; Punctuation Token Types

(defvar parse-js-BRACE-L (parse-js--make-tt "{" 'before-expr 'starts-expr))
(defvar parse-js-BRACE-R (parse-js--make-tt "}"))
(defvar parse-js-PAREN-L (parse-js--make-tt "(" 'before-expr 'starts-expr))
(defvar parse-js-PAREN-R (parse-js--make-tt ")"))
(defvar parse-js-BRACKET-L (parse-js--make-tt "[" 'before-expr 'starts-expr))
(defvar parse-js-BRACKET-R (parse-js--make-tt "]"))
(defvar parse-js-COMMA (parse-js--make-tt "," 'before-expr))
(defvar parse-js-SEMI (parse-js--make-tt ";" 'before-expr))
(defvar parse-js-COLON (parse-js--make-tt ":" 'before-expr))
(defvar parse-js-DOT (parse-js--make-tt "."))
(defvar parse-js-QUESTION (parse-js--make-tt "?" 'before-expr))
(defvar parse-js-ARROW (parse-js--make-tt "=>" 'before-expr))
(defvar parse-js-TEMPLATE (parse-js--make-tt "template"))
(defvar parse-js-ELLIPSIS (parse-js--make-tt "..." 'before-expr))
(defvar parse-js-DOLLAR-BRACE-L (parse-js--make-tt "${" 'before-expr 'starts-expr))
(defvar parse-js-BACKQUOTE (parse-js--make-tt "`" 'before-expr))

;;; Operator Token Types

;; Quoted from acorn/src/tokentypes.js

;;  "Operators. These carry several kinds of properties to help the
;;   parser use them properly (the presence of these properties is
;;   what categorizes them as operators).
;;
;;   `binop`, when present, specifies that this operator is a binary
;;   operator, and will refer to its precedence.
;;
;;   `prefix` and `postfix` mark the operator as a prefix or postfix
;;   unary operator.
;;
;;   `isAssign` marks all of `=`, `+=`, `-=` etcetera, which act as
;;   binary operators with a very low precedence, that should result
;;   in AssignmentExpression nodes."

(defvar parse-js-EQ (parse-js--make-tt "=" 'before-expr 'is-assign))
(defvar parse-js-ASSIGN (parse-js--make-tt "_=" 'before-expr 'is-assign))
(defvar parse-js-INC-DEC (parse-js--make-tt "++/--" 'starts-expr 'prefix 'postfix))
(defvar parse-js-PREFIX (parse-js--make-tt "prefix" 'before-expr 'starts-expr 'prefix))
(defvar parse-js-LOGICAL-OR (parse-js--make-binop "||" 1))
(defvar parse-js-LOGICAL-AND (parse-js--make-binop "&&" 2))
(defvar parse-js-BITWISE-OR (parse-js--make-binop "|" 3))
(defvar parse-js-BITWISE-XOR (parse-js--make-binop "^" 4))
(defvar parse-js-BITWISE-AND (parse-js--make-binop "&" 5))
(defvar parse-js-EQUALITY (parse-js--make-binop "==/!=" 6)) ; == != === !==
(defvar parse-js-RELATIONAL (parse-js--make-binop "</>" 7)) ; < > <= >=
(defvar parse-js-BITSHIFT (parse-js--make-binop "<</>>" 8)) ; << >> >>>
(defvar parse-js-PLUS-MIN (parse-js--make-tt "+/-" 9 'before-expr 'starts-expr 'prefix))
(defvar parse-js-MODULO (parse-js--make-binop "%" 10))
(defvar parse-js-STAR (parse-js--make-binop "*" 10))
(defvar parse-js-SLASH (parse-js--make-binop "/" 10))
(defvar parse-js-STARSTAR (parse-js--make-tt "**" 'before-expr))

;;; Keywords Token Types

(defvar parse-js-BREAK (parse-js--make-keyword "break"))
(defvar parse-js-CASE (parse-js--make-keyword "case" 'before-expr))
(defvar parse-js-CATCH (parse-js--make-keyword "catch"))
(defvar parse-js-CONTINUE (parse-js--make-keyword "continue"))
(defvar parse-js-DEBUGGER (parse-js--make-keyword "debugger"))
(defvar parse-js-DEFAULT (parse-js--make-keyword "default"))
(defvar parse-js-DO (parse-js--make-keyword "do" 'is-loop 'before-expr))
(defvar parse-js-ELSE (parse-js--make-keyword "else" 'before-expr))
(defvar parse-js-FINALLY (parse-js--make-keyword "finally"))
(defvar parse-js-FOR (parse-js--make-keyword "for" 'is-loop))
(defvar parse-js-FUNCTION (parse-js--make-keyword "function" 'starts-expr))
(defvar parse-js-IF (parse-js--make-keyword "if"))
(defvar parse-js-RETURN (parse-js--make-keyword "return" 'before-expr))
(defvar parse-js-SWITCH (parse-js--make-keyword "switch"))
(defvar parse-js-THROW (parse-js--make-keyword "throw" 'before-expr))
(defvar parse-js-TRY (parse-js--make-keyword "try"))
(defvar parse-js-VAR (parse-js--make-keyword "var"))
(defvar parse-js-CONST (parse-js--make-keyword "const"))
(defvar parse-js-WHILE (parse-js--make-keyword "while" 'is-loop))
(defvar parse-js-WITH (parse-js--make-keyword "with"))
(defvar parse-js-NEW (parse-js--make-keyword "new" 'before-expr 'starts-expr))
(defvar parse-js-THIS (parse-js--make-keyword "this" 'starts-expr))
(defvar parse-js-SUPER (parse-js--make-keyword "super" 'starts-expr))
(defvar parse-js-CLASS (parse-js--make-keyword "class"))
(defvar parse-js-EXTENDS (parse-js--make-keyword "extends" 'before-expr))
(defvar parse-js-EXPORT (parse-js--make-keyword "export"))
(defvar parse-js-IMPORT (parse-js--make-keyword "import"))
(defvar parse-js-NULL (parse-js--make-keyword "null" 'starts-expr))
(defvar parse-js-UNDEFINED (parse-js--make-keyword "undefined" 'starts-expr))
(defvar parse-js-TRUE (parse-js--make-keyword "true" 'starts-expr))
(defvar parse-js-FALSE (parse-js--make-keyword "false" 'starts-expr))
(defvar parse-js-IN (parse-js--make-keyword "in" 7 'before-expr))
(defvar parse-js-INSTANCEOF (parse-js--make-keyword "instanceof" 7 'before-expr))
(defvar parse-js-TYPEOF (parse-js--make-keyword "typeof" 'prefix 'before-expr 'starts-expr))
(defvar parse-js-VOID (parse-js--make-keyword "void" 'prefix 'before-expr 'starts-expr))
(defvar parse-js-DELETE (parse-js--make-keyword "delete" 'prefix 'before-expr 'starts-expr))

;;; Miscellaneous Token Types

(defvar parse-js-COMMENT (parse-js--make-tt "comment"))

;;; Context Types

(defvar parse-js-B-STAT '((token "{")))
(defvar parse-js-B-EXPR '((token "{") (is-expr t)))
(defvar parse-js-B-TMPL '((token "${") (is-expr t)))
(defvar parse-js-P-STAT '((token "(")))
(defvar parse-js-P-EXPR '((token "(") (is-expr t)))
(defvar parse-js-F-EXPR '((token "function") (is-expr t)))
(defvar parse-js-Q-TMPL '((token "`") (is-expr t) (preserve-space t)
                  (override parse-js--read-tmpl-token)))

;;; Regular Expressions

(defvar parse-js--binary-re "[0-1]+")
(defvar parse-js--octal-re "[0-7]+")
(defvar parse-js--decimal-re "[0-9]+")
(defvar parse-js--hexadecimal-re "[0-9A-Fa-f]+")

;;; Parser State

;; Use `point' to track the current position of the lexer in the buffer.
;; Unlike Acorn, this parser is working inside an Emacs buffer, in
;; which positions are between characters and start at 1

;; State properties of current token:

(defvar-local parse-js--type parse-js-EOF
  "Lexer state token type.")

(defvar-local parse-js--value nil
  "Lexer state token value, additional information beyond type.")

(defvar-local parse-js--start 1
  "Lexer state token start position.")

(defvar-local parse-js--end 1
  "Lexer state token end position.")

(defvar-local parse-js--newline-before nil
  "Lexer state newline between previous and current tokens.")

;; Properties of previous token:

(defvar-local parse-js--prev-type parse-js-EOF
  "Lexer state last token type.")

(defvar-local parse-js--prev-start 1
  "Lexer state last token start position.")

(defvar-local parse-js--prev-end 1
  "Lexer state last token end position.")

;; Quoted acorn/src/state.js
;;
;;   "The context stack is used to superficially track syntactic
;;    context to predict whether a regular expression is allowed in a
;;    given position."

(defvar-local parse-js--ctx-stack '()
  "Lexer state token context stack.")

(defvar-local parse-js--expr-allowed t
  "Lexer state expression allowed at point.")

(defvar-local parse-js--contains-esc nil
  "Signal current word contains escape sequences.")

(defvar-local parse-js--strict (or parse-js-always-strict nil)
  "Parser state to indicate module code.")

(defvar-local parse-js--potential-arrow-at -1
  "Parser state to signify the start of a potential arrow function.")

(defvar-local parse-js--in-function nil
  "Parser state to flag within a function.")

(defvar-local parse-js--in-generator nil
  "Parser state to flag within a generator function.")

(defvar-local parse-js--in-async nil
  "Parser state to flag within an async function.")

;; Positions to delayed-check that yield/await does not exist in default parameters.

(defvar-local parse-js--yield-pos 0
  "Parser state yeild position.")

(defvar-local parse-js--await-pos 0
  "Parser state await position.")

(defvar-local parse-js--string-buffer nil
  "List of chars built up while scanning various tokens.")

(defun parse-js-config ()
  "Configure Parser State at beginning of buffer or optionally START-POS."
  ;; Can the parser start at a position other than the beginning of the
  ;; buffer? `parse-js--ctx-stack' requires starting at a top level position
  ;; and to populate `parse-js--labels' would require starting at the beginning.
  (save-restriction
    (widen)
    (goto-char 1)
    (setq parse-js--contains-esc nil)
    (setq parse-js--type parse-js-EOF)
    (setq parse-js--value nil)
    (setq parse-js--start (point))
    (setq parse-js--end (point))
    (setq parse-js--newline-before nil)
    (setq parse-js--prev-type parse-js-EOF)
    (setq parse-js--prev-start (point))
    (setq parse-js--prev-end (point))
    (setq parse-js--ctx-stack (parse-js--initial-ctx))
    (setq parse-js--expr-allowed t)
    (setq parse-js--potential-arrow-at -1)
    (setq parse-js--in-function nil)
    (setq parse-js--in-generator nil)
    (setq parse-js--in-async nil)
    (setq parse-js--labels '())
    (setq parse-js--string-buffer nil)))

;;; Utility Functions

(defun parse-js--unexpected (&optional pos)
  "Signal unexpected character error, optionally at POS."
  (signal 'parse-js-unexpected-character-error (list (or pos parse-js--start))))

(defun parse-js--raise (type start end &optional message)
  "Signal error of TYPE with an argument alist of START, END and MESSAGE."
  (signal type (if message
                   `((start ,start) (end ,end) (message, message))
                 `((start ,start) (end ,end)))))

;; http://nullprogram.com/blog/2017/01/30/
;; Builting a list and using `nreverse' is the correct way to build a
;; list in reverse. `nreverse' is a very fast C builtin function.

(defun parse-js--collect-string ()
  "Convert `parse-js--string-buffer', a list of chars, to a string.
Reverses the list before converting."
  (if parse-js--string-buffer
      (prog1 ;; Wouldn't `concat' work?
          (apply #'string (nreverse parse-js--string-buffer))
        (setq parse-js--string-buffer nil))
    ""))

(defsubst parse-js--add-to-string (char)
  "Add CHAR to `parse-js--string-buffer'."
  (push char parse-js--string-buffer))

;;; Helper Predicate Functions

;; Taken from `js2-mode-identifier-start-p'
(defun parse-js-identifier-start-p (char)
  "Is CHAR a valid start to an ES5 Identifier?
See http://es5.github.io/#x7.6"
  (or
   (memq char '(?$ ?_))
   (memq (get-char-code-property char 'general-category)
         ;; Letters
         '(Lu Ll Lt Lm Lo Nl))))

;; Taken from `js2-mode-identifier-part-p'
(defun parse-js-identifier-part-p (char)
  "Is CHAR a valid part of an ES5 Identifier?
See http://es5.github.io/#x7.6"
  (or
   (memq char '(?$ ?_ ?\u200c  ?\u200d))
   (memq (get-char-code-property char 'general-category)
         '(;; Letters
           Lu Ll Lt Lm Lo Nl
           ;; Combining Marks
           Mn Mc
           ;; Digits
           Nd
           ;; Connector Punctuation
           Pc))))

;;; Lexer Utility Functions - Movement

(defun parse-js--peek (&optional count)
  "Peek forward one or COUNT number of `char-after' point.
This function does not move point."
  (char-after (+ (point) (or count 1))))

(defun parse-js--match (regexp)
  "Return the `match-end' if text after point match REGEXP."
  ;; NOTE: `looking-at-p' does not update match data.
  (and (looking-at regexp)
       (match-end 0)))

(defun parse-js--eat (regexp)
  "Goto the `match-end' if text after point match REGEXP.
Set point to the end of match, and return point."
  (and (looking-at regexp)
       (goto-char (match-end 0))))

(defun parse-js--expect-char (character)
  "Return t and `forward-char' if `char-after' point is eq CHARACTER.
Otherwise signal `parse-js-unexpected-character-error'."
  (if (eq character (char-after))
      (progn (forward-char) t)
    (signal 'parse-js-unexpected-character-error
            `((start ,(point) (end ,(1+ (point))))))))

(defun parse-js--skip-whitespace ()
  "Skip forward over whitespace and newlines."
  ;; Return t if moved over whitespace and/or newline.
  (let ((start (point)))
    (while (or (< 0 (skip-syntax-forward " "))
               (when (eq ?\C-j (char-after))
                 (forward-char)
                 (or parse-js--newline-before     ; Just return it if already t
                     (setq parse-js--newline-before t)))))
    (< start (point))))

;;; Context Functions

(defun parse-js--current-ctx ()
  "Return the context on the top of the stack `parse-js-ctx-stack'."
  (car parse-js--ctx-stack))

(defun parse-js--initial-ctx ()
  "Return the initial context for use in `parse-js-ctx-stack'."
  (list parse-js-B-STAT))

(defun parse-js--brace-is-block-p (prev-tt)
  "Use `parse-js--current-ctx' and PREV-TT to determine if brace is a block."
  (cond ((eq parse-js-COLON prev-tt)
         ;; If previous token type is a colon, determine its use as either
         ;; a statement label or a property key in an object literal.
         (let ((parent (parse-js--current-ctx)))
           (if (or (eq parse-js-B-STAT parent)
                   (eq parse-js-B-EXPR parent))
               (not (assq 'is-expr parent)))))
        ((eq parse-js-RETURN prev-tt)
         parse-js--newline-before)
        ((memq prev-tt (list parse-js-ELSE parse-js-SEMI parse-js-EOF parse-js-PAREN-R))
         t)
        ((eq parse-js-BRACE-L prev-tt)
         (eq parse-js-B-STAT (parse-js--current-ctx)))
        (t
         (not parse-js--expr-allowed))))

(defun parse-js--update-ctx (prev-tt)
  "Modify `parse-js--ctx-stack' to reflect change of PREV-TT."
  (cond ((and (assq 'keyword parse-js--type)
              (eq parse-js-DOT prev-tt))
         ;; Don't know what situation this is trying to catch.
         (setq parse-js--expr-allowed nil))
        ;; '{'  Enter brace statement, expression context.
        ((eq parse-js-BRACE-L parse-js--type)
         (push (if (parse-js--brace-is-block-p prev-tt)
                   parse-js-B-STAT
                 parse-js-B-EXPR)
               parse-js--ctx-stack)
         (setq parse-js--expr-allowed t))
        ;; '}' or ')'  Exit either brace or paren context.
        ((or (eq parse-js-BRACE-R parse-js--type)
             (eq parse-js-PAREN-R parse-js--type))
         (if (= 1 (length parse-js--ctx-stack))
             (setq parse-js--expr-allowed t)
           (let ((out (pop parse-js--ctx-stack)))
             (cond ((and (eq parse-js-B-STAT out)
                         (eq parse-js-F-EXPR (parse-js--current-ctx)))
                    (pop parse-js--ctx-stack)  ; Exit of function body.
                    (setq parse-js--expr-allowed nil))
                   ((eq parse-js-B-TMPL out)
                    (setq parse-js--expr-allowed t))
                   (t
                    (setq parse-js--expr-allowed
                          (not (assq 'is-expr out))))))))
        ;; ?(  Enter parenthesis context.
        ((eq parse-js-PAREN-L parse-js--type)
         (push (if (or (eq parse-js-IF prev-tt)
                       (eq parse-js-FOR prev-tt)
                       (eq parse-js-WITH prev-tt)
                       (eq parse-js-WHILE prev-tt))
                   parse-js-P-STAT
                 parse-js-P-EXPR)
               parse-js--ctx-stack)
         (setq parse-js--expr-allowed t))
        ;; '${' Enter brace template context.
        ((eq parse-js-DOLLAR-BRACE-L parse-js--type)
         (push parse-js-B-TMPL parse-js--ctx-stack)
         (setq parse-js--expr-allowed t))
        ;; '`'  Enter or exit a template literal context.
        ((eq parse-js-BACKQUOTE parse-js--type)
         (if (eq parse-js-Q-TMPL (parse-js--current-ctx))
             (pop parse-js--ctx-stack)
           (push parse-js-Q-TMPL parse-js--ctx-stack))
         (setq parse-js--expr-allowed nil))
        ;; '--' or '++'  Do not alter `parse-js--expr-allowed'.
        ((or (eq parse-js-INC-DEC parse-js--type) (eq parse-js-COMMENT parse-js--type)))
        ;; 'function'  Enter function expression context.
        ((eq parse-js-FUNCTION parse-js--type)
         (if (and (assq 'before-expr prev-tt)
                  (not (or (eq parse-js-SEMI prev-tt)
                           (eq parse-js-ELSE prev-tt)
                           (eq parse-js-COLON prev-tt)
                           (eq parse-js-BRACE-L prev-tt)))
                  (eq parse-js-B-STAT (parse-js--current-ctx)))
             (push parse-js-F-EXPR parse-js--ctx-stack))
         (setq parse-js--expr-allowed nil))
        ;; If the token type does not have before-expr in its alist
        ;; `parse-js--expr-allowed' will be set to nil.
        (t
         (setq parse-js--expr-allowed (or parse-js--newline-before
                                  (cadr (assq 'before-expr parse-js--type)))))))

;;; Token Reading Functions

(defun parse-js--finish-token (type &optional val)
  "Finish token of TYPE and value of VAL then update the context."
  (setq parse-js--end (point))
  (let ((prev-tt parse-js--type))
    (setq parse-js--type type)
    (setq parse-js--value val)
    (parse-js--update-ctx prev-tt)))

(defun parse-js--finish-punc (type)
  "Finish punctuation token of TYPE."
  (forward-char)
  (parse-js--finish-token type))

(defun parse-js--finish-op (type size)
  "Finish operator token of TYPE and SIZE."
  ;; Acorn uses `pos' think `parse-js--start' should work... Seems to
  (let ((str (buffer-substring-no-properties parse-js--start (+ parse-js--start size))))
    (goto-char (+ (point) size))
    (parse-js--finish-token type str)))

(defun parse-js--read-token-dot ()
  "Read a token starting with a period."
  (let ((next (parse-js--peek)))
    (cond ((and (characterp next)       ; Protect from end of buffer
                (<= ?0 next ?9))
           (parse-js--read-number t))
          ((and (eq ?. next) (eq ?. (parse-js--peek 2))) ; ...
           (forward-char 3)             ; Why not `parse-js--finish-op'?
           (parse-js--finish-token parse-js-ELLIPSIS))
          (t
           (forward-char)
           (parse-js--finish-token parse-js-DOT)))))

(defun parse-js--read-token-slash ()
  "Read a token starting with a ?\/."
  (let ((next (parse-js--peek)))
    (cond
     ((eq ?/ next)
      (if (setq parse-js--newline-before (search-forward "\n" nil t))
          (parse-js--finish-token parse-js-COMMENT)
        ;; Hit eof, finish commment at the end of the buffer.
        (goto-char (point-max))
        (parse-js--finish-token parse-js-COMMENT)))
     ((eq ?* next)
      ;; Don't look for a newline if one has already been found.
      (or (and (re-search-forward "*/\\|\n" nil t)
               (if (eq ?\C-j (char-before))
                   (setq parse-js--newline-before (search-forward "*/" nil t))
                 (setq parse-js--newline-before (point))))
          (signal 'parse-js-token-error '("Unterminated multiline comment")))
      (parse-js--finish-token parse-js-COMMENT))
     (parse-js--expr-allowed      ; Must be a regular expression.
      (parse-js--read-regexp))
     ((eq ?= next)
      (parse-js--finish-op parse-js-ASSIGN 2))
     (t
      (parse-js--finish-op parse-js-SLASH 1)))))

(defun parse-js--read-token-mult-modulo-exp (first)
  "Read a token starting with a ?* or ?%, FIRST indicates which."
  (let ((second (parse-js--peek)))
    (if (eq ?* first)
        ;; * *= ** **=
        (if (eq ?* second)
            (if (eq ?= (parse-js--peek 2))
                (parse-js--finish-op parse-js-ASSIGN 3)  ; **=
              (parse-js--finish-op parse-js-STARSTAR 2)) ; **
          (if (eq ?= second)
              (parse-js--finish-op parse-js-ASSIGN 2) ; *=
            (parse-js--finish-op parse-js-STAR 1)))   ; *
      ;; % %=
      (if (eq ?= second)
          (parse-js--finish-op parse-js-ASSIGN 2)   ; %=
        (parse-js--finish-op parse-js-MODULO 1))))) ; %

(defun parse-js--read-token-pipe-amp (first)
  "Read a token starting with a ?| or ?&, FIRST indicates which."
  (let ((second (parse-js--peek)))
    (cond ((eq first second)            ; && ||
           (parse-js--finish-op (if (eq ?& first) parse-js-LOGICAL-AND parse-js-LOGICAL-OR) 2))
          ((eq ?= second)               ; &= |=
           (parse-js--finish-op parse-js-ASSIGN 2))
          (t                            ; & |
           (parse-js--finish-op (if (eq ?& first) parse-js-BITWISE-AND parse-js-BITWISE-OR) 1)))))

(defun parse-js--read-token-caret ()
  "Read a token starting with a ?^."
  (if (eq ?= (parse-js--peek))
      (parse-js--finish-op parse-js-ASSIGN 2)
    (parse-js--finish-op parse-js-BITWISE-XOR 1)))

(defun parse-js--read-token-plus-min (first)
  "Read a token starting with a ?+ or ?-, FIRST indicates which."
  (let ((second (parse-js--peek)))
    (cond ((eq first second)
           (parse-js--finish-op parse-js-INC-DEC 2))
          ((eq ?= second)
           (parse-js--finish-op parse-js-ASSIGN 2))
          (t
           (parse-js--finish-op parse-js-PLUS-MIN 1)))))

;; Not implementing XML-style comments for now <!--
(defun parse-js--read-token-lt-gt (first)
  "Read a token starting with a ?< or ?>, FIRST indicates which."
  (let ((second (parse-js--peek))
        (third (parse-js--peek 2)))
    (if (eq first second)
        ;; << >> >>> <<= >>= >>>=
        (if (eq ?> third)
            (if (eq ?= (parse-js--peek 3))
                (parse-js--finish-op parse-js-ASSIGN 4)  ; >>>=
              (parse-js--finish-op parse-js-BITSHIFT 3)) ; >>>
          (if (eq ?= third)
              (parse-js--finish-op parse-js-ASSIGN 3)    ; <<= >>=
            (parse-js--finish-op parse-js-BITSHIFT 2)))  ; << >>
      ;; < > <= >=
      (parse-js--finish-op parse-js-RELATIONAL (if (eq ?= second) 2 1)))))

(defun parse-js--read-token-eq-excl (first)
  "Read a token starting with a ?= or ?!, FIRST indicates which."
  (let ((second (parse-js--peek)))
    (cond ((eq ?> second)               ; =>
           (forward-char 2)
           (parse-js--finish-token parse-js-ARROW))
          ((eq ?= second)               ; == != === !==
           (parse-js--finish-op parse-js-EQUALITY (if (eq ?= (parse-js--peek 2)) 3 2)))
          (t                            ; = !
           (parse-js--finish-op (if (eq ?= first) parse-js-EQ parse-js-PREFIX) 1)))))

(defun parse-js--buffer-to-number (start end radix)
  "Attempt to read a number from buffer from START to END in RADIX."
  (string-to-number (buffer-substring-no-properties start end) radix))

;; NOTE: This function is used to read and parse an ocatal integer.
;; Used in `parse-js--read-escape-char'
(defun parse-js--read-octal (upto)
  "Read and parse value of an octal integer from point.
Read UPTO a specific number of characters."
  (let ((start (point)))
    (when (parse-js--eat (concat "[0-7]\\{" (number-to-string upto) "\\}"))
      (parse-js--buffer-to-number start (point) 8))))

(defun parse-js--read-code-point ()
  "Read Unicode escape from buffer and return an integer."
  (if (eq ?{ (char-after))
      ;; Unicode Code Point Escape \u{...}
      (progn
        (forward-char)                  ; Move over '{'
        (let ((code nil)
              (start (point)))
          (when (parse-js--eat parse-js--hexadecimal-re)
            (setq code (parse-js--buffer-to-number start (point) 16)))
          (cond ((not code)
                 (parse-js--raise 'parse-js-code-point-error start (point)))
                ((> code #x10ffff) ; Think this error should be removed
                 (parse-js--raise 'parse-js-code-point-bound-error start (point)))
                (t
                 (parse-js--expect-char ?})
                 code))))
    ;; Unicode Escape Sequence \uXXXX
    (let ((start (point)))
      (or (and (parse-js--eat "[0-9A-Fa-f]\\{4\\}")
               (parse-js--buffer-to-number start (point) 16))
          (parse-js--raise 'parse-js-escape-sequence-error start (point))))))

(defun parse-js--read-escape-char (&optional in-template)
  "Read ECMAScript escape sequence from buffer.
The IN-TEMPLATE option invalidates the use of octal literals in the string."
  (forward-char)                        ; Move over '\'
  (let ((char (char-after)))
    (cond ((eq ?u char)
           (forward-char)
           (parse-js--read-code-point))
          ((eq ?x char)
           (forward-char)
           (let ((start (point)))
             (or (parse-js--eat "[0-9A-Fa-f]\\{2\\}")
                 (parse-js--raise 'parse-js-string-hex-escape-error (- start 2) (point)))))
          ((<= ?0 char ?7)
           (let ((start (point))
                 (num (parse-js--read-octal 3)))
             (when (or (not num)
                       (> num 255))
               (goto-char start)
               (setq num (parse-js--read-octal 2))
               (when (not num) ; HACK: Find better solution than 3 attempts.
                 (goto-char start)
                 (setq num (parse-js--read-octal 1))))
             (when (and (or parse-js--strict in-template)
                        (or (< 1 (- (point) start))
                            (not (= 0 num))))
               ;; TODO: Only warn about illegal octals.
               (if in-template
                   (parse-js--raise 'parse-js-template-octal-error (1- start) (point))
                 (parse-js--raise 'parse-js-string-octal-strict-error (1- start) (point))))))
          (t
           (forward-char)))))

(defun parse-js--read-number (&optional starts-with-dot)
  "Read JavaScript number from the buffer.
STARTS-WITH-DOT indicates the previous character was a period."
  ;; Inputs: '.' or /[0-9]*/
  ;; Attempt to parse as either integer, float, or possible octal.
  (let ((start (point))
        (octal (eq ?0 (char-after)))) ; Would we ever get a zero
    (when (and (not starts-with-dot)  ; to this function?
               ;; Yes we can, `parse-js--read-zero' passes to this function
               ;; if the number starts with a zero and is not a
               ;; a literal using "0b, 0o, 0x" syntax.
               (not (parse-js--eat parse-js--decimal-re)))
      (parse-js--raise 'parse-js-number-invalid-error parse-js--start (point)))
    (when (and octal (= (point) (1+ start))) ; A single ?0
      (setq octal nil))                 ; Could a number that starts
    (when (and (eq ?\. (char-after))    ; with a dot fall though?
               (not octal))
      (forward-char)
      (parse-js--eat parse-js--decimal-re)) ; (parse-js--eat "[0-9]+\\([eE][-+]?[0-9]+\\)?")
    (when (and (memq (char-after) '(?e ?E))
               (not octal))
      (forward-char)
      (when (memq (char-after) '(?+ ?-))
        (forward-char))
      (when (null (parse-js--eat parse-js--decimal-re))
        (parse-js--raise 'parse-js-number-invalid-error parse-js--start (point))))
    (when (and (char-after)           ; Protect regex search from nil.
               (parse-js-identifier-start-p (char-after)))
      (parse-js--raise 'parse-js-unexpected-identifier-error parse-js--start (point)))
    (parse-js--finish-token parse-js-NUM)))

(defun parse-js--read-zero ()
  "Read a token starting with a ?0."
  (let ((next (parse-js--peek 1)))
    (if (memq next '(?x ?X ?o ?O ?b ?B)) ; 0b, 0o, 0x, etc.
        (progn
          (forward-char 2)
          (cond
           ((memq next '(?x ?X))
            (parse-js--eat parse-js--hexadecimal-re))
           ((memq next '(?o ?O))
            (parse-js--eat parse-js--octal-re))
           ((memq next '(?b ?B))
            (parse-js--eat parse-js--binary-re)))
          (if (and (char-after)       ; Protect regex search from nil.
                   (parse-js-identifier-start-p (char-after)))
              (parse-js--raise 'parse-js-unexpected-identifier-error parse-js--start (point))
            (parse-js--finish-token parse-js-NUM)))
      (parse-js--read-number nil))))

(defun parse-js--read-regexp ()
  "Read regular expression.
Criteria include ending delimiter and flags."
  (forward-char)                        ; Move over opening delimiter.
  (let (char
        in-class
        (looking t))
    (while looking
      ;; Advance to next critical character.
      (re-search-forward "[^\][/\\\\\C-j]*" nil t) ; ][/\ or \n
      (setq char (char-after))
      (cond
       ;; Found escape.
       ((eq ?\\ char)
        (parse-js--read-escape-char nil))
       ;; Found closing delimiter, exit the loop if not in a class.
       ((eq ?\/ char)
        (forward-char)
        (unless in-class (setq looking nil)))
       ((and (not in-class) (eq ?\[ char))
        (forward-char)
        (setq in-class t))              ; Enter character class
       ((and in-class (eq ?\] char))
        (forward-char)
        (setq in-class nil))            ; Exit character class
       ;; Hit eol or eof, signal error.
       (t
        (parse-js--raise 'parse-js-regexp-delimiter-error parse-js--start (point))))))
  (skip-syntax-forward "w")       ; Advance over flags, valid or not
  (parse-js--finish-token parse-js-REGEXP))

(defun parse-js--read-string (punc)
  "Search for ending delimiter matching PUNC.
Signal error if the eol or eof is reached before the matching
delimiter."
  (forward-char)                        ; Move over opening delimiter.
  (let (char
        (regexp (concat "[^" (string punc) "\\\\\C-j]*"))
        (looking t))
    (while looking
      ;; Advance to next critical character.
      (re-search-forward regexp nil t)
      (setq char (char-after))
      (cond
       ;; Found escape.
       ((eq ?\\ char)
        (parse-js--read-escape-char nil))
       ;; Found closing delimiter, exit the loop.
       ((eq punc char)
        (forward-char)
        (setq looking nil))
       ;; Hit eol or eof, signal error.
       (t
        (parse-js--raise 'parse-js-string-delimiter-error parse-js--start (point))))))
  (parse-js--finish-token parse-js-STRING))

(defun parse-js--string-builder (list)
  "Return a string built from a LIST of strings."
  (eval `(concat ,@list)))

(defun parse-js--read-tmpl-token ()
  "Read template string tokens."
  ;; Don't think the values of the templates need to be built.
  ;; But don't remove it without some thought.
  (let ((out '())
        (char (char-after))
        (chunk-start (point)))
    (catch 'tmpl
      (while t
        (cond
         ((= (point) (point-max))
          (parse-js--raise 'parse-js-template-delimiter-errror parse-js--start (point)))
         ((or (eq ?\` char)
              (and (eq ?\$ char) (eq ?\{ (parse-js--peek))))
          (if (and (= parse-js--start (point))
                   (eq parse-js-TEMPLATE parse-js--type))
              (if (eq ?\$ char)
                  (progn
                    (forward-char 2)
                    (throw 'tmpl (parse-js--finish-token parse-js-DOLLAR-BRACE-L)))
                (forward-char)
                (throw 'tmpl (parse-js--finish-token parse-js-BACKQUOTE)))
            (push (buffer-substring-no-properties chunk-start (point)) out)
            (throw 'tmpl (parse-js--finish-token parse-js-TEMPLATE
                                         (parse-js--string-builder (nreverse out))))))
         ((eq ?\\ char)
          (push (buffer-substring-no-properties chunk-start (point)) out)
          ;; (forward-char)                ; HACK
          (push (string (parse-js--read-escape-char t)) out)
          (setq chunk-start (point)))
         ((eq ?\C-j char)
          (push (buffer-substring-no-properties chunk-start (point)) out)
          (push (string char) out)
          (forward-char)
          (setq chunk-start (point))) ; Need to think about this one
         (t
          (forward-char)))
        (setq char (char-after))))))

(defun parse-js--read-word-internal ()
  "Read ECMAScript Identifier."
  (setq parse-js--contains-esc nil)
  (let (code                            ; Interpreted code point.
        char
        (reading t))
    (while reading
      (setq char (char-after))
      (cond ((not char)               ; EOF
             (setq reading nil))
            ((parse-js-identifier-part-p char) ; Start tested in `parse-js-get-token'.
             (forward-char)
             (parse-js--add-to-string char))
            ((eq ?\\ char)
             (forward-char)
             (parse-js--expect-char ?u)
             (setq parse-js--contains-esc t)
             (setq code (parse-js--read-code-point))
             (when (and (null parse-js--string-buffer) ; Must be start if null.
                        ;; Start not tested on Unicode escapes.
                        (not (parse-js-identifier-start-p code)))
               (parse-js--raise 'parse-js-identifier-start-error parse-js--start (point)))
             (if (parse-js-identifier-part-p code)
                 (parse-js--add-to-string code)
               (parse-js--raise 'parse-js-identifier-part-error parse-js--start (point))))
            (t
             (setq reading nil))))
    (parse-js--collect-string)))

(defun parse-js--read-word ()
  "Read from buffer an ECMAScript Identifier Name or Keyword."
  (let ((word (parse-js--read-word-internal))
        (type parse-js-NAME))
    (when (and (or (>= parse-js-ecma-version 6)
                   (not parse-js--contains-esc))
               (not (eq parse-js-DOT parse-js--prev-type)))
      (setq type (or (gethash (intern word) parse-js-keywords)
                     parse-js-NAME)))
    (parse-js--finish-token type word)))

(defun parse-js--read-token (char)
  "Read token from CHAR."
  ;; Implements getTokenFromCode from acorn.
  (cond
   ((eq ?\. char) (parse-js--read-token-dot))
   ((eq ?\( char) (parse-js--finish-punc parse-js-PAREN-L))
   ((eq ?\) char) (parse-js--finish-punc parse-js-PAREN-R))
   ((eq ?\; char) (parse-js--finish-punc parse-js-SEMI))
   ((eq ?\, char) (parse-js--finish-punc parse-js-COMMA))
   ((eq ?\[ char) (parse-js--finish-punc parse-js-BRACKET-L))
   ((eq ?\] char) (parse-js--finish-punc parse-js-BRACKET-R))
   ((eq ?{ char) (parse-js--finish-punc parse-js-BRACE-L))
   ((eq ?} char) (parse-js--finish-punc parse-js-BRACE-R))
   ((eq ?? char) (parse-js--finish-punc parse-js-QUESTION))
   ((eq ?: char) (parse-js--finish-punc parse-js-COLON))
   ((and (eq ?\` char) (<= 6 parse-js-ecma-version)) (parse-js--finish-punc parse-js-BACKQUOTE))
   ((eq ?0 char) (parse-js--read-zero))
   ((<= ?1 char ?9)  (parse-js--read-number nil))
   ((or (eq ?\' char) (eq ?\" char)) (parse-js--read-string char))
   ((eq ?\/ char) (parse-js--read-token-slash))
   ((memq char '(?* ?%)) (parse-js--read-token-mult-modulo-exp char))
   ((memq char '(?\| ?\&)) (parse-js--read-token-pipe-amp char))
   ((eq ?^ char) (parse-js--read-token-caret))
   ((memq char '(?+ ?-)) (parse-js--read-token-plus-min char))
   ((memq char '(?< ?>)) (parse-js--read-token-lt-gt char))
   ((memq char '(?= ?!)) (parse-js--read-token-eq-excl char))
   ((eq ?~ char) (parse-js--finish-op parse-js-PREFIX 1))
   (t
    (parse-js--raise 'parse-js-unexpected-character-error (point) (1+ (point))))))

(defun parse-js--next ()
  "Transition current state to last state and read next token."
  ;; Load previous token state before advancing.
  (setq parse-js--prev-type parse-js--type)
  (setq parse-js--prev-end parse-js--end)
  (setq parse-js--prev-start parse-js--start)
  ;; Load current token into parser state.
  ;; Implements logic of nextToken and readToken from Acorn
  (let ((ctx (parse-js--current-ctx)))
    (when (or (not ctx)
              (not (assq 'preserve-space ctx)))
      ;; (parse-js--skip-whitespace-and-comments)
      (parse-js--skip-whitespace)
      )
    (setq parse-js--start (point))
    ;; HACK: Perhaps add comment and whitespace skipping functions.
    ;; (if (save-excursion (search-backward "\n" parse-js--prev-end t))
    ;;     (setq parse-js--newline-before t)
    ;;   (setq parse-js--newline-before nil))
    (if (eq (point) (point-max))
        (parse-js--finish-token parse-js-EOF)
      ;; At the moment the only override is for the Q-TEMP context,
      ;; should I just call `parse-js--read-tmpl-token' from here?
      ;; UPDATE: No, planning to add an override for regexps.
      (cond ((assq 'override ctx)
             (funcall (cadr (assq 'override ctx))))
            ((or (parse-js-identifier-start-p (char-after)) ; Identifiers
                 (eq ?\\ (char-after)))
             (parse-js--read-word))
            (t
             (parse-js--read-token (char-after)))))))

(defun parse-js--make-token ()
  "Create token alist from parser state."
  (append
   `((type ,parse-js--type)
     (start ,parse-js--start)
     (end ,parse-js--end))
   (if (and parse-js--value
            (not (assq 'keyword parse-js--type)))
       `((value ,parse-js--value)))))

(defun parse-js-get-token ()
  "Load next token into parser state and return token alist."
  (parse-js--next)
  (parse-js--make-token))

;;; Begin Parser Code:

;;; Parser Utilities

;; Perhaps the name should be `parse-js--expect-token'
(defun parse-js--unexpected-token (&optional type)
  "Signal an unexpected token parse error."
  (signal 'parse-js-parse-error
          (list parse-js--start parse-js--end
                (if type
                    (format "Unexpected token: %s, Expected: %s"
                            (cadr (assq 'label parse-js--type))
                            (cadr (assq 'label type)))
                  (format "Unexpected token: %s"
                          (cadr (assq 'label parse-js--type)))))))

(defun parse-js--eat-token (type)
  "Test whether the next token is of TYPE, if so consume it as a side effect.
Returns t if token of TYPE was consumed, otherwise nil."
  (if (eq type parse-js--type)
      (progn (parse-js--next) t)
    (parse-js--unexpected-token)))

(defun parse-js--insert-semi-p ()
  "Test whether or not a semi-colon can be inserted."
  (or (eq parse-js-EOF parse-js--type)
      (eq parse-js-BRACE-R parse-js--type)
      (parse-js--linebreak-p)))

;;; Expression Parsing

;; Quoted from acorn/src/expression.js

;;  "These nest, from the most general expression type at the top to
;;   'atomic', nondivisible expression types at the bottom. Most of
;;   the functions will simply let the function(s) below them parse,
;;   and, *if* the syntactic construct they handle is present, wrap
;;   the AST node that the inner parser gave them in another node.
;;
;;   Parse a full expression. The optional arguments are used to
;;   forbid the `in` operator (in for loops initalization expressions)
;;   and provide reference for storing '=' operator inside shorthand
;;   property assignment in contexts where both object expression
;;   and object pattern might appear (so it's possible to raise
;;   delayed syntax error at correct position)."

(provide 'parse-js)

;;; parse-js.el ends here
