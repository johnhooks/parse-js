# -*- Makefile -*-

# Using Emacs from emacsformacosx.com
EMACS = emacs

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch --quick

test-lexer:
	${EMACS} $(BATCHFLAGS) -L . -l parse-js.el -l test/lexer-test.el \
	  -l test/context-test.el -f ert-run-tests-batch-and-exit

test: test-lexer
