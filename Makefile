# -*- Makefile -*-

# Using Emacs from emacsformacosx.com
EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch --quick

test-lexer:
	${EMACS} $(BATCHFLAGS) -L . -l parse-js.el -l test/lexer.el \
	  -l test/context.el -f ert-run-tests-batch-and-exit

test: test-lexer
