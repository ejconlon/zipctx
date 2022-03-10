include Makefile.base

.PHONY: exe
exe: build
	stack exec -- zipctx-exe
