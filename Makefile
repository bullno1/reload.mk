PROJECT = reload_mk
PROJECT_DESCRIPTION = Live reload plugin for erlang.mk

BUILD_DEPS = elvis_mk
DEP_PLUGINS = elvis_mk

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

include erlang.mk

.PHONY: test

test: distclean
	$(verbose) rm -rf .test
	$(verbose) cp -r test .test
	$(verbose) mkdir -p .test/deps/reload_mk
	$(verbose) cp -r src Makefile erlang.mk plugins.mk .test/deps/reload_mk
	$(verbose) cd .test && unset DEPS_DIR && $(MAKE) test
