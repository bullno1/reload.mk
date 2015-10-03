PROJECT = reload_mk
PROJECT_DESCRIPTION = Live reload plugin for erlang.mk

include erlang.mk

.PHONY: test

test: distclean
	$(verbose) rm -rf .test
	$(verbose) cp -r test .test
	$(verbose) mkdir -p .test/deps/reload_mk
	$(verbose) cp -r src Makefile erlang.mk plugins.mk .test/deps/reload_mk
	$(verbose) cd .test && unset DEPS_DIR && $(MAKE) test
