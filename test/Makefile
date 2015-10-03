PROJECT = test
BUILD_DEPS = reload_mk
DEP_PLUGINS = reload_mk

include erlang.mk

.PHONY: test

RELEASE = _rel/reload_mk_test/bin/reload_mk_test

test: all
	@echo ----------------------------------------------------------------
	$(RELEASE) stop || true
	$(RELEASE) start
	sleep 2
	$(RELEASE) ping

	[ `$(RELEASE) rpcterms example test` = 0 ]
	cp ./priv/example.erl ./src/example.erl
	$(MAKE) reload
	[ `$(RELEASE) rpcterms example test` = 1 ]
