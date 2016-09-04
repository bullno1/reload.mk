PROJECT = test
BUILD_DEPS = reload_mk
DEP_PLUGINS = reload_mk

include erlang.mk

.PHONY: test

RELEASE = _rel/reload_mk_test/bin/reload_mk_test

define assert
$(verbose) \
	echo ----------------------------------------------------------------; \
	VAL=`$(RELEASE) rpcterms $(1)`; \
	echo $(1) -\> $$VAL; \
	echo assert\($$VAL $(2) $(3)\); \
	echo ----------------------------------------------------------------; \
	[ "$$VAL" $(2) "$(3)" ]
endef

test: all
	@echo ----------------------------------------------------------------
	$(RELEASE) stop || true
	$(RELEASE) start
	sleep 2
	$(RELEASE) ping

	$(call assert,test1 test,=,0)
	cp ./priv/test1.erl ./src/test1.erl
	$(MAKE) reload
	$(call assert,test1 test,=,1)

	$(call assert,code is_loaded test2.,=,false)
	cp ./priv/test2.erl ./src/test2.erl
	$(MAKE) reload
	$(call assert,code is_loaded test2.,!=,false)
	$(call assert,test2 test,=,2)

	$(call assert,code is_loaded test3.,!=,false)
	cp ./priv/test3.erl ./src/test3.erl
	$(MAKE) reload
	[ "`cat _rel/reload_mk_test/reloaded`" = "true" ]

	$(RELEASE) stop
