# A list of directories that "make auto-reload" should watch
RELOAD_MK_WATCH_DIRS ?= src

# Internal

.PHONY: reload auto-reload

watch_verbose_0 = @echo " WATCH " $(RELOAD_MK_WATCH_DIRS);
watch_verbose = $(watch_verbose_$(V))

reload_verbose_0 = @echo " RELOAD" $(PROJECT);
reload_verbose = $(reload_verbose_$(V))

RELOAD_MK_RPC = $(RELX_OUTPUT_DIR)/$(RELX_RELEASE)/bin/$(RELX_RELEASE) rpcterms
BUILD_DEPS += reload_mk

reload: deps app bootstrap-reload.mk
	$(reload_verbose) $(RELOAD_MK_RPC) reload_mk reload

auto-reload: bootstrap-reload.mk
	$(watch_verbose) \
		while :; \
		do \
			inotifywait -q -e close_write $(RELOAD_MK_WATCH_DIRS) > /dev/null; \
			make reload; \
		done;

bootstrap-reload.mk:
	$(verbose) \
		if [ $$($(RELOAD_MK_RPC) code is_loaded reload_mk.) = false ]; then \
			$(RELOAD_MK_RPC) code add_pathz '"$(DEPS_DIR)/reload_mk/ebin/".'; \
			$(RELOAD_MK_RPC) code load_abs '"$(DEPS_DIR)/reload_mk/ebin/reload_mk".'; \
		fi > /dev/null;
