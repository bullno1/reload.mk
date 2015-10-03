# A list of directories that "make auto-reload" should watch
RELOAD_MK_WATCH_DIRS ?= src

# Internal

.PHONY: reload auto-reload

watch_verbose_0 = @echo " WATCH " $(RELOAD_MK_WATCH_DIRS);
watch_verbose = $(watch_verbose_$(V))

reload_verbose_0 = @echo " RELOAD" $(PROJECT);
reload_verbose = $(reload_verbose_$(V))

reload: app
	$(reload_verbose) $(RELX_OUTPUT_DIR)/$(RELX_RELEASE)/bin/$(RELX_RELEASE) rpcterms reload_mk reload

auto-reload:
	$(watch_verbose) \
	while :; \
	do \
		inotifywait -q -e close_write $(RELOAD_MK_WATCH_DIRS) > /dev/null; \
		make reload; \
	done;
