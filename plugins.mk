.PHONY: reload

reload: app
	$(verbose) $(RELX_OUTPUT_DIR)/$(RELX_RELEASE)/bin/$(RELX_RELEASE) rpcterms reload_mk reload
