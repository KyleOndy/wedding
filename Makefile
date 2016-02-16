SITE_EXE=stack exec site-wedding
PROVIDER_FOLDER=provider

.PHONY: build
build:
	stack install
	$(SITE_EXE) -- build

.PHONY: check
check:  build
	$(SITE_EXE) -- check

.PHONY: clean
clean:
	$(SITE_EXE) -- clean
	rm -rf $(BUILD_FOLDER)

.PHONY: server
server:  build
	$(SITE_EXE) -- server

.PHONY: watch
watch:  build
	$(SITE_EXE) -- watch
