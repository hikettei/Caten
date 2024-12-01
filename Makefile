PROJECT_ROOT  := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
QLOT          := qlot
ROSWELL       := ros
PIP           := pip

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: install_extra
install_extra: ## Install extra dependencies for testing
	$(PIP) install numpy torch

.PHONY: test
test: ## Runs test harness
	$(QLOT) exec $(ROSWELL) run \
		--source-registry $(PROJECT_ROOT) \
		--eval '(ql:quickload "caten/test")' \
		--eval '(asdf:test-system "caten/test")'\
		--quit

.PHONY: install_docs
install_docs: ## Install documentation dependencies
	$(PIP) install mkdocs markdown-katex lantana

.PHONY: build_docs
build_docs: ## Build documentation
	./roswell/caten.ros docs

.PHONY: serve_docs
serve_docs: ## Serve documentation
	mkdocs serve
