ROSWELL       := ros
QUICKLOAD     := --eval '(progn (cl:push (cl:pathname "./") ql:*local-project-directories*) (asdf:load-asd "caten.asd") (ql:quickload "caten" :silent t))'
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
	$(ROSWELL) $(QUICKLOAD) --eval '(asdf:test-system "caten")'
