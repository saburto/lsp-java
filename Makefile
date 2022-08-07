EMACS ?= emacs
EASK ?= eask

build:
	$(EASK) package
	$(EASK) install

ci: test checkdoc lint compile build

compile:
	@echo "Compiling..."
	@$(EASK) compile

lint:
	@echo "package linting..."
	@$(EASK) lint package

clean:
	$(EASK) clean-all

test:
	$(EASK) install-deps --dev
	$(EASK) test buttercup

.PHONY: build test compile checkdoc lint
