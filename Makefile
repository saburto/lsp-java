EMACS ?= emacs
EASK ?= eask

build:
	$(EASK) package
	$(EASK) install

ci: build compile test checkdoc lint

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
	$(EASK) buttercup tests

.PHONY: build test compile checkdoc lint
