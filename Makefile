# ============================================================================ #
# SHELL CONFIGURATION
# ============================================================================ #

SHELL := /bin/bash

# ============================================================================ #
# HELP
# ============================================================================ #

## help: Show this help message
.PHONY: help
help:
	@echo 'goasm - Go Assembly Viewer for Emacs'
	@echo ''
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' | sed -e 's/^/  /'
	@echo ''
	@echo 'Examples:'
	@echo '  make test     # Run ERT test suite'
	@echo '  make lint     # Byte-compile and check warnings'
	@echo ''

# ============================================================================ #
# DEVELOPMENT
# ============================================================================ #

## test: Run ERT test suite
.PHONY: test
test:
	@emacs -batch -l goasm-mode.el -l goasm-mode-test.el -f ert-run-tests-batch-and-exit

## lint: Byte-compile goasm.el and check for warnings
.PHONY: lint
lint:
	@emacs -batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile goasm-mode.el

## clean: Remove compiled files
.PHONY: clean
clean:
	@rm -f goasm-mode.elc
