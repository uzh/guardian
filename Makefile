.DEFAULT_GOAL := all

.PHONY: all
all:
	opam exec -- dune build --root . @install

.PHONY: lock
lock: ## Generate a lock file
	opam lock .

.PHONY: build
build:
	opam exec -- dune build --root .

.PHONY: build-watch
build-watch:
	opam exec -- dune build --root . --watch

.PHONY: install
install: all ## Install the packages on the system
	opam exec -- dune install --root .

.PHONY: test
test: ## Run the all tests
	opam exec -- dune build --root . @runtest

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: format
format: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings
