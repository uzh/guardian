# Devcontainer Makefile — delegates to the workspace root Makefile with
# the database URLs pre-set to match the docker-compose service hostnames.
#
# Usage (from anywhere inside the container):
#
#   make -f .devcontainer/Makefile test
#
# postCreate.sh adds this alias to ~/.zshrc:
#
#   alias make='make -f /workspace/.devcontainer/Makefile'

ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))..

# The tests drop and recreate guardian's tables: point them at the `test` databases.
MARIADB_URL  ?= mariadb://admin:password@database-mariadb/test
POSTGRES_URL ?= postgresql://root:root@database-postgres/test

export MARIADB_URL POSTGRES_URL

# Forward the goals to the root Makefile (no goal: its default, the build).
# They are phony here, so one named like a path runs too.
GOALS := $(or $(MAKECMDGOALS),all)
.PHONY: $(GOALS)
$(GOALS):
	$(MAKE) -C $(ROOT) $@
