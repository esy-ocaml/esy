ESY_EXT := $(shell command -v esy 2> /dev/null)

RELEASE_TAG ?= latest
BIN = $(PWD)/node_modules/.bin

#
# Tools
#

.DEFAULT: help

define HELP

 Run "make bootstrap" if this is your first time with esy development. After
 that you can use "bin/esy" executable to run the development version of esy
 command. Enjoy!

 Common tasks:

   bootstrap           Bootstrap the development environment
   test                Run tests
   clean               Clean build artefacts

 Release tasks:

   publish             Build release and run 'npm publish'
   build-release       Produce an npm package ready to be published (useful for debug)

   bump-major-version  Bump major package version (commits & tags)
   bump-minor-version  Bump minor package version (commits & tags)
   bump-patch-version  Bump patch package version (commits & tags)

endef
export HELP

help:
	@echo "$$HELP"

bootstrap:
	@git submodule init
	@git submodule update
ifndef ESY_EXT
	$(error "esy command is not avaialble, run 'npm install -g esy'")
endif
	@make -C esy-core install build-dev
	@yarn

doctoc:
	@$(BIN)/doctoc --notitle ./README.md

clean:
	@rm -rf lib/
	@make -C esy-core clean

#
# Test
#

JEST = $(BIN)/jest --runInBand

test-unit:
	@$(MAKE) -C esy-core test

test-e2e:
	@$(JEST) \
		--config jest-e2e.config.js \
		./__tests__/build/*-test.sh \
		./__tests__/export-import-build/*-test.sh \
		./__tests__/common/*-test.sh

test-opam:
	$(MAKE) -C __tests__/opam

ci::
	$(MAKE) test-unit
	$(MAKE) test-e2e

test::
	$(MAKE) test-unit
	$(MAKE) test-e2e

#
# Release
#

RELEASE_ROOT = dist
RELEASE_FILES = \
	bin/esy \
	bin/esy-darwin \
	bin/esyBuildPackage-darwin \
	bin/esy-linux \
	bin/esyBuildPackage-linux \
	bin/esy-install.js \
	bin/esyExportBuild \
	bin/esyImportBuild \
	bin/esyRuntime.sh \
	bin/realpath.sh \
	scripts/postinstall.sh \
	package.json

define BIN_ESY
#!/bin/bash

>&2 echo "esy: installed incorrectly because of failed postinstall hook"
exit 1
endef
export BIN_ESY

build-release:
	@$(MAKE) -C esy-core build
	@$(MAKE) -C linux-build build
	@$(MAKE) build-release-copy-artifacts

build-release-copy-artifacts:
	@rm -rf $(RELEASE_ROOT)
	@$(MAKE) -j $(RELEASE_FILES:%=$(RELEASE_ROOT)/%)

$(RELEASE_ROOT)/package.json:
	@node ./scripts/generate-esy-install-package-json.js > $(@)

$(RELEASE_ROOT)/bin/esy:
	@mkdir -p $(@D)
	@echo 'echo "esy was not installed correctly, exiting..." && exit 1' > $(@)

$(RELEASE_ROOT)/bin/esy-darwin:
	@mkdir -p $(@D)
	@cp esy-core/_build/default/esy/bin/esyCommand.exe $(@)

$(RELEASE_ROOT)/bin/esy-linux:
	@mkdir -p $(@D)
	@cp linux-build/esy $(@)

$(RELEASE_ROOT)/bin/esyBuildPackage-linux:
	@mkdir -p $(@D)
	@cp linux-build/esyBuildPackage $(@)

$(RELEASE_ROOT)/bin/esyBuildPackage-darwin:
	@mkdir -p $(@D)
	@cp esy-core/_build/default/esy-build-package/bin/esyBuildPackageCommand.exe $(@)

$(RELEASE_ROOT)/bin/esy-install.js:
	@$(MAKE) -C esy-install BUILD=../$(@) build

$(RELEASE_ROOT)/%: $(PWD)/%
	@mkdir -p $(@D)
	@cp $(<) $(@)

publish: build-release
	@(cd $(RELEASE_ROOT) && npm publish --access public --tag $(RELEASE_TAG))
	@git push && git push --tags

bump-major-version:
	@npm version major

bump-minor-version:
	@npm version minor

bump-patch-version:
	@npm version patch
