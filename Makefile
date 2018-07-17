.DELETE_ON_ERROR:

ESY_EXT := $(shell command -v esy 2> /dev/null)

BIN = $(PWD)/node_modules/.bin
PROJECTS = esy esy-build-package esyi
VERSION = $(shell node -p "require('./package.json').version")
PLATFORM = $(shell uname | tr '[A-Z]' '[a-z]')
NPM_RELEASE_TAG ?= latest
ESY_RELEASE_TAG ?= v$(VERSION)

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
   release             Produce an npm release inside _release, use ESY_RELEASE_TAG
                       to control for which tag to fetch platform releases from GitHub

   platform-release    Produce a plartform specific release inside _platformrelease.

   bump-major-version  Bump major package version (commits & tags)
   bump-minor-version  Bump minor package version (commits & tags)
   bump-patch-version  Bump patch package version (commits & tags)

 Website tasks:

   site-serve          Serve site locally
   site-publish        Publish site to https://esy.sh (powered by GitHub Pages)
                       Note that the current USER environment variable will be used as a
                       GitHub user used for push. You can override it by setting GIT_USER
                       env: make GIT_USER=anna publish

 Other tasks:

   refmt               Reformal all *.re source with refmt

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
	@esy install
	@make -C esy-install bootstrap
	@make build-dev
	@make -C test-e2e bootstrap
	@make -C test-e2e-esyi bootstrap
	@ln -s $$(esy which fastreplacestring) $(PWD)/bin/fastreplacestring
	@make -C site bootstrap

doctoc:
	@$(BIN)/doctoc --notitle ./README.md

clean:
	@esy jbuilder clean

build:
	@esy b jbuilder build -j 4 $(TARGETS)

doc:
	@esy b jbuilder build @doc

b: build-dev
build-dev:
	@esy b jbuilder build -j 4 --dev $(TARGETS)

refmt::
	@find $(PROJECTS) -name '*.re' \
		| xargs -n1 esy refmt --in-place --print-width 80

#
# Test
#

JEST = $(BIN)/jest --runInBand

test-unit::
	@esy b jbuilder build --dev @runtest

test-e2e::
	@make -C test-e2e test

test-e2e-esyi::
	@make -C test-e2e-esyi test

test-opam::
	$(MAKE) -C __tests__/opam


test::
	@echo "Running test suite: unit tests"
	@$(MAKE) test-unit
	@echo "Running test suite: e2e"
	@$(MAKE) test-e2e
	@echo "Running test suite: e2e installer"
	@$(MAKE) test-e2e-esyi

ci:: test

#
# Release
#

RELEASE_ROOT = _release
RELEASE_FILES = \
	platform-linux \
	platform-darwin \
	bin/esy \
	bin/esyi \
	bin/esyInstallRelease.js \
	postinstall.js \
	LICENSE \
	README.md \
	package.json \
	bin/esy-install.js

release:
	@echo "Creating $(ESY_RELEASE_TAG) release"
	@rm -rf $(RELEASE_ROOT)
	@mkdir -p $(RELEASE_ROOT)
	@$(MAKE) -j $(RELEASE_FILES:%=$(RELEASE_ROOT)/%)

$(RELEASE_ROOT)/bin/esy-install.js:
	@$(MAKE) -C esy-install BUILD=../$(@) build

$(RELEASE_ROOT)/bin/esy $(RELEASE_ROOT)/bin/esyi:
	@mkdir -p $(@D)
	@echo "#!/bin/sh\necho 'error: esy is not installed correctly...'; exit 1" > $(@)
	@chmod +x $(@)

$(RELEASE_ROOT)/%: $(PWD)/%
	@mkdir -p $(@D)
	@cp $(<) $(@)

$(RELEASE_ROOT)/platform-linux $(RELEASE_ROOT)/platform-darwin: PLATFORM=$(@:$(RELEASE_ROOT)/platform-%=%)
$(RELEASE_ROOT)/platform-linux $(RELEASE_ROOT)/platform-darwin:
	@wget \
		-q --show-progress \
		-O $(RELEASE_ROOT)/$(PLATFORM).tgz \
		'https://github.com/esy/esy/releases/download/$(ESY_RELEASE_TAG)/esy-$(ESY_RELEASE_TAG)-$(PLATFORM).tgz'
	@mkdir $(@)
	@tar -xzf $(RELEASE_ROOT)/$(PLATFORM).tgz -C $(@)
	@rm $(RELEASE_ROOT)/$(PLATFORM).tgz

define MAKE_PACKAGE_JSON
let esyJson = require('./package.json');
console.log(
  JSON.stringify({
		name: esyJson.name,
		version: esyJson.version,
		license: esyJson.license,
		description: esyJson.description,
		repository: esyJson.repository,
		dependencies: {
			"@esy-ocaml/esy-opam": "0.0.15",
			"esy-solve-cudf": esyJson.dependencies["esy-solve-cudf"]
		},
		scripts: {
			postinstall: "node ./postinstall.js"
		},
		bin: {
			esy: "bin/esy",
			esyi: "bin/esyi"
		},
		files: [
			"bin/",
			"postinstall.js",
			"platform-linux/",
			"platform-darwin/"
		]
	}, null, 2));
endef
export MAKE_PACKAGE_JSON

$(RELEASE_ROOT)/package.json:
	@node -e "$$MAKE_PACKAGE_JSON" > $(@)

define POSTINSTALL_JS
var path = require('path');
var fs = require('fs');
var platform = process.platform;

switch (platform) {
  case 'linux':
  case 'darwin':
    fs.renameSync(
      path.join(__dirname, 'platform-' + platform, '_build'),
      path.join(__dirname, '_build')
    );
    fs.renameSync(
      path.join(__dirname, 'platform-' + platform, 'bin', 'fastreplacestring'),
      path.join(__dirname, 'bin', 'fastreplacestring')
    );

    fs.unlinkSync(path.join(__dirname, 'bin', 'esy'));
    fs.symlinkSync(
      path.join(__dirname, '_build', 'default', 'esy', 'bin', 'esyCommand.exe'),
      path.join(__dirname, 'bin', 'esy')
  	);

		fs.unlinkSync(path.join(__dirname, 'bin', 'esyi'));
    fs.symlinkSync(
      path.join(__dirname, '_build', 'default', 'esyi', 'bin', 'esyi.exe'),
      path.join(__dirname, 'bin', 'esyi')
  	);
    break;
  default:
    console.warn("error: no release built for the " + platform + " platform");
    process.exit(1);
}
endef
export POSTINSTALL_JS

$(RELEASE_ROOT)/postinstall.js:
	@echo "$$POSTINSTALL_JS" > $(@)

#
# Platform Specific Release
#

PLATFORM_RELEASE_NAME = _platformrelease/esy-$(ESY_RELEASE_TAG)-$(PLATFORM).tgz
PLATFORM_RELEASE_ROOT = _platformrelease/$(PLATFORM)
PLATFORM_RELEASE_FILES = \
	bin/fastreplacestring \
	_build/default/esy-build-package/bin/esyBuildPackageCommand.exe \
	_build/default/esyi/bin/esyi.exe \
	_build/default/esy/bin/esyCommand.exe \

platform-release: $(PLATFORM_RELEASE_NAME)

$(PLATFORM_RELEASE_NAME): $(PLATFORM_RELEASE_FILES)
	@echo "Creating $(PLATFORM_RELEASE_NAME)"
	@rm -rf $(PLATFORM_RELEASE_ROOT)
	@$(MAKE) $(^:%=$(PLATFORM_RELEASE_ROOT)/%)
	@tar czf $(@) -C $(PLATFORM_RELEASE_ROOT) .
	@rm -rf $(PLATFORM_RELEASE_ROOT)

$(PLATFORM_RELEASE_ROOT)/_build/default/esy/bin/esyCommand.exe:
	@mkdir -p $(@D)
	@cp _build/default/esy/bin/esyCommand.exe $(@)

$(PLATFORM_RELEASE_ROOT)/_build/default/esy-build-package/bin/esyBuildPackageCommand.exe:
	@mkdir -p $(@D)
	@cp _build/default/esy-build-package/bin/esyBuildPackageCommand.exe $(@)

$(PLATFORM_RELEASE_ROOT)/_build/default/esyi/bin/esyi.exe:
	@mkdir -p $(@D)
	@cp _build/default/esyi/bin/esyi.exe $(@)

$(PLATFORM_RELEASE_ROOT)/bin/fastreplacestring:
	@mkdir -p $(@D)
	@cp $(shell esy which fastreplacestring) $(@)

#
# npm publish workflow
#

publish: release
	@(cd $(RELEASE_ROOT) && npm publish --access public --tag $(NPM_RELEASE_TAG))

bump-major-version:
	@npm version major

bump-minor-version:
	@npm version minor

bump-patch-version:
	@npm version patch

## Website

site-start:
	@$(MAKE) -C site start
site-build:
	@$(MAKE) -C site build
site-publish:
	@$(MAKE) -C site publish
