# EXCLUDE_FROM_SOURCE="_build,_grisp,config,_elixir_build"

REBAR            = rebar3
REVISION 		    ?= $(shell git rev-parse --short HEAD)
PROJECT         ?= $(shell basename `find src -name "*.app.src"` .app.src)
DEP_DIR         ?= "deps"
EBIN_DIR        ?= "ebin"

PACKAGE         ?= node
GRISPAPP	       = node
# VERSION         ?= $(shell git describe --tags)
BASE_DIR         = $(shell pwd)
HOME_DIR         = $(shell echo $HOME)
CACHE_DIR        = $(HOME)/.cache/rebar3
ERLANG_BIN       = $(shell dirname $(shell which erl))
HOSTNAME         = $(shell hostname)
COOKIE           = MyCookie
VERSION 	       = 0.1.0
MAKE						 = make

.PHONY: grispbuild rel deps plots dcos logs fpm no-cfg-build tarball-build \
	build compile-no-deps test docs xref dialyzer-run dialyzer-quick dialyzer \
	cleanplt upload-docs wipeout clean cacheclean
# EXCLUDE=$(subst src/bar.cpp,,${SRC_FILES})
# SRC_FILES = $(filter-out $(wildcard ./_*))

# .PHONY: grispbuild rel deps test plots dcos logs fpm no-cfg-build tarball-build build

all: compile

##
## Compilation targets
##

compile:
	$(REBAR) compile

build:
	$(REBAR) grisp build

no-cfg-build:
	$(REBAR) grisp build -c false
	# $(shell rebar3 grisp build -c false)

tarball-build:
	$(REBAR) grisp build -t true

##
## Cleaning targets
##

wipeout: cacheclean clean
	$(REBAR) unlock

clean: buildclean grispclean
	$(REBAR) clean

buildclean:
	rm -rdf $(BASE_DIR)/_build

grispclean:
	rm -rdf $(BASE_DIR)/_grisp

cacheclean:
	rm -rdf $(CACHE_DIR)/hex
	rm -rdf $(CACHE_DIR)/plugins/rebar3_grisp
	$(REBAR) update

##
## Test targets
##

shell:
	$(REBAR) as test shell --sname $(PROJECT) --setcookie $(COOKIE)

lasp-shell:
	$(REBAR) shell --apps lasp --name lasp@localhost


##
## Release targets
##

rel:
	${REBAR} release

stage:
	${REBAR} release -d

grispbuild:
	${REBAR} grisp build -c false skip_deps=true

deploy:
	${REBAR} grisp deploy -n $(PROJECT) -v $(VERSION)


##
## XCOMP targets
##
fpm:
	gem install --no-ri --no-rdoc fpm

package: grispbuild fpm
	fpm -s dir -t deb -n $(PACKAGE) -v $(VERSION) \
		--deb-user $(PACKAGE) \
		--deb-group $(PACKAGE) \
		--before-install=rel/before-install \
		_build/default/rel/$(PACKAGE)=/opt/ \
		rel/init=/etc/init.d/$(PACKAGE) \
		rel/var/lib/$(PACKAGE)/=/var/lib/$(PACKAGE)/ \
		rel/var/log/$(PACKAGE)/=/var/log/$(PACKAGE)/ \
		rel/etc/$(PACKAGE)/$(PACKAGE).config=/etc/$(PACKAGE)/$(PACKAGE).config \
		rel/etc/default/$(PACKAGE)=/etc/default/$(PACKAGE)
# include tools.mk

compile-no-deps:
	${REBAR} compile skip_deps=true

docs:
	${REBAR} doc skip_deps=true

xref: compile
	${REBAR} xref skip_deps=true

dialyzer: compile
	${REBAR} dialyzer
