# Common directories and paths
TOP_DIR := $(dir $(firstword $(MAKEFILE_LIST)))
ABS_DIR := $(abspath $(TOP_DIR))

# Specific Erlang flags that is compatible with this project
BEAM_FLAGS := ERL_FLAGS="$(ERL_FLAGS)"

# Specific Erlang flags for running a local server
SERVER_BEAM_FLAGS := ERL_FLAGS="$(ERL_FLAGS) -args_file $(ABS_DIR)/config/vm.args"

# Build directory
BUILD_DIR := $(ABS_DIR)/_build

#-------------------------------------------------------------------------------
# Targets
#-------------------------------------------------------------------------------

# Default target
.PHONY: default
default: all

# Default targets
.PHONY: all
all: test

.PHONY: test
all: docs compile xref dialyzer eunit ct cover

# Cleanup targets, generate docs, compile the source code and run tests
.PHONY: everything
everything: mrproper all

# Cleanup targets
.PHONY: mrproper
mrproper:
	$(RM) -r $(BUILD_DIR) $(ABS_DIR)/data.* $(ABS_DIR)/log.*

# Cleanup built binaries
.PHONY: clean
clean:
	rebar3 clean

# Install dependencies
.PHONY: install-deps
install-deps:
	rebar3 get-deps

# Upgrade dependencies
.PHONY: deps
upgrade-deps:
	rebar3 upgrade

# Compile and optimize the application
.PHONY: compile
compile:
	rebar3 compile

# Generate docs
.PHONY: docs doc
docs: doc
doc:
	rebar3 edoc

.PHONY: xref
xref:
	rebar3 xref

# Check code with dialyzer
.PHONY: dialyzer
dialyzer:
	rebar3 dialyzer

.PHONY: eunit
eunit:
	$(BEAM_FLAGS) rebar3 as test eunit --cover -v

.PHONY: ct
ct:
	$(BEAM_FLAGS) rebar3 as test ct --cover -v

.PHONY: ct-suite
ct-suite:
	$(BEAM_FLAGS) rebar3 as test ct --cover -v --suite=$(SUITE)

.PHONY: ct-retry
ct-retry:
	$(BEAM_FLAGS) rebar3 as test ct --cover -v --retry

.PHONY: cover
cover:
	rebar3 as test cover --verbose

.PHONY: cover-reset
cover-reset:
	rebar3 as test cover --reset

.PHONY: shell console server
server: shell
console: shell
shell:
	$(SERVER_BEAM_FLAGS) rebar3 as $(or $(REBAR_PROFILE),development) shell

.PHONY: release
release:
	$(BEAM_FILES) rebar3 as $(or $(REBAR_PROFILE), production) tar
