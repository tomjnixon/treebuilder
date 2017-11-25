PROJECT = treebuilder_apps
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

SP = 4

RELX_URL = https://github.com/erlware/relx/releases/download/v3.23.1/relx

release ?= local

RELX_CONFIG = $(CURDIR)/rel/$(release)/relx.config
RELX_OPTS = $(relx_opts) --sys_config $(CURDIR)/rel/$(release)/sys.config
RELX_OPTS += --vm_args $(CURDIR)/rel/$(release)/vm.args

SHELL_OPTS=-config rel/local/sys.config -args_file rel/local/vm.args
SHELL_DEPS = sync

DEP_PLUGINS = cowboy

include erlang.mk
