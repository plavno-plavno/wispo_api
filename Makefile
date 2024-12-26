PROJECT = wispo_api
PROJECT_DESCRIPTION = Wispo API
PROJECT_VERSION = 0.1.0

ERLC_OPTS = +warn_unused_vars
ERLC_OPTS += +warn_unused_function
ERLC_OPTS += +warn_unused_import
ERLC_OPTS += +warn_unused_record
ERLC_OPTS += +warn_deprecated_function +warn_deprecated_type
#ERLC_OPTS += +warn_export_vars +warn_shadow_vars +warn_obsolete_guards
#ERLC_OPTS += +warn_export_all
#ERLC_OPTS += +warn_missing_spec
#ERLC_OPTS += +warn_untyped_record
ERLC_OPTS += +debug_info +bin_opt_info +recv_opt_info
#ERLC_OPTS += +warnings_as_errors

export WISPO_CONFIG=$(CURDIR)/config/wispo.config

CT_OPTS = -erl_args -config $(CURDIR)/config/sys.config

# LOCAL_DEPS += wx
# LOCAL_DEPS += observer et debugger

DEPS += cowboy
dep_cowboy = git https://github.com/ninenines/cowboy.git 2.12.0

DEPS += jose
dep_jose = git https://github.com/potatosalad/erlang-jose.git 1.11.10

DEPS += uuid
dep_uuid = git https://github.com/okeuday/uuid.git v2.0.7

DEPS += epgsql
dep_epgsql = git https://github.com/epgsql/epgsql.git 4.7.1

DEPS += jsx
dep_jsx = git https://github.com/talentdeficit/jsx.git v3.1.0

DIALYZER_DIRS = -r ebin

REL_DEPS += relx
include erlang.mk
