PROJECT = debris

ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
DEPS = simple_bridge erlydtl

include erlang.mk

clean::
	-find . -name "*~" -delete
