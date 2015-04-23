PROJECT = grape
ERLC_OPTS= "+{parse_transform, lager_transform}"

DEPS = lager
include erlang.mk

