SHELL := /bin/bash
UGLIFY_CMD := uglifyjs
RED := \e[1;31m
GRN := \e[1;32m
END := \e[0m
SRC_DIR := src
SRC_FILES := $(wildcard $(SRC_DIR)/*.elm)
APP := elm.js
APP_MIN := elm.min.js

mode := prod

ELMFLAGS_prod := --optimize
ELMFLAGS_dev :=
ELMFLAGS_debug := --debug
ELMFLAGS := ${ELMFLAGS_${mode}}

.SHELLFLAGS = -c -o pipefail -e

.PHONY: all $(MAKECMDGOALS) 

all: clean dist

clean:
	printf "$(GRN)Clean...$(END)\n"
	rm -rf elm-stuff
	rm -f *.js

dist:
	printf "$(GRN)Make $(APP)...$(END)\n"
	elm make $(ELMFLAGS) $(SRC_FILES) --output $(APP)

uglify: dist
	printf "$(GRN)Uglify $(APP)...$(END)\n"
	hash $(UGLIFY_CMD) 2>/dev/null && \
		( $(UGLIFY_CMD) $(APP) \
		--compress \
		'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' | \
		$(UGLIFY_CMD) --mangle --output=$(APP_MIN) ) || \
		printf "$(RED)$(UGLIFY_CMD) is not installed$(END)\n"

