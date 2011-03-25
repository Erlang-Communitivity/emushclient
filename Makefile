.SUFFIXES: .erl .beam .yrl

# use vpath to tell make where to search for %.erl files
vpath %.erl src eunit
# or use VPATH to tell make where to search for any prerequisite
# VPATH=src:eunit    

ERL_OBJ = $(patsubst src/%.erl,ebin/%.beam, $(wildcard src/*erl))
ERL_OBJ += $(patsubst eunit/%.erl,ebin/%.beam, $(wildcard eunit/*erl))

all: main

main: ${ERL_OBJ}

ebin/%.beam: %.erl
    erlc +debug_info -W -o ebin $<

clean:
	rm -fr ebin/*.beam


