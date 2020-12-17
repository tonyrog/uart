#
# Make the uart test modules
#

MODULES = \
	uart_SUITE


EBIN = ../ebin
ERLC = erlc

ERLDIR  := $(shell erl -noshell -eval "io:format([126,115,126,110],[code:root_dir()])" -s erlang halt)

override ERLC_FLAGS = -MMD -MP -MF .$<.d -Wall -I ../include 

debug: ERLC_FLAGS += +debug_info -Ddebug

OBJS = $(MODULES:%=$(EBIN)/%.beam)

all: $(OBJS)

debug: all

clean:
	rm -f $(OBJS) 

-include depend.mk

$(EBIN)/%.beam:	%.erl
	erlc -o $(EBIN) $(ERLC_FLAGS) $<
