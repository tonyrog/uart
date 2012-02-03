
MODULES = \
	uart \
	uart_test

EBIN=../ebin
ERLC = erlc

include ../vsn.mk
VSN=$(UART_VSN)

APP_FILE= uart.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

override ERLC_FLAGS += -I../include

debug: ERLC_FLAGS += +debug_info -Ddebug

OBJS := $(MODULES:%=../ebin/%.beam)
TARGET_FILES = $(OBJS) $(APP_TARGET)

all: $(TARGET_FILES)

debug: all

depend:
	edep -MM -o ../ebin $(ERLC_FLAGS) $(MODULES:%=%.erl) > depend.mk

dialyze:
	dialyzer --src -o dia.out $(ERLC_FLAGS) -c $(MODULES:%=%.erl)

edoc:
	erl -noshell -pa doc -run edoc_run application "sl" '".."' '[{private,false},no_packages,{sort_functions,false}]'


clean:
	rm -f $(OBJS)

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);'  $< > $@

-include depend.mk

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERLC_FLAGS) -o$(EBIN) $<

