#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CFLAGS+=-finline-functions -std=c99 -Wall -O -DDLOG_DEFAULT=DLOG_NONE
LDFLAGS+=-shared

PRIVDIR := ../priv

DTHREAD_OBJS = dlib.o dlog.o dterm.o dthread.o
COMMON_OBJS = uart_drv.o uart_buf.o uart_queue.o uart_com_state.o \
		uart_modem_state.o uart_options.o uart_message.o
PLATFORM_OBJS = uart_unix.o
# PLATFORM_OBJS = uart_win32.o
OBJS = $(DTHREAD_OBJS) $(COMMON_OBJS) $(PLATFORM_OBJS)

all: $(PRIVDIR)/uart_drv.so

$(PRIVDIR)/uart_drv.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

clean:
	rm -f $(PRIVDIR)/uart_drv.so $(OBJS)

-include ".*.d"
