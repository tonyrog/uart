#
# Make the uart (serial line driver)
#

HOST_CC := gcc
HOST_AR := ar
HOST_RANLIB := ranlib

ifeq ($(TARGET_SYS),)
OSNAME  := $(shell uname -s)
MACHINE := $(shell uname -m)
CC := $(HOST_CC)
AR := $(HOST_AR)
RANLIB := $(HOST_RANLIB)
else
OSNAME := Linux
MACHINE := arm
CC := $(TARGET_SYS)-gcc
AR := $(TARGET_SYS)-ar
RANLIB := $(TARGET_SYS)-ranlib
endif

OBJDIR = .
PRIVDIR = ../priv

ERLDIR	:= $(shell erl -noshell -eval "io:format([126,115,126,110],[code:root_dir()])" -s erlang halt)
ERL_C_INCLUDE_DIR := $(ERLDIR)/usr/include

CFLAGS += -fPIC -Wall -Wextra -Wswitch-default -Wswitch-enum -D_THREAD_SAFE -D_REENTRANT -fno-common -I../include

ifeq ($(TYPE), debug)
CFLAGS +=  -Ddebug -DDEBUG -g 
endif

ifeq ($(OSNAME), Linux)
LINUX = Yes
LD_SHARED := $(CC) -shared
endif

ifeq ($(OSNAME), Darwin)
CFLAGS += -arch i386 -arch x86_64 -DDARWIN -no-cpp-precomp
LD_SHARED := $(CC) -arch i386 -arch x86_64 -bundle -flat_namespace -undefined suppress
endif

ifeq ($(WIN32),Yes)
	CFLAGS += -DWIN32
endif


UART_DRV = $(PRIVDIR)/uart_drv.so

UART_DRV_OBJS = \
	$(OBJDIR)/uart_drv.o \
	$(OBJDIR)/uart_unix.o \
	$(OBJDIR)/packet_parser.o

release:
	$(MAKE) driver TYPE=release

debug:
	$(MAKE) driver TYPE=debug

driver: $(UART_DRV)

clean:
	rm -f $(UART_DRV)
	rm -f $(UART_DRV_OBJS)

$(UART_DRV): $(UART_DRV_OBJS)
	$(LD_SHARED) -o $@ $(UART_DRV_OBJS) $(LD_FLAGS)

$(OBJDIR)/%.o:	%.c
	$(CC) -c -o $@ $(CFLAGS) -I$(ERL_C_INCLUDE_DIR) $<
