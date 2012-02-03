//
// uart_drv.c
//
//   Windows/Unix uart driver
//
//

#ifdef __WIN32__

#include <stdio.h>
#include "windows.h"
#include "erl_driver.h"

typedef HANDLE  com_t;
#define INVALID INVALID_HANDLE_VALUE
#define DRV_EVENT(evt) ((ErlDrvEvent)(evt))
#else

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <sys/ioctl.h>

typedef int     com_t;
#define INVALID -1
#define DRV_EVENT(evt) ((ErlDrvEvent)((long)(evt)))
#define uart_errno() errno

#endif

#ifdef HAVE_FTDI
#include "ftd2xx.h"
#endif

#include <stdint.h>

#include "uart_api.h"
#include "packet_parser.h"

#ifdef DEBUG
#define DEBUGF(args...) uart_drv_emit_error(__FILE__,__LINE__,args)
#else
#define DEBUGF(args...)
#endif

typedef struct {
    char         device_name[256];
    int          high;               // high watermark
    int          low;                // low watermark
    int          send_timeout;       // timeout to use in send 
    int          send_timeout_close; // auto-close fd on send_timeout
    int          active;             // PASSIC|ACTIVE|ONCE
    int          delay_send;         // just buffer data 
    int          deliver;            // TERM | PORT
    int          mode;               // LIST | BINARY
    unsigned int htype;              // header type 
    unsigned int psize;              // max packet size
    unsigned int hsz;                // the list header size, -1 is large !!!
    unsigned int bsize;              // input buffer size (buffer)
    int          bit8f;              // check if data has bit number 8 set 
    int          exitf;              // exit on error
} uart_opt_t;    

static int async_ref = 0xFEED;          /* async reference id generator */
#define NEW_ASYNC_ID() ((async_ref++) & 0xffff)

#define UART_MAX_ASYNC (1 << 3)   // always an multiple of 2! > 1
#define UART_MAX_MASK  (UART_MAX_ASYNC-1)

typedef struct {
    int            id;      /* id used to identify reply */
    ErlDrvTermData caller;  /* recipient of async reply */
    int            req;     /* Request id (RECV) */
    union {
	unsigned       value; /* Request timeout (since op issued,not started) */
//	MultiTimerData *mtd;
    } tmo;
    ErlDrvMonitor monitor;
} uart_async_op;


typedef struct _uart_ctx_t
{
    ErlDrvPort       port;
    ErlDrvTermData   dport;     // the port identifier as DriverTermData
    uart_handle_t    handle;    // Connection handle
    uint32_t         flags;     // uart UART_F_xxx

    uint32_t         sflags;    // flags for update state & opts
    uart_com_state_t state;     // communication params 
    uart_opt_t       option;

    int        busy_on_send;    // busy on send with timeout!
    int        bit8;

    ErlDrvTermData caller;      // recipient of sync reply
    ErlDrvTermData busy_caller; // recipient of sync reply when caller busy.
				// Only valid while UART_F_BUSY.

    int op_head;          // queue head
    int op_tail;          // queue tail
    uart_async_op  op_queue[UART_MAX_ASYNC];  // call queue

    int        i_bufsz;        // corrent input buffer size < opt.buffer_size
    ErlDrvBinary* i_buf;       // current binary buffer
    char*         i_ptr;       // current pos in buf 
    char*         i_ptr_start; // packet start pos in buf 
    int           i_remain;     // remaining chars to read
    int           http_state;   // 0 = response|request  1=headers fields

    int        olen;           // length of output buffer
    char*      obuf;           // Overlapped output buffer
    int        o_pending;      // Output is pending

} uart_ctx_t;

#define UART_MAX_PACKET_SIZE 0x01000000  /* 1M */
/*
** Binary Buffer Managment
** We keep a stack of usable buffers 
*/
#define BUFFER_STACK_SIZE 14
#define BUFFER_STACK_MAX_MEM_SIZE (8*1024)
#define CACHE_LINE_SIZE ((ErlDrvUInt) 64)
#define CACHE_LINE_MASK (CACHE_LINE_SIZE - 1)

ErlDrvTSDKey buffer_stack_key;

typedef struct {
    int mem_size;
    int pos;
    ErlDrvBinary* stk[BUFFER_STACK_SIZE];
} buffer_stack_base_t;

typedef struct {
    buffer_stack_base_t buf;
    char align[(((sizeof(buffer_stack_base_t) - 1) / CACHE_LINE_SIZE) + 1)
	       * CACHE_LINE_SIZE];
} buffer_stack_t;


#define UART_MAX_OPT_BUFFER (64*1024)
#define UART_DEF_BUFFER     1024
#define UART_MIN_BUFFER     1

#define UART_HIGH_WATERMARK (1024*2)    // 2k pending high => busy
#define UART_LOW_WATERMARK  (1024*1)    // 1k pending => allow more
#define UART_INFINITY       0xffffffff  // infinity value

#define UART_CMD_OPEN       1
#define UART_CMD_HANGUP     2
#define UART_CMD_CLOSE      4
#define UART_CMD_XON        5
#define UART_CMD_XOFF       6
#define UART_CMD_BREAK      7
#define UART_CMD_SETOPTS    8
#define UART_CMD_GETOPTS    9
#define UART_CMD_SENDCHAR   10
#define UART_CMD_SEND       11
#define UART_CMD_GET_MODEM  12
#define UART_CMD_SET_MODEM  13
#define UART_CMD_CLR_MODEM  14
#define UART_CMD_UNRECV     15
#define UART_CMD_RECV       16


#define UART_OPT_DEVICE     1
#define UART_OPT_IBAUD      2
#define UART_OPT_OBAUD      3
#define UART_OPT_CSIZE      4
#define UART_OPT_BUFSZ      5
#define UART_OPT_BUFTM      6
#define UART_OPT_STOPB      7
#define UART_OPT_PARITY     8
#define UART_OPT_HWFLOW     9
#define UART_OPT_SWFLOW     10
#define UART_OPT_XOFFCHAR   11
#define UART_OPT_XONCHAR    12
#define UART_OPT_EOLCHAR    13
#define UART_OPT_EOL2CHAR   14
#define UART_OPT_ACTIVE     15
#define UART_OPT_DELAY_SEND 16
#define UART_OPT_DELIVER    17
#define UART_OPT_MODE       18
#define UART_OPT_HEADER     20
#define UART_OPT_PACKET     21
#define UART_OPT_PSIZE      22
#define UART_OPT_HIGH       23
#define UART_OPT_LOW        24
#define UART_OPT_SENDTMO    25  // send timeout
#define UART_OPT_CLOSETMO   26  // send close timeout
#define UART_OPT_BUFFER     27
#define UART_OPT_BIT8       28
#define UART_OPT_EXITF      29
#define UART_OPT_MAX        31

#define UART_OPT_COMM \
    ((1 << UART_OPT_IBAUD) | (1 << UART_OPT_OBAUD) | \
     (1 << UART_OPT_CSIZE) | (1 << UART_OPT_BUFSZ) |		\
     (1 << UART_OPT_BUFTM) | (1 << UART_OPT_STOPB) |		\
     (1 << UART_OPT_PARITY) | (1 << UART_OPT_HWFLOW) |		\
     (1 << UART_OPT_SWFLOW) | (1 << UART_OPT_XOFFCHAR) |	\
     (1 << UART_OPT_XONCHAR) | (1 << UART_OPT_EOLCHAR) |	\
     (1 << UART_OPT_EOL2CHAR))

#define UART_PB_LITTLE_ENDIAN 0x00008000  // UART_PB_<n> 
#define UART_PB_BYTES_MASK    0x00000F00  // UART_PB_<n> 0..8 allowed
#define UART_PB_FIXED_MASK    0xFFFF0000  // UART_PB_RAW
#define UART_PB_TYPE_MASK     0x000000FF  // UART_PB_x

#define UART_PB_RAW        0
#define UART_PB_N          1
#define UART_PB_ASN1       2
#define UART_PB_RM         3
#define UART_PB_CDR        4
#define UART_PB_FCGI       5
#define UART_PB_LINE_LF    6
#define UART_PB_TPKT       7
#define UART_PB_HTTP       8
#define UART_PB_HTTPH      9
#define UART_PB_SSL_TLS    10
#define UART_PB_HTTP_BIN   11
#define UART_PB_HTTPH_BIN  12

#define UART_PASSIVE  0
#define UART_ACTIVE   1
#define UART_ONCE     2

#define UART_PARITY_NONE  0
#define UART_PARITY_ODD   1
#define UART_PARITY_EVEN  2
#define UART_PARITY_MARK  3

#define UART_DELIVER_PORT  0
#define UART_DELIVER_TERM  1

#define UART_MODE_LIST    0
#define UART_MODE_BINARY  1

// UART_OPT_BIT8
#define UART_BIT8_CLEAR 0
#define UART_BIT8_SET   1
#define UART_BIT8_ON    2
#define UART_BIT8_OFF   3

#define UART_OK       0
#define UART_ERROR    1
#define UART_OPTIONS  2

#define UART_F_OPEN               0x0001
#define UART_F_BUSY               0x0080
#define UART_F_CLOSE_SENT         0x0002
#define UART_F_DELAYED_CLOSE_RECV 0x0004
#define UART_F_DELAYED_CLOSE_SEND 0x0008

#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

#define LOAD_ATOM_CNT 2
#define LOAD_ATOM(vec, i, atom) \
  (((vec)[(i)] = ERL_DRV_ATOM), \
  ((vec)[(i)+1] = (atom)), \
  ((i)+LOAD_ATOM_CNT))

#define LOAD_INT_CNT 2
#define LOAD_INT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_INT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  ((i)+LOAD_INT_CNT))

#define LOAD_UINT_CNT 2
#define LOAD_UINT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_UINT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  ((i)+LOAD_UINT_CNT))

#define LOAD_PORT_CNT 2
#define LOAD_PORT(vec, i, port) \
  (((vec)[(i)] = ERL_DRV_PORT), \
  ((vec)[(i)+1] = (port)), \
  ((i)+LOAD_PORT_CNT))

#define LOAD_PID_CNT 2
#define LOAD_PID(vec, i, pid) \
  (((vec)[(i)] = ERL_DRV_PID), \
  ((vec)[(i)+1] = (pid)), \
  ((i)+LOAD_PID_CNT))

#define LOAD_BINARY_CNT 4
#define LOAD_BINARY(vec, i, bin, offs, len) \
  (((vec)[(i)] = ERL_DRV_BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(bin)), \
  ((vec)[(i)+2] = (len)), \
  ((vec)[(i)+3] = (offs)), \
  ((i)+LOAD_BINARY_CNT))

#define LOAD_BUF2BINARY_CNT 3
#define LOAD_BUF2BINARY(vec, i, buf, len) \
  (((vec)[(i)] = ERL_DRV_BUF2BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(buf)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_BUF2BINARY_CNT))

#define LOAD_STRING_CNT 3
#define LOAD_STRING(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_STRING_CNT))

#define LOAD_STRING_CONS_CNT 3
#define LOAD_STRING_CONS(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING_CONS), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_STRING_CONS_CNT))

#define LOAD_TUPLE_CNT 2
#define LOAD_TUPLE(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_TUPLE), \
  ((vec)[(i)+1] = (size)), \
  ((i)+LOAD_TUPLE_CNT))

#define LOAD_NIL_CNT 1
#define LOAD_NIL(vec, i) \
  (((vec)[(i)] = ERL_DRV_NIL), \
  ((i)+LOAD_NIL_CNT))

#define LOAD_LIST_CNT 2
#define LOAD_LIST(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_LIST), \
  ((vec)[(i)+1] = (size)), \
  ((i)+LOAD_LIST_CNT))

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

// forward
static int async_error(uart_ctx_t* ctx, int err);
static int async_error_am_all(uart_ctx_t* ctx, ErlDrvTermData reason);
static int http_load_string(uart_ctx_t* ctx, ErlDrvTermData* spec, int i,
			    const char* str, int len);
static int deq_async(uart_ctx_t* ctx, int* ap, ErlDrvTermData* cp, int* rp);


static int  uart_drv_init(void);
static void uart_drv_finish(void);
static void uart_drv_stop(ErlDrvData);
static void uart_drv_output(ErlDrvData, char*, ErlDrvSizeT);
static void uart_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void uart_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData uart_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT uart_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**, ErlDrvSizeT);
static void uart_drv_timeout(ErlDrvData);
static void uart_drv_stop_select(ErlDrvEvent, void*); 

static ErlDrvTermData am_ok;
static ErlDrvTermData am_uart;
static ErlDrvTermData am_error;
static ErlDrvTermData am_uart_async;
static ErlDrvTermData am_uart_reply;
static ErlDrvTermData am_timeout;
static ErlDrvTermData am_closed;
static ErlDrvTermData am_uart_closed;
static ErlDrvTermData am_uart_error;
static ErlDrvTermData am_empty_out_q;
static ErlDrvTermData am_ssl_tls;

static ErlDrvTermData am_http_eoh;
static ErlDrvTermData am_http_header;
static ErlDrvTermData am_http_request;
static ErlDrvTermData am_http_response;
static ErlDrvTermData am_http_error;
static ErlDrvTermData am_abs_path;
static ErlDrvTermData am_absoluteURI;
static ErlDrvTermData am_star;
static ErlDrvTermData am_undefined;
static ErlDrvTermData am_http;
static ErlDrvTermData am_https;
static ErlDrvTermData am_scheme;


static ErlDrvEntry uart_drv_entry;

#ifdef HAVE_FTDI
const char* ft_strerror(FT_STATUS status)
{
    switch(status) {
    case FT_OK:	return "ok";
    case FT_INVALID_HANDLE: return "invalid handle";
    case FT_DEVICE_NOT_FOUND: return "device not found";
    case FT_DEVICE_NOT_OPENED: return "device not opened";
    case FT_IO_ERROR:	return "io error";
    case FT_INSUFFICIENT_RESOURCES: return "insufficent resources";
    case FT_INVALID_PARAMETER: return "invalid paramter";
    case FT_INVALID_BAUD_RATE: return "invalid baud rate";
    case FT_DEVICE_NOT_OPENED_FOR_ERASE: return "device not opened for erase";
    case FT_DEVICE_NOT_OPENED_FOR_WRITE: return "device not opened for write";
    case FT_FAILED_TO_WRITE_DEVICE: return "faile to write device";
    case FT_EEPROM_READ_FAILED: return "eeprom read failed";
    case FT_EEPROM_WRITE_FAILED: return "eeprom write failed";
    case FT_EEPROM_ERASE_FAILED: return "eeprom erase failed";
    case FT_EEPROM_NOT_PRESENT: return "eeprom not present";
    case FT_EEPROM_NOT_PROGRAMMED: return "eeprom not programmed";
    case FT_INVALID_ARGS: return "invalid arguments";
    case FT_NOT_SUPPORTED: return "not supported";
    case FT_OTHER_ERROR: return "other error";
    default: return "unknown error (%d)";
    }
}
#endif

#ifdef DEBUG
void uart_drv_emit_error(char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);

    fprintf(stderr, "%s:%d: ", file, line); 
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
}

static char* format_hex(uint8_t* ptr, int len, char* dst, int dst_len)
{
    char* dst0 = (char*) dst;
    const char* hex = "0123456789ABCDEF";

    while(len--) {
	if (dst_len > 2) {
	    *dst++ = hex[(*ptr >> 4) & 0xF];
	    *dst++ = hex[*ptr & 0xF];
	    dst_len -= 2;
	}
	ptr++;
    }
    *dst++ = '\0';
    return dst0;
}

#endif

static inline uint32_t get_uint32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return value;
}

static inline uint16_t get_uint16(uint8_t* ptr)
{
    uint16_t value = (ptr[0]<<8) | (ptr[1]<<0);
    return value;
}

static inline uint16_t get_uint8(uint8_t* ptr)
{
    uint8_t value = (ptr[0]<<0);
    return value;
}

static inline void put_uint16(uint8_t* ptr, uint16_t v)
{
    ptr[0] = v>>8;
    ptr[1] = v;
}


static inline void put_uint32(uint8_t* ptr, uint32_t v)
{
    ptr[0] = v>>24;
    ptr[1] = v>>16;
    ptr[2] = v>>8;
    ptr[3] = v;
}

static int close_device(uart_ctx_t* ctx)
{
    if (ctx->flags & UART_F_OPEN) {
	driver_select(ctx->port,DRV_EVENT(ctx->handle.data),ERL_DRV_USE,0);
	ctx->flags &= ~UART_F_OPEN;
    }
    return 0;
}

static void close_read_device(uart_ctx_t* ctx)
{
    if (ctx->flags & UART_F_OPEN) {
	driver_select(ctx->port,DRV_EVENT(ctx->handle.data),ERL_DRV_READ,0);
    }
}

static int open_device(uart_ctx_t* ctx)
{
    int r;

    if (ctx->flags & UART_F_OPEN) {  // must close first!
	errno = EALREADY; 
	return -1;
    }
    // FIXME: handle ftdi flavours when avaiable
#ifdef __WIN32__
    r = uart_win_open(&ctx->handle, ctx->option.device_name);
#else
    r = uart_unix_open(&ctx->handle, ctx->option.device_name);
#endif
    if (r >= 0)
	ctx->flags |= UART_F_OPEN;
    return r;
}

void set_default_state(uart_com_state_t* ptr)
{
    ptr->ibaud    = 9600;
    ptr->obaud    = 9600;
    ptr->parity   = 0;
    ptr->stopb    = 1;
    ptr->csize    = 8;
    ptr->bufsz    = 1;
    ptr->buftm    = 0;
    ptr->xonchar  = 17;
    ptr->xoffchar = 19;
    ptr->eolchar  = '\n';
    ptr->eol2char = '\r';
    ptr->swflow   = 0;
    ptr->hwflow   = 0;
}


ErlDrvSSizeT get_opts(uart_ctx_t* ctx, char* buf, ErlDrvSizeT len,
		      char** rbuf, ErlDrvSizeT rsize)
{
    uint8_t* ptr = (uint8_t*) buf;
    uint8_t* ptr_end = ptr + len;
    uint8_t* dst = (uint8_t*) *rbuf;
    uint8_t* dst0 = dst;
    uint32_t need = 0;
    int i;

#define PUT_OPT(x) do {							\
	put_uint32(dst, (x));						\
	dst += sizeof(uint32_t);					\
    } while(0)

    // Calculate how much buffer is needed
    need = 1;
    for (i = 0; i < (int)len; i++) {
	if (buf[i] == UART_OPT_DEVICE)
	    need += (1+1+strlen(ctx->option.device_name));
	else
	    need += (1 + sizeof(uint32_t));
    }

    DEBUGF("get_opts: need = %d, dst0=%p", need, dst0);

    if (need > rsize) {
	ErlDrvBinary* bin;
	if (!(bin = driver_alloc_binary(need)))
	    return -1;
	dst  = (uint8_t*) bin->orig_bytes;
	dst0 = dst;
	*rbuf = (char*)bin;
    }

    *dst++ = UART_OPTIONS;
    while(ptr < ptr_end) {
	uint8_t opt = *ptr++;
	*dst++ = opt;
	switch(opt) {
	case UART_OPT_DEVICE: {
	    int n = strlen(ctx->option.device_name);
	    *dst++ = n;
	    memcpy(dst, ctx->option.device_name, n);
	    dst += n;
	    break;
	}
	case UART_OPT_IBAUD: PUT_OPT(ctx->state.ibaud); break;
	case UART_OPT_OBAUD: PUT_OPT(ctx->state.obaud);  break;
	case UART_OPT_CSIZE: PUT_OPT(ctx->state.csize);  break;
	case UART_OPT_BUFSZ: PUT_OPT(ctx->state.bufsz);  break;
	case UART_OPT_BUFTM: PUT_OPT(ctx->state.buftm);  break;
	case UART_OPT_STOPB: PUT_OPT(ctx->state.stopb);  break;
	case UART_OPT_PARITY:PUT_OPT(ctx->state.parity); break;
	case UART_OPT_HWFLOW:PUT_OPT(ctx->state.hwflow); break;
	case UART_OPT_SWFLOW:PUT_OPT(ctx->state.swflow); break;
	case UART_OPT_XOFFCHAR: PUT_OPT(ctx->state.xoffchar); break;
	case UART_OPT_XONCHAR: PUT_OPT(ctx->state.xonchar); break;
	case UART_OPT_EOLCHAR: PUT_OPT(ctx->state.eolchar); break;
	case UART_OPT_EOL2CHAR: PUT_OPT(ctx->state.eol2char); break;
	case UART_OPT_ACTIVE: PUT_OPT(ctx->option.active); break;
	case UART_OPT_DELAY_SEND: PUT_OPT(ctx->option.delay_send); break;
	case UART_OPT_DELIVER: PUT_OPT(ctx->option.deliver); break;
	case UART_OPT_MODE: PUT_OPT(ctx->option.mode); break;
	case UART_OPT_HEADER: PUT_OPT(ctx->option.hsz); break;
	case UART_OPT_PACKET: PUT_OPT(ctx->option.htype); break;
	case UART_OPT_PSIZE: PUT_OPT(ctx->option.psize); break;
	case UART_OPT_HIGH: PUT_OPT(ctx->option.high); break;
	case UART_OPT_LOW: PUT_OPT(ctx->option.low); break;
	case UART_OPT_SENDTMO: PUT_OPT(ctx->option.send_timeout); break;
	case UART_OPT_CLOSETMO: PUT_OPT(ctx->option.send_timeout_close); break;
	case UART_OPT_BUFFER: PUT_OPT(ctx->option.bsize);  break;
	case UART_OPT_BIT8: PUT_OPT(ctx->option.bit8f);  break;
	case UART_OPT_EXITF: PUT_OPT(ctx->option.exitf);  break;
	default: errno = EINVAL; return -1;
	}
    }
    DEBUGF("get_opts: filled=%d,dst0=%p", dst-dst0, dst0);
    return (dst-dst0);
#undef PUT_OPT
}

void dump_com_state(FILE* f, uart_com_state_t* state)
{
    fprintf(f, "ibaud: %d\r\n", state->ibaud);
    fprintf(f, "obaud: %d\r\n", state->obaud);
    fprintf(f, "parity: %d\r\n", state->parity);
    fprintf(f, "stopb: %d\r\n", state->stopb);
    fprintf(f, "csize: %d\r\n", state->csize);
    fprintf(f, "bufsz: %d\r\n", state->bufsz);
    fprintf(f, "buftm: %d\r\n", state->buftm);
    fprintf(f, "xonchar: %d\r\n", state->xonchar);
    fprintf(f, "xoffchar: %d\r\n", state->xoffchar);
    fprintf(f, "eolchar: %d\r\n", state->eolchar);
    fprintf(f, "eol2char: %d\r\n", state->eol2char);
    fprintf(f, "swflow: %d\r\n", state->swflow);
    fprintf(f, "hwflow: %d\r\n", state->hwflow);
}

void dump_modem_state(FILE* f, uart_modem_state_t state)
{
    fprintf(f, "modem state:");
    if (state & UART_MODEM_DTR) fprintf(f," DTR");
    if (state & UART_MODEM_RTS) fprintf(f," RTS");
    if (state & UART_MODEM_CTS) fprintf(f," CTS");
    if (state & UART_MODEM_DCD) fprintf(f," DCD");
    if (state & UART_MODEM_RI)  fprintf(f," RI");
    if (state & UART_MODEM_DSR) fprintf(f," DSR");
    fprintf(f, "\r\n");
}

// Copy changed com state configs from source to destination
void copy_com_state(uart_com_state_t* dst,uart_com_state_t* src,
		    uint32_t sflags)
{
    // update the state1 
    if (sflags & (1<<UART_OPT_IBAUD)) dst->ibaud = src->ibaud;
    if (sflags & (1<<UART_OPT_OBAUD)) dst->obaud = src->obaud;
    if (sflags & (1<<UART_OPT_CSIZE)) dst->csize = src->csize;
    if (sflags & (1<<UART_OPT_BUFSZ)) dst->bufsz = src->bufsz;
    if (sflags & (1<<UART_OPT_BUFTM)) dst->buftm = src->buftm;
    if (sflags & (1<<UART_OPT_STOPB)) dst->stopb = src->stopb;
    if (sflags & (1<<UART_OPT_PARITY)) dst->parity = src->parity;
    if (sflags & (1<<UART_OPT_HWFLOW)) dst->hwflow = src->hwflow; 
    if (sflags & (1<<UART_OPT_SWFLOW)) dst->swflow = src->swflow; 
    if (sflags & (1<<UART_OPT_XOFFCHAR)) dst->xoffchar = src->xoffchar; 
    if (sflags & (1<<UART_OPT_XONCHAR)) dst->xonchar = src->xonchar; 
    if (sflags & (1<<UART_OPT_EOLCHAR)) dst->eolchar = src->eolchar; 
    if (sflags & (1<<UART_OPT_EOL2CHAR)) dst->eol2char = src->eol2char; 
}


int set_opts(uart_ctx_t* ctx, char* buf, ErlDrvSizeT len)
{
    uint32_t         sflags = 0;
    uart_com_state_t state;
    uart_opt_t       option;
    unsigned char*   ptr = (unsigned char*) buf;
    unsigned char*   ptr_end = ptr + len;
    int old_active;
    unsigned int old_htype;
    int bit8;


#define GET_UINT32(v) do { \
	if ((ptr_end - ptr) < 4) return -1; \
	v = (int) get_uint32(ptr); \
	DEBUGF("option %s value=0x%08x", #v, v); \
	ptr += 4; \
    } while(0)

    DEBUGF("set_opts: called");

    // copy current state - maybe pending updates
    bit8  = ctx->bit8;
    option = ctx->option;
    state = ctx->state;
    sflags = ctx->sflags;

    // process updates 
    while(ptr < ptr_end) {
	DEBUGF("set_opts: opt=%u", ptr[0]);
	sflags |= (1 << ptr[0]);

	switch(*ptr++) {
	case UART_OPT_DEVICE: {
	    int n;
	    if ((ptr_end - ptr) < 1) return -1;
	    n = get_uint8(ptr);
	    ptr++;
	    if ((ptr_end - ptr) < n) return -1;
	    memcpy(option.device_name, ptr, n);
	    option.device_name[n] = 0;
	    DEBUGF("set_opts: device_name = %s", option.device_name);
	    ptr += n;
	    break;
	}

	case UART_OPT_IBAUD:    GET_UINT32(state.ibaud); break;
	case UART_OPT_OBAUD:    GET_UINT32(state.obaud); break;
	case UART_OPT_CSIZE:    GET_UINT32(state.csize); break;
	case UART_OPT_BUFSZ:    GET_UINT32(state.bufsz); break;
	case UART_OPT_BUFTM:    GET_UINT32(state.buftm); break;
	case UART_OPT_STOPB:    GET_UINT32(state.stopb); break;
	case UART_OPT_PARITY:   GET_UINT32(state.parity); break;
	case UART_OPT_HWFLOW:   GET_UINT32(state.hwflow); break;
	case UART_OPT_SWFLOW:   GET_UINT32(state.swflow); break;
	case UART_OPT_XOFFCHAR: GET_UINT32(state.xoffchar); break;
	case UART_OPT_XONCHAR:  GET_UINT32(state.xonchar); break;
	case UART_OPT_EOLCHAR:  GET_UINT32(state.eolchar); break;
	case UART_OPT_EOL2CHAR: GET_UINT32(state.eol2char); break;

	case UART_OPT_ACTIVE:   GET_UINT32(option.active); break;
	case UART_OPT_DELAY_SEND: GET_UINT32(option.delay_send); break;
	case UART_OPT_DELIVER:  GET_UINT32(option.deliver); break;
	case UART_OPT_MODE:     GET_UINT32(option.mode); break;
	case UART_OPT_HEADER:   GET_UINT32(option.hsz); break;
	case UART_OPT_PACKET:   GET_UINT32(option.htype); break;
	case UART_OPT_PSIZE:    GET_UINT32(option.psize); break;
	case UART_OPT_HIGH:     GET_UINT32(option.high);  break;
	case UART_OPT_LOW:      GET_UINT32(option.low);   break;
	case UART_OPT_SENDTMO:  GET_UINT32(option.send_timeout); break;
	case UART_OPT_CLOSETMO: GET_UINT32(option.send_timeout_close); break;
	case UART_OPT_BUFFER:   GET_UINT32(option.bsize); break;
	case UART_OPT_BIT8: {
	    int val;
	    GET_UINT32(val);
	    switch(val) {
	    case UART_BIT8_ON:
		option.bit8f = 1;
		bit8  = 0;
		break;
	    case UART_BIT8_OFF:
		option.bit8f = 0;
		bit8  = 0;
		break;
	    case UART_BIT8_CLEAR:
		option.bit8f = 1;
		bit8  = 0;
		break;
	    case UART_BIT8_SET:
		option.bit8f = 1;
		bit8  = 1;
		break;
	    default:
		return -1;
	    }
	    break;
	}
	case UART_OPT_EXITF: GET_UINT32(option.exitf); break;
	default:
	    return -1;
	}
    }

    if ((sflags & (1 << UART_OPT_DEVICE)) &&
	(strcmp(option.device_name, ctx->option.device_name) != 0)) {
	close_device(ctx);
	strcpy(ctx->option.device_name, option.device_name);
	if (open_device(ctx) < 0)
	    return -1;
	if (uart_set_com_state(&ctx->handle, &state) < 0)
	    return -1;
	sflags = 0;
	if (uart_get_com_state(&ctx->handle, &state) >= 0) {
	    DEBUGF("set_opts: com_state: after");
#ifdef DEBUG
	    dump_com_state(stderr, &state);
#endif
	}
    }
    else if (sflags & UART_OPT_COMM) {
	if (ctx->flags & UART_F_OPEN) {
	    DEBUGF("set_opts: com_state before:");
	    dump_com_state(stderr, &state);
	    if (uart_set_com_state(&ctx->handle, &state) < 0)
		return -1;
	    sflags = 0;
	    if (uart_get_com_state(&ctx->handle, &state) >= 0) {
		DEBUGF("set_opts: com_state: after");
#ifdef DEBUG
		dump_com_state(stderr, &state);
#endif
	    }
	}
    }

    old_active = ctx->option.active;
    old_htype = ctx->option.htype;

    ctx->sflags = sflags;
    ctx->state  = state;
    ctx->option = option;
    ctx->bit8   = bit8;

    if (ctx->flags & UART_F_OPEN) {
	if (old_active != option.active)
	    driver_select(ctx->port,DRV_EVENT(ctx->handle.data),ERL_DRV_READ,
			  (option.active > 0));
	if (ctx->option.active) { // was active
	    if (!old_active || (ctx->option.htype != old_htype)) {
		/* passive => active change OR header type change in active mode */
		return 1;
	    }
	    return 0;
	}
    }
    return 0;
#undef GET_UINT32
}


static buffer_stack_t* get_bufstk(void)
{
    buffer_stack_t* bs = erl_drv_tsd_get(buffer_stack_key);
    if (bs)
	return bs;
    bs = driver_alloc(sizeof(buffer_stack_t) + CACHE_LINE_SIZE - 1);
    if (!bs)
	return NULL;
    if ((((ErlDrvUInt) bs) & CACHE_LINE_MASK) != 0)
	bs = ((buffer_stack_t*) ((((ErlDrvUInt) bs) & ~CACHE_LINE_MASK)
				 + CACHE_LINE_SIZE));
    erl_drv_tsd_set(buffer_stack_key, bs);
    bs->buf.pos = 0;
    bs->buf.mem_size = 0;
    return bs;
}

static ErlDrvBinary* alloc_buffer(ErlDrvSizeT minsz)
{
    buffer_stack_t* bs = get_bufstk();
    if (bs && bs->buf.pos > 0) {
	long size;
	ErlDrvBinary* buf = bs->buf.stk[--bs->buf.pos];
	size = buf->orig_size;
	bs->buf.mem_size -= size;
	if (size >= (long)minsz)
	    return buf;
	driver_free_binary(buf);
    }
    return driver_alloc_binary(minsz);
}

static void release_buffer(ErlDrvBinary* buf)
{
    buffer_stack_t* bs;
    long size;

    if (!buf)
	return;
    size = buf->orig_size;

    if (size > BUFFER_STACK_MAX_MEM_SIZE)
	goto free_binary;
    bs = get_bufstk();
    if (!bs
	|| (bs->buf.mem_size + size > BUFFER_STACK_MAX_MEM_SIZE)
	|| (bs->buf.pos >= BUFFER_STACK_SIZE)) {
    free_binary:
	driver_free_binary(buf);
    }
    else {
	bs->buf.mem_size += size;
	bs->buf.stk[bs->buf.pos++] = buf;
    }
}

static ErlDrvBinary* realloc_buffer(ErlDrvBinary* buf, ErlDrvSizeT newsz)
{
    return driver_realloc_binary(buf, newsz);
}

/* use a TRICK, access the refc field to see if any one else has
 * a ref to this buffer then call driver_free_binary else 
 * release_buffer instead
 */
static void free_buffer(ErlDrvBinary* buf)
{
    DEBUGF("free_buffer: %ld", (buf==NULL) ? 0 : buf->orig_size);

    if (buf != NULL) {
	if (driver_binary_get_refc(buf) == 1)
	    release_buffer(buf);
	else
	    driver_free_binary(buf);
    }
}

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, char* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
	ptr = driver_alloc(len+1);
	*rbuf = ptr;
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

/* general control error reply function */
static ErlDrvSSizeT ctl_error(int err, char** rbuf, ErlDrvSizeT rsize)
{
    char response[256];		/* Response buffer. */
    char* s;
    char* t;

    for (s = erl_errno_id(err), t = response; *s; s++, t++)
	*t = tolower(*s);
    return ctl_reply(UART_ERROR, response, t-response, rbuf, rsize);
}

static ErlDrvTermData error_atom(int err)
{
    char errstr[256];
    char* s;
    char* t;

    for (s = erl_errno_id(err), t = errstr; *s; s++, t++)
	*t = tolower(*s);
    *t = '\0';
    return driver_mk_atom(errstr);
}


static int http_response_uart_drv(void *arg, int major, int minor,
				 int status, const char* phrase, int phrase_len)
{
    uart_ctx_t* ctx = (uart_ctx_t*) arg;
    int i = 0;
    ErlDrvTermData spec[27];
    ErlDrvTermData caller = ERL_DRV_NIL;
    
    if (ctx->option.active == UART_PASSIVE) {
        /* {uart_async,S,Ref,{ok,{http_response,Version,Status,Phrase}}} */
        int req;
        int aid;
        
        if (deq_async(ctx, &aid, &caller, &req) < 0)
            return -1;
        i = LOAD_ATOM(spec, i,  am_uart_async);
        i = LOAD_PORT(spec, i,  ctx->dport);
        i = LOAD_INT(spec, i,   aid);
        i = LOAD_ATOM(spec, i,  am_ok);
    }
    else {
        /* {http, S, {http_response,Version,Status,Phrase}} */
        i = LOAD_ATOM(spec, i, am_http);
        i = LOAD_PORT(spec, i, ctx->dport);
    }
    i = LOAD_ATOM(spec, i,  am_http_response);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_INT(spec, i, status);
    i = http_load_string(ctx, spec, i, phrase, phrase_len);
    i = LOAD_TUPLE(spec, i, 4);
    
    if (ctx->option.active == UART_PASSIVE) {
        i = LOAD_TUPLE(spec, i, 2);
        i = LOAD_TUPLE(spec, i, 4);
        return driver_send_term(ctx->port, caller, spec, i);
    }
    else {
        i = LOAD_TUPLE(spec, i, 3);
        return driver_output_term(ctx->port, spec, i);
    }
}

static int http_load_uri(uart_ctx_t* ctx, ErlDrvTermData* spec, int i,
			 const PacketHttpURI* uri)
{
    ErlDrvTermData scheme;

    switch (uri->type) {
    case URI_STAR:
        i = LOAD_ATOM(spec, i, am_star);
        break;
    case URI_ABS_PATH:
        i = LOAD_ATOM(spec, i, am_abs_path);
        i = http_load_string(ctx, spec, i, uri->s1_ptr, uri->s1_len);
        i = LOAD_TUPLE(spec, i, 2);
        break;
    case URI_HTTP:
        scheme = am_http;
        goto http_common;
    case URI_HTTPS:
        scheme = am_https;
    http_common:
        i = LOAD_ATOM(spec, i, am_absoluteURI);
        i = LOAD_ATOM(spec, i, scheme);
        i = http_load_string(ctx, spec, i, uri->s1_ptr, uri->s1_len);
        if (uri->port == 0) {
            i = LOAD_ATOM(spec, i, am_undefined);
        } else {
            i = LOAD_INT(spec, i, uri->port);
        }
        i = http_load_string(ctx, spec, i, uri->s2_ptr, uri->s2_len);
        i = LOAD_TUPLE(spec, i, 5);
        break;

    case URI_STRING:
        i = http_load_string(ctx, spec, i, uri->s1_ptr, uri->s1_len);
        break;
    case URI_SCHEME:
        i = LOAD_ATOM(spec, i, am_scheme);
        i = http_load_string(ctx, spec, i, uri->s1_ptr, uri->s1_len);
        i = http_load_string(ctx, spec, i, uri->s2_ptr, uri->s2_len);
        i = LOAD_TUPLE(spec, i, 3);
    default:
	break;
    }
    return i;
}


static int
http_request_uart_drv(void* arg, const http_atom_t* meth, const char* meth_ptr,
		     int meth_len, const PacketHttpURI* uri,
		     int major, int minor)
{
    uart_ctx_t* ctx = (uart_ctx_t*) arg;
    int i = 0;
    ErlDrvTermData spec[43];
    ErlDrvTermData caller = ERL_DRV_NIL;
    
    if (ctx->option.active == UART_PASSIVE) {
        /* {uart_async, S, Ref, {ok,{http_request,Meth,Uri,Version}}} */
        int req;
        int aid;
        
        if (deq_async(ctx, &aid, &caller, &req) < 0)
            return -1;
        i = LOAD_ATOM(spec, i,  am_uart_async);
        i = LOAD_PORT(spec, i,  ctx->dport);
        i = LOAD_INT(spec, i,   aid);
        i = LOAD_ATOM(spec, i,  am_ok);
    }
    else {
        /* {http, S, {http_request,Meth,Uri,Version}}} */
        i = LOAD_ATOM(spec, i, am_http);
        i = LOAD_PORT(spec, i, ctx->dport);
    }

    i = LOAD_ATOM(spec, i,  am_http_request);
    if (meth != NULL)
      i = LOAD_ATOM(spec, i, meth->atom);
    else
      i = http_load_string(ctx, spec, i, meth_ptr, meth_len);
    i = http_load_uri(ctx, spec, i, uri);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);

    if (ctx->option.active == UART_PASSIVE) {
        i = LOAD_TUPLE(spec, i, 2);
        i = LOAD_TUPLE(spec, i, 4);
        return driver_send_term(ctx->port, caller, spec, i);
    }
    else {
        i = LOAD_TUPLE(spec, i, 3);
        return driver_output_term(ctx->port, spec, i);
    }
}

static int
http_header_uart_drv(void* arg, const http_atom_t* name, const char* name_ptr,
		    int name_len, const char* value_ptr, int value_len)
{
    uart_ctx_t* ctx = (uart_ctx_t*) arg;
    int i = 0;
    ErlDrvTermData spec[26];
    ErlDrvTermData caller = ERL_DRV_NIL;
    
    if (ctx->option.active == UART_PASSIVE) {
        /* {uart_async,S,Ref,{ok,{http_header,Bit,Name,IValue,Value}} */
        int req;
        int aid;
        
        
        if (deq_async(ctx, &aid, &caller, &req) < 0)
            return -1;
        i = LOAD_ATOM(spec, i,  am_uart_async);
        i = LOAD_PORT(spec, i,  ctx->dport);
        i = LOAD_INT(spec, i,   aid);
        i = LOAD_ATOM(spec, i,  am_ok);
    }
    else {
        /* {http, S, {http_header,Bit,Name,IValue,Value}} */
        i = LOAD_ATOM(spec, i, am_http);
        i = LOAD_PORT(spec, i, ctx->dport);
    }

    i = LOAD_ATOM(spec, i,  am_http_header);
    if (name != NULL) {
      i = LOAD_INT(spec, i,  name->index+1);
      i = LOAD_ATOM(spec, i, name->atom);
    }
    else {
      i = LOAD_INT(spec, i,  0);
      i = http_load_string(ctx, spec, i, name_ptr, name_len);
    }
    i = LOAD_ATOM(spec, i, am_undefined);
    i = http_load_string(ctx, spec, i, value_ptr, value_len);
    i = LOAD_TUPLE(spec, i, 5);

    if (ctx->option.active == UART_PASSIVE) {
        i = LOAD_TUPLE(spec, i, 2);
        i = LOAD_TUPLE(spec, i, 4);
        return driver_send_term(ctx->port, caller, spec, i);
    }
    else {
        i = LOAD_TUPLE(spec, i, 3);
        return driver_output_term(ctx->port, spec, i);
    }
}

static int http_eoh_uart_drv(void* arg)
{
  uart_ctx_t* ctx = (uart_ctx_t*) arg;
  int i = 0;
  ErlDrvTermData spec[14];

  if (ctx->option.active == UART_PASSIVE) {
    /* {uart_async,S,Ref,{ok,http_eoh}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(ctx, &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_uart_async);
    i = LOAD_PORT(spec, i,  ctx->dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_eoh);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    return driver_send_term(ctx->port, caller, spec, i);
  }
  else {
      /* {http, S, http_eoh} */
      i = LOAD_ATOM(spec, i,  am_http);
      i = LOAD_PORT(spec, i,  ctx->dport);
      i = LOAD_ATOM(spec, i,  am_http_eoh);
      i = LOAD_TUPLE(spec, i, 3);
      return driver_output_term(ctx->port, spec, i);
  }
}

static int http_error_uart_drv(void* arg, const char* buf, int len)
{
  uart_ctx_t* ctx = (uart_ctx_t*) arg;
  int i = 0;
  ErlDrvTermData spec[19];

  if (ctx->option.active == UART_PASSIVE) {
    /* {uart_async,S,Ref,{ok,{http_error,Line}}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(ctx, &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_uart_async);
    i = LOAD_PORT(spec, i,  ctx->dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_error);
    i = http_load_string(ctx, spec, i, buf, len);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    return driver_send_term(ctx->port, caller, spec, i);
  }
  else {
      /* {http, S, {http_error,Line} */
      i = LOAD_ATOM(spec, i,  am_http);
      i = LOAD_PORT(spec, i,  ctx->dport);
      i = LOAD_ATOM(spec, i,  am_http_error);
      i = http_load_string(ctx, spec, i, buf, len);
      i = LOAD_TUPLE(spec, i, 2);
      i = LOAD_TUPLE(spec, i, 3);
      return driver_output_term(ctx->port, spec, i);
  }
}


static
int ssl_tls_uart_drv(void* arg, unsigned type, unsigned major, unsigned minor,
                    const char* buf, int len, const char* prefix, int plen)
{
    uart_ctx_t* ctx = (uart_ctx_t*) arg;
    int i = 0;
    ErlDrvTermData spec[28];
    ErlDrvTermData caller = ERL_DRV_NIL;
    ErlDrvBinary* bin;
    int ret;

    if ((bin = driver_alloc_binary(plen+len)) == NULL)
        return async_error(ctx, ENOMEM);
    memcpy(bin->orig_bytes+plen, buf, len);
    if (plen) {
        memcpy(bin->orig_bytes, prefix, plen);
        len += plen;
    }

    if (ctx->option.active == UART_PASSIVE) {
        /* {uart_async,S,Ref,{ok,{ssl_tls,...}}} */
        int req;
        int aid;

        if (deq_async(ctx, &aid, &caller, &req) < 0) {
            ret = -1;
            goto done;
        }
        i = LOAD_ATOM(spec, i,  am_uart_async);
        i = LOAD_PORT(spec, i,  ctx->dport);
        i = LOAD_INT(spec, i,   aid);
        i = LOAD_ATOM(spec, i,  am_ok);
    }

    /* {ssl_tls,S,ContentType,{Major,Minor},Bin} */
    i = LOAD_ATOM(spec, i,  am_ssl_tls);
    i = LOAD_PORT(spec, i,  ctx->dport);
    i = LOAD_INT(spec, i,   type);
    i = LOAD_INT(spec, i,   major);
    i = LOAD_INT(spec, i,   minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_BINARY(spec, i, bin, 0, len);
    i = LOAD_TUPLE(spec, i, 5);

    if (ctx->option.active == UART_PASSIVE) {
        i = LOAD_TUPLE(spec, i, 2);
        i = LOAD_TUPLE(spec, i, 4);
        ret = driver_send_term(ctx->port, caller, spec, i);
    }
    else {
        ret = driver_output_term(ctx->port, spec, i);
    }
done:
    driver_free_binary(bin);
    return ret;
}

static PacketCallbacks packet_callbacks =
{
    http_response_uart_drv,
    http_request_uart_drv,
    http_eoh_uart_drv,
    http_header_uart_drv,
    http_error_uart_drv,
    ssl_tls_uart_drv
};


static int http_load_string(uart_ctx_t* ctx, ErlDrvTermData* spec, int i,
			    const char* str, int len)
{
    if ((ctx->option.htype == UART_PB_HTTP_BIN) ||
	(ctx->option.htype == UART_PB_HTTPH_BIN))  {
	i = LOAD_BUF2BINARY(spec, i, str, len);
    } else {
	i = LOAD_STRING(spec, i, str, len);
    }
    return i;
}

/* setup a new async id + caller (format async_id into buf) */

static int enq_async_w_tmo(uart_ctx_t* ctx, int id, int req, 
			   unsigned timeout, ErlDrvMonitor *monitorp)
{
    uart_async_op* opp;
    int qsize = abs(ctx->op_head - ctx->op_tail);

    if (qsize == UART_MAX_MASK) {
	DEBUGF("enq(%ld): queue full", (long)ctx->port);
	return -1;
    }
    opp = &ctx->op_queue[ctx->op_head];
    opp->id = id;
    opp->caller = driver_caller(ctx->port);
    opp->req = req;
    opp->tmo.value = timeout;
    if (monitorp != NULL) {
	memcpy(&(opp->monitor),monitorp,sizeof(ErlDrvMonitor));
    }
    DEBUGF("enq(%ld): %d %ld %d", 
	(long) ctx->port, opp->id, opp->caller, opp->req);
    ctx->op_head = (ctx->op_head + 1) % UART_MAX_ASYNC;
    return 0;
}

static int enq_async(uart_ctx_t* ctx, int id, int req) 
{
    return enq_async_w_tmo(ctx,id,req,UART_INFINITY,NULL);
}

static int deq_async_w_tmo(uart_ctx_t* ctx, int* ap, ErlDrvTermData* cp, 
			   int* rp, unsigned *tp, ErlDrvMonitor *monitorp)
{
    uart_async_op* opp;
    int qsize = abs(ctx->op_head - ctx->op_tail);

    if (qsize == 0) {
	DEBUGF("deq(%ld): queue empty", (long)ctx->port);
	return -1;
    }
    opp = &ctx->op_queue[ctx->op_tail];
    *ap = opp->id;
    *cp = opp->caller;
    *rp = opp->req;
    if (tp != NULL)
	*tp = opp->tmo.value;
    if (monitorp != NULL)
	memcpy(monitorp,&(opp->monitor),sizeof(ErlDrvMonitor));
    DEBUGF("deq(%ld): %d %ld %d",
	(long)ctx->port, opp->id, opp->caller, opp->req);
    ctx->op_tail = (ctx->op_tail + 1) % UART_MAX_ASYNC;    
    return 0;
}

static int deq_async(uart_ctx_t* ctx, int* ap, ErlDrvTermData* cp, int* rp)
{
    return deq_async_w_tmo(ctx,ap,cp,rp,NULL,NULL);
}

/* send message:
**     {uart_async, Port, Ref, ok} 
*/
static int send_async_ok(ErlDrvPort port, ErlDrvTermData Port, int Ref, 
			 ErlDrvTermData recipient)
{
    ErlDrvTermData spec[2*LOAD_ATOM_CNT + LOAD_PORT_CNT + 
			LOAD_INT_CNT + LOAD_TUPLE_CNT];
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_uart_async);
    i = LOAD_PORT(spec, i, Port);
    i = LOAD_INT(spec, i, Ref);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 4);
    
    return driver_send_term(port, recipient, spec, i);
}

/* send message:
**      {uart_async, Port, Ref, {error,Reason}}
*/
static int send_async_error(ErlDrvPort port, ErlDrvTermData Port, int Ref,
			    ErlDrvTermData recipient, ErlDrvTermData Reason)
{
    ErlDrvTermData spec[3*LOAD_ATOM_CNT + LOAD_PORT_CNT + 
			LOAD_INT_CNT + 2*LOAD_TUPLE_CNT];
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_uart_async);
    i = LOAD_PORT(spec, i, Port);
    i = LOAD_INT(spec, i, Ref);
    {
	i = LOAD_ATOM(spec, i, am_error);
	i = LOAD_ATOM(spec, i, Reason);
	i = LOAD_TUPLE(spec, i, 2);
    }
    i = LOAD_TUPLE(spec, i, 4);
    DEBUGF("send_async_error %ld %ld", recipient, Reason);
    return driver_send_term(port, recipient, spec, i);
}


static int async_ok(uart_ctx_t* ctx)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(ctx, &aid, &caller, &req) < 0)
	return -1;
    return send_async_ok(ctx->port, ctx->dport, aid, caller);
}

static int async_error_am(uart_ctx_t* ctx, ErlDrvTermData reason)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(ctx, &aid, &caller, &req) < 0)
	return -1;
    return send_async_error(ctx->port, ctx->dport, aid, caller,
			    reason);
}


/* dequeue all operations */
static int async_error_am_all(uart_ctx_t* ctx, ErlDrvTermData reason)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    while (deq_async(ctx, &aid, &caller, &req) == 0) {
	send_async_error(ctx->port, ctx->dport, aid, caller,
			 reason);
    }
    return 0;
}

static int async_error(uart_ctx_t* ctx, int err)
{
    return async_error_am(ctx, error_atom(err));
}

/* send:
**   {uart_reply, S, ok} 
*/

static int uart_reply_ok(uart_ctx_t* ctx)
{
    ErlDrvTermData spec[2*LOAD_ATOM_CNT + LOAD_PORT_CNT + LOAD_TUPLE_CNT];
    ErlDrvTermData caller = ctx->caller;
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_uart_reply);
    i = LOAD_PORT(spec, i, ctx->dport);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 3);
    
    ctx->caller = 0;
    return driver_send_term(ctx->port, caller, spec, i);    
}

/* send:
**   {uart_reply, S, {error, Reason}} 
*/
static int uart_reply_error_am(uart_ctx_t* ctx, ErlDrvTermData reason)
{
    ErlDrvTermData spec[3*LOAD_ATOM_CNT + LOAD_PORT_CNT + 2*LOAD_TUPLE_CNT];
    ErlDrvTermData caller = ctx->caller;
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_uart_reply);
    i = LOAD_PORT(spec, i, ctx->dport);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, reason);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 3);
    ctx->caller = 0;
    
    DEBUGF("uart_reply_error_am %ld %ld", caller, reason);
    return driver_send_term(ctx->port, caller, spec, i);
}

/* send:
**   {uart_reply, S, {error, Reason}} 
*/
static int uart_reply_error(uart_ctx_t* ctx, int err)
{
    return uart_reply_error_am(ctx, error_atom(err));
}

/* 
** Deliver port data from buffer 
*/
static int uart_port_data(uart_ctx_t* ctx, const char* buf, int len)
{
    unsigned int hsz = ctx->option.hsz;

    DEBUGF("uart_port_data(%ld): len = %d", (long)ctx->port, len);

    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len))
	return driver_output2(ctx->port, (char*)buf, len, NULL, 0);
    else if (hsz > 0)
	return driver_output2(ctx->port, (char*)buf, hsz, (char*)buf+hsz, len-hsz);
    else
	return driver_output(ctx->port, (char*)buf, len);
}

/* 
** Deliver port data from binary (for an active mode socket)
*/
static int
uart_port_binary_data(uart_ctx_t* ctx, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = ctx->option.hsz;

    DEBUGF("uart_port_binary_data(%ld): offs=%d, len = %d", 
	(long)ctx->port, offs, len);

    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len)) 
	return driver_output2(ctx->port, bin->orig_bytes+offs, len, NULL, 0);
    else 
	return driver_output_binary(ctx->port, bin->orig_bytes+offs, hsz,
				    bin, offs+hsz, len-hsz);
}

/* 
** passive mode reply:
**        {uart_async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int uart_async_data(uart_ctx_t* ctx, const char* buf, int len)
{
    unsigned int hsz = ctx->option.hsz;
    ErlDrvTermData spec[20];
    ErlDrvTermData caller;
    int req;
    int aid;
    int i = 0;

    DEBUGF("uart_async_data(%ld): len = %d", (long)ctx->port, len);

    if (deq_async(ctx, &aid, &caller, &req) < 0)
	return -1;

    i = LOAD_ATOM(spec, i, am_uart_async);
    i = LOAD_PORT(spec, i, ctx->dport);
    i = LOAD_INT(spec, i, aid);

    i = LOAD_ATOM(spec, i, am_ok);
    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len)) {
	i = LOAD_STRING(spec, i, buf, len); /* => [H1,H2,...Hn] */ 
	i = LOAD_TUPLE(spec, i, 2);
	i = LOAD_TUPLE(spec, i, 4);
	ctx->caller = 0;
	return driver_send_term(ctx->port, caller, spec, i);
    }
    else {
	/* UART_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;
	int code;

	i = LOAD_BUF2BINARY(spec, i, buf+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, buf, hsz);
	i = LOAD_TUPLE(spec, i, 2);
	i = LOAD_TUPLE(spec, i, 4);
	ctx->caller = 0;
	code = driver_send_term(ctx->port, caller, spec, i);
	return code;
    }
}

/* 
** passive mode reply:
**        {uart_async, S, Ref, {ok, Data=[H1,...,Hsz | BinData]}}
*/
static int
uart_async_binary_data(uart_ctx_t* ctx, unsigned  int phsz,
		       ErlDrvBinary* bin, int offs, int len, void* extra)
{
    (void) extra;
    unsigned int hsz = ctx->option.hsz + phsz;
    ErlDrvTermData spec [32];
    ErlDrvTermData caller = ctx->caller;
    int aid;
    int req;
    int i = 0;

    DEBUGF("uart_async_binary_data(%ld): offs=%d, len=%d", 
	(long)ctx->port, offs, len);

    if (deq_async(ctx, &aid, &caller, &req) < 0)
	return -1;

    i = LOAD_ATOM(spec, i, am_uart_async);	/* 'uart_async' */
    i = LOAD_PORT(spec, i, ctx->dport);	/* S		*/
    i = LOAD_INT (spec, i, aid);		/* Ref		*/


    i = LOAD_ATOM(spec, i, am_ok);

    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len)) {
	/* UART_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* UART_MODE_BINARY => [H1,H2,...HSz | Binary] or [Binary]: */
	int sz = len - hsz;
	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    /* Close up the {ok, ...} or {error, ...} tuple: */
    i = LOAD_TUPLE(spec, i, 2);

    /* Close up the outer {uart_async, S, Ref, {ok|error, ...}} tuple: */
    i = LOAD_TUPLE(spec, i, 4);
    ctx->caller = 0;
    return driver_send_term(ctx->port, caller, spec, i);
}

/* 
** active mode message:
**        {uart, S, [H1,...Hsz | Data]}
*/
static int uart_message(uart_ctx_t* ctx, const char* buf, int len)
{
    unsigned int hsz = ctx->option.hsz;
    ErlDrvTermData spec[20];
    int i = 0;

    DEBUGF("uart_message(%ld): len = %d", (long)ctx->port, len);

    i = LOAD_ATOM(spec, i, am_uart);
    i = LOAD_PORT(spec, i, ctx->dport);

    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len)) {
	i = LOAD_STRING(spec, i, buf, len); /* => [H1,H2,...Hn] */ 
	i = LOAD_TUPLE(spec, i, 3);
	return driver_output_term(ctx->port, spec, i);
    }
    else {
	/* UART_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;
	int code;

	i = LOAD_BUF2BINARY(spec, i, buf+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, buf, hsz);
	i = LOAD_TUPLE(spec, i, 3);
	code = driver_output_term(ctx->port, spec, i);
	return code;
    }
}

/* 
** active mode message:
**        {uart, S, [H1,...Hsz | Data]}
*/
static int
uart_binary_message(uart_ctx_t* ctx, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = ctx->option.hsz;
    ErlDrvTermData spec[20];
    int i = 0;

    DEBUGF("uart_binary_message(%ld): len = %d", (long)ctx->port, len); 

    i = LOAD_ATOM(spec, i, am_uart);
    i = LOAD_PORT(spec, i, ctx->dport);

    if ((ctx->option.mode == UART_MODE_LIST) || ((int)hsz > len)) {
	/* UART_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* UART_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;

	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    i = LOAD_TUPLE(spec, i, 3);
    return driver_output_term(ctx->port, spec, i);
}

/*
** send:  active mode  {uart_closed, S}
*/
static int uart_closed_message(uart_ctx_t* ctx)
{
    ErlDrvTermData spec[6];
    int i = 0;

    DEBUGF("uart_closed_message(%ld):", (long)ctx->port); 
    if (!(ctx->flags & UART_F_CLOSE_SENT)) {
	ctx->flags |= UART_F_CLOSE_SENT;

	i = LOAD_ATOM(spec, i, am_uart_closed);
	i = LOAD_PORT(spec, i, ctx->dport);
	i = LOAD_TUPLE(spec, i, 2);
	return driver_output_term(ctx->port, spec, i);
    } 
    return 0;
}

/*
** send active message {uart_error, S, Error}
*/
static int uart_error_message(uart_ctx_t* ctx, int err)
{
    ErlDrvTermData spec[8];
    ErlDrvTermData am_err = error_atom(err);
    int i = 0;

    DEBUGF("uart_error_message(%ld): %d", (long)ctx->port, err); 

    i = LOAD_ATOM(spec, i, am_uart_error);
    i = LOAD_PORT(spec, i, ctx->dport);
    i = LOAD_ATOM(spec, i, am_err);
    i = LOAD_TUPLE(spec, i, 3);
    return driver_output_term(ctx->port, spec, i);
}

// scan buffer for bit 7 
static void scanbit8(uart_ctx_t* ctx, const char* buf, int len)
{
    int c;

    if (!ctx->option.bit8f || ctx->bit8) return;
    c = 0;
    while(len--) c |= *buf++;
    ctx->bit8 = ((c & 0x80) != 0);
}

/* 
**  {uart_async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int uart_reply_data(uart_ctx_t* ctx, char* buf, int len)
{
    int code;
    const char* body = buf;
    int bodylen = len;
    
    uart_packet_get_body(ctx->option.htype, &body, &bodylen);

    scanbit8(ctx, body, bodylen);

    if (ctx->option.deliver == UART_DELIVER_PORT) {
        code = uart_port_data(ctx, body, bodylen);
    }
    else if ((code=uart_packet_parse(ctx->option.htype, buf, len,
				     &ctx->http_state, &packet_callbacks,
				     ctx)) == 0) {
        /* No body parsing, return raw binary */
        if (ctx->option.active == UART_PASSIVE)
            return uart_async_data(ctx, body, bodylen);
        else
            code = uart_message(ctx, body, bodylen);
    }

    if (code < 0)
	return code;
    if (ctx->option.active == UART_ONCE)
	ctx->option.active = UART_PASSIVE;
    return code;
}

static int
uart_reply_binary_data(uart_ctx_t* ctx, ErlDrvBinary* bin, int offs, int len)
{
    int code;
    const char* buf = bin->orig_bytes + offs;
    const char* body = buf;
    int bodylen = len;

    uart_packet_get_body(ctx->option.htype, &body, &bodylen);
    offs = body - bin->orig_bytes; /* body offset now */

    scanbit8(ctx, body, bodylen);

    if (ctx->option.deliver == UART_DELIVER_PORT)
        code = uart_port_binary_data(ctx, bin, offs, bodylen);
    else if ((code=uart_packet_parse(ctx->option.htype, buf, len,
				     &ctx->http_state,
				     &packet_callbacks,ctx)) == 0) {
        /* No body parsing, return raw data */
        if (ctx->option.active == UART_PASSIVE)
            return uart_async_binary_data(ctx, 0, bin, offs, bodylen, NULL);
        else
            code = uart_binary_message(ctx, bin, offs, bodylen);
    }
    if (code < 0)
	return code;
    if (ctx->option.active == UART_ONCE)
	ctx->option.active = UART_PASSIVE;
    return code;
}


/* Move data so that ptr_start point at buf->orig_bytes */
static void rx_restart_input(uart_ctx_t* ctx)
{
    if (ctx->i_ptr_start != ctx->i_buf->orig_bytes) {
	int n = ctx->i_ptr - ctx->i_ptr_start;
	DEBUGF("rx_restart_input: move %d bytes", n);
	memmove(ctx->i_buf->orig_bytes, ctx->i_ptr_start, n);
	ctx->i_ptr_start = ctx->i_buf->orig_bytes;
	ctx->i_ptr = ctx->i_ptr_start + n;
    }
}

/* push data into i_buf  */
static int uart_push_buffer(uart_ctx_t* ctx, char* buf, int len)
{
    ErlDrvBinary* bin;

    if (ctx->i_buf == NULL) {
	bin = alloc_buffer(len);
	memcpy(bin->orig_bytes, buf, len);
	ctx->i_buf = bin;
	ctx->i_bufsz = len;
	ctx->i_ptr_start = ctx->i_buf->orig_bytes;
	ctx->i_ptr = ctx->i_ptr_start + len;
    }
    else {
	char* start =  ctx->i_buf->orig_bytes;
	int sz_before = ctx->i_ptr_start - start;
	int sz_filled = ctx->i_ptr - ctx->i_ptr_start;
	
	if (len <= sz_before) {
	    memcpy(ctx->i_ptr_start - len, buf, len);
	    ctx->i_ptr_start -= len;
	}
	else {
	    bin = alloc_buffer(ctx->i_bufsz+len);
	    memcpy(bin->orig_bytes, buf, len);
	    memcpy(bin->orig_bytes+len, ctx->i_ptr_start, sz_filled);
	    free_buffer(ctx->i_buf);
	    ctx->i_bufsz += len;
	    ctx->i_buf = bin;
	    ctx->i_ptr_start = bin->orig_bytes;
	    ctx->i_ptr = ctx->i_ptr_start + sz_filled + len;
	}
    }
    ctx->i_remain = 0;	
    return 0;
}

// clear CURRENT input buffer
static void clear_input(uart_ctx_t* ctx)
{
    if (ctx->i_buf != NULL)
	free_buffer(ctx->i_buf);
    ctx->i_buf       = NULL;
    ctx->i_remain    = 0;
    ctx->i_ptr       = NULL;
    ctx->i_ptr_start = NULL;
    ctx->i_bufsz     = 0;
}

// clear QUEUED output
static void clear_output(uart_ctx_t* ctx)
{
    ErlDrvPort ix  = ctx->port;
    ErlDrvSizeT qsz = driver_sizeq(ix);

    driver_deq(ix, qsz);
    // send_empty_out_q_msgs(ctx);
}



/*
** Set new size on buffer, used when packet size is determined
** and the buffer is to small.
** buffer must have a size of at least len bytes (counting from ptr_start!)
*/
static int rx_expand_buffer(uart_ctx_t* ctx, int len)
{
    ErlDrvBinary* bin;
    int offs1;
    int offs2;
    int used = ctx->i_ptr_start - ctx->i_buf->orig_bytes;
    int ulen = used + len;

    if (ctx->i_bufsz >= ulen) /* packet will fit */
	return 0;
    else if (ctx->i_buf->orig_size >= ulen) { /* buffer is large enough */
	ctx->i_bufsz = ulen;  /* set "virtual" size */
	return 0;
    }

    DEBUGF("rx_expand_buffer(%ld): s=%ld, from %ld to %d",
	(long)ctx->port, (long)ctx->handle.data, ctx->i_buf->orig_size, ulen);

    offs1 = ctx->i_ptr_start - ctx->i_buf->orig_bytes;
    offs2 = ctx->i_ptr - ctx->i_ptr_start;

    if ((bin = realloc_buffer(ctx->i_buf, ulen)) == NULL)
	return -1;

    ctx->i_buf = bin;
    ctx->i_ptr_start = bin->orig_bytes + offs1;
    ctx->i_ptr       = ctx->i_ptr_start + offs2;
    ctx->i_bufsz     = ulen;
    return 0;
}

// The modem has closed, cleanup and send event
static int uart_recv_closed(uart_ctx_t* ctx)
{
#ifdef DEBUG
    long port = (long) ctx->port; /* Used after driver_exit() */
#endif
    DEBUGF("uart_recv_closed(%ld): s=%ld", port, (long)ctx->handle.data);
    if (ctx->flags & UART_F_BUSY) {
	/* A send is blocked */
	ctx->caller = ctx->busy_caller;
	clear_output(ctx);
	if (ctx->busy_on_send) {
	    DEBUGF("uart_recv_closed(%ld): cancel_timer", ctx->port);
	    driver_cancel_timer(ctx->port);
	    ctx->busy_on_send = 0;
	    DEBUGF("uart_recv_closed(%ld): busy on send", ctx->port);
	}
	ctx->flags &= ~UART_F_BUSY;
	set_busy_port(ctx->port, 0);
	uart_reply_error_am(ctx, am_closed);
	DEBUGF("uart_recv_closed(%ld): busy reply 'closed'", port);
    }
    if (!ctx->option.active) {
	DEBUGF("uart_recv_closed(%ld): cancel_timer", ctx->port);
	driver_cancel_timer(ctx->port);
	clear_input(ctx);
	if (ctx->option.exitf) {
	    clear_output(ctx);
	    close_device(ctx);
	} else {
	    close_read_device(ctx);
	}
	async_error_am_all(ctx, am_closed);
	/* next time EXBADSEQ will be delivered  */
	DEBUGF("tcp_recv_closed(%ld): passive reply all 'closed'", port);
    } else {
	clear_input(ctx);
	uart_closed_message(ctx);
	if (ctx->option.exitf) {
	    driver_exit(ctx->port, 0);
	} else {
	    close_read_device(ctx);
	}
	DEBUGF("tcp_recv_closed(%ld): active close\r\n", port);
    }
    DEBUGF("tcp_recv_closed(%ld): done\r\n", port);
    return -1;
}

/* We have a read error determine the action */
static int rx_recv_error(uart_ctx_t* ctx, int err)
{
    if (err != EAGAIN) {
	if (ctx->flags & UART_F_BUSY) {
	    /* A send is blocked */
	    ctx->caller = ctx->busy_caller;
	    clear_output(ctx);
	    if (ctx->busy_on_send) {
		DEBUGF("uart_recv_error(%ld): cancel_timer", ctx->port);
		driver_cancel_timer(ctx->port);
		ctx->busy_on_send = 0;
	    }
	    ctx->flags &= ~UART_F_BUSY;
	    set_busy_port(ctx->port, 0);
	    uart_reply_error_am(ctx, am_closed);
	}
	if (!ctx->option.active) {
	    DEBUGF("uart_recv_error(%ld): cancel_timer", ctx->port);
	    driver_cancel_timer(ctx->port);
	    clear_input(ctx);
	    if (ctx->option.exitf) {
		close_device(ctx);
	    } else {
		close_read_device(ctx);
	    }
	    async_error_am_all(ctx, error_atom(err));
	} else {
	    clear_input(ctx);
	    uart_error_message(ctx, err); // first error
	    uart_closed_message(ctx);     /* then closed */
	    if (ctx->option.exitf)
		driver_exit(ctx->port, err);
	    else
		close_device(ctx);
	}
	return -1;
    }
    return 0;
}

/*
** Calculate number of bytes that remain to read before deliver
** Assume buf, ptr_start, ptr has been setup
**
** return  > 0 if more to read
**         = 0 if holding complete packet
**         < 0 on error
**
** if return value == 0 then *len will hold the length of the first packet
**    return value > 0 then if *len == 0 then value means upperbound
**                             *len > 0  then value means exact
**
*/
static int rx_remain(uart_ctx_t* ctx, int* len)
{
    char* ptr = ctx->i_ptr_start;
    int nfill = (ctx->i_ptr - ctx->i_buf->orig_bytes);  // filled
    int nsz   = ctx->i_bufsz - nfill;                   // remain
    int n = ctx->i_ptr - ptr;  // number of bytes read
    int tlen;

    DEBUGF("rx_remain(%ld): s=%ld, htype=%08x, n=%d, nfill=%d nsz=%d", 
	   (long)ctx->port, (long)ctx->handle.data, 
	   ctx->option.htype, n, nfill, nsz);

    tlen = uart_packet_get_length(ctx->option.htype, ptr, n, 
				  ctx->option.psize, ctx->i_bufsz,
				  &ctx->http_state);
    if (tlen > 0) {
        if (tlen <= n) { // got a packet 
            *len = tlen;
            DEBUGF(" => nothing remain packet=%d", tlen);
            return 0;
        }
        else { // need known more
            if (rx_expand_buffer(ctx, tlen) < 0)
                return -1;
            *len = tlen - n;
            DEBUGF(" => remain=%d", *len);
            return *len;
        }
    }
    else if (tlen == 0) { // need unknown more
        *len = 0;
        if (nsz == 0) {
            if (nfill == n) {
                if ((ctx->option.psize != 0) && 
		    ((int)ctx->option.psize > nfill)) {
                    if (rx_expand_buffer(ctx, ctx->option.psize) < 0)
                        return -1;
                    return ctx->option.psize;
                }
                else
                    goto error;
            }
            DEBUGF(" => restart more=%d", nfill - n);
            return nfill - n;
        }
        else {
            DEBUGF(" => more=%d", nsz);
            return nsz;
        }	    
    }

error:
    DEBUGF(" => packet error");
    return -1;
}

/*
** Deliver all packets ready 
** if len == 0 then check start with a check for ready packet
*/
static int uart_deliver(uart_ctx_t* ctx, int len)
{
    int count = 0;
    int n;

    /* Poll for ready packet */
    if (len == 0) {
	/* empty buffer or waiting for more input */
	if ((ctx->i_buf == NULL) || (ctx->i_remain > 0))
	    return count;
	if ((n = rx_remain(ctx, &len)) != 0) {
	    if (n < 0) /* packet error */
		return n;
	    if (len > 0)  /* more data pending */
		ctx->i_remain = len;
	    return count;
	}
    }

    while (len > 0) {
	int code;

	// uart_input_count(ctx, len);

	/* deliver binary? */
	if (len*4 >= ctx->i_buf->orig_size*3) { /* >=75% */
	    code = uart_reply_binary_data(ctx, ctx->i_buf,
					  (ctx->i_ptr_start -
					   ctx->i_buf->orig_bytes),
					  len);
	    if (code < 0)
		return code;

	    /* something after? */
	    if (ctx->i_ptr_start + len == ctx->i_ptr) { /* no */
		clear_input(ctx);
	    }
	    else { /* move trail to beginning of a new buffer */
		ErlDrvBinary* bin = alloc_buffer(ctx->i_bufsz);
		char* ptr_end = ctx->i_ptr_start + len;
		int sz = ctx->i_ptr - ptr_end;

		memcpy(bin->orig_bytes, ptr_end, sz);
		free_buffer(ctx->i_buf);
		ctx->i_buf = bin;
		ctx->i_ptr_start = ctx->i_buf->orig_bytes;
		ctx->i_ptr = ctx->i_ptr_start + sz;
		ctx->i_remain = 0;
	    }
	}
	else {
	    code = uart_reply_data(ctx, ctx->i_ptr_start, len);
	    /* XXX The buffer gets thrown away on error  (code < 0)    */
	    /* Windows needs workaround for this in uart_uart_event...  */
	    if (code < 0)
		return code;
	    ctx->i_ptr_start += len;
	    if (ctx->i_ptr_start == ctx->i_ptr)
		clear_input(ctx);
	    else
		ctx->i_remain = 0;
	}

	count++;
	len = 0;

	if (!ctx->option.active) {
	    if (!ctx->busy_on_send) {
		DEBUGF("uart_deliver(%ld): cancel_timer", ctx->port);
		driver_cancel_timer(ctx->port);
	    }
	    driver_select(ctx->port,DRV_EVENT(ctx->handle.data),ERL_DRV_READ,0);
	    if (ctx->i_buf != NULL)
		rx_restart_input(ctx);
	}
	else if (ctx->i_buf != NULL) {
	    if ((n = rx_remain(ctx, &len)) != 0) {
		if (n < 0) /* packet error */
		    return n;
		rx_restart_input(ctx);
		if (len > 0)
		    ctx->i_remain = len;
		len = 0;
	    }
	}
    }
    return count;
}


static int uart_recv(uart_ctx_t* ctx, int request_len)
{
    int n;
    int len;
    int nread;

    if (ctx->i_buf == NULL) {  /* allocte a read buffer */
	int sz = (request_len > 0) ? request_len : (int) ctx->option.bsize;

	if ((ctx->i_buf = alloc_buffer(sz)) == NULL)
	    return -1;
	ctx->i_bufsz = sz;
	ctx->i_ptr_start = ctx->i_buf->orig_bytes;
	ctx->i_ptr = ctx->i_ptr_start;
	nread = sz;
	if (request_len > 0)
	    ctx->i_remain = request_len;
	else
	    ctx->i_remain = 0;
    }
    else if (request_len > 0) { /* we have a data in buffer and a request */
	n = ctx->i_ptr - ctx->i_ptr_start;
	if (n >= request_len)
	    return uart_deliver(ctx, request_len);
	else if (rx_expand_buffer(ctx, request_len) < 0)
	    return rx_recv_error(ctx, ENOMEM);
	else
	    ctx->i_remain = nread = request_len - n;
    }
    else if (ctx->i_remain == 0) {  /* poll remain from buffer data */
	if ((nread = rx_remain(ctx, &len)) < 0)
	    return rx_recv_error(ctx, EMSGSIZE);
	else if (nread == 0)
	    return uart_deliver(ctx, len);
	else if (len > 0)
	    ctx->i_remain = len;  /* set remain */
    }
    else  /* remain already set use it */
	nread = ctx->i_remain;
    
    DEBUGF("uart_recv(%ld): s=%ld about to read %d bytes...",
	(long)ctx->port, (long)ctx->handle.data, nread);

    n = uart_read(&ctx->handle, ctx->i_ptr, nread);

    if (n < 0) {
	int err = uart_errno();
	if (err == ECONNRESET) {
	    DEBUGF(" => detected close");
	    return uart_recv_closed(ctx);
	}
	if (err == EAGAIN) {
	    DEBUGF(" => would block");
	    return 0;
	}
	else {
	    DEBUGF(" => error: %d", err);
	    return rx_recv_error(ctx, err);
	}
    }
    else if (n == 0) {
	DEBUGF("  => detected close");
	return uart_recv_closed(ctx);
    }

    DEBUGF(" => got %d bytes", n);
    ctx->i_ptr += n;
    if (ctx->i_remain > 0) {
	ctx->i_remain -= n;
	if (ctx->i_remain == 0)
	    return uart_deliver(ctx, ctx->i_ptr - ctx->i_ptr_start);
    }
    else {
	if ((nread = rx_remain(ctx, &len)) < 0)
	    return rx_recv_error(ctx, EMSGSIZE);
	else if (nread == 0)
	    return uart_deliver(ctx, len);
	else if (len > 0)
	    ctx->i_remain = len;  /* set remain */
    }
    return 0;
}

static int tx_send_error(uart_ctx_t* ctx, int err)
{
    (void) err;
    /*
     * If the port is busy, we must do some clean-up before proceeding.
     */
    if (ctx->flags & UART_F_BUSY) {
	ctx->caller = ctx->busy_caller;
	if (ctx->busy_on_send) {
	    DEBUGF("tx_send_error(%ld): cancel_timer", ctx->port);
	    driver_cancel_timer(ctx->port);
	    ctx->busy_on_send = 0;	
	}
	ctx->flags &= ~UART_F_BUSY;
	set_busy_port(ctx->port, 0);
    }

    /*
     * We used to handle "expected errors" differently from unexpected ones.
     * Now we handle all errors in the same way. We just have to distinguish
     * between passive and active sockets.
     */
    DEBUGF("tx_send_error(%ld)", (long)ctx->port);
    if (ctx->option.active) {
	uart_closed_message(ctx);
	uart_reply_error_am(ctx, am_closed);
	if (ctx->option.exitf)
	    driver_exit(ctx->port, 0);
	else
	    close_device(ctx);
    }
    else {
	clear_output(ctx);
	clear_input(ctx);
	uart_reply_error_am(ctx, am_closed);
	close_device(ctx);

	if (ctx->caller) {
	    uart_reply_error_am(ctx, am_closed);
	}
	else {
	    /* No blocking send op to reply to right now.
	     * If next op is a send, make sure it returns {error,closed}
	     * rather than {error,enotconn}.
	     */
	    ctx->flags |= UART_F_DELAYED_CLOSE_SEND;
	}

	/*
	 * Make sure that the next receive operation gets an {error,closed}
	 * result rather than {error,enotconn}. That means that the caller
	 * can safely ignore errors in the send operations and handle them
	 * in the receive operation.
	 */
	ctx->flags |= UART_F_DELAYED_CLOSE_RECV;
    }
    return -1;
}

static int tx_again(uart_ctx_t* ctx)
{
    ErlDrvPort ix = ctx->port;

    for (;;) {
	int vsize;
	ssize_t n;
	int i;
	SysIOVec* iov;

	if ((iov = driver_peekq(ix, &vsize)) == NULL) {
	    // select(FD(ctx), FD_WRITE, 0);
	    goto done;
	}

	for (i = 0; i < vsize; i++) {
	    int r;
	    r = uart_write(&ctx->handle, iov[i].iov_base, iov[i].iov_len);
	    if (r < 0) break;
	    n += r;
	    if (r < (int)iov[i].iov_len)
		break;
	}

	if ((int)driver_deq(ix, n) <= ctx->option.low) {
	    if (ctx->flags & UART_F_BUSY) {
		ctx->caller = ctx->busy_caller;
		ctx->flags &= ~UART_F_BUSY;
		set_busy_port(ctx->port, 0);
		// if we have a timer then cancel and send ok to client
		if (ctx->busy_on_send) {
		    DEBUGF("tx_again(%ld): cancel_timer", ctx->port);
		    driver_cancel_timer(ctx->port);
		    ctx->busy_on_send = 0;
		}
		uart_reply_ok(ctx);
	    }
	}
    }
done:
    return 0;
}

//
// Send or enqueue for TX
//
static int uart_send(uart_ctx_t* ctx, char* ptr, int len)
{
    int sz;
    char buf[8];
    int h_len;
    int n;
    ErlDrvPort ix = ctx->port;
    SysIOVec iov[2];
#define UINT8(x) ((uint8_t) ((x) & 0xff))

    DEBUGF("uart_send: length = %d", len);

    if ((ctx->option.htype & UART_PB_TYPE_MASK) == UART_PB_N) {
	uint64_t pl = (uint64_t)len;
	DEBUGF("uart_send: pl = %lld", pl);
	h_len = (ctx->option.htype & UART_PB_BYTES_MASK) >> 8;
	if (ctx->option.htype & UART_PB_LITTLE_ENDIAN) {
	    uint8_t* ptr = (uint8_t*) buf;
	    switch(h_len) {
	    case 8: *ptr++ = UINT8(pl); pl >>= 8;
	    case 7: *ptr++ = UINT8(pl); pl >>= 8;
	    case 6: *ptr++ = UINT8(pl); pl >>= 8;
	    case 5: *ptr++ = UINT8(pl); pl >>= 8;
	    case 4: *ptr++ = UINT8(pl); pl >>= 8;
	    case 3: *ptr++ = UINT8(pl); pl >>= 8;
	    case 2: *ptr++ = UINT8(pl); pl >>= 8;
	    case 1: *ptr = UINT8(pl); break;
	    default: return -1;
	    }
	}
	else {
	    uint8_t* ptr = (uint8_t*)buf + h_len;
	    switch(h_len) {
	    case 8: *--ptr = UINT8(pl); pl >>= 8;
	    case 7: *--ptr = UINT8(pl); pl >>= 8;
	    case 6: *--ptr = UINT8(pl); pl >>= 8;
	    case 5: *--ptr = UINT8(pl); pl >>= 8;
	    case 4: *--ptr = UINT8(pl); pl >>= 8;
	    case 3: *--ptr = UINT8(pl); pl >>= 8;
	    case 2: *--ptr = UINT8(pl); pl >>= 8;
	    case 1: *--ptr = UINT8(pl); break;
	    default: return -1;
	    }
	}
#ifdef DEBUG
	{
	    char xbuf[32];
	    DEBUGF("uart_send: hdr=%s", 
		   format_hex((uint8_t*)buf, h_len, xbuf,sizeof(xbuf)));
	}
#endif
    }
    else {
	if (len == 0)
	    return 0;
	h_len = 0;
    }

    if ((sz = driver_sizeq(ix)) > 0) {
	DEBUGF("uart_send: enqueue = %d", h_len + len);
	if (h_len > 0)
	    driver_enq(ix, buf, h_len);
	driver_enq(ix, ptr, len);
	if (sz+h_len+len >= ctx->option.high) {
	    ctx->flags |= UART_F_BUSY;  /* mark for low-watermark */
	    ctx->busy_caller = ctx->caller;
	    set_busy_port(ctx->port, 1);
	    if (ctx->option.send_timeout != (int)UART_INFINITY) {
		ctx->busy_on_send = 1;
		driver_set_timer(ctx->port, ctx->option.send_timeout);
	    }
	    return 1;
	}
    }
    else {
	iov[0].iov_base = buf;
	iov[0].iov_len  = h_len;
	iov[1].iov_base = ptr;
	iov[1].iov_len  = len;

	n = 0;

	if (!ctx->option.delay_send) {
	    int i;
	    for (i = 0; i < 2; i++) {
		int r;
		r = uart_write(&ctx->handle, iov[i].iov_base, iov[i].iov_len);
		if (r < 0) {
		    int err = uart_errno();
		    if ((err != EAGAIN) && (err != EINTR)) {
			DEBUGF("uart_tx(%ld): s=%ld, sock_send errno = %ld",
			    (long)ctx->port,(long)ctx->handle.data, err);
			return tx_send_error(ctx, err);
		    }
		    r = 0;
		}
		n += r;
		if (r < (int)iov[i].iov_len)
		    break;
	    }
	}

	if (n == len+h_len) {
	    DEBUGF("uart_send: sent=%d, enque=%d", n, 0);
	    return 0;
	}

	if (n < h_len) {
	    driver_enq(ix, buf+n, h_len-n);
	    driver_enq(ix, ptr, len);
	    DEBUGF("uart_send: sent=%d, enque=%d", n, len + (h_len-n));
	}
	else {
	    DEBUGF("uart_send: sent=%d, enque=%d", n, len - (n-h_len));
	    n -= h_len;
	    driver_enq(ix, ptr+n, len-n);

	}
	driver_select(ctx->port, DRV_EVENT(ctx->handle.data), ERL_DRV_WRITE, 1);
    }
    return 0;
}

// setup global object area
// load atoms etc.

static int uart_drv_init(void)
{
    DEBUGF("uart_driver_init");
    if (erl_drv_tsd_key_create("uart_buffer_stack_key", &buffer_stack_key) != 0)
	goto error;
    INIT_ATOM(ok);
    INIT_ATOM(uart);
    INIT_ATOM(error);
    INIT_ATOM(uart_async);
    INIT_ATOM(uart_reply);
    INIT_ATOM(timeout);
    INIT_ATOM(closed);
    INIT_ATOM(uart_closed);
    INIT_ATOM(uart_error);
    INIT_ATOM(empty_out_q);
    INIT_ATOM(ssl_tls);
    INIT_ATOM(http_eoh);
    INIT_ATOM(http_header);
    INIT_ATOM(http_request);
    INIT_ATOM(http_response);
    INIT_ATOM(http_error);
    INIT_ATOM(abs_path);
    INIT_ATOM(absoluteURI);
    am_star = driver_mk_atom("*");
    INIT_ATOM(undefined);
    INIT_ATOM(http);
    INIT_ATOM(https);
    INIT_ATOM(scheme);


    uart_packet_parser_init();
    return 0;
 error:
    return -1;
}

// clean up global settings
static void uart_drv_finish(void)
{
    // cleanup global stuff!
}

static ErlDrvData uart_drv_start(ErlDrvPort port, char* command)
{
    uart_ctx_t* ctx;
    (void) command;

    DEBUGF("uart_drv_start");

    if ((ctx = (uart_ctx_t*) driver_alloc(sizeof(uart_ctx_t))) != NULL) {
	memset(ctx, 0, sizeof(uart_ctx_t));
	uart_init(&ctx->handle);

	ctx->port  = port;
	ctx->dport = driver_mk_port(port);
	ctx->busy_on_send = 0;           // busy port while sending data
	ctx->option.bsize = UART_DEF_BUFFER; 

	ctx->option.high = UART_HIGH_WATERMARK;
	ctx->option.low  = UART_LOW_WATERMARK;
	ctx->option.send_timeout = UART_INFINITY;
	ctx->option.send_timeout_close = 0;

	ctx->option.hsz = 0;                     // list header size
	ctx->option.htype = UART_PB_RAW;         // default packet type
	ctx->option.psize = 0;                   // no size check
	ctx->option.mode    = UART_MODE_LIST;    // list mode
	ctx->option.deliver = UART_DELIVER_TERM; // standard term format
	ctx->option.active  = UART_PASSIVE;      // start passive

	ctx->option.exitf   = 0;
	ctx->option.bit8f   = 0;
	ctx->bit8           = 0;

	ctx->op_head = 0;
	ctx->op_tail = 0;

	set_default_state(&ctx->state);
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
	return (ErlDrvData) ctx;
    }
    return ERL_DRV_ERROR_ERRNO;    
}

static void uart_drv_stop(ErlDrvData d)
{
    uart_ctx_t* ctx = (uart_ctx_t*) d;

    async_error_am_all(ctx, am_closed);

    // free input buffer & output buffer
    if (ctx->i_buf != NULL)
	release_buffer(ctx->i_buf);
    ctx->i_buf = NULL;
    DEBUGF("uart_drv_stop(%ld)", (long)ctx->port);
    close_device(ctx);
    driver_free(ctx);
}

static ErlDrvSSizeT uart_drv_ctl(ErlDrvData d, 
				 unsigned int cmd, char* buf, ErlDrvSizeT len,
				 char** rbuf, ErlDrvSizeT rsize)
{
    uart_ctx_t* ctx = (uart_ctx_t*) d;

    DEBUGF("uart_drv_ctl: cmd=%u, input-len=%d", cmd, len);

    switch(cmd) {
    case UART_CMD_OPEN:
	close_device(ctx);
	if (open_device(ctx) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);

    case UART_CMD_HANGUP:
	if (uart_hangup(&ctx->handle) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);

    case UART_CMD_CLOSE:
	if (close_device(ctx) < 0) 
	    return ctl_error(uart_errno(), rbuf, rsize);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);

    case UART_CMD_XON:
	if (uart_send_xon(&ctx->handle) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);

    case UART_CMD_XOFF:
	if (uart_send_xoff(&ctx->handle) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);

    case UART_CMD_BREAK: {
	int duration;
	if (len < 4) 
	    return ctl_error(EINVAL, rbuf, rsize);
	duration = get_uint32((unsigned char*)buf);
	if (uart_send_break(&ctx->handle, duration) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);
    }	    

    case UART_CMD_SETOPTS:
	switch(set_opts(ctx, buf, len)) {
	case -1:
	    return ctl_error(EINVAL, rbuf, rsize);
	case 0:
	    return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);
	default:  // active changed => try devliver
	    (void) uart_deliver(ctx, 0);
	    return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);
	}

    case UART_CMD_GETOPTS: {
	ErlDrvSSizeT rlen;
	if ((rlen = get_opts(ctx, buf, len, rbuf, rsize)) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	DEBUGF("uart_drv_ctl: UART_CMD_GETOPTS: rlen = %ld", rlen);
	return rlen;
    }

    case UART_CMD_SENDCHAR:
	if (len < 1)
	    return ctl_error(EINVAL, rbuf, rsize);
	uart_send(ctx, &buf[0], 1);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);


    case UART_CMD_SEND: {
	if (len < 1) 
	    return ctl_error(EINVAL, rbuf, rsize);
	uart_send(ctx, buf, len);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);
    }

    case UART_CMD_GET_MODEM: {
	char resp[sizeof(uint16_t)];
	uart_modem_state_t state;
	if (uart_get_modem_state(&ctx->handle, &state) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	put_uint16((unsigned char*)resp, (uint16_t) state);
	return ctl_reply(UART_OK, resp, sizeof(resp), rbuf, rsize);
    }

    case UART_CMD_SET_MODEM: {
	uart_modem_state_t state;
	if (len < 4)
	    return ctl_error(EINVAL, rbuf, rsize);
	state = get_uint32((unsigned char*)buf);
	if (uart_set_modem_state(&ctx->handle, state, 1) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);
    }

    case UART_CMD_CLR_MODEM: {
	uart_modem_state_t state;
	if (len < 4)
	    return ctl_error(EINVAL, rbuf, rsize);
	state = get_uint32((unsigned char*)buf);
	if (uart_set_modem_state(&ctx->handle, state, 0) < 0)
	    return ctl_error(uart_errno(), rbuf, rsize);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);
    }

    case UART_CMD_UNRECV:
	DEBUGF("uart_ctl(%ld): UNRECV\r\n", (long)ctx->port); 
	if (!(ctx->flags & UART_F_OPEN))
   	    return ctl_error(ENOTCONN, rbuf, rsize);
	uart_push_buffer(ctx, buf, len);
	if (ctx->option.active)  // FIXME: may work in all active modes ?
	    uart_deliver(ctx, 0);
	return ctl_reply(UART_OK, NULL, 0, rbuf, rsize);

    case UART_CMD_RECV: { // Timeout(4),Length(4)
	char resp[sizeof(uint16_t)];
	uint32_t timeout;
	uint32_t n;
	uint16_t id;

	if (len < 8)
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_uint32((unsigned char*)buf);
	n = get_uint32((unsigned char*)buf+4);

	DEBUGF("uart_drv_ctl(%ld): RECV timeout=%d, n=%d\r\n", 
	    (long)ctx->port, timeout, n);

	if (ctx->option.active)
	    return ctl_error(EINPROGRESS, rbuf, rsize);

	if (!(ctx->flags & UART_F_OPEN)) {
	    if (ctx->flags & UART_F_DELAYED_CLOSE_RECV) {
		ctx->flags &= ~(UART_F_DELAYED_CLOSE_RECV|
				UART_F_DELAYED_CLOSE_SEND);
		return ctl_reply(UART_ERROR, "closed", 6, rbuf, rsize);
	    }
	    return ctl_error(ENOTCONN, rbuf, rsize);
	}
	if ((ctx->option.htype != UART_PB_RAW) && (n != 0))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (n > UART_MAX_PACKET_SIZE)
	    return ctl_error(ENOMEM, rbuf, rsize);
	id = NEW_ASYNC_ID();
	if (enq_async(ctx, id, UART_CMD_RECV) < 0)
	    return ctl_error(EALREADY, rbuf, rsize);
	if (uart_recv(ctx, n) == 0) {
	    if (timeout == 0)
		async_error_am(ctx, am_timeout);
	    else {
		if (timeout != UART_INFINITY)
		    driver_set_timer(ctx->port, timeout);
		driver_select(ctx->port,DRV_EVENT(ctx->handle.data),
			      ERL_DRV_READ,1);
	    }
	}
	put_uint16((unsigned char*)resp, (uint16_t) id);
	return ctl_reply(UART_OK, resp, sizeof(resp), rbuf, rsize);
    }

    default:
	DEBUGF("uart_drv_ctl: unknown command %d", cmd);
	return ctl_error(EINVAL, rbuf, rsize);
    }
}

static void uart_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    uart_ctx_t*   ctx = (uart_ctx_t*) d;
    uart_send(ctx, buf, len);
}

static void uart_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    (void) e;
    (void) uart_recv((uart_ctx_t*) d, 0);
}

static void uart_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    uart_ctx_t* ctx = (uart_ctx_t*) d;
#ifdef __WIN32__
    DWORD n;
    // This code handle OVERLAPPED io output event 
    DEBUGF("uart_ready_output: qsize=%d", driver_sizeq(ctx->port));
    ctx->o_pending = 0;
    driver_select(ctx->port, DRV_EVENT(ctx->out.hEvent),ERL_DRV_READ,0);
    if (!GetOverlappedResult(ctx->com, &ctx->out, &n, FALSE)) {
	/* Output error */
	return;
    }
    /* Dequeue ? */
#else
    (void) e;
#endif
    tx_again(ctx);
}

// operation timed out
static void uart_drv_timeout(ErlDrvData d)
{
    uart_ctx_t*   ctx = (uart_ctx_t*) d;

    DEBUGF("uart_drv_timeout(%ld) s=%d", (long)ctx->port, (long)ctx->handle.data);
    if (ctx->busy_on_send) {
	ctx->caller = ctx->busy_caller;
	ctx->flags &= ~UART_F_BUSY;
	ctx->busy_on_send = 0;
	set_busy_port(ctx->port, 0);
	uart_reply_error_am(ctx, am_timeout);
	if (ctx->option.send_timeout_close) {
	    close_device(ctx);
	}
    }
    else {
	/* assume recv timeout */
	driver_select(ctx->port,DRV_EVENT(ctx->handle.data),ERL_DRV_READ,0);
	ctx->i_remain = 0;
	async_error_am(ctx, am_timeout);
    }
}


static void uart_drv_stop_select(ErlDrvEvent event, void* ctx)
{
    (void) ctx;

    DEBUGF("uart_drv_stop_select s=%d", (long)event);
#ifdef __WIN2__
#error "fixme"
#else
    close((int)(long)event);
#endif
}


DRIVER_INIT(uart_drv)
{
    ErlDrvEntry* ptr = &uart_drv_entry;

    DEBUGF("driver_init");

    ptr->init  = uart_drv_init;
    ptr->start = uart_drv_start;
    ptr->stop  = uart_drv_stop;
    ptr->output = uart_drv_output;
    ptr->ready_input  = uart_drv_ready_input;
    ptr->ready_output = uart_drv_ready_output;
    ptr->finish = uart_drv_finish;
    ptr->driver_name = "uart_drv";
    ptr->control = uart_drv_ctl;
    ptr->timeout = uart_drv_timeout;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = uart_drv_stop_select;
    return ptr;
}
