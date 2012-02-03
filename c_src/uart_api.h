//
// Serial line interface
//

#ifndef __UART_API__
#define __UART_API__

#include <stdint.h>
// This is the backend API - this make it possible to have alternate
// implementations in the same binary - like FTDI and Linux for example.
//
typedef struct {
    int ibaud;       // input baud rate (9600)
    int obaud;       // output baud rate (9600)
    int parity;      // parity (0)
    int stopb;       // stop bits (1)
    int csize;       // characters size  (8)
    int bufsz;       // 1
    int buftm;       // 1
    int xonchar;     // 0
    int xoffchar;    // 0
    int eolchar;     // 0
    int eol2char;    // 0
    int swflow;      // 0
    int hwflow;      // 0
} uart_com_state_t;

#define UART_MODEM_DTR  0x0002
#define UART_MODEM_RTS  0x0004
#define UART_MODEM_CTS  0x0008
#define UART_MODEM_DCD  0x0010
#define UART_MODEM_RI   0x0020
#define UART_MODEM_DSR  0x0040

typedef uint16_t uart_modem_state_t;

typedef struct {
    // device
    int (*close)(void* hndl);
    int (*read)(void* hndl, void* buf, size_t nbytes);
    int (*write)(void* hndl, void* buf, size_t nbytes);
    // comm parameter
    int (*get_com_state)(void* hndl, uart_com_state_t* state);
    int (*set_com_state)(void* hndl, uart_com_state_t* state);
    // modem parameter
    int (*get_modem_state)(void* hndl, uart_modem_state_t* state);
    int (*set_modem_state)(void* hndl, uart_modem_state_t state, int on);
    // actions
    int (*send_break)(void* hndl, int duration);
    int (*send_xon)(void* hndl);
    int (*send_xoff)(void* hndl);
    int (*hangup)(void* hndl);
} uart_api_t;

#define UART_API_NAME(pfx,name) pfx ## _ ## name

#define UART_API_MAKE(pfx) \
    { .close = UART_API_NAME(pfx, close),				\
      .read = UART_API_NAME(pfx, read),				\
      .write = UART_API_NAME(pfx, write),				\
      .get_com_state = UART_API_NAME(pfx, get_com_state),		\
      .set_com_state = UART_API_NAME(pfx, set_com_state),		\
      .get_modem_state = UART_API_NAME(pfx, get_modem_state),		\
      .set_modem_state = UART_API_NAME(pfx, set_modem_state),		\
      .send_break = UART_API_NAME(pfx, send_break),			\
      .send_xon = UART_API_NAME(pfx, send_xon),			\
      .send_xoff = UART_API_NAME(pfx, send_xoff),			\
      .hangup  = UART_API_NAME(pfx, hangup),			\
    }

#define UART_API_STATIC(pfx) \
static int UART_API_NAME(pfx,close)(void* hndl);  \
static int UART_API_NAME(pfx,read)(void* hndl, void* buf, size_t nbytes);  \
static int UART_API_NAME(pfx,write)(void* hndl, void* buf, size_t nbytes);  \
static int UART_API_NAME(pfx,get_com_state)(void* hndl, uart_com_state_t* state);  \
static int UART_API_NAME(pfx,set_com_state)(void* hndl, uart_com_state_t* state);  \
static int UART_API_NAME(pfx,send_break)(void* hndl, int duration);  \
static int UART_API_NAME(pfx,send_xon)(void* hndl);  \
static int UART_API_NAME(pfx,send_xoff)(void* hndl);  \
static int UART_API_NAME(pfx,hangup)(void* hndl); \
static int UART_API_NAME(pfx,get_modem_state)(void* hndl, uart_modem_state_t* state);  \
static int UART_API_NAME(pfx,set_modem_state)(void* hndl, uart_modem_state_t state, int on)

#define UART_HF_OPEN  0x01

typedef struct {
    uart_api_t*  api;
    unsigned int flags;
    void*        data;
} uart_handle_t;

extern int uart_unix_open(uart_handle_t* hndl, char* devicename);
extern int uart_win_open(uart_handle_t* hndl, char* devicename);
extern int uart_ftdi_open(uart_handle_t* hndl, char* devicename);

static inline void uart_init(uart_handle_t* hndl)
{
    hndl->api = 0;
    hndl->flags = 0;
    hndl->data  = (void*)((long)-1);
}

static inline int uart_close(uart_handle_t* hndl)
{
    return (hndl->api->close)(hndl->data);
}

static inline int uart_read(uart_handle_t* hndl, void* buf, size_t nbytes)
{
    return (hndl->api->read)(hndl->data,buf,nbytes);
}

static inline int uart_write(uart_handle_t* hndl, void* buf, size_t nbytes)
{
    return (hndl->api->write)(hndl->data,buf,nbytes);
}

static inline int uart_get_com_state(uart_handle_t* hndl, uart_com_state_t* state)
{
    return (hndl->api->get_com_state)(hndl->data,state);
}

static inline int uart_set_com_state(uart_handle_t* hndl, uart_com_state_t* state)
{
    return (hndl->api->set_com_state)(hndl->data,state);
}

static inline int uart_send_break(uart_handle_t* hndl, int duration)
{
    return (hndl->api->send_break)(hndl->data, duration);
}

static inline int uart_send_xon(uart_handle_t* hndl)
{
    return (hndl->api->send_xon)(hndl->data);
}

static inline int uart_send_xoff(uart_handle_t* hndl)
{
    return (hndl->api->send_xoff)(hndl->data);
}

static inline int uart_hangup(uart_handle_t* hndl)
{
    return (hndl->api->hangup)(hndl->data);
}

static inline int uart_get_modem_state(uart_handle_t* hndl, 
				       uart_modem_state_t* state)
{
    return (hndl->api->get_modem_state)(hndl->data, state);
}

static inline int uart_set_modem_state(uart_handle_t* hndl, 
				       uart_modem_state_t state, int on)
{
    return (hndl->api->set_modem_state)(hndl->data, state, on);
}

#endif
