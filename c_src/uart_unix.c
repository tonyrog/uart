//
//  UART "linux"/"unix"/"posix" implementation
//
// Nice ref about serial stuff is:
// http://www.easysw.com/~mike/serial/serial.html
// http://www.unixwiz.net/techtips/termios-vmin-vtime.html
//
//
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

#include "uart_api.h"

typedef struct {
    ErlDrvPort port;  // from open
    int        fd;
    int        reading;
    int        writing;
} uart_unix_data_t;

#define HANDLE(h) ((void*) ((long)(h)))
#define FD(h)     ((int)((long)(h)))

// declare API functions
UART_API_STATIC(uart_unix);

uart_api_t uart_unix_api = UART_API_MAKE(uart_unix);


static struct _rate {
    int baud;
    unsigned int speed;
} rtab[] = 
{
    {0,       B0     },
    {50,      B50    },
    {75,      B75    },
    {110,     B110   },
    {134,     B134   },
    {150,     B150   },
    {200,     B200   },
    {300,     B300   },
    {600,     B600   },
    {1200,    B1200  },
    {1800,    B1800  },
    {2400,    B2400  },
    {4800,    B4800  },
    {9600,    B9600  },
#ifdef B19200
    {19200,   B19200 },
#elif defined(EXTA)
    {19200,   EXTA },
#endif
#ifdef B38400
    {38400,   B38400 },
#elif defined(EXTB)
    {38400,   EXTB },
#endif
#ifdef B57600
    {57600,   B57600 },
#endif
#ifdef B76800
    {76800,   B76800 },
#endif
#ifdef B115200
    {115200,  B115200 },
#endif
#ifdef B153600
    {153600,  B153600 }, 	
#endif
#ifdef B230400
    {230400,  B230400 }, 	
#endif
#ifdef B307200
    {307200,  B307200 }, 	
#endif
#ifdef B460800
    {460800,  B460800 }, 	
#endif
#ifdef B500000
    {500000,  B500000 },
#endif
#ifdef B576000
    {576000,  B576000 },
#endif
#ifdef B921600 
    {921600,  B921600 },
#endif
#ifdef B1000000
    {1000000, B1000000 },
#endif
#ifdef B1152000
    {1152000, B1152000 },
#endif
#ifdef B1500000
    {1500000, B1500000 },
#endif
#ifdef B2000000
    {2000000, B2000000 },
#endif
#ifdef B2500000
    {2500000, B2500000 },
#endif
#ifdef B3000000
    {3000000, B3000000 },
#endif
#ifdef B3500000
    {3500000, B3500000 },
#endif
#ifdef B4000000
    {4000000, B4000000 },
#endif
    { -1, B0 }
};


#ifdef DARWIN
#define HAVE_C_ISPEED 1
#define HAVE_C_OSPEED 1
#endif

static int from_speed(unsigned int speed)
{
#ifdef DIRECT_SPEED
    return (int) speed;
#else
    int i = 0;
    int baud;

    while((rtab[i].baud != -1) && (rtab[i].speed != speed))
	i++;
    baud = rtab[i].baud;
    return baud;
#endif
}

static unsigned int to_speed(int baud)
{
#ifdef DIRECT_SPEED
    return (unsigned int) baud;
#else
    int i = 0;
    int speed = 0;
    while((rtab[i].baud != -1) && (baud > rtab[i].baud))
	i++;
    if (rtab[i].baud == -1)
	speed = rtab[i-1].speed;
    else 
	speed = rtab[i].speed;
    return speed;
#endif
}

int uart_unix_open(ErlDrvPort port,uart_handle_t* hndl,char* devicename)
{
    int fd;
    int flags;
    uart_unix_data_t* ud;

    if (!(ud = (uart_unix_data_t*) driver_alloc(sizeof(uart_unix_data_t))))
	return -1;
    memset(ud, 0, sizeof(uart_unix_data_t));
    if ((ud->fd = open(devicename, O_RDWR|O_NDELAY|O_NOCTTY)) < 0)
	goto error;
    ud->port = port;
    fd = ud->fd;
    flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);   // non-blocking!!!

    tcflush(fd, TCOFLUSH);
    tcflush(fd, TCIFLUSH);
    hndl->data = ud;
    hndl->api = &uart_unix_api;
    hndl->flags = UART_HF_OPEN;   // device is open
    return 0;
error:
    if (ud->fd >= 0) close(ud->fd);
    driver_free(ud);
    return -1;
}

static int uart_unix_close(void* arg)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    driver_select(ud->port, (ErlDrvEvent)((long)(ud->fd)), ERL_DRV_USE, 0);
    DEBUGF("uart_unix_close:");
    driver_free(ud);
    return 0;
}

static int uart_unix_read(void* arg, void* buf, size_t nbytes)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    return read(ud->fd, buf, nbytes);
}

static int uart_unix_write(void* arg, void* buf, size_t nbytes)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    return write(ud->fd, buf, nbytes);
}

static int uart_unix_select(void* arg, int mode, int on)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    return driver_select(ud->port, (ErlDrvEvent)((long)(ud->fd)), mode, on);
}


// FIXME: at least on darwin there are 
// CCTS_OFLOW  - CTS flow control output
// CRTS_IFLOW  - RTS flow control input
// CDTR_IFLOW  - DTR flow control input
// CDSR_OFLOW  - DSR flow control output
// CCAR_OFLOW  - DCD flow control output
// 
// CRTSCTS = (CCTS_OFLOW | CRTS_IFLOW) !
//
// TIOCMIWAIT   wait for modem bits to change
// TCIOGICOUNT  count number of changes
//
#if defined(CCAR_OFLOW)
#define HWFLOW  CCAR_OFLOW   // DCD flow control of output
#else
#define HWFLOW CRTSCTS
#endif

static int uart_unix_get_com_state(void* arg, uart_com_state_t* state)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    struct termios tio;
    
    if (tcgetattr(ud->fd, &tio) < 0) 
	return -1;

    // input baud reate
    state->ibaud = from_speed(cfgetispeed(&tio));
    state->obaud = from_speed(cfgetospeed(&tio));

    // parity
    if (tio.c_cflag & PARENB) {
	if (tio.c_iflag & PARMRK)
	    state->parity = 3;
	else if (tio.c_cflag & PARODD)
	    state->parity = 1;
	else
	    state->parity = 2;
    }
    else
	state->parity = 0;
    
    // stop bits
    if (tio.c_cflag & CSTOPB)
	state->stopb = 2;
    else
	state->stopb = 1;

    // csize
    switch(tio.c_cflag & CSIZE) {
    case CS5: state->csize = 5; break;
    case CS6: state->csize = 6; break;
    case CS7: state->csize = 7; break;
    case CS8: state->csize = 8; break;
    default: break;
    }
    
    // may be used for {packet, {size,N}} and also when
    // in {packet,N} (N!=0) when waiting for a certain amount of data
    state->bufsz = tio.c_cc[VMIN];       // min number of bytes buffered
    state->buftm = tio.c_cc[VTIME]*100;
    state->xonchar = tio.c_cc[VSTART];
    state->xoffchar = tio.c_cc[VSTOP];
    state->eolchar = tio.c_cc[VEOL];
    state->eol2char = tio.c_cc[VEOL2];

    state->swflow = (tio.c_iflag & (IXON | IXOFF)) != 0;
    state->hwflow = (tio.c_cflag & HWFLOW) != 0;
    return 0;
}

static int uart_unix_set_com_state(void* arg, uart_com_state_t* state)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    struct termios tio;

    // read current state
    if (tcgetattr(ud->fd, &tio) < 0)
	return -1;

    cfsetispeed(&tio, to_speed(state->ibaud));
    cfsetospeed(&tio, to_speed(state->obaud));

    // update from state
    switch(state->parity) {
    case 0: // none
	tio.c_iflag &= ~PARMRK;
	tio.c_cflag &= ~PARENB;
	break;
    case 1: // odd 
	tio.c_iflag &= ~PARMRK;
	tio.c_cflag  |= PARODD;
	tio.c_cflag |= PARENB;
	break;
    case 2: // even
	tio.c_iflag &= ~PARMRK;
	tio.c_cflag &= ~PARODD;
	tio.c_cflag |= PARENB;
	break;
    case 3:  // mark (FIXME)
	tio.c_iflag |= PARMRK;
	tio.c_cflag |= PARENB;
	break;
    case 4:  // space (FIXME) 
	tio.c_iflag &= ~PARMRK;
	tio.c_cflag &= ~PARENB;
	break;
    default:
	break;
    }

    if (state->stopb == 1)
	tio.c_cflag &= ~CSTOPB;
    else if (state->stopb == 2)
	tio.c_cflag |= CSTOPB;

    tio.c_cflag &= ~CSIZE;
    switch(state->csize) {
    case 5: tio.c_cflag |= CS5; break;
    case 6: tio.c_cflag |= CS6; break;
    case 7: tio.c_cflag |= CS7; break;
    case 8: tio.c_cflag |= CS8; break;
    default: break;
    }
    // Set the buffer number of bytes buffered before interrupt
    if (state->bufsz > 255)
	tio.c_cc[VMIN] = 255;
    else
	tio.c_cc[VMIN] = state->bufsz;
    // Set the max time to buffer bytes 
    if (state->buftm > 25500)  // 25500 ms = 25.5 sec
	tio.c_cc[VTIME] = 255;
    else
	tio.c_cc[VTIME] = state->buftm / 100;
    tio.c_cc[VSTART] = state->xonchar;
    tio.c_cc[VSTOP] = state->xoffchar;
    if (state->swflow)
	tio.c_iflag |= (IXON | IXOFF | IXANY);
    else
	tio.c_iflag &= ~(IXON | IXOFF | IXANY);

    if (state->hwflow)
	tio.c_cflag |= HWFLOW;
    else
	tio.c_cflag &= ~HWFLOW;

    // local line + enable receiver
    tio.c_cflag |= (CLOCAL | CREAD);
    // raw input processing
    tio.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
    // no output processing
    tio.c_oflag &= ~(OPOST);

//    tio.c_cflag &= ~HUPCL;   // do NOT hangup-on-close 
    
    tcflush(ud->fd, TCIFLUSH);
    return tcsetattr(ud->fd, TCSANOW, &tio);
}

static int uart_unix_send_break(void* arg, int duration)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    return tcsendbreak(ud->fd, duration);
}

static int uart_unix_send_xon(void* arg)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    return tcflow(ud->fd, TCION);
}

static int uart_unix_send_xoff(void* arg)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    return tcflow(ud->fd, TCIOFF);
}

static int uart_unix_hangup(void* arg)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    struct termios tio;

    // read current state
    if (tcgetattr(ud->fd, &tio) < 0)
	return -1;
    cfsetispeed(&tio, B0);
    cfsetospeed(&tio, B0);
    return tcsetattr(ud->fd, TCSAFLUSH, &tio);
}

static int uart_unix_get_modem_state(void* arg, uart_modem_state_t* state)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    int status, r, s;

    if ((r = ioctl(ud->fd, TIOCMGET, &status)) < 0)
	return r;
    s = 0;
    if ((status & TIOCM_DTR) != 0) s |= UART_MODEM_DTR;
    if ((status & TIOCM_RTS) != 0) s |= UART_MODEM_RTS;
    if ((status & TIOCM_CTS) != 0) s |= UART_MODEM_CTS;
    if ((status & TIOCM_CD)  != 0) s |= UART_MODEM_DCD;
    if ((status & TIOCM_RNG) != 0) s |= UART_MODEM_RI;
    if ((status & TIOCM_DSR) != 0) s |= UART_MODEM_DSR;
    *state = s;
    return 0;
}

static int uart_unix_set_modem_state(void* arg, uart_modem_state_t state, int on)
{
    uart_unix_data_t* ud = (uart_unix_data_t*) arg;
    if (state) {
	int status = 0;
	if (state & UART_MODEM_DTR) status |= TIOCM_DTR;  // out
	if (state & UART_MODEM_RTS) status |= TIOCM_RTS;  // out
	if (state & UART_MODEM_CTS) status |= TIOCM_CTS;  // in
	if (state & UART_MODEM_DCD) status |= TIOCM_CD;   // in
	if (state & UART_MODEM_RI)  status |= TIOCM_RI;   // in|out?
	if (state & UART_MODEM_DSR) status |= TIOCM_DSR;  // in
	if (on) 
	    return ioctl(ud->fd, TIOCMBIS, &status);
	else
	    return ioctl(ud->fd, TIOCMBIC, &status);
    }
    return 0;
}
