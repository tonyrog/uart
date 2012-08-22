//
//  UART "win32" implementation
//
// http://msdn.microsoft.com/en-us/library/ms810467.aspx
//

#include <stdio.h>
#include "windows.h"

#define EAGAIN       ERROR_IO_PENDING
#define EWOULDBLOCK  ERROR_IO_PENDING
#define ENOMEM       ERROR_NOT_ENOUGH_MEMORY
#define EINVAL       ERROR_BAD_ARGUMENTS
#define EBUSY        ERROR_BUSY
#define EOVERFLOW    ERROR_TOO_MANY_CMDS
#define EMSGSIZE     ERROR_NO_DATA
#define ENOTCONN     ERROR_PIPE_NOT_CONNECTED
#define EINTR        ERROR_INVALID_AT_INTERRUPT_TIME //dummy

#include "uart_api.h"

// debug output - preserve last error
#ifdef DEBUG
#define DEBUG_ERROR(args...) do {			\
    DWORD error = GetLastError();			\
    uart_drv_emit_error(__FILE__,__LINE__,args);	\
    SetLastError(error);				\
    } while(0)
#else
#define DEBUG_ERROR(args...)
#endif


typedef struct {
    ErlDrvPort port;
    HANDLE     fh;        // File handle
    OVERLAPPED in;        // Overlapped input
    OVERLAPPED out;       // Overlapped output
    OVERLAPPED stat;      // Overlapped status
    DWORD      statm;     // Status result
    BOOLEAN    reading;
    BOOLEAN    writing;
} uart_win32_data_t;

static struct _rate {
    int baud;
    DWORD speed;
} rtab[] = 
{
    {0,      0},
    {110,    CBR_110},
    {300,    CBR_300},
    {600,    CBR_600},
    {1200,   CBR_1200},
    {2400,   CBR_2400},
    {4800,   CBR_4800},
    {9600,   CBR_9600},
    {14400,  CBR_14400},
    {19200,  CBR_19200},
    {38400,  CBR_38400},
    {57600,  CBR_57600},
    {115200, CBR_115200},
    {128000, CBR_128000},
    {256000, CBR_256000},
    { -1,    0}
};

extern void _dosmaperr(DWORD);
extern int  errno;

static int uart_errno(uart_ctx_t* ctx)
{
    int error = GetLastError();
    _dosmaperr(error);
    ctx->error = errno;
    return errno;
}

static int from_speed(DWORD speed)
{
    int i = 0;
    int baud;

    while((rtab[i].baud != -1) && (rtab[i].speed != speed))
	i++;
    baud = rtab[i].baud;
    return baud;
}

static DWORD to_speed(int baud)
{
    int i = 0;
    int speed = 0;
    while((rtab[i].baud != -1) && (baud > rtab[i].baud))
	i++;
    if (rtab[i].baud == -1)
	speed = rtab[i-1].speed;
    else 
	speed = rtab[i].speed;
    return speed;
}
//
// http://support.microsoft.com/kb/115831
// devicename = COM1..COM9
// for COM10 and other (includes COM1..COM9)
// devicename = \\.\COM10  == "\\\\.\\COM10"
// 
//

int uart_win32_open(ErlDrvPort port,uart_handle_t* hndl, char* devicename)
{
    HANDLE h;
    uart_win32_data_t* wd;

    if (!(wd = (uart_win32_data_t*) driver_alloc(sizeof(uart_win32_data_t))))
	return -1;
    memset(wd, 0, sizeof(uart_win32_data_t));
    wd->port = port;
    wd->fh = CreateFile(devicename,
			GENERIC_READ | GENERIC_WRITE, 
			0, 
			0, 
			OPEN_EXISTING,
			FILE_FLAG_OVERLAPPED,
			0);
    if (wd->fh == INVALID_HANDLE_VALUE) {
	DEBUG_ERROR("CreateFile: invalid handle: error %d", GetLastError());
	goto error;
    }

    if (!(wd->in.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL))) {
	DEBUG_ERROR("CreateEvent: 1: error %d", GetLastError());
	goto error;
    }

    if (!(wd->out.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL))) {
	DEBUG_ERROR("CreateEvent: 1: error %d", GetLastError());
	goto error;
    }

    if (!(wd->stat.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL))) {
	DEBUG_ERROR("CreateEvent: 1: error %d", GetLastError());
	goto error;
    }

    // SetCommMask(wd->fh, EV_RXCHAR);
    WaitCommEvent(wd->fh, &wd->statm, &wd->stat);
    hndl->data = (void*) wd;
    hndl->api = &uart_win32_api;
    return 0;

error:
    if (wd->fh != INVALID_HANDLE_VALUE) CloseHandle(wd->fh);
    if (wd->in.hEvent) CloseHandle(wd->in.hEvent);
    if (wd->out.hEvent) CloseHandle(wd->out.hEvent);
    if (wd->stat.hEvent) CloseHandle(wd->stat.hEvent);
    driver_free(wd);
    return -1;
}

static int uart_win32_close(void* arg)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;

    if (wd->fh != INVALID_HANDLE_VALUE)
	driver_select(wd->port, wd->fh, ERL_DRV_USE,0);
    if (wd->in.hEvent)
	driver_select(wd->port,wd->in.hEvent,ERL_DRV_USE,0);
    if (wd->out.hEvent)
	driver_select(wd->port,wd->out.hEvent,ERL_DRV_USE,0);
    if (wd->stat.hEvent) 
	driver_select(wd->port,wd->stat.hEvent,ERL_DRV_USE,0);

    DEBUGF("uart_win32_close:");
    driver_free(wd);
    return 0;
}

static int uart_win32_read(void* arg, void* buf, size_t nbytes)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    DWORD nread;
    if (!wd->reading) {
	// store buf & nbytes to check later ?
	if (!ReadFile(wd->fh, buf, (DWORD) nbytes, &nread, &wd->in)) {
	    if (GetLastError() != ERROR_IO_PENDING)
		return -1;
	    driver_select(wd->port,(ErlDrvEvent) wd->in.hEvent,ERL_DRV_READ,1);
	    wd->reading = 1;
	    return 0;
	}
    }
    else {
	// check that arguments are the same as last call ?
	if (!GetOverlappedResult(wd->fh, &wd->in, &nread, FALSE)) {
	    if (GetLastError() == ERROR_IO_INCOMPLETE) // still waiting
		return 0;
	    wd->reading = 0;
	    return -1;
	}
	wd->reading = 0;
    }
    return nread;
}

static int uart_win32_write(void* arg, void* buf, size_t nbytes)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    DWORD nwritten;

    // For windows we probably need to copy buf to be safe
    // when OVERLAPPED, so that buf is not changed.

    if (!wd->writing) {
	if (!WriteFile(wd->fh, buf, (DWORD) nbytes, &nwritten, &wd->out)) {
	    if (GetLastError() != ERROR_IO_PENDING)
		return -1;
	    driver_select(wd->port,wd->out.hEvent,ERL_DRV_WRITE, 1);
	    wd->writing = 1;
	    return 0;
	}
    }
    else {
	if (!GetOverlappedResult(wd->fh, &wd->out, &nwritten, FALSE)) {
	    if (GetLastError() == ERROR_IO_INCOMPLETE) // still waiting
		return 0;
	    wd->writing = 0;
	    return -1;
	}
	wd->writing = 0;
    }
    return nwritten;
}

//
//  ERL_DRV_READ, 1   => start overlapped read operation
//  ERL_DEV_READ, 0   => possbly cancel the overlapped read operation
//  
//  ERL_DRV_WRITE, 1  => check that a overlapped write is in progress
//  ERL_DRV_WRITE, 0  => possbly cancel the overlapped write operation
//
//  ERL_DRV_USE, 0    => stop using fh & events 
//
static int uart_win32_select(void* arg, int mode, int on)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    switch(mode) {
    case ERL_DRV_USE:
	if (wd->fh != INVALID_HANDLE_VALUE)
	    driver_select(wd->port, wd->fh,ERL_DRV_USE,on);
	if (wd->in.hEvent)
	    driver_select(wd->port, wd->in.hEvent,ERL_DRV_USE,on);
	if (wd->out.hEvent)
	    driver_select(wd->port, wd->out.hEvent,ERL_DRV_USE,on);
	if (wd->stat.hEvent) 
	    driver_select(wd->port, wd->stat.hEvent,ERL_DRV_USE,on);
	break;
    case ERL_DRV_READ:
	if (on == 0) {
	    if (wd->reading) {
		driver_select(wd->port, wd->in.hEvent, ERL_DRV_READ, 0);
		// CancelIoEx(wd->fh, &wd->in);
		wd->reading = 0;
	    }
	}
	break;
    case ERL_DRV_WRITE:
	if (on == 0) {
	    if (wd->writing) {
		driver_select(wd->port, wd->out.hEvent, ERL_DRV_WRITE, 0);
		// CancelIoEx(wd->fh, &wd->in);
		wd->writing = 0;
	    }
	}
	break;
    }
    return 0;
}


static int uart_win32_get_com_state(void* arg, uart_com_state_t* state)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    DCB        dcb;       // Comm parameter block
    COMMTIMEOUTS commtimeouts;

    memset(&dcb, 0, sizeof(dcb));
    if (!GetCommState(wd->fh, &dcb)) {
	DEBUG_ERROR("GetCommState: error %d", GetLastError());
	return -1;
    }

    // input baud reate
    state->ibaud = from_speed(dcb.BaudRate);
    state->obaud = state->ibaud;

    // parity
    switch (dcb.Parity) {
    case ODDPARITY:   state->parity = 1; break;
    case EVENPARITY:  state->parity = 2; break;
    case MARKPARITY:  state->parity = 3; break;
    case SPACEPARITY: state->parity = 4; break;
    case NOPARITY:
    default: state->parity = 0; break;
    }

    // stop bits
    if (dcb.StopBits == ONESTOPBIT)
	state->stopb = 1;
    else if (dcb.StopBits == TWOSTOPBITS)
	state->stopb = 2;
    else if (dcb.StopBits == ONE5STOPBITS)
	state->stopb = 3; // error?
    else
	state->stopb = 0;

    state->csize = dcb.ByteSize;

    // simulate some args
    state->bufsz = 1;
    state->buftm = 0;
    state->xonchar  = dcb.XonChar;
    state->xoffchar = dcb.XoffChar;
    state->eolchar  = '\n';

    state->swflow = dcb.fOutX;
    state->hwflow = dcb.fOutxCtsFlow;
    return 0;
}

static int uart_win32_set_com_state(void* arg, uart_com_state_t* state)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    DCB        dcb;       // Comm parameter block
    COMMTIMEOUTS commtimeouts;
    int baud;

    memset(&dcb, 0, sizeof(dcb));
    if (!GetCommState(wd->fh, &dcb)) {
	DEBUG_ERROR("GetCommState: error %d", GetLastError());
	return -1;
    }

    baud = (state->ibaud > state->obaud) ? state->ibaud : state->obaud;
    dcb.BaudRate = to_speed(baud);
    switch(state->parity) {
    case 0:
	dcb.fParity = FALSE;
	dcb.Parity = NOPARITY; 
	break;
    case 1: 
	dcb.fParity = TRUE;
	dcb.Parity = ODDPARITY; 
	break;
    case 2: 
	dcb.fParity = TRUE;
	dcb.Parity = EVENPARITY; 
	break;
    case 3: 
	dcb.fParity = TRUE;
	dcb.Parity = MARKPARITY; 
	break;
    }    

    if (state->stopb == 1)
	dcb.StopBits = ONESTOPBIT;
    else if (state->stopb == 2)
	dcb.StopBits = TWOSTOPBITS;
    else if (state->stopb == 3)
	dcb.StopBits = ONE5STOPBITS;

    dcb.ByteSize = state->csize;

    // FIXME bufsz, buftm (possible?)
    dcb.XonChar          = state->xonchar;
    dcb.XoffChar         = state->xoffchar;

    // FIXME better flow handling!
    // {flow,[xoff|dtr|rts],[xoff|cts|dsr]} ?
    //
    dcb.fOutX = dcb.fInX = state->swflow;
    dcb.fOutxCtsFlow     = state->hwflow;
    dcb.fOutxDsrFlow     = FALSE;
    dcb.fNull            = FALSE;
    // dcb.fDtrControl      = DTR_CONTROL_DISABLE;
    // dcb.fRtsControl      = RTS_CONTROL_DISABLE;
    
    dcb.DCBlength = sizeof(DCB);
    if (!SetCommState(wd->fh, &dcb)) {
	DEBUG_ERROR("SetCommState: error %d", GetLastError());
	return -1;
    }

    // SetupComm(wd->fh, 32000, 32000);

    // commtimeouts.ReadIntervalTimeout         = MAXDWORD;
    // commtimeouts.ReadTotalTimeoutMultiplier  =    0;
    // commtimeouts.ReadTotalTimeoutConstant    =    1;
    // commtimeouts.WriteTotalTimeoutMultiplier =    0;
    // commtimeouts.WriteTotalTimeoutConstant   =    0;
    // SetCommTimeouts(IspEnvironment->hCom, &commtimeouts);

    return 0;
}

// FIXME
int uart_win32_send_break(void* arg, int duration)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    EscapeCommFunction(wd->fh, SETBREAK);
    // delay duration
    EscapeCommFunction(wd->fh, CLRBREAK);
    return 0;
}

int uart_win32_send_xon(void* arg)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    (void) wd;
    // Force, send the Xon character
    return 0;
}

int uart_win32_send_xoff(void* arg)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    (void) wd;
    // Force, send the Xoff character
    return 0;
}

static int uart_win32_hangup(void* arg)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    (void) wd;
    // FIXME
    return 0;
}


static int uart_win32_set_modem_state(void* arg, uart_modem_state_t state, int on)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    if (state & UART_MODEM_DTR)
	EscapeCommFunction(wd->fh, on ? SETDTR : CLRDTR);
    if (state & UART_MODEM_RTS)
	EscapeCommFunction(wd->fh, on ? SETRTS : CLRRTS);
    return 0;
}

static int uart_win32_get_modem_state(void* arg, uart_modem_state_t* state)
{
    uart_win32_data_t* wd = (uart_win32_data_t*) arg;
    DCB dcb;
    DWORD status;
    int s = 0;

    if (!GetCommState(wd->fh, &dcb)) {
	DEBUG_ERROR("GetCommState: error %d", GetLastError());
	return -1;
    }
    if (dcb.fDtrControl == DTR_CONTROL_ENABLE)
	s |= UART_MODEM_DTR;
    if (dcb.fRtsControl == RTS_CONTROL_ENABLE)
	s |= UART_MODEM_RTS;
    if (!GetCommModemStatus(wd->fh, &status)) {
	DEBUG_ERROR("GetModemStatus: error %d", GetLastError());
	return -1;
    }
    if (status & MS_DSR_ON)
	s |= UART_MODEM_DSR;
    if (status & MS_CTS_ON)
	s |= UART_MODEM_CTS;
    if (status & MS_RING_ON)
	s |= UART_MODEM_RI;
    if (status & MS_RLSD_ON)  // carrier detect
	s |= UART_MODEM_DCD;  // data carrier detect
    return s;
}
