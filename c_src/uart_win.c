//
//  UART "win32" implementation
//
// http://msdn.microsoft.com/en-us/library/ms810467.aspx
//

#include <stdio.h>
#include "windows.h"

#include "uart_api.h"

typedef struct {
    HANDLE     fh;        // File handle
    OVERLAPPED in;        // Overlapped input
    OVERLAPPED out;       // Overlapped output
    OVERLAPPED stat;      // Overlapped status
    DWORD      statm;     // Status result
    DCB        tio;       // Comm parameter block
} uart_win_data_t;


// declare API functions
UART_API_STATIC(uart_win);

uart_api_t uart_win_api = UART_API_MAKE(uart_win);

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

int uart_win_open(uart_handle_t* hndl, char* devicename)
{
    HANDLE h;
    uart_win_data_t* wd;

    h = CreateFile(devicename,
		   GENERIC_READ | GENERIC_WRITE, 
		   0, 
		   0, 
		   OPEN_EXISTING,
		   FILE_FLAG_OVERLAPPED,
		   0);
    if (h == INVALID_HANDLE_VALUE)	
	return -1;
    
    wd = (uart_win_data_t*) malloc(sizeof(uart_win_data_t));
    
    wd->fh = h;
    wd->in.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    wd->out.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    wd->stat.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    SetCommMask(h, EV_RXCHAR);
    WaitCommEvent(h, &wd->statm, &wd->stat);

    hndl->api_data = (void*) wd;
    hndl->api = &uart_win_api;
    return 0;
}

static int uart_win_close(void* arg)
{
    CloseHandle(FH(arg));
    return 0;
}

static int uart_win_read(void* arg, void* buf, size_t nbytes)
{
    return read(FD(arg), buf, nbytes);
}

static int uart_win_write(void* arg, void* buf, size_t nbytes)
{
    return write(FD(arg), buf, nbytes);
}

static int uart_win_get_com_state(void* arg, uart_com_state_t* state)
{
    DCB        dcb;       // Comm parameter block
    COMMTIMEOUTS commtimeouts;

    if (!GetCommState(FH(arg), &dcb)) {
	fprintf(stderr, "GetCommState: error %d", GetLastError());
	return -1;
    }

    // input baud reate
    state->ibaud = from_speed(dcb.BaudRate);
    state->obaud = state->ibaud;

    // parity
    switch (dcb.Parity) {
    case ODDPARITY: state->parity = 1; break;
    case EVENPARITY: state->parity = 2; break;
    case MARKPARITY: state->parity = 3; break;
    case SPACEPARITY: state->parity = 4; break;
    case NOPARITY:
    default: state->parity = 0; break;
    }

    // stop bits
    if (uartp->dcb.StopBits == ONESTOPBIT)
	state->stopb = 1;
    else if (uartp->dcb.StopBits == TWOSTOPBITS)
	state->stopb = 2;
    else if (uartp->dcb.StopBits == ONE5STOPBITS)
	state->stopb = 3; // error?
    else
	state->stopb = 0;

    state->csize = dcb.ByteSize;

    state->bufsz = 1; // FIXME
    state->buftm = 1; // FIXME
    state->xonchar = dcb.XonChar;
    state->xoffchar = dcb.XoffChar;
    state->eolchar = '\n';
    state->eol2char = 0;

    state->swflow = dcb.fOutX;
    state->hwflow = dcb.fOutxCtsFlow;
    return 0;
}

static int uart_win_set_com_state(void* arg, uart_com_state_t* state)
{
    DCB        dcb;       // Comm parameter block
    COMMTIMEOUTS commtimeouts;

    if (!GetCommState(FH(arg), &dcb)) {
	fprintf(stderr, "GetCommState: error %d", GetLastError());
	return -1;
    }

    dcb.BaudRate = to_speed(state->obaud);
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

    if (state->stopb == 1)
	dcb.c_cflag &= ~CSTOPB;
    else if (state->stopb == 2)
	dcb.c_cflag |= CSTOPB;


    dcb.ByteSize = state->csize;

    // FIXME bufsz, buftm
    dcb.XonChar          = state->xonchar;
    dcb.XoffChar         = state->xoffchar;

    dcb.fOutX = dcb.fInX = state->swflow;
    dcb.fOutxCtsFlow     = state->hwflow;
    dcb.fOutxDsrFlow     = FALSE;
    dcb.fNull            = FALSE;
    // dcb.fDtrControl      = DTR_CONTROL_DISABLE;
    // dcb.fRtsControl      = RTS_CONTROL_DISABLE;
    
    dcb.DCBlength = sizeof(DCB);
    if (!SetCommState(FH(arg), &dcb)) {
	fprintf(stderr, "SetCommState: error %d", GetLastError());
	return -1;
    }

    // SetupComm(FH(arg), 32000, 32000);

    // commtimeouts.ReadIntervalTimeout         = MAXDWORD;
    // commtimeouts.ReadTotalTimeoutMultiplier  =    0;
    // commtimeouts.ReadTotalTimeoutConstant    =    1;
    // commtimeouts.WriteTotalTimeoutMultiplier =    0;
    // commtimeouts.WriteTotalTimeoutConstant   =    0;
    // SetCommTimeouts(IspEnvironment->hCom, &commtimeouts);

    return 0;
}


int uart_win_send_break(void* arg, int duration)
{
    EscapeCommFunction(FH(arg), SETBREAK);
    // delay duration
    EscapeCommFunction(FH(arg), CLRBREAK);
    return 0;
}

int uart_win_send_xon(void* arg)
{
    // send the Xon character
    return 0;
}

int uart_win_send_xoff(void* arg)
{
    // send the Xoff character
    return 0;
}

int uart_win_set_dtr(void* arg, int on)
{
    if (on)
	EscapeCommFunction(FH(arg), SETDTR);
    else
	EscapeCommFunction(FH(arg), CLRDTR);
    return 0;
}

int uart_win_get_dtr(void* arg)
{
    DCB dcb;
    if (GetCommState(FH(arg), &dcb)) {
	if (dcb.fDtrControl == DTR_CONTROL_DISABLE)
	    return 0;
	else if (dcb.fDtrControl == DTR_CONTROL_ENABLE)
	    return 1;
	else // DTR_CONTROL_HANDSHAKE!?!?
	    return -1;
    }
    return -1;
}

int uart_win_get_dsr(void* arg)
{
    DWORD status;
    if (GetCommModemStatus(FH(arg), &status))
	return (status & MS_DSR_ON) != 0;
    return -1;
}

int uart_win_set_rts(void* arg, int on)
{
    if (on)
	EscapeCommFunction(FH(arg), SETRTS);
    else
	EscapeCommFunction(FH(arg), CLRRTS);
    return 0;
}

int uart_win_get_rts(void* arg)
{
    DCB dcb;
    if (GetCommState(FH(arg), &dcb)) {
	if (dcb.fRtsControl == RTS_CONTROL_DISABLE)
	    return 0;
	else if (dcb.fRtsControl == RTS_CONTROL_ENABLE)
	    return 1;
	else // DTR_CONTROL_HANDSHAKE!?!?
	    return -1;
    }
    return -1;
}
