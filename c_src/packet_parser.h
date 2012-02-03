/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */

/* A protocol decoder. Simple packet length extraction as well as packet
 * body parsing with protocol specific callback interfaces (http and ssl).
 */
#ifndef __PACKET_PARSER_H__
#define __PACKET_PARSER_H__

#include <erl_driver.h>
// #include "sys.h"
#define ERTS_GLB_INLINE inline

/* INET_LOPT_PACKET options */
/* Bit encoding:
 *   0x8000 0000
 *
 */
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

typedef struct http_atom {
    struct http_atom* next;   /* next in bucket */
    unsigned long h;          /* stored hash value */
    const char* name;
    int   len;
    int index;                /* index in table + bit-pos */
    ErlDrvTermData atom;      /* erlang atom rep */
} http_atom_t;  

typedef struct {
    enum {
        URI_STAR,    /* '*' */
        URI_STRING,  /* "string(s1)" */
        URI_ABS_PATH,/* {abs_path, "path(s1)"} */
        URI_SCHEME,  /* {scheme, "scheme(s1)", "string(s2)"} */
        URI_HTTP,    /* {absoluteURI, http, "host(s1)", Port, "path(s2)"} */
        URI_HTTPS    /* {absoluteURI, https, ... */
    } type;
    const char* s1_ptr;
    int s1_len;
    const char* s2_ptr;
    int s2_len;
    int port; /* 0=undefined */
}PacketHttpURI;

typedef int HttpResponseMessageFn(void* arg, int major, int minor, int status,
				  const char* phrase, int phrase_len);
typedef int HttpRequestMessageFn(void* arg, const http_atom_t* meth, const char* meth_ptr,
				 int meth_len, const PacketHttpURI*, int major, int minor);
typedef int HttpEohMessageFn(void *arg);
typedef int HttpHeaderMessageFn(void* arg, const http_atom_t* name, const char* name_ptr,
				int name_len, const char* value_ptr, int value_len);
typedef int HttpErrorMessageFn(void* arg, const char* buf, int len);
typedef int SslTlsFn(void* arg, unsigned type, unsigned major, unsigned minor,
                     const char* data, int len, const char* prefix, int plen);

typedef struct {
    HttpResponseMessageFn* http_response;
    HttpRequestMessageFn* http_request;
    HttpEohMessageFn* http_eoh;
    HttpHeaderMessageFn* http_header;
    HttpErrorMessageFn* http_error;
    SslTlsFn* ssl_tls;
}PacketCallbacks;


/* Called once at emulator start
 */
void uart_packet_parser_init(void);

/* Returns > 0 Total packet length.
 *         = 0 Length unknown, need more data.
 *         < 0 Error, invalid format.
 */
int uart_packet_get_length(unsigned int htype,
		      const char* ptr, unsigned n,  /* Bytes read so far */
		      unsigned max_plen,      /* Packet max length, 0=no limit */
		      unsigned trunc_len,     /* Truncate (lines) if longer, 0=no limit */
		      int* statep);           /* Internal protocol state */

ERTS_GLB_INLINE
void uart_packet_get_body(unsigned int htype,
                     const char** bufp, /* In: Packet header, Out: Packet body */
                     int* lenp);        /* In: Packet length, Out: Body length */

/* Returns 1 = Packet parsed and handled by callbacks.
**         0 = No parsing support for this packet type
**        -1 = Error
*/
ERTS_GLB_INLINE
int uart_packet_parse(unsigned int htype, 
		 const char* buf, int len, /* Total packet */
		 int* statep, PacketCallbacks* pcb, void* arg);



/* Internals for the inlines below: */

#define FCGI_VERSION_1 1
struct fcgi_head {
    unsigned char version;
    unsigned char type;
    unsigned char requestIdB1;
    unsigned char requestIdB0;
    unsigned char contentLengthB1;
    unsigned char contentLengthB0;
    unsigned char paddingLength;
    unsigned char reserved;
    /* char data[] */
    /* char padding[paddingLength] */
};
int uart_packet_parse_http(const char*, int, int*, PacketCallbacks*, void*);
int uart_packet_parse_ssl(const char*, int, PacketCallbacks*, void*);


#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE
void uart_packet_get_body(unsigned int htype, const char** bufp, int* lenp)
{
    switch (htype & UART_PB_TYPE_MASK) {
    case UART_PB_FCGI:
	*lenp -= ((struct fcgi_head*)*bufp)->paddingLength;
        break;
    case UART_PB_N: {
	unsigned n = (htype & UART_PB_BYTES_MASK) >> 8;
	*bufp += n;
	*lenp -= n;
	break;
    }
    default:
        ;/* Return other packets "as is" */
    }
}

ERTS_GLB_INLINE
int uart_packet_parse(unsigned int htype, const char* buf, int len,
		 int* statep, PacketCallbacks* pcb, void* arg)
{	
    switch (htype & UART_PB_TYPE_MASK) {
    case UART_PB_HTTP:
    case UART_PB_HTTPH:
    case UART_PB_HTTP_BIN:
    case UART_PB_HTTPH_BIN:
        if (uart_packet_parse_http(buf, len, statep, pcb, arg) < 0)
            pcb->http_error(arg, buf, len);
        return 1;
    case UART_PB_SSL_TLS:
	return uart_packet_parse_ssl(buf, len, pcb, arg);
    default:;
    }
    return 0;
}
#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !__PACKET_PARSER_H__ */

