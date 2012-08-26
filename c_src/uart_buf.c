//
// Input buffer processing
//
#include "uart_drv.h"

void uart_buf_init(uart_buf_t* bf)
{
    memset(bf, 0, sizeof(uart_buf_t));
}

void uart_buf_finish(uart_buf_t* bf)
{
    if (bf->base != NULL)
	DFREE(bf->base);
    uart_buf_init(bf);
}

void uart_buf_reset(uart_buf_t* bf)
{
    bf->ptr_start = bf->base;
    bf->ptr       = bf->base;
}

int uart_buf_alloc(uart_buf_t* bf, size_t sz)
{
    uint8_t* base;

    if ((base = DALLOC(sz)) == NULL)
	return -1;
    bf->sz        = sz;
    bf->base      = base;
    bf->ptr_start = base;
    bf->ptr       = base;
    return (int) sz;
}

//
// Set new size on buffer, used when packet size is determined
// and the buffer is to small.
// buffer must have a size of at least len bytes (counting from ptr_start!)
//
int uart_buf_expand(uart_buf_t* bf, size_t len)
{
    uint8_t* base;
    int offs1;
    int offs2;
    size_t used = bf->ptr_start - bf->base;
    size_t ulen = used + len;

    if (bf->sz >= ulen) /* packet will fit */
	return 0;

    offs1 = bf->ptr_start - bf->base;
    offs2 = bf->ptr - bf->ptr_start;

    if ((base = DREALLOC(bf->base, ulen)) == NULL)
	return -1;

    bf->base      = base;
    bf->ptr_start = bf->base + offs1;
    bf->ptr       = bf->ptr_start + offs2;
    bf->sz        = ulen;
    return 0;
}


// Move data so that ptr_start point at buf->base
void uart_buf_restart(uart_buf_t* bf)
{
    if (bf->ptr_start != bf->base) {
	int n = bf->ptr - bf->ptr_start;
	memmove(bf->base, bf->ptr_start, n);
	bf->ptr_start = bf->base;
	bf->ptr = bf->ptr_start + n;
    }
}

// push data into base
int uart_buf_push(uart_buf_t* bf, char* buf, int len)
{
    if (bf->base == NULL) {
	if (uart_buf_alloc(bf, len) < 0)
	    return -1;
	memcpy(bf->base, buf, len);
	bf->ptr = bf->ptr_start + len;
    }
    else {
	size_t sz_before = bf->ptr_start - bf->base;
	size_t sz_filled = bf->ptr - bf->ptr_start;
	
	if (len <= sz_before) {
	    memcpy(bf->ptr_start - len, buf, len);
	    bf->ptr_start -= len;
	}
	else {
	    uint8_t* base = DALLOC(bf->sz+len);
	    if (base == NULL)
		return -1;
	    memcpy(base, buf, len);
	    memcpy(base+len, bf->ptr_start, sz_filled);
	    DFREE(bf->base);
	    bf->sz += len;
	    bf->base = base;
	    bf->ptr_start = base;
	    bf->ptr = base + len + sz_filled;
	}
    }
    return 0;
}

// Return > 0 Total packet length.in bytes
//        = 0 Length unknown, need more data.
//        < 0 Error, invalid format.
// max_plen  - Max packet length, 0=no limit
// trunc_len - Truncate (lines) if longer, 0=no limit
int uart_buf_packet(uart_buf_t* bf, unsigned int htype, unsigned max_plen,
		    unsigned trunc_len)
{
    uint8_t* ptr = bf->ptr_start;
    size_t   n   = bf->ptr - bf->ptr_start;
    size_t   hlen, plen;

    switch (htype & UART_PB_TYPE_MASK) {
    case UART_PB_RAW: {
	unsigned m;
        if (n == 0) 
	    goto more;
	hlen = 0;
	m = (htype & UART_PB_FIXED_MASK) >> 16;
	if ((plen = m) == 0) {
            DEBUGF(" => nothing remain packet=%d", n);
            return n;
        }
	goto remain;
    }

    case UART_PB_N: {
	uint64_t pl = 0;
	hlen = (htype & UART_PB_BYTES_MASK) >> 8;
	if (n < hlen) goto more;
	if (htype & UART_PB_LITTLE_ENDIAN) {
	    ptr += hlen;
	    switch(hlen) {
	    case 8: pl = (pl << 8) | *--ptr;
	    case 7: pl = (pl << 8) | *--ptr;
	    case 6: pl = (pl << 8) | *--ptr;
	    case 5: pl = (pl << 8) | *--ptr;
	    case 4: pl = (pl << 8) | *--ptr;
	    case 3: pl = (pl << 8) | *--ptr;
	    case 2: pl = (pl << 8) | *--ptr;
	    case 1: pl = (pl << 8) | *--ptr;
		break;
	    default: return -1;		
	    }
	}
	else {
	    switch(hlen) {
	    case 8: pl = (pl << 8) | *ptr++;
	    case 7: pl = (pl << 8) | *ptr++;
	    case 6: pl = (pl << 8) | *ptr++;
	    case 5: pl = (pl << 8) | *ptr++;
	    case 4: pl = (pl << 8) | *ptr++;
	    case 3: pl = (pl << 8) | *ptr++;
	    case 2: pl = (pl << 8) | *ptr++;
	    case 1: pl = (pl << 8) | *ptr++;
		break;
	    default: return -1;
	    }
	}
	plen = (unsigned) pl;
	goto remain;
    }

    case UART_PB_LINE_LF: {
        /* UART_PB_LINE_LF:  [Data ... \n]  */
        const uint8_t* ptr2;
        if ((ptr2 = memchr(ptr, '\n', n)) == NULL) {
            if (n >= trunc_len && trunc_len!=0) { /* buffer full */
                DEBUGF(" => line buffer full (no NL)=%d", n);
                return trunc_len;
            }
            goto more;
        }
        else {
            int len = (ptr2 - ptr) + 1; /* including newline */
            if (len > (int)trunc_len && trunc_len!=0) {
                DEBUGF(" => truncated line=%d", trunc_len);
                return trunc_len;
            }
            DEBUGF(" => nothing remain packet=%d", len);
            return len;
        }
    }

    default:
        DEBUGF(" => case error");
        return -1;
    }

more:
    return 0;

remain:
    {
        int tlen = hlen + plen;
	if ((max_plen != 0 && plen > max_plen)
	    || tlen < (int)hlen) { /* wrap-around protection */
	    return -1;
	}
	return tlen;
    }		


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
int uart_buf_remain(uart_buf_t* bf, int* len,
		    unsigned int htype, unsigned int psize)
{
    uint8_t* ptr = bf->ptr_start;
    int nfill = (bf->ptr - bf->base);  // filled
    int nsz   = bf->sz   - nfill;        // remain
    int n     = bf->ptr  - ptr;  // number of bytes read
    int tlen;

    tlen = uart_buf_packet(bf, htype, psize, bf->sz);
    if (tlen > 0) {
        if (tlen <= n) { // got a packet 
            *len = tlen;
            DEBUGF(" => nothing remain packet=%d", tlen);
            return 0;
        }
        else { // need (known) more
            if (uart_buf_expand(bf, tlen) < 0)
                return -1;
            *len = tlen - n;
            DEBUGF(" => remain=%d", *len);
            return *len;
        }
    }
    else if (tlen == 0) { // need (unknown) more
        *len = 0;
        if (nsz == 0) {
            if (nfill == n) {
                if ((psize != 0) && 
		    ((int)psize > nfill)) {
                    if (uart_buf_expand(bf, psize) < 0)
                        return -1;
                    return psize;
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
