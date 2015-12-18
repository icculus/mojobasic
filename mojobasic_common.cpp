/**
 * MojoBASIC; a modern reimplementation of QuickBASIC.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// This started as a copy/paste from MojoShader, which was written by Ryan and
//  is also under the zlib license.  https://icculus.org/mojoshader/

#define __MOJOBASIC_INTERNAL__ 1
#include "mojobasic_internal.h"

// this is djb's xor hashing function.
static inline uint32 hashCStringXorDJB(const char *str, size_t len)
{
    register uint32 hash = 5381;
    while (len--)
        hash = ((hash << 5) + hash) ^ *(str++);
    return hash;
} // hashCStringXorDJB

template<> uint32 hashCalculate(const char * const &str)
{
    return hashCStringXorDJB(str, strlen(str));
} // hashCalculate

template<> bool hashFromMatch(const char * const &a, const char *const &b)
{
    return (strcmp(a, b) == 0);
} // hashFromMatch


StringCache::~StringCache()
{
    cachemap.cull(&StringCache::freeCache);
} // StringCache::~StringCache

const char *StringCache::cache(const char *str)
{
    const char **val = cachemap.get(str);
    if (val)
        return *val;
    char *cpy = new char[strlen(str) + 1];
    strcpy(cpy, str);
    cachemap.insert(cpy, cpy);
    return cpy;
} // StringCache::cache

const char *StringCache::cache(const char *str, const unsigned int len)
{
    char *ptr = (len > 1024) ? new char[len+1] : NULL;
    char *buf = ptr ? ptr : (char *) alloca(len+1);
    strncpy(buf, str, len);
    buf[len] = '\0';
    const char *retval = cache(buf);
    delete[] ptr;
    return retval;
} // StringCache::cache

const char *StringCache::cacheFmt(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char *buf = mojoavsprintf(fmt, ap);
    va_end(ap);

    const char *retval = cache(buf);
    delete[] buf;
    return retval;
} // StringCache::cacheFmt

bool StringCache::freeCache(const char * const &from, const char * const &to, void *data)
{
    delete[] (char *) from;
    return true;
} // StringCache::freeCache


Buffer::Buffer(const size_t blksz)
    : total_bytes(0)
    , head(NULL)
    , tail(NULL)
    , block_size(blksz)
{}

char *Buffer::reserve(const size_t len)
{
    // note that we make the blocks bigger than blocksize when we have enough
    //  data to overfill a fresh block, to reduce allocations.
    if (len == 0)
        return NULL;

    if (tail != NULL)
    {
        const size_t avail = (tail->bytes >= block_size) ? 0 : block_size - tail->bytes;
        if (len <= avail)
        {
            tail->bytes += len;
            total_bytes += len;
            assert(tail->bytes <= block_size);
            return (char *) tail->data + tail->bytes;
        } // if
    } // if

    // need to allocate a new block (even if a previous block wasn't filled,
    //  so this buffer is contiguous).
    const size_t bytecount = len > block_size ? len : block_size;
    const size_t blocklen = sizeof (BufferBlock) + bytecount;
    BufferBlock *item = (BufferBlock *) new char[blocklen];
    item->data = ((uint8 *) item) + sizeof (BufferBlock);
    item->bytes = len;
    item->next = NULL;
    if (tail != NULL)
        tail->next = item;
    else
        head = item;
    tail = item;

    total_bytes += len;

    return (char *) item->data;
} // Buffer::reserve

void Buffer::append(const void *data, size_t len)
{
    if (len > 0)
        memcpy(reserve(len), data, len);
} // Buffer::append

void Buffer::appendFmt(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    appendVa(fmt, ap);
    va_end(ap);
} // Buffer::appendFmt

void Buffer::appendVa(const char *fmt, va_list va)
{
    va_list ap;
    va_copy(ap, va);
    char *buf = mojoavsprintf(fmt, ap);
    va_end(ap);
    append(buf, strlen(buf));
    delete[] buf;
} // Buffer::appendVa

void Buffer::flush()
{
    BufferBlock *item = head;
    while (item != NULL)
    {
        BufferBlock *next = item->next;
        delete[] (char *) item;
        item = next;
    } // while
    head = tail = NULL;
    total_bytes = 0;
} // Buffer::flush

char *Buffer::flatten()
{
    char *retval = new char[total_bytes + 1];
    BufferBlock *item = head;
    char *ptr = retval;
    while (item != NULL)
    {
        BufferBlock *next = item->next;
        memcpy(ptr, item->data, item->bytes);
        ptr += item->bytes;
        delete[] (char *) item;
        item = next;
    } // while
    *ptr = '\0';

    assert(ptr == (retval + total_bytes));

    head = tail = NULL;
    total_bytes = 0;

    return retval;
} // Buffer::flatten


char *mojoasprintf(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char *retval = mojoavsprintf(fmt, ap);
    va_end(ap);
    return retval;
} // mojoasprintf


#undef vsnprintf  // remove our warning temporarily.
char *mojoavsprintf(const char *fmt, va_list va)
{
    va_list ap;
    va_copy(ap, va);
    #ifdef _MSC_VER
    const int len = _vscprintf(fmt, ap);
    #else
    char ch = 0;
    const int len = vsnprintf(&ch, 1, fmt, ap);
    #endif
    va_end(ap);

    assert(len >= 0);
    if (len < 0)
        return NULL;  // ugh.

    char *retval = new char[len + 1];
    va_copy(ap, va);
    #ifdef _MSC_VER
    const int rc = _vsnprintf(retval, len + 1, fmt, ap);
    #else
    const int rc = vsnprintf(retval, len + 1, fmt, ap);
    #endif
    va_end(ap);
    
    assert(rc == len);
    if (rc != len)
    {
        delete[] retval;
        return NULL;
    } // if

    return retval;
} // mojoavsprintf
#define vsnprintf DONT_USE_VSNPRINTF__IT_IS_NOT_PORTABLE

// end of mojobasic_common.cpp ...

