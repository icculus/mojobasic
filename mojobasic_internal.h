/**
 * MojoBASIC; a modern reimplementation of QuickBASIC.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// This started as a copy/paste from MojoShader, which was written by Ryan and
//  is also under the zlib license.  https://icculus.org/mojoshader/

#ifndef _INCLUDE_MOJOBASIC_INTERNAL_H_
#define _INCLUDE_MOJOBASIC_INTERNAL_H_

#ifndef __MOJOBASIC_INTERNAL__
#error Do not include this header from your applications.
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <new>
#include <vector>
#include <string>

#include "mojobasic.h"

#define DEBUG_LEXER 0
#define DEBUG_PREPROCESSOR 0
#define DEBUG_TOKENIZER (DEBUG_PREPROCESSOR || DEBUG_LEXER)
#define DEBUG_COMPILER_PARSER 1
#define DEBUG_COMPILER_AST 1

// Get basic wankery out of the way here...

#ifdef _WINDOWS
#define ENDLINE_STR "\r\n"
#else
#define ENDLINE_STR "\n"
#endif

typedef unsigned int uint;  // this is a printf() helper. don't use for code.

#ifdef _MSC_VER
#define va_copy(a, b) a = b
#define strcasecmp stricmp
#define strncasecmp strnicmp
typedef __int8 int8;
typedef unsigned __int8 uint8;
typedef unsigned __int16 uint16;
typedef unsigned __int32 uint32;
typedef unsigned __int64 uint64;
typedef __int32 int32;
typedef __int64 int64;
#ifdef _WIN64
typedef __int64 ssize_t;
#elif defined _WIN32
typedef __int32 ssize_t;
#else
#error Please define your platform.
#endif
// Warning Level 4 considered harmful.  :)
#pragma warning(disable: 4100)  // "unreferenced formal parameter"
#pragma warning(disable: 4389)  // "signed/unsigned mismatch"
#else
#include <stdint.h>
typedef int8_t int8;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef int32_t int32;
typedef int64_t int64;
typedef uint64_t uint64;
#endif

#ifdef sun
#include <alloca.h>
#endif

#ifdef __GNUC__
#define ISPRINTF(x,y) __attribute__((format (printf, x, y)))
#else
#define ISPRINTF(x,y)
#endif

#define STATICARRAYLEN(x) ( (sizeof ((x))) / (sizeof ((x)[0])) )

#define snprintf DONT_USE_SNPRINTF__IT_IS_NOT_PORTABLE
#define vsnprintf DONT_USE_VSNPRINTF__IT_IS_NOT_PORTABLE
char *mojoasprintf(const char *fmt, ...) ISPRINTF(1,2);
char *mojoavsprintf(const char *fmt, va_list va);


static inline int Min(const int a, const int b)
{
    return ((a < b) ? a : b);
} // Min


// !!! FIXME: make this atomic!
class RefCounter
{
public:
    RefCounter() : refcount(1) {}
    ~RefCounter() { assert(refcount == 0); }
    void ref() { assert(refcount > 0); refcount++; }
    void unref() { assert(refcount > 0); if (--refcount <= 0) { delete this; } }

private:
    int refcount;
};

// Hashtables...

template<class MapFrom> uint32 hashCalculate(const MapFrom &from);
template<class MapFrom> bool hashFromMatch(const MapFrom &obj1, const MapFrom &obj2);

template <class MapFrom, class MapTo>
class HashMap
{
public:
    HashMap(const uint32 initialBuckets=10)
        : buckets(new HashBucketNode*[initialBuckets])
        , bucketCount(initialBuckets)
        , size(0)
    {
        memset(buckets, '\0', sizeof (*buckets) * initialBuckets);
    } // HashMap::HashMap

    ~HashMap() { flush(); delete[] buckets; }

    bool isEmpty() const { return size == 0; }

    void rehash(const uint32 newBucketCount)
    {
        HashBucketNode **newBuckets = new HashBucketNode*[newBucketCount];
        memset(newBuckets, '\0', sizeof (*newBuckets) * newBucketCount);

        for (uint32 i = 0; i < bucketCount; i++)
        {
            HashBucketNode *next;
            for (HashBucketNode *node = buckets[i]; node; node = next)
            {
                const uint32 hash = node->hash % newBucketCount;
                next = node->next;
                node->next = newBuckets[hash];
                newBuckets[hash] = node;
            } // for
        } // for

        delete[] buckets;
        buckets = newBuckets;
        bucketCount = newBucketCount;
    } // rehash

    MapTo &insert(const MapFrom &from, const MapTo &to)
    {
        if (size >= bucketCount * 5)  // !!! FIXME: tweak these values.
            rehash(bucketCount * 2);

        const uint32 hash = hashCalculate<MapFrom>(from);
        const uint32 bucketHash = hash % bucketCount;
        HashBucketNode *node = new HashBucketNode(from, to, hash);
        node->next = buckets[bucketHash];
        buckets[bucketHash] = node;
        size++;
        return node->to;
    } // insert

    MapTo &put(const MapFrom &from, const MapTo &to)
    {
        HashBucketNode *node = findHashNode(from);
        if (node == NULL)
            return insert(from, to);

        Assert(node->hash == hashCalculate<MapFrom>(from));
        const uint32 hash = node->hash;
        HashBucketNode *next = node->next;
        // we manually destruct and placement new so our pointer doesn't change.
        node->~HashBucketNode();
        node = new (node) HashBucketNode(from, to, hash);
        node->next = next;
        return node->to;
    } // put

    MapTo *get(const MapFrom &from) const
    {
        HashBucketNode *node = findHashNode(from);
        return node ? &node->to : NULL;
    } // get

    MapTo *get(const MapFrom &from, MapTo &deflt) const
    {
        HashBucketNode *node = findHashNode(from);
        return (node) ? &node->to : &deflt;
    } // get

    void remove(const MapFrom &from)
    {
        const uint32 hash = hashCalculate<MapFrom>(from);
        HashBucketNode *prev = NULL;
        for (HashBucketNode *node = buckets[hash % bucketCount]; node; node = node->next)
        {
            if ((hash == node->hash) && (hashFromMatch<MapFrom>(from, node->from)))
            {
                if (prev)
                    prev->next = node->next;
                else
                    buckets[hash % bucketCount] = node->next;
                delete node;
                size--;
                return;
            } // if
            prev = node;
        } // for
    } // remove

    void flush()
    {
        for (uint32 i = 0; i < bucketCount; i++)
        {
            HashBucketNode *next = NULL;
            for (HashBucketNode *node = buckets[i]; node != NULL; node = next)
            {
                next = node->next;
                delete node;
            } // for
            buckets[i] = NULL;
        } // for
        size = 0;
    } // flush

    typedef bool (*CullFunc)(const MapFrom &from, const MapTo &to, void *data);

    void cull(CullFunc shouldCull, void *userdata=NULL)
    {
        for (uint32 i = 0; i < bucketCount; i++)
        {
            HashBucketNode *prev = NULL;
            HashBucketNode *next = NULL;
            for (HashBucketNode *node = buckets[i]; node != NULL; node = next)
            {
                next = node->next;
                if (!shouldCull(node->from, node->to, userdata))
                    prev = node;
                else
                {
                    if (prev)
                        prev->next = next;
                    else
                        buckets[i] = next;
                    delete node;
                    size--;
                } // if
            } // for
        } // for
    } // cull

    typedef void (*IterFunc)(const MapFrom &from, MapTo &to, void *data);

    void iterate(IterFunc iter, void *userdata=NULL)
    {
        for (uint32 i = 0; i < bucketCount; i++)
        {
            for (HashBucketNode *node = buckets[i]; node; node = node->next)
                iter(node->from, node->to, userdata);
        } // for
    } // HashMap::iterate

    uint32 count() const { return size; }

private:
    struct HashBucketNode
    {
        HashBucketNode(const MapFrom &_from, const MapTo &_to, const uint32 _hash)
            : from(_from), to(_to), hash(_hash), next(NULL) {}
        MapFrom from;
        MapTo to;
        uint32 hash;
        HashBucketNode *next;
    };

    HashBucketNode **buckets;
    uint32 bucketCount;
    uint32 size;

    HashBucketNode *findHashNode(const MapFrom &from) const
    {
        const uint32 hash = hashCalculate<MapFrom>(from);
        for (HashBucketNode *node = buckets[hash % bucketCount]; node; node = node->next)
        {
            if ((hash == node->hash) && (hashFromMatch<MapFrom>(from, node->from)))
                return node;
        } // for

        return NULL;
    } // findHashNode
};

class StringCache
{
public:
    StringCache() {}
    ~StringCache();
    const char *cache(const char *str);
    const char *cache(const char *str, const unsigned int len);
    const char *cacheFmt(const char *fmt, ...);

private:
    static bool freeCache(const char * const &from, const char * const &to, void *data);
    HashMap<const char *, const char *> cachemap;
};


// Dynamic buffers...

class Buffer
{
public:
    Buffer(const size_t blksz=128);
    ~Buffer() { flush(); }
    size_t getBlockSize() const { return block_size; }
    size_t getTotalSize() const { return total_bytes; }
    char *reserve(const size_t len);
    void append(const void *data, size_t len);
    void appendFmt(const char *fmt, ...);
    void appendVa(const char *fmt, va_list va);
    char *flatten();
    void flush();

private:
    struct BufferBlock
    {
        uint8 *data;
        size_t bytes;
        BufferBlock *next;
    };

    size_t total_bytes;
    BufferBlock *head;
    BufferBlock *tail;
    const size_t block_size;
};


template<class T>
class ObjectPool
{
public:
    ObjectPool() : pool(NULL) {}
    ~ObjectPool() { flush(); }

    T *allocate()
    {
        T *item = pool;
        if (item)
            pool = pool->next;
        else
            item = new T;
        item->next = NULL;
        return item;
    } // allocate

    void deallocate(T *item)
    {
        item->next = pool;
        pool = item;
    } // deallocate

    void flush()
    {
        T *item = pool;
        while (item != NULL)
        {
            T *next = item->next;
            delete item;
            item = next;
        } // while
    } // flush

private:
    T *pool;
};


// preprocessor stuff.

enum Token
{
    // Basic operators and other characters.
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_BACKSLASH,
    TOKEN_LESSTHAN,
    TOKEN_GREATERTHAN,
    TOKEN_EXPONENT,
    TOKEN_COLON,
    TOKEN_ASSIGN,
    TOKEN_SEMICOLON,
    TOKEN_QUESTION,
    TOKEN_HASH,
    TOKEN_NEWLINE,
    TOKEN_WHITESPACE,
    TOKEN_UNKNOWN,

    // These are all C-like constructs. Tokens < 256 may be single
    //  chars (like '+' or whatever). These are just multi-char sequences
    //  (like "<>" or whatever).
    TOKEN_IDENTIFIER,
    TOKEN_INT_LITERAL,
    TOKEN_FLOAT_LITERAL,
    TOKEN_STRING_LITERAL,
    TOKEN_LEQ,
    TOKEN_GEQ,
    TOKEN_NEQ,

    // This is returned if the preprocessor isn't stripping comments. Note
    //  this eats a newline token: at the ending newline on a single-line
    //  comment.
    TOKEN_SINGLE_COMMENT,

    // This is returned at the end of input...no more to process.
    TOKEN_EOI,

    // This is returned for char sequences we think are bogus. You'll have
    //  to judge for yourself. In most cases, you'll probably just fail with
    //  bogus syntax without explicitly checking for this token.
    TOKEN_BAD_CHARS,

    // This is returned if there's an error condition (the error is returned
    //  as a NULL-terminated string from preprocessor_nexttoken(), instead
    //  of actual token data). You can continue getting tokens after this
    //  is reported. It happens for things like missing #includes, etc.
    TOKEN_PREPROCESSING_ERROR,

    // Some BASIC metacommands, like '$DYNAMIC, etc...
    TOKEN_METACOMMAND,

    // These are all caught by the preprocessor.
    //  They control the preprocessor ('$INCLUDE new files, etc).
    TOKEN_PP_INCLUDE,

    // actual BASIC keywords start here...
    TOKEN_AS,
    TOKEN_CLOSE,
    TOKEN_CONST,
    TOKEN_DECLARE,
    TOKEN_DEFDBL,
    TOKEN_DEFINT,
    TOKEN_DEFLNG,
    TOKEN_DEFSNG,
    TOKEN_DEFSTR,
    TOKEN_DIM,
    TOKEN_DO,
    TOKEN_DYNAMIC,
    TOKEN_ELSE,
    TOKEN_ELSEIF,
    TOKEN_END,
    TOKEN_ERROR,
    TOKEN_EXIT,
    TOKEN_FOR,
    TOKEN_FUNCTION,
    TOKEN_IF,
    TOKEN_LINE,
    TOKEN_LOCAL,
    TOKEN_ON,
    TOKEN_OPEN,
    TOKEN_PRINT,
    TOKEN_REDIM,
    TOKEN_SELECT,
    TOKEN_STATIC,
    TOKEN_SUB,
    TOKEN_THEN,
    TOKEN_TYPE,
    TOKEN_VIEW,
    TOKEN_WHILE,
    TOKEN_MOD,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_XOR,
    TOKEN_EQV,
    TOKEN_IMP,
    TOKEN_NOT,
    TOKEN_SHARED,
    TOKEN_TO,
    TOKEN_UNTIL,
    TOKEN_LOOP,
    TOKEN_CASE,
    TOKEN_IS,
    TOKEN_RESUME,
    TOKEN_NEXT,
    TOKEN_GOTO,
    TOKEN_RANDOM,  // RANDOM through APPEND _must_ be next to each other in this order! (we convert to int and subtract TOKEN_RANDOM to get OPEN() value).
    TOKEN_BINARY,
    TOKEN_INPUT,
    TOKEN_OUTPUT,
    TOKEN_APPEND,
    TOKEN_READ,
    TOKEN_WRITE,
    TOKEN_LOCK,
    TOKEN_ACCESS
};


// This is opaque.
struct Preprocessor;

struct SourcePosition
{
    SourcePosition() : filename(NULL), line(0) {}
    SourcePosition(const char *f, const unsigned int l) : filename(f), line(l) {}
    SourcePosition(const SourcePosition &pos) : filename(pos.filename), line(pos.line) {}
    const char *filename;
    unsigned int line;
    // !!! FIXME: offset, length
};

struct IncludeState
{
    IncludeState() : source_base(NULL), source(NULL), token(NULL), tokenlen(0),
                     tokenval((Token) 0), pushedback(false), lexer_marker(NULL),
                     report_whitespace(false), report_comments(false),
                     orig_length(0), bytes_left(0),
                     close_callback(NULL), next(NULL) {}

    SourcePosition position;
    const char *source_base;
    const char *source;
    const char *token;
    unsigned int tokenlen;
    Token tokenval;
    bool pushedback;
    const unsigned char *lexer_marker;
    bool report_whitespace;
    bool report_comments;
    unsigned int orig_length;
    unsigned int bytes_left;
    MOJOBASIC_includeClose close_callback;
    IncludeState *next;
};

Token preprocessor_lexer(IncludeState *s);

Preprocessor *preprocessor_start(const char *fname, const char *source,
                            unsigned int sourcelen,
                            MOJOBASIC_includeOpen open_callback,
                            MOJOBASIC_includeClose close_callback);
void preprocessor_end(Preprocessor *pp);
int preprocessor_outofmemory(Preprocessor *pp);
const char *preprocessor_nexttoken(Preprocessor *_ctx, unsigned int *_len, Token *_token);
void preprocessor_sourcepos(Preprocessor *pp, SourcePosition &pos);

#endif  // _INCLUDE_MOJOBASIC_INTERNAL_H_

// end of mojoshader_internal.h ...

