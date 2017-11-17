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

#if DEBUG_PREPROCESSOR
    #define print_debug_token(token, len, val) \
        MOJOBASIC_print_debug_token("PREPROCESSOR", token, len, val)
#else
    #define print_debug_token(token, len, val)
#endif

#if DEBUG_LEXER
static void MOJOBASIC_print_debug_token(const char *subsystem, const char *token, const unsigned int tokenlen, const Token tokenval);
static Token debug_preprocessor_lexer(IncludeState *s)
{
    const Token retval = preprocessor_lexer(s);
    MOJOBASIC_print_debug_token("LEXER", s->token, s->tokenlen, retval);
    return retval;
} // debug_preprocessor_lexer
#define preprocessor_lexer(s) debug_preprocessor_lexer(s)
#endif

#if DEBUG_TOKENIZER
static void print_debug_lexing_position(IncludeState *s)
{
//    if (s != NULL)
//        printf("NOW LEXING %s:%d ...\n", s->filename, s->line);
} // print_debug_lexing_position
#else
#define print_debug_lexing_position(s)
#endif

class Preprocessor
{
public:
    Preprocessor(MOJOBASIC_includeOpen incopen, MOJOBASIC_includeClose incclose);
    ~Preprocessor();
    void push_source(const char *fname, const char *source,
                     unsigned int srclen, unsigned int linenum,
                     MOJOBASIC_includeClose close_callback);
    void pop_source();

    const char *nextToken(unsigned int *_len, Token *_token);
    void getSourcePosition(SourcePosition &pos) const;

private:
    bool isfail;
    char failstr[256];
    bool parsing_pragma;
    IncludeState *include_stack;
    ObjectPool<IncludeState> include_pool;
    StringCache filename_cache;
    MOJOBASIC_includeOpen open_callback;
    MOJOBASIC_includeClose close_callback;

    void failf(const char *fmt, ...) ISPRINTF(2,3);
    void fail(const char *reason) { failf("%s", reason); }
    void handle_pp_include();
};

static inline char *StrDup(const char *str)
{
    char *retval = (char *) new char[strlen(str) + 1];
    strcpy(retval, str);
    return retval;
} // StrDup

Preprocessor::Preprocessor(MOJOBASIC_includeOpen incopen, MOJOBASIC_includeClose incclose)
    : isfail(false)
    , parsing_pragma(false)
    , include_stack (NULL)
    , open_callback(incopen)
    , close_callback(incclose)
{
    memset(failstr, '\0', sizeof (failstr));
} // Preprocessor::Preprocessor

Preprocessor::~Preprocessor()
{
    while (include_stack != NULL)
        pop_source();
} // Preprocessor::~Preprocessor

void Preprocessor::getSourcePosition(SourcePosition &pos) const
{
    if (include_stack == NULL)
    {
        pos.filename = "";
        pos.line = 0;
        return;
    } // if

    pos = include_stack->position;
} // Preprocessor::getSourcePosition

void Preprocessor::failf(const char *fmt, ...)
{
    isfail = true;
    va_list ap;
    va_start(ap, fmt);
    char *str = mojoavsprintf(fmt, ap);
    va_end(ap);
    strncpy(failstr, str, sizeof (failstr) - 1);
    failstr[sizeof (failstr) - 1] = '\0';
    delete[] str;
} // Preprocessor::failf


#if DEBUG_TOKENIZER
static void MOJOBASIC_print_debug_token(const char *subsystem, const char *token,
                                 const unsigned int tokenlen,
                                 const Token tokenval)
{
    printf("%s TOKEN: \"", subsystem);
    unsigned int i;
    for (i = 0; i < tokenlen; i++)
    {
        if (token[i] == '\n')
            printf("\\n");
        else if (token[i] == '\r')
            printf("\\r");
        else if (token[i] == '\t')
            printf("\\t");
        else if (token[i] == '\\')
            printf("\\\\");
        else
            printf("%c", token[i]);
    } // for
    printf("\" (");
    switch ((size_t) tokenval)
    {
        #define TOKENCASE(x) case x: printf("%s", #x); break
        TOKENCASE(TOKEN_LPAREN);
        TOKENCASE(TOKEN_RPAREN);
        TOKENCASE(TOKEN_DOT);
        TOKENCASE(TOKEN_COMMA);
        TOKENCASE(TOKEN_MINUS);
        TOKENCASE(TOKEN_PLUS);
        TOKENCASE(TOKEN_STAR);
        TOKENCASE(TOKEN_SLASH);
        TOKENCASE(TOKEN_BACKSLASH);
        TOKENCASE(TOKEN_LESSTHAN);
        TOKENCASE(TOKEN_GREATERTHAN);
        TOKENCASE(TOKEN_EXPONENT);
        TOKENCASE(TOKEN_COLON);
        TOKENCASE(TOKEN_ASSIGN);
        TOKENCASE(TOKEN_SEMICOLON);
        TOKENCASE(TOKEN_QUESTION);
        TOKENCASE(TOKEN_HASH);
        TOKENCASE(TOKEN_NEWLINE);
        TOKENCASE(TOKEN_WHITESPACE);
        TOKENCASE(TOKEN_UNKNOWN);
        TOKENCASE(TOKEN_IDENTIFIER);
        TOKENCASE(TOKEN_INT_LITERAL);
        TOKENCASE(TOKEN_FLOAT_LITERAL);
        TOKENCASE(TOKEN_STRING_LITERAL);
        TOKENCASE(TOKEN_LEQ);
        TOKENCASE(TOKEN_GEQ);
        TOKENCASE(TOKEN_NEQ);
        TOKENCASE(TOKEN_SINGLE_COMMENT);
        TOKENCASE(TOKEN_EOI);
        TOKENCASE(TOKEN_BAD_CHARS);
        TOKENCASE(TOKEN_PREPROCESSING_ERROR);
        TOKENCASE(TOKEN_METACOMMAND);
        TOKENCASE(TOKEN_PP_INCLUDE);
        TOKENCASE(TOKEN_AS);
        TOKENCASE(TOKEN_CLOSE);
        TOKENCASE(TOKEN_CONST);
        TOKENCASE(TOKEN_DECLARE);
        TOKENCASE(TOKEN_DEFDBL);
        TOKENCASE(TOKEN_DEFINT);
        TOKENCASE(TOKEN_DEFLNG);
        TOKENCASE(TOKEN_DEFSNG);
        TOKENCASE(TOKEN_DEFSTR);
        TOKENCASE(TOKEN_DIM);
        TOKENCASE(TOKEN_DO);
        TOKENCASE(TOKEN_DYNAMIC);
        TOKENCASE(TOKEN_ELSE);
        TOKENCASE(TOKEN_ELSEIF);
        TOKENCASE(TOKEN_END);
        TOKENCASE(TOKEN_ERROR);
        TOKENCASE(TOKEN_EXIT);
        TOKENCASE(TOKEN_FOR);
        TOKENCASE(TOKEN_FUNCTION);
        TOKENCASE(TOKEN_IF);
        TOKENCASE(TOKEN_LINE);
        TOKENCASE(TOKEN_LOCAL);
        TOKENCASE(TOKEN_ON);
        TOKENCASE(TOKEN_OPEN);
        TOKENCASE(TOKEN_PRINT);
        TOKENCASE(TOKEN_REDIM);
        TOKENCASE(TOKEN_SELECT);
        TOKENCASE(TOKEN_STATIC);
        TOKENCASE(TOKEN_SUB);
        TOKENCASE(TOKEN_THEN);
        TOKENCASE(TOKEN_TYPE);
        TOKENCASE(TOKEN_VIEW);
        TOKENCASE(TOKEN_WHILE);
        TOKENCASE(TOKEN_MOD);
        TOKENCASE(TOKEN_AND);
        TOKENCASE(TOKEN_OR);
        TOKENCASE(TOKEN_XOR);
        TOKENCASE(TOKEN_EQV);
        TOKENCASE(TOKEN_IMP);
        TOKENCASE(TOKEN_NOT);
        TOKENCASE(TOKEN_SHARED);
        TOKENCASE(TOKEN_TO);
        TOKENCASE(TOKEN_UNTIL);
        TOKENCASE(TOKEN_LOOP);
        TOKENCASE(TOKEN_CASE);
        TOKENCASE(TOKEN_IS);
        TOKENCASE(TOKEN_RESUME);
        TOKENCASE(TOKEN_NEXT);
        TOKENCASE(TOKEN_GOTO);
        TOKENCASE(TOKEN_RANDOM);
        TOKENCASE(TOKEN_BINARY);
        TOKENCASE(TOKEN_INPUT);
        TOKENCASE(TOKEN_OUTPUT);
        TOKENCASE(TOKEN_APPEND);
        TOKENCASE(TOKEN_READ);
        TOKENCASE(TOKEN_WRITE);
        TOKENCASE(TOKEN_LOCK);
        TOKENCASE(TOKEN_ACCESS);
        TOKENCASE(TOKEN_DEF);
        #undef TOKENCASE

        default:
            assert(!"shouldn't hit this. Missing token?");
            printf("? ? ?");
            break;
    } // switch
    printf(")\n");
} // MOJOBASIC_print_debug_token
#endif



#if !MOJOBASIC_FORCE_INCLUDE_CALLBACKS
// !!! FIXME: most of these _MSC_VER should probably be _WINDOWS?
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>  // GL headers need this for WINGDIAPI definition.
#else
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#endif

static int MOJOBASIC_internal_include_open(MOJOBASIC_includeType inctype,
                                     const char *fname, const char *parent,
                                     const char **outdata,
                                     unsigned int *outbytes)
{
#ifdef _MSC_VER
    WCHAR wpath[MAX_PATH];
    if (!MultiByteToWideChar(CP_UTF8, 0, fname, -1, wpath, MAX_PATH))
        return 0;

    const DWORD share = FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE;
    const HANDLE handle = CreateFileW(wpath, FILE_GENERIC_READ, share,
                                      NULL, OPEN_EXISTING, NULL, NULL);
    if (handle == INVALID_HANDLE_VALUE)
        return 0;

    const DWORD fileSize = GetFileSize(handle, NULL);
    if (fileSize == INVALID_FILE_SIZE)
    {
        CloseHandle(handle);
        return 0;
    } // if

    char *data = new char[fileSize];
    DWORD readLength = 0;
    const bool failed = !ReadFile(handle, data, fileSize, &readLength, NULL);
    CloseHandle(handle);

    if (failed || (readLength != fileSize))
    {
        delete[] data;
        return 0;
    } // if

    *outdata = data;
    *outbytes = fileSize;
    return 1;
#else
    struct stat statbuf;
    if (stat(fname, &statbuf) == -1)
        return 0;
    const int fd = open(fname, O_RDONLY);
    if (fd == -1)
        return 0;

    char *data = new char[statbuf.st_size];
    const ssize_t br = read(fd, data, statbuf.st_size);
    close(fd);

    if (br != statbuf.st_size)
    {
        delete[] data;
        return 0;
    } // if

    *outdata = data;
    *outbytes = (unsigned int) statbuf.st_size;
    return 1;
#endif
} // MOJOBASIC_internal_include_open

static void MOJOBASIC_internal_include_close(const char *data)
{
    delete[] (char *) data;
} // MOJOBASIC_internal_include_close
#endif  // !MOJOBASIC_FORCE_INCLUDE_CALLBACKS

void Preprocessor::push_source(const char *fname, const char *source,
                              unsigned int srclen, unsigned int linenum,
                              MOJOBASIC_includeClose close_callback)
{
    IncludeState *state = include_pool.allocate();
    state->position.filename = fname ? filename_cache.cache(fname) : NULL;
    state->position.line = linenum;
    state->close_callback = close_callback;
    state->source_base = source;
    state->source = source;
    state->token = source;
    state->tokenval = ((Token) '\n');
    state->orig_length = srclen;
    state->bytes_left = srclen;
    state->next = include_stack;

    print_debug_lexing_position(state);

    include_stack = state;
} // Preprocessor::push_source

void Preprocessor::pop_source()
{
    IncludeState *state = include_stack;
    assert(state != NULL);  // more pops than pushes!
    if (state == NULL)
        return;

    if (state->close_callback)
        state->close_callback(state->source_base);

    // state->filename is a pointer to the filename cache; don't free it here!

    include_stack = state->next;

    print_debug_lexing_position(include_stack);

    include_pool.deallocate(state);
} // Preprocessor::pop_source

static inline void pushback(IncludeState *state)
{
    #if DEBUG_PREPROCESSOR
    printf("PREPROCESSOR PUSHBACK\n");
    #endif
    assert(!state->pushedback);
    state->pushedback = true;
} // pushback


static Token lexer(IncludeState *state)
{
    if (!state->pushedback)
        return preprocessor_lexer(state);
    state->pushedback = false;
    return state->tokenval;
} // lexer


// !!! FIXME: parsing fails on preprocessor directives should skip rest of line.
static bool require_newline(IncludeState *state)
{
    const Token token = lexer(state);
    pushback(state);  // rewind no matter what.
    return ( (token == ((Token) '\n')) || (token == TOKEN_EOI) );
} // require_newline

void Preprocessor::handle_pp_include()
{
    fail("FIXME: implement '$INCLUDE support here");
#if 0
    IncludeState *state = include_stack;
    char *filename = NULL;
    bool bogus = false;

    if (lexer(state) != ((Token) ':'))
        bogus = true;

    else if (token == ((Token) '<'))
    {
        incltype = MOJOBASIC_INCLUDETYPE_SYSTEM;
        // can't use lexer, since every byte between the < > pair is
        //  considered part of the filename.  :/
        while (!bogus)
        {
            if ( !(bogus = (state->bytes_left == 0)) )
            {
                const char ch = *state->source;
                if ( !(bogus = ((ch == '\r') || (ch == '\n'))) )
                {
                    state->source++;
                    state->bytes_left--;

                    if (ch == '>')
                        break;
                } // if
            } // if
        } // while
    } // else if
    else
    {
        bogus = true;
    } // else

    if (!bogus)
    {
        state->token++;  // skip '<' or '\"'...
        const unsigned int len = ((unsigned int) (state->source-state->token));
        filename = (char *) alloca(len);
        memcpy(filename, state->token, len-1);
        filename[len-1] = '\0';
        bogus = !require_newline(state);
    } // if

    const char *newdata = NULL;
    unsigned int newbytes = 0;

    if (bogus)
        fail("Invalid #include directive");
    else if ((open_callback == NULL) || (close_callback == NULL))
        fail("Saw #include, but no include callbacks defined");
    else if (!open_callback(incltype, filename, state->source_base, &newdata, &newbytes))
        fail("Include callback failed");  // !!! FIXME: better error
    else
        push_source(filename, newdata, newbytes, 1, close_callback);
#endif
} // Preprocessor::handle_pp_include


const char *Preprocessor::nextToken(unsigned int *_len, Token *_token)
{
    while (true)
    {
        if (isfail)
        {
            isfail = false;
            *_token = TOKEN_PREPROCESSING_ERROR;
            *_len = strlen(failstr);
            return failstr;
        } // if

        IncludeState *state = include_stack;
        if (state == NULL)
        {
            *_token = TOKEN_EOI;
            *_len = 0;
            return NULL;  // we're done!
        } // if

        const Token token = lexer(state);

        if (token == TOKEN_EOI)
        {
            assert(state->bytes_left == 0);
            pop_source();
            continue;  // pick up again after parent's #include line.
        } // if

        else if (token == TOKEN_METACOMMAND)
        {
            if ((lexer(state) == TOKEN_IDENTIFIER) && (state->tokenlen == 7) && (strncasecmp(state->token, "include", 7) == 0))
            {
                handle_pp_include();
                continue;  // will return error or use new top of include_stack.
            } // if
            pushback(state);
        } // else if

        // you don't ever see these unless you enable state->report_comments.
        else if (token == TOKEN_SINGLE_COMMENT)
            print_debug_lexing_position(state);

        else if (token == ((Token) '\n'))
            print_debug_lexing_position(state);

        *_token = token;
        *_len = state->tokenlen;
        return state->token;
    } // while

    assert(0 && "shouldn't hit this code");
    *_token = TOKEN_UNKNOWN;
    *_len = 0;
    return NULL;
} // Preprocessor::nexttoken

const char *preprocessor_nexttoken(Preprocessor *pp, unsigned int *len,
                                   Token *token)
{
    const char *retval = pp->nextToken(len, token);
    print_debug_token(retval, *len, *token);
    return retval;
} // preprocessor_nexttoken

void preprocessor_sourcepos(Preprocessor *pp, SourcePosition &pos)
{
    pp->getSourcePosition(pos);
} // preprocessor_sourcepos


Preprocessor *preprocessor_start(const char *fname, const char *source,
                            unsigned int sourcelen,
                            MOJOBASIC_includeOpen open_callback,
                            MOJOBASIC_includeClose close_callback)
{
    #if !MOJOBASIC_FORCE_INCLUDE_CALLBACKS
    if (!open_callback) open_callback = MOJOBASIC_internal_include_open;
    if (!close_callback) close_callback = MOJOBASIC_internal_include_close;
    #endif

    Preprocessor *pp = new Preprocessor(open_callback, close_callback);
    pp->push_source(fname, source, sourcelen, 1, NULL);
    return pp;
} // preprocessor_start

void preprocessor_end(Preprocessor *pp)
{
    delete pp;
} // preprocessor_end

// end of mojobasic_preprocessor.cpp ...

