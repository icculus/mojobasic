/**
 * MojoBASIC; a modern reimplementation of QuickBASIC.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// This started as a copy/paste from MojoShader, which was written by Ryan and
//  is also under the zlib license.  https://icculus.org/mojoshader/

// This was originally based on examples/pp-c.re from re2c: http://re2c.org/
//   re2c is public domain code.
//
// You build mojobasic_lexer.cpp from the .re file with re2c...
// re2c -is -o mojobasic_lexer.cpp mojobasic_lexer.re
//
// Changes to the lexer are done to the .re file, not the C++ code!

#define __MOJOBASIC_INTERNAL__ 1
#include "mojobasic_internal.h"

typedef unsigned char uchar;

/*!max:re2c */
#define RET(t) return update_state(s, eoi, cursor, token, (Token) t)
#define YYCTYPE uchar
#define YYCURSOR cursor
#define YYLIMIT limit
#define YYMARKER s->lexer_marker
#define YYFILL(n) { if ((n) == 1) { cursor = sentinel; limit = cursor + YYMAXFILL; eoi = 1; } }

static uchar sentinel[YYMAXFILL];

static Token update_state(IncludeState *s, int eoi, const uchar *cur,
                          const uchar *tok, const Token val)
{
    if (eoi)
    {
        s->bytes_left = 0;
        s->source = (const char *) s->source_base + s->orig_length;
        if ( (tok >= sentinel) && (tok < (sentinel+YYMAXFILL)) )
            s->token = s->source;
        else
            s->token = (const char *) tok;
    } // if
    else
    {
        s->bytes_left -= (unsigned int) (cur - ((const uchar *) s->source));
        s->source = (const char *) cur;
        s->token = (const char *) tok;
    } // else
    s->tokenlen = (unsigned int) (s->source - s->token);
    s->tokenval = val;
    return val;
} // update_state

Token preprocessor_lexer(IncludeState *s)
{
    const uchar *cursor = (const uchar *) s->source;
    const uchar *token = cursor;
    const uchar *matchptr;
    const uchar *limit = cursor + s->bytes_left;
    int eoi = 0;

/*!re2c
    ANY = [\000-\377];
    ANYLEGAL = [a-zA-Z0-9_/'*=+%^&|!#<>()[{}.,~^:;? \t\v\f\r\n\-\]\\];
    O = [0-7];
    D = [0-9];
    L = [a-zA-Z_];
    H = [a-fA-F0-9];
    E = [Ee] [+-]? D+;
    FS = [fFhH];
    ESC = [\\] ([abfnrtv?'"\\] | "x" H+ | O+);
    NEWLINE = ("\r\n" | "\r" | "\n");
    WHITESPACE = [ \t\v\f]+;
    SUFFIX = [$!@#$%&];
*/

scanner_loop:
    if (YYLIMIT == YYCURSOR) YYFILL(1);
    token = cursor;

/*!re2c
    "'" { goto singlelinecomment; }
    "REM" NEWLINE { s->position.line++; RET(TOKEN_NEWLINE); }
    "REM" WHITESPACE { goto singlelinecomment; }

    L (L|D)* SUFFIX?       { RET(TOKEN_IDENTIFIER); }
    ("&H" H+) | ("&O" O+) | (D+)  { RET(TOKEN_INT_LITERAL); }
    (D+ E) | (D* "." D+ E?) | (D+ "." D* E?) { RET(TOKEN_FLOAT_LITERAL); }
    (["] (ANY\[\r\n"])* ["]) { RET(TOKEN_STRING_LITERAL); }
    "<="            { RET(TOKEN_LEQ); }
    ">="            { RET(TOKEN_GEQ); }
    "<>"            { RET(TOKEN_NEQ); }
    "("             { RET(TOKEN_LPAREN); }
    ")"             { RET(TOKEN_RPAREN); }
    "."             { RET(TOKEN_DOT); }
    ","             { RET(TOKEN_COMMA); }
    "-"             { RET(TOKEN_MINUS); }
    "+"             { RET(TOKEN_PLUS); }
    "*"             { RET(TOKEN_STAR); }
    "/"             { RET(TOKEN_SLASH); }
    "\\"            { RET(TOKEN_BACKSLASH); }
    "<"             { RET(TOKEN_LESSTHAN); }
    ">"             { RET(TOKEN_GREATERTHAN); }
    "^"             { RET(TOKEN_EXPONENT); }
    ":"             { RET(TOKEN_COLON); }
    "="             { RET(TOKEN_ASSIGN); }
    ";"             { RET(TOKEN_SEMICOLON); }
    "?"             { RET(TOKEN_QUESTION); }
    "#"             { RET(TOKEN_HASH); }

    "\000"          { if (eoi) { RET(TOKEN_EOI); } goto bad_chars; }

    WHITESPACE      { if (s->report_whitespace) RET(TOKEN_WHITESPACE); goto scanner_loop; }
    NEWLINE         { s->position.line++; RET(TOKEN_NEWLINE); }
    ANY             { goto bad_chars; }
*/

singlelinecomment:
    if (YYLIMIT == YYCURSOR) YYFILL(1);
    matchptr = cursor;

/*!re2c
    WHITESPACE      { goto singlelinecomment; }
    "$"             { RET(TOKEN_METACOMMAND); }
    ANY             { cursor = matchptr; goto singlelinecomment_loop; }
*/

singlelinecomment_loop:
    if (YYLIMIT == YYCURSOR) YYFILL(1);
    matchptr = cursor;

/*!re2c
    NEWLINE         {
                        s->position.line++;
                        if (s->report_comments)
                        {
                            cursor = matchptr;  // so we RET(TOKEN_NEWLINE) next.
                            RET(TOKEN_SINGLE_COMMENT);
                        }
                        token = matchptr;
                        RET(TOKEN_NEWLINE);
                    }
    "\000"          {
                        if (eoi)
                        {
                            if (s->report_comments)
                                RET(TOKEN_SINGLE_COMMENT);
                            else
                                RET(TOKEN_EOI);
                        }
                        goto singlelinecomment_loop;
                    }
    ANY             { goto singlelinecomment_loop; }
*/

bad_chars:
    if (YYLIMIT == YYCURSOR) YYFILL(1);
/*!re2c
    ANYLEGAL        { cursor--; RET(TOKEN_BAD_CHARS); }
    "\000"          {
                        if (eoi)
                        {
                            assert( !((token >= sentinel) &&
                                     (token < sentinel+YYMAXFILL)) );
                            eoi = 0;
                            cursor = (uchar *) s->source_base + s->orig_length;
                            RET(TOKEN_BAD_CHARS);  // next call will be EOI.
                        }
                        goto bad_chars;
                    }

    ANY             { goto bad_chars; }
*/

    assert(0 && "Shouldn't hit this code");
    RET(TOKEN_UNKNOWN);
} // preprocessor_lexer

// end of mojobasic_lexer.re (or .cpp) ...

