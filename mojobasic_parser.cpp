/**
 * MojoBASIC; a modern reimplementation of QuickBASIC.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#define __MOJOBASIC_INTERNAL__ 1
#include "mojobasic_internal_llvm.h"

struct StatementCollector
{
    StatementCollector(const SourcePosition &pos) : position(pos), start(SourcePosition()), tail(&start) {}
    virtual ~StatementCollector() { start.next = NULL; }
    AstStatementBlock *newStatementBlock() { return new AstStatementBlock(position, start.next); start.next = NULL; tail = &start; }
    AstExitStatement start;  // just picked a simple not-pure-virtual class.
    AstStatement *tail;
    const SourcePosition position;
};

struct TokenData
{
    Token tokenval;
    const char *token;
    unsigned int tokenlen;
    SourcePosition position;
    union
    {
        int64 i64;
        double dbl;
        const char *string;
    };
};

class Parser
{
public:
    Parser(void *_ctx, StringCache &_strcache) : ctx(_ctx), pp(NULL), strcache(_strcache) {}
    ~Parser() { assert(pp == NULL); }
    AstProgram *run(const char *filename, const char *source, unsigned int sourcelen, MOJOBASIC_includeOpen include_open, MOJOBASIC_includeClose include_close);
    
private:
    void fail(const char *err) { failCompile(ctx, err, currentToken.position); }
    Token getNextToken();
    void convertToParserToken(TokenData &data);
    bool want(const Token t);
    bool need(const Token t, const char *err);
    void dumpUntilEndOfStatement(); // dump tokens until we hit something that looks like the end of a statement.
    void dumpUntilEndOfLine(); // dump tokens until we hit eol.
    bool needEndOfStatement();
    AstStatement *failAndDumpStatement(const char *err);  // always returns NULL
    bool needEndOfLine();
    AstStatement *parseStatement();
    bool parseStatements(StatementCollector &collector);
    AstStatement *parseMetacommand();  // !!! FIXME: maybe move this into the lexer?
    AstProcedureDeclaration *parseDeclare();
    AstProcedureSignature *parseProcedureSignature(const bool bIsFunction);
    AstProcedure *parseProcedure(const bool bIsFunction);
    AstConstStatement *parseConst();
    AstVariableDeclarationStatement *parseVarDeclaration();
    AstTypeDeclarationStatement *parseType();
    bool parseDefLetterRange(char *a, char *z);
    AstDefStatement *parseDefType(const char *typ);
    AstDefStatement *parseDefInt() { return parseDefType("I"); }
    AstDefStatement *parseDefSng() { return parseDefType("S"); }
    AstDefStatement *parseDefDbl() { return parseDefType("D"); }
    AstDefStatement *parseDefLng() { return parseDefType("L"); }
    AstDefStatement *parseDefStr() { return parseDefType("$"); }
    AstStatement *parseOnError(const bool bLocal);
    AstStatement *parseOn();
    AstIfStatement *parseIf();
    AstStatement *parseEnd();

    void *ctx;
    StringCache &strcache;
    Preprocessor *pp;
    TokenData currentToken;
};

AstProgram *parseSource(void *ctx, StringCache &strcache, const char *filename, const char *source, unsigned int sourcelen, MOJOBASIC_includeOpen include_open, MOJOBASIC_includeClose include_close)
{
    Parser parser(ctx, strcache);
    return parser.run(filename, source, sourcelen, include_open, include_close);
} // parseSource


static int64 strtoi64(const char *str, unsigned int len)
{
    int64 retval = 0;
    int64 mult = 1;
    int i = 0;

    while ((len) && (*str == ' '))
    {
        str++;
        len--;
    } // while

    if ((len) && (*str == '-'))
    {
        mult = -1;
        str++;
        len--;
    } // if

    while (((unsigned int) i) < len)
    {
        const char ch = str[i];
        if ((ch < '0') || (ch > '9'))
            break;
        i++;
    } // while

    while (--i >= 0)
    {
        const char ch = str[i];
        retval += ((int64) (ch - '0')) * mult;
        mult *= 10;
    } // while

    return retval;
} // strtoi64

static double strtodouble(const char *_str, unsigned int len)
{
    // !!! FIXME: laziness prevails.
    char *str = (char *) alloca(len+1);
    memcpy(str, _str, len);
    str[len] = '\0';
    return strtod(str, NULL);
} // strtodouble

void Parser::convertToParserToken(TokenData &data)
{
    data.i64 = 0;

    if (data.tokenval == TOKEN_INT_LITERAL)
        data.i64 = strtoi64(data.token, data.tokenlen);

    else if (data.tokenval == TOKEN_FLOAT_LITERAL)
        data.dbl = strtodouble(data.token, data.tokenlen);

    else if (data.tokenval == TOKEN_IDENTIFIER)    // need to see if this is actually a keyword, or really an identifier.
    {
        char *buf = (char *) alloca(data.tokenlen);  // !!! FIXME: this stinks.
        for (size_t i = 0; i < data.tokenlen; i++)
        {
            const char ch = data.token[i];
            buf[i] = ((ch >= 'A') && (ch <= 'Z')) ? (ch + ('A' - 'a')) : ch;
        } // for

        data.string = strcache.cache(buf, data.tokenlen);

        // !!! FIXME: do a hash, just compare numbers.
        #define TOKENCMP(t) else if ((data.tokenlen == (sizeof (#t)-1)) && (memcmp(buf, #t, data.tokenlen) == 0)) data.tokenval = TOKEN_##t
        if (false) {}
        TOKENCMP(AS);
        TOKENCMP(CLOSE);
        TOKENCMP(CONST);
        TOKENCMP(DECLARE);
        TOKENCMP(DEFDBL);
        TOKENCMP(DEFINT);
        TOKENCMP(DEFLNG);
        TOKENCMP(DEFSNG);
        TOKENCMP(DEFSTR);
        TOKENCMP(DIM);
        TOKENCMP(DO);
        TOKENCMP(DYNAMIC);
        TOKENCMP(ELSE);
        TOKENCMP(ELSEIF);
        TOKENCMP(END);
        TOKENCMP(ERROR);
        TOKENCMP(EXIT);
        TOKENCMP(FOR);
        TOKENCMP(FUNCTION);
        TOKENCMP(IF);
        TOKENCMP(LINE);
        TOKENCMP(LOCAL);
        TOKENCMP(ON);
        TOKENCMP(OPEN);
        TOKENCMP(PRINT);
        TOKENCMP(REDIM);
        TOKENCMP(SELECT);
        TOKENCMP(STATIC);
        TOKENCMP(SUB);
        TOKENCMP(THEN);
        TOKENCMP(TYPE);
        TOKENCMP(VIEW);
        TOKENCMP(WHILE);
        #undef TOKENCMP
    } // if
} // Parser::convertToParserToken

AstProgram *Parser::run(const char *filename, const char *source, unsigned int sourcelen, MOJOBASIC_includeOpen include_open, MOJOBASIC_includeClose include_close)
{
    pp = preprocessor_start(filename, source, sourcelen, include_open, include_close);

    getNextToken();
    const SourcePosition startpos(currentToken.position);

    StatementCollector collector(startpos);
    while (!want(TOKEN_EOI))
        parseStatements(collector);

    preprocessor_end(pp);
    pp = NULL;

    return new AstProgram(startpos, collector.newStatementBlock());
} // Parser::run

Token Parser::getNextToken()
{
    while (true)
    {
        currentToken.token = preprocessor_nexttoken(pp, &currentToken.tokenlen, &currentToken.tokenval);
        preprocessor_sourcepos(pp, currentToken.position);
        currentToken.position.filename = currentToken.position.filename ? strcache.cache(currentToken.position.filename) : NULL;

        if (currentToken.tokenval == TOKEN_BAD_CHARS)
        {
            fail("Bad characters in source file");
            continue;
        } // if
        else if (currentToken.tokenval == TOKEN_PREPROCESSING_ERROR)
        {
            fail(currentToken.token);  // this happens to be null-terminated.
            continue;
        } // else if

        convertToParserToken(currentToken);
        break;
    } // while

    return currentToken.tokenval;
} // Parser::getNextToken


bool Parser::want(const Token t)
{
    if (currentToken.tokenval != t)
        return false;
    getNextToken();
    return true;
} // Parser::want

bool Parser::need(const Token t, const char *err) {
    if (currentToken.tokenval == t)
    {
        getNextToken();
        return true;
    } // if
    failAndDumpStatement(err);
    return false;
} // Parser::need

AstStatement *Parser::failAndDumpStatement(const char *err) {
    fail(err);
    dumpUntilEndOfStatement();
    return NULL;
} // Parser::failAndDumpStatement


// Below is, more or less, the grammar.
// this is sort of crunched down from my usual C++ coding style, but I wanted
//  this to be as compact as possible, like a Lemon grammar or whatever.
//  It's not anywhere as compact in practice, but there are other tradeoffs.  --ryan.

void Parser::dumpUntilEndOfStatement() { // dump tokens until we hit something that looks like the end of a statement.
    while (!want(TOKEN_NEWLINE) && !want(TOKEN_COLON) && !want(TOKEN_EOI)) { getNextToken(); }  // flush whatever is there, try again.
} // Parser::dumpUntilEndOfStatement

void Parser::dumpUntilEndOfLine() { // dump tokens until we hit eol.
    while (!want(TOKEN_NEWLINE) && !want(TOKEN_EOI)) { getNextToken(); }  // flush whatever is there, try again.
} // Parser::dumpUntilEndOfLine

bool Parser::needEndOfStatement() {
    if (want(TOKEN_NEWLINE) || want(TOKEN_COLON) || want(TOKEN_EOI)) return true;
    failAndDumpStatement("Expected end of statement"); return false;
} // Parser::needEndOfStatement

bool Parser::needEndOfLine() {
    if (want(TOKEN_NEWLINE) || want(TOKEN_EOI)) return true;
    failAndDumpStatement("Expected end of line"); return false;
} // Parser::needEndOfLine

// almost every part of the grammar passes through this hub.
AstStatement *Parser::parseStatement() {
    if (want(TOKEN_NEWLINE)) return NULL; // skip blank lines.
    else if (want(TOKEN_EOI)) return NULL;   // we're done.
    else if (want(TOKEN_METACOMMAND)) return parseMetacommand();
    else if (want(TOKEN_DECLARE)) return parseDeclare();
    else if (want(TOKEN_FUNCTION)) return parseProcedure(true);
    else if (want(TOKEN_SUB)) return parseProcedure(false);
    else if (want(TOKEN_CONST)) return parseConst();
    else if (want(TOKEN_TYPE)) return parseType();
    else if (want(TOKEN_DEFINT)) return parseDefInt();
    else if (want(TOKEN_DEFSNG)) return parseDefSng();
    else if (want(TOKEN_DEFDBL)) return parseDefDbl();
    else if (want(TOKEN_DEFLNG)) return parseDefLng();
    else if (want(TOKEN_DEFSTR)) return parseDefStr();
    else if (want(TOKEN_ON)) return parseOn();
    //else if (want(TOKEN_DIM)) return parseDim();
    //else if (want(TOKEN_REDIM)) return parseReDim();
    else if (want(TOKEN_IF)) return parseIf();
    //else if (want(TOKEN_FOR)) return parseFor();
    //else if (want(TOKEN_DO)) return parseDo();
    //else if (want(TOKEN_WHILE)) return parseWhile();
    //else if (want(TOKEN_SELECT)) return parseSelect();
    //else if (want(TOKEN_PRINT)) return parsePrint();
    //else if (want(TOKEN_EXIT)) return parseExit();
    else if (want(TOKEN_END)) return parseEnd();

        // these are just function calls into the standard runtime, but they
        //  have magic syntactic sugar in BASIC...
    //else if (want(TOKEN_OPEN)) return parseOpen();
    //else if (want(TOKEN_CLOSE)) return parseClose();
    //else if (want(TOKEN_LINE)) return parseLine();
    //else if (want(TOKEN_VIEW)) return parseView();

    //else if (want(TOKEN_IDENTIFIER)) return parseIdentifierStatement();

    return NULL;  // nothing we handle here.
} // Parser::parseStatement

bool Parser::parseStatements(StatementCollector &collector) {
    AstStatement *stmt = parseStatement();
    if (stmt) {
        needEndOfStatement();
        collector.tail->next = stmt;
        collector.tail = stmt;
        return true;
    }
    return false;
} // Parser::parseStatements

AstStatement *Parser::parseMetacommand() {  // !!! FIXME: maybe move this into the lexer?
    // We don't check for '$INCLUDE here; the lexer handles that.
    // !!! FIXME: write me.
    //if (want(TOKEN_STATIC)) varsAreDynamic = false;
    //else if (want(TOKEN_DYNAMIC)) varsAreDynamic = true;

    // it's okay if this wasn't a valid metacommand; these are comments anyhow.
    //  we need to flush out until eol, though, to skip past the rest of the comment.
    dumpUntilEndOfLine();

    return NULL;  // always NULL.
} // Parser::parseMetacommand()

AstProcedureSignature *Parser::parseProcedureSignature(const bool bIsFunction) {
    const SourcePosition position(currentToken.position);
    if (!need(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
    const char *identifier = currentToken.string;  // these are strcache()'d.
    StatementCollector collector(currentToken.position);
    if (want(TOKEN_LPAREN)) {
        AstStatement *stmt;
        while ((stmt = parseVarDeclaration()) != NULL) {
            collector.tail->next = stmt;
            collector.tail = stmt;
            if (!want(TOKEN_COMMA)) break;
        }
        need(TOKEN_RPAREN, "Expected ')'");
    }

    const char *rettype = NULL;
    if (bIsFunction && want(TOKEN_AS)) {
        if (need(TOKEN_IDENTIFIER, "Expected datatype"))
            rettype = currentToken.string;  //  these are strcache()'d.
    } // if

    needEndOfStatement();

    AstProcedureSignature *retval = new AstProcedureSignature(position, bIsFunction, identifier, (AstVariableDeclarationStatement *) collector.start.next, rettype);
    collector.start.next = NULL;  // prevent destruction.
    return retval;
} // Parser::parseProcedureSignature

AstProcedureDeclaration *Parser::parseDeclare() {
    const SourcePosition position(currentToken.position);
    const bool bIsFunction = want(TOKEN_FUNCTION);
    if (!bIsFunction && !need(TOKEN_SUB, "Expected FUNCTION or SUB")) return NULL;
    AstProcedureSignature *sig = parseProcedureSignature(bIsFunction);
    return sig ? new AstProcedureDeclaration(position, sig) : NULL;
} // Parser::parseDeclare

AstProcedure *Parser::parseProcedure(const bool bIsFunction) {
    const SourcePosition position(currentToken.position);
    AstProcedureSignature *sig = parseProcedureSignature(bIsFunction);
    if (!sig) return NULL;

    StatementCollector collector(currentToken.position);
    while (true) {
        if (!parseStatements(collector)) {
            if (want(TOKEN_END)) {
                if (bIsFunction) {
                    if (need(TOKEN_FUNCTION, "Expected END FUNCTION")) break;
                } else {
                    if (need(TOKEN_SUB, "Expected END SUB")) break;
                }
            } else if (want(TOKEN_EOI)) {
                fail(bIsFunction ? "Expected END FUNCTION" : "Expected END SUB");
                break;
            } else {
                failAndDumpStatement("Syntax error");
            }
        }
    }

    return new AstProcedure(position, sig, collector.newStatementBlock());
} // Parser::parseProcedure

AstConstStatement *Parser::parseConst() {
    const SourcePosition position(currentToken.position);
    if (!need(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
    const char *identifier = currentToken.string;  // these are strcache()'d.
    if (!need(TOKEN_ASSIGN, "Expected '='")) return NULL;
    AstExpression *expr = parseExpression();
    if (!expr) return NULL;
    return new AstConstStatement(position, identifier, expr);
} // Parser::parseConst

AstVariableDeclarationStatement *Parser::parseVarDeclaration() {
    const SourcePosition position(currentToken.position);
    if (!want(TOKEN_IDENTIFIER)) return NULL;  // maybe end of a variable list?
    const char *identifier = currentToken.string;  // these are strcache()'d.
    if (!need(TOKEN_AS, "Expected AS")) return NULL;
    if (!need(TOKEN_IDENTIFIER, "Expected datatype")) return NULL;
    const char *datatype = currentToken.string;  // these are strcache()'d.
    int64 len = 0;
    int64 *plen = NULL;
    if (want(TOKEN_STAR) && need(TOKEN_INT_LITERAL, "Expected integer value")) {
        len = currentToken.i64;
        plen = &len;
    }

    return new AstVariableDeclarationStatement(position, identifier, plen, datatype);
} // Parser::parseVarDeclaration

AstTypeDeclarationStatement *Parser::parseType() {
    const SourcePosition position(currentToken.position);
    if (!need(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
    const char *identifier = currentToken.string;  // these are strcache()'d.
    needEndOfLine();  // can't use a ':' here in QB45, dunno why.

    StatementCollector collector(currentToken.position);
    AstStatement *stmt;
    while ((stmt = parseVarDeclaration()) != NULL) {
        collector.tail->next = stmt;
        collector.tail = stmt;
        needEndOfStatement();
    }

    if (need(TOKEN_END, "Expected variable declaration or END TYPE")) need(TOKEN_TYPE, "Expected END TYPE");

    // !!! FIXME: check this in semantic analysis instead.
    if (collector.start.next == NULL) fail("TYPE with no members");

    AstTypeDeclarationStatement *retval = new AstTypeDeclarationStatement(position, identifier, (AstVariableDeclarationStatement *) collector.start.next);
    collector.start.next = NULL;  // prevent destruction.
    return retval;
} // Parser::parseType

bool Parser::parseDefLetterRange(char *a, char *z) {
    const char *err = "Expected {letter}-{letter}";
    if (!need(TOKEN_IDENTIFIER, err)) return false;
    const char lo = currentToken.string[0];
    if (currentToken.string[1] != '\0') { failAndDumpStatement(err); return false; }
    if (!need(TOKEN_MINUS, err)) return false;
    if (!need(TOKEN_IDENTIFIER, err)) return false;
    const char hi = currentToken.string[1];
    if (currentToken.string[1] != '\0') { failAndDumpStatement(err); return false; }
    if ( ! (((lo >= 'a') && (lo <= 'z')) || ((lo >= 'A') && (lo <= 'Z'))) ) { failAndDumpStatement(err); return false; }
    if ( ! (((hi >= 'a') && (hi <= 'z')) || ((hi >= 'A') && (hi <= 'Z'))) ) { failAndDumpStatement(err); return false; }
    *a = lo;
    *z = hi;
    return true;
} // Parser::parseDefLetterRange

AstDefStatement *Parser::parseDefType(const char *typ) {
    const SourcePosition position(currentToken.position);
    char a, z;
    return parseDefLetterRange(&a, &z) ? new AstDefStatement(position, typ, a, z) : NULL;
} // Parser::parseDefType

AstStatement *Parser::parseOnError(const bool bLocal) {
    return failAndDumpStatement("write me (parseOnError)");
}

AstStatement *Parser::parseOn() {
    const bool bLocal = want(TOKEN_LOCAL);
    if (want(TOKEN_ERROR)) return parseOnError(bLocal);
    // (etc) if (want(TOKEN_PEN)) return parseOnPen();
    return failAndDumpStatement("Syntax error");  // !!! FIXME: "expected ERROR,etc"?
} // Parser::parseOn

AstIfStatement *Parser::parseIf() {
    const SourcePosition position(currentToken.position);
    AstExpression *expr = parseExpression();
    if (expr == NULL) return NULL;
    need(TOKEN_THEN, "Expected THEN"); // keep going if missing.
    AstStatementBlock *statements = NULL;
    AstStatementBlock *else_statements = NULL;
    if (!want(TOKEN_NEWLINE)) {  // it's a one-liner.
        AstStatement *stmt = parseStatement();
        if (stmt == NULL) {
            failAndDumpStatement("Expected valid statement");
        } else {
            statements = new AstStatementBlock(position, stmt);
            if (want(TOKEN_ELSE)) {
                stmt = parseStatement();
                if (stmt == NULL)
                    failAndDumpStatement("Expected valid statement");
                else
                    else_statements = new AstStatementBlock(position, stmt);
            }
        }
    } else {  // normal if/elseif/end if block.
        bool bIsElse = false;
        StatementCollector collector(currentToken.position);
        while (true) {
            if (!parseStatements(collector)) {
                if (want(TOKEN_ELSEIF)) {
                    else_statements = new AstStatementBlock(currentToken.position, parseIf());  // just splits into another if tree.
                    break;
                } else if (want(TOKEN_ELSE)) {
                    if (bIsElse) {
                        failAndDumpStatement("ELSE without IF");
                    } else {
                        needEndOfStatement();
                        bIsElse = true;
                        statements = collector.newStatementBlock();
                    }
                } else if (want(TOKEN_END) && need(TOKEN_IF, "Expected END IF")) {
                    break;
                } else if (want(TOKEN_EOI)) {
                    fail("Expected END IF");
                    break;
                } else {
                    failAndDumpStatement("Syntax error");
                }
            }
        }

        if (bIsElse) { else_statements = collector.newStatementBlock(); } else { statements = collector.newStatementBlock(); }
    }

    return new AstIfStatement(position, expr, statements, else_statements);
} // Parser::parseIf

AstStatement *Parser::parseEnd() {
    //if (want(TOKEN_NEWLINE) || want(TOKEN_COLON) || want(TOKEN_EOI)) return new AstEndStatement(this);  // !!! FIXME: eventually just a standard runtime function call.
    //pushedEndToken = true;  // !!! FIXME: or whatever
    // !!! FIXME: write me
    return NULL;
} // Parser::parseEnd


//assignment_statement
//label

// end of mojobasic_parser.cpp ...

