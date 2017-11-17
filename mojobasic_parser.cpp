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
    StatementCollector(const SourcePosition &pos) : start(pos, TOKEN_UNKNOWN), tail(&start) {}
    virtual ~StatementCollector() { start.next = NULL; }
    AstStatementBlock *newStatementBlock() { AstStatementBlock *retval = new AstStatementBlock(start.position, start.next); start.next = NULL; tail = &start; return retval; }
    AstExitStatement start;  // just picked a simple not-pure-virtual class.
    AstStatement *tail;
};

struct VariableDeclarationCollector
{
    VariableDeclarationCollector(const SourcePosition &pos) : start(pos, NULL, NULL, -1, NULL, NULL), tail(&start) {}
    virtual ~VariableDeclarationCollector() { start.next = NULL; }
    AstVariableDeclaration start;
    AstVariableDeclaration *tail;
};

struct CaseCollector
{
    CaseCollector(const SourcePosition &pos) : start(pos, NULL, NULL), tail(&start) {}
    virtual ~CaseCollector() { start.next = NULL; }
    AstCase start;
    AstCase *tail;
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
    Parser(void *_ctx, StringCache &_strcache)
        : ctx(_ctx)
        , pp(NULL)
        , strcache(_strcache)
        , bPushedBack(false)
        , bWasNumericLabel(false)
    {}

    ~Parser() { assert(pp == NULL); }
    AstProgram *run(const char *filename, const char *source, unsigned int sourcelen, MOJOBASIC_includeOpen include_open, MOJOBASIC_includeClose include_close);
    
private:
    void fail(const char *err) { failCompile(ctx, err, currentToken.position); }
    void pushback();
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
    AstExpressionList *parseFunctionArgs();
    AstStatement *parseMetacommand();  // !!! FIXME: maybe move this into the lexer?
    AstProcedureDeclaration *parseDeclare();
    AstProcedureSignature *parseProcedureSignature(const bool bIsFunction);
    AstProcedure *parseProcedure(const bool bIsFunction);
    AstStatement *parseNumericLabel();
    AstExpression *parseIdentifierExpression(AstExpression *left);
    AstStatement *parseIdentifierStatement();
    AstConstStatement *parseConst();
    AstVariableDeclaration *parseVarDeclaration();
    AstTypeDeclarationStatement *parseType();
    AstVariableDeclarationStatement *parseDim();
    bool parseDefLetterRange(char *a, char *z);
    AstDefStatement *parseDefType(const char *typ);
    AstDefStatement *parseDefInt() { return parseDefType("INT"); }
    AstDefStatement *parseDefSng() { return parseDefType("SNG"); }
    AstDefStatement *parseDefDbl() { return parseDefType("DBL"); }
    AstDefStatement *parseDefLng() { return parseDefType("LNG"); }
    AstDefStatement *parseDefStr() { return parseDefType("STR"); }
    AstStatement *parseOnError(const bool bLocal);
    AstStatement *parseOn();
    AstSubCallStatement *parseOpen();
    AstSubCallStatement *parseClose();
    AstIfStatement *parseIf();
    AstStatement *parseDo();
    AstStatement *parseEnd();
    AstExitStatement *parseExit();
    AstSelectStatement *parseSelect();
    AstExpression *parseExpression(const int precedence=0);
    AstExpression *parseSubExpression();
    Token wantBinaryOperator();
    Token wantUnaryOperator();
    bool wantIdentifier(const char *ident);

    void *ctx;
    StringCache &strcache;
    Preprocessor *pp;
    TokenData previousToken;
    TokenData currentToken;
    TokenData pushbackToken;
    bool bPushedBack;
    bool bWasNumericLabel;
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

    else if (data.tokenval == TOKEN_STRING_LITERAL)
        data.string = strcache.cache(data.token+1, data.tokenlen - 2);  // cache the string but not the quotes.

    else if (data.tokenval == TOKEN_IDENTIFIER)    // need to see if this is actually a keyword, or really an identifier.
    {
        char *buf = (char *) alloca(data.tokenlen);  // !!! FIXME: this stinks.
        for (size_t i = 0; i < data.tokenlen; i++)
        {
            const char ch = data.token[i];
            buf[i] = ((ch >= 'a') && (ch <= 'z')) ? (ch - ('a' - 'A')) : ch;
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
        TOKENCMP(MOD);
        TOKENCMP(AND);
        TOKENCMP(OR);
        TOKENCMP(XOR);
        TOKENCMP(EQV);
        TOKENCMP(IMP);
        TOKENCMP(NOT);
        TOKENCMP(SHARED);
        TOKENCMP(TO);
        TOKENCMP(UNTIL);
        TOKENCMP(LOOP);
        TOKENCMP(CASE);
        TOKENCMP(IS);
        TOKENCMP(RESUME);
        TOKENCMP(NEXT);
        TOKENCMP(GOTO);
        TOKENCMP(RANDOM);
        TOKENCMP(BINARY);
        TOKENCMP(INPUT);
        TOKENCMP(OUTPUT);
        TOKENCMP(APPEND);
        TOKENCMP(READ);
        TOKENCMP(WRITE);
        TOKENCMP(LOCK);
        TOKENCMP(ACCESS);
        TOKENCMP(DEF);
        #undef TOKENCMP
    } // if
} // Parser::convertToParserToken

AstProgram *Parser::run(const char *filename, const char *source, unsigned int sourcelen, MOJOBASIC_includeOpen include_open, MOJOBASIC_includeClose include_close)
{
    pp = preprocessor_start(filename, source, sourcelen, include_open, include_close);

    memset(&currentToken, '\0', sizeof (currentToken));
    getNextToken();
    const SourcePosition startpos(currentToken.position);

    StatementCollector collector(startpos);
    while (!want(TOKEN_EOI))
        parseStatements(collector);

    preprocessor_end(pp);
    pp = NULL;

    return new AstProgram(startpos, collector.newStatementBlock());
} // Parser::run

void Parser::pushback()
{
    assert(!bPushedBack);
    bPushedBack = true;
    pushbackToken = currentToken;
    currentToken = previousToken;
} // Parser::pushback

Token Parser::getNextToken()
{
    if (bPushedBack)
    {
        bPushedBack = false;
        currentToken = pushbackToken;
        return currentToken.tokenval;
    } // if

    previousToken = currentToken;
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

bool Parser::wantIdentifier(const char *ident) {
    if (currentToken.tokenval == TOKEN_IDENTIFIER) {
        if (strcmp(currentToken.string, ident) == 0) {
            getNextToken();
            return true;
        }
    }
    return false;
} // Parser::wantIdentifier

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
    // numeric labels just carry on to the line's actual statement.
    if (bWasNumericLabel) { bWasNumericLabel = false; return true; }
    if (want(TOKEN_NEWLINE) || want(TOKEN_COLON) || want(TOKEN_EOI)) return true;
    failAndDumpStatement("Expected end of statement"); return false;
} // Parser::needEndOfStatement

bool Parser::needEndOfLine() {
    if (want(TOKEN_NEWLINE) || want(TOKEN_EOI)) return true;
    failAndDumpStatement("Expected end of line"); return false;
} // Parser::needEndOfLine



// almost every part of the grammar passes through this hub.
AstStatement *Parser::parseStatement() {

    while (want(TOKEN_NEWLINE)) { /* spin */ } // skip blank lines.

    if (want(TOKEN_EOI)) return NULL;   // we're done.
    else if (want(TOKEN_METACOMMAND)) return parseMetacommand();
    else if (want(TOKEN_DECLARE)) return parseDeclare();
    else if (want(TOKEN_FUNCTION)) return parseProcedure(true);
    else if (want(TOKEN_SUB)) return parseProcedure(false);
    else if (want(TOKEN_CONST)) return parseConst();
    else if (want(TOKEN_TYPE)) return parseType();
    else if (want(TOKEN_DIM)) return parseDim();
    else if (want(TOKEN_DEFINT)) return parseDefInt();
    else if (want(TOKEN_DEFSNG)) return parseDefSng();
    else if (want(TOKEN_DEFDBL)) return parseDefDbl();
    else if (want(TOKEN_DEFLNG)) return parseDefLng();
    else if (want(TOKEN_DEFSTR)) return parseDefStr();
    else if (want(TOKEN_ON)) return parseOn();
    //else if (want(TOKEN_REDIM)) return parseReDim();
    else if (want(TOKEN_IF)) return parseIf();
    //else if (want(TOKEN_FOR)) return parseFor();
    else if (want(TOKEN_DO)) return parseDo();
    //else if (want(TOKEN_WHILE)) return parseWhile();
    else if (want(TOKEN_SELECT)) return parseSelect();
    //else if (want(TOKEN_PRINT) || want(TOKEN_QUESTION)) return parsePrint();
    else if (want(TOKEN_EXIT)) return parseExit();
    else if (want(TOKEN_END)) return parseEnd();

        // these are just function calls into the standard runtime, but they
        //  have magic syntactic sugar in BASIC...
    else if (want(TOKEN_OPEN)) return parseOpen();
    else if (want(TOKEN_CLOSE)) return parseClose();
    //else if (want(TOKEN_GET)) return parseGet();
    //else if (want(TOKEN_PUT)) return parsePut();
    //else if (want(TOKEN_LINE)) return parseLine();
    //else if (want(TOKEN_VIEW)) return parseView();
    else if (want(TOKEN_IDENTIFIER)) return parseIdentifierStatement();

    // numeric labels have to be at the start of the line, a ':' separator won't do.
    else if ((previousToken.tokenval == TOKEN_NEWLINE) && (want(TOKEN_INT_LITERAL) || want(TOKEN_FLOAT_LITERAL))) return parseNumericLabel();

    // nothing we handle here. Dump to end of statement and continue.
    //dumpUntilEndOfStatement();
    return NULL;
} // Parser::parseStatement

bool Parser::parseStatements(StatementCollector &collector) {
    AstStatement *stmt = parseStatement();
    if (stmt) {
        needEndOfStatement();
        collector.tail->next = stmt;
        // iterate so tail is the last of any chained statements.
        for (collector.tail = stmt, stmt = stmt->next; stmt; stmt = stmt->next) {
            collector.tail = stmt;
        }
        return true;
    }
    return false;
} // Parser::parseStatements

AstStatement *Parser::parseNumericLabel()
{
    // believe it or not, you can do floating point labels. o_O
    const SourcePosition position(previousToken.position);
    const char *label = strcache.cache(previousToken.token, previousToken.tokenlen);
    bWasNumericLabel = true;
    return new AstLineLabel(position, label);
} // Parser::parseNumericLabel

AstExpression *Parser::parseIdentifierExpression(AstExpression *left)
{
    if (want(TOKEN_LPAREN)) {  // sub call or array dereference
        AstExpressionList *args = parseFunctionArgs();
        need(TOKEN_RPAREN, "Expected ')'");
        // !!! FIXME: a sub (not function) called as "mysub()" is illegal in BASIC. Check for that.
        return parseIdentifierExpression(new AstFunctionCallExpression(left->position, left, args));
    } else if (want(TOKEN_DOT)) {  // a struct dereference.
        if (!need(TOKEN_IDENTIFIER, "Expected field name")) {
            return left;  // oh well.
        }
        const SourcePosition position(previousToken.position);
        const char *identifier = previousToken.string;  // these are strcache()'d.
        return parseIdentifierExpression(new AstStructDereferenceExpression(position, left, identifier));
    }
    return left;
} // Parser::parseIdentifierExpression

AstStatement *Parser::parseIdentifierStatement()
{
    // this covers things that start with an identifier: could be a
    //  function call, sub call, an assignment, a line label...
    const SourcePosition position(previousToken.position);
    const char *identifier = previousToken.string;  // these are strcache()'d.

    // there's a syntax ambiguity: at the start of a line,
    //  "x: y" is either a line label named x with a sub call to y
    //  after it, or it's a call to a sub with no arguments named x,
    //  a statement separator and then a sub call to y. Microsoft's parser
    //  appears to resolve this by always treating this as a line label, even
    //  if there's already a declared sub named x. We will do so as well.

    if (want(TOKEN_COLON)) {  // a line label
        // this might have more tokens on the line, so we need to not eat
        //  the ':' so the parser will think that's an end-of-statement marker.
        pushback();
        return new AstLineLabel(position, identifier);
    }

    // okay, at this point, it's either a sub call or array dereference:
    //   x(expr ...
    // or a struct dereference:
    //   x.y ...
    // or an assignment:
    //   x = ...
    // ...and these can stack:
    //   x.y(1, z(5, 7)) = ...
    // ...so do this in a loop.
    //  But! it might also be a sub call without parentheses:
    //   x ...
    // we can't really know for sure if this is valid until
    //  we go through semantic analysis, but we can build the
    //  correct AST here until then.

    const Token t = currentToken.tokenval;
    if ((t != TOKEN_ASSIGN) && (t != TOKEN_COLON) && (t != TOKEN_LPAREN)) {
        // sub call or syntax error
        AstExpressionList *args = parseFunctionArgs();
        return new AstSubCallStatement(position, identifier, args);
    }

    // Now it's got to be an assignment statement or a syntax error.
    //  figure out what we're assigning to...
    AstExpression *lvalue = parseIdentifierExpression(new AstIdentifierExpression(position, identifier));
    if (want(TOKEN_ASSIGN)) {  // assignment!
        AstExpression *rvalue = parseExpression();
        if (rvalue) {
            return new AstAssignmentStatement(position, lvalue, rvalue);
        }
    }

    return NULL;  // i give up
} // Parser::parseIdentifierStatement

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

AstExpressionList *Parser::parseFunctionArgs()
{
    const SourcePosition position(previousToken.position);
    AstExpression *expr = parseExpression();
    if (!expr) {
        return NULL;
    }

    AstExpressionList *retval = new AstExpressionList(position, expr);
    while (want(TOKEN_COMMA)) {
        expr = parseExpression();
        if (expr) {
            retval->append(expr);
        }
    }

    return retval;
} // Parser::parseFunctionArgs

AstProcedureSignature *Parser::parseProcedureSignature(const bool bIsFunction) {
    const SourcePosition position(previousToken.position);
    if (!need(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
    const char *identifier = previousToken.string;  // these are strcache()'d.
    VariableDeclarationCollector collector(currentToken.position);
    if (want(TOKEN_LPAREN)) {
        AstVariableDeclaration *decl;
        while ((decl = parseVarDeclaration()) != NULL) {
            collector.tail->next = decl;
            collector.tail = decl;
            if (!want(TOKEN_COMMA)) break;
        }
        need(TOKEN_RPAREN, "Expected ')'");
    }

    const char *rettype = NULL;
    if (bIsFunction && want(TOKEN_AS)) {
        if (need(TOKEN_IDENTIFIER, "Expected datatype"))
            rettype = previousToken.string;  //  these are strcache()'d.
    } // if

    AstProcedureSignature *retval = new AstProcedureSignature(position, bIsFunction, identifier, collector.start.next, rettype);
    collector.start.next = NULL;  // prevent destruction.
    return retval;
} // Parser::parseProcedureSignature

AstProcedureDeclaration *Parser::parseDeclare() {
    const SourcePosition position(previousToken.position);
    const bool bIsFunction = want(TOKEN_FUNCTION);
    if (!bIsFunction && !need(TOKEN_SUB, "Expected FUNCTION or SUB")) return NULL;
    AstProcedureSignature *sig = parseProcedureSignature(bIsFunction);
    return sig ? new AstProcedureDeclaration(position, sig) : NULL;
} // Parser::parseDeclare

AstProcedure *Parser::parseProcedure(const bool bIsFunction) {
    const SourcePosition position(previousToken.position);
    AstProcedureSignature *sig = parseProcedureSignature(bIsFunction);
    if (!sig) return NULL;

    needEndOfStatement();

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
    const SourcePosition position(previousToken.position);
    if (!need(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
    const char *identifier = previousToken.string;  // these are strcache()'d.
    if (!need(TOKEN_ASSIGN, "Expected '='")) return NULL;
    AstExpression *expr = parseExpression();
    if (!expr) return NULL;
    return new AstConstStatement(position, identifier, expr);
} // Parser::parseConst

AstVariableDeclaration *Parser::parseVarDeclaration() {
    const SourcePosition position(currentToken.position);
    if (!want(TOKEN_IDENTIFIER)) return NULL;  // maybe end of a variable list?
    const char *identifier = previousToken.string;  // these are strcache()'d.

    AstExpression *lower = NULL;
    AstExpression *upper = NULL;
    if (want(TOKEN_LPAREN)) {
        lower = parseExpression();
        if (!want(TOKEN_TO)) {  // no TO? it's just the upper value.
            upper = lower;
            lower = NULL;
        } else {
            upper = parseExpression();
        }
        need(TOKEN_RPAREN, "Expected ')'");
    }

    const char *datatype = NULL;
    int64 recordsize = -1;
    if (want(TOKEN_AS)) {
        if (need(TOKEN_IDENTIFIER, "Expected datatype")) {
            datatype = previousToken.string;  // these are strcache()'d.
            if (want(TOKEN_STAR) && need(TOKEN_INT_LITERAL, "Expected integer value")) {
                recordsize = previousToken.i64;
            }
        }
    }

    return new AstVariableDeclaration(position, identifier, datatype, recordsize, lower, upper);
} // Parser::parseVarDeclaration

AstVariableDeclarationStatement *Parser::parseDim() {
    const SourcePosition position(previousToken.position);
    const bool bIsShared = want(TOKEN_SHARED);

    VariableDeclarationCollector collector(currentToken.position);
    AstVariableDeclaration *decl;
    while ((decl = parseVarDeclaration()) != NULL) {
        collector.tail->next = decl;
        collector.tail = decl;
        if (!want(TOKEN_COMMA)) {
            break;
        }
    }

    AstVariableDeclarationStatement *retval = new AstVariableDeclarationStatement(position, bIsShared, collector.start.next);
    collector.start.next = NULL;  // prevent destruction.
    return retval;
}

AstTypeDeclarationStatement *Parser::parseType() {
    const SourcePosition position(previousToken.position);
    if (!need(TOKEN_IDENTIFIER, "Expected identifier")) return NULL;
    const char *identifier = previousToken.string;  // these are strcache()'d.
    needEndOfLine();  // can't use a ':' here in QB45, dunno why.

    VariableDeclarationCollector collector(currentToken.position);
    AstVariableDeclaration *decl;
    while ((decl = parseVarDeclaration()) != NULL) {
        collector.tail->next = decl;
        collector.tail = decl;
        needEndOfStatement();
    }

    if (need(TOKEN_END, "Expected variable declaration or END TYPE")) need(TOKEN_TYPE, "Expected END TYPE");

    // !!! FIXME: check this in semantic analysis instead.
    if (collector.start.next == NULL) fail("TYPE with no members");

    AstTypeDeclarationStatement *retval = new AstTypeDeclarationStatement(position, identifier, collector.start.next);
    collector.start.next = NULL;  // prevent destruction.
    return retval;
} // Parser::parseType

bool Parser::parseDefLetterRange(char *a, char *z) {
    const char *err = "Expected {letter}-{letter}";
    if (!need(TOKEN_IDENTIFIER, err)) return false;
    const char lo = previousToken.string[0];
    if (previousToken.string[1] != '\0') { failAndDumpStatement(err); return false; }
    if (!need(TOKEN_MINUS, err)) return false;
    if (!need(TOKEN_IDENTIFIER, err)) return false;
    const char hi = previousToken.string[0];
    if (previousToken.string[1] != '\0') { failAndDumpStatement(err); return false; }
    if ( ! (((lo >= 'a') && (lo <= 'z')) || ((lo >= 'A') && (lo <= 'Z'))) ) { failAndDumpStatement(err); return false; }
    if ( ! (((hi >= 'a') && (hi <= 'z')) || ((hi >= 'A') && (hi <= 'Z'))) ) { failAndDumpStatement(err); return false; }
    *a = lo;
    *z = hi;
    return true;
} // Parser::parseDefLetterRange

AstDefStatement *Parser::parseDefType(const char *typ) {
    const SourcePosition position(previousToken.position);
    char a, z;
    return parseDefLetterRange(&a, &z) ? new AstDefStatement(position, typ, a, z) : NULL;
} // Parser::parseDefType

AstStatement *Parser::parseOnError(const bool bLocal) {
    const SourcePosition position(previousToken.position);
    const char *label = NULL;
    if (want(TOKEN_RESUME)) {
        need(TOKEN_NEXT, "Expected NEXT");
    } else if (want(TOKEN_GOTO)) {
        if (want(TOKEN_STRING_LITERAL)) {
            label = previousToken.string;
        } else if (want(TOKEN_INT_LITERAL) || want(TOKEN_FLOAT_LITERAL)) {
            label = strcache.cache(previousToken.token, previousToken.tokenlen);
        }
    } else {
        fail("Expected GOTO or RESUME");
    }

    return new AstOnErrorStatement(position, bLocal, label);
} // Parser::parseOnError

AstStatement *Parser::parseOn() {
    const bool bLocal = want(TOKEN_LOCAL);
    if (want(TOKEN_ERROR)) return parseOnError(bLocal);
    // (etc) if (want(TOKEN_PEN)) return parseOnPen();
    return failAndDumpStatement("Syntax error");  // !!! FIXME: "expected ERROR,etc"?
} // Parser::parseOn

AstStatement *Parser::parseDo() {
    const SourcePosition position(previousToken.position);
    AstExpression *cond = NULL;
    bool bIsConditionalAtStart = true;
    bool bIsWhile = false;

    if (want(TOKEN_WHILE)) {
        bIsWhile = true;
        cond = parseExpression();
    } else if (want(TOKEN_UNTIL)) {
        bIsWhile = false;
        cond = parseExpression();
    } else {
        bIsConditionalAtStart = false;
    }

    needEndOfStatement();

    StatementCollector collector(currentToken.position);
    while (true) {
        if (!parseStatements(collector)) {
            if (want(TOKEN_LOOP)) {
                if (!bIsConditionalAtStart) {
                    if (want(TOKEN_WHILE)) {
                        bIsWhile = true;
                        cond = parseExpression();
                    } else if (want(TOKEN_UNTIL)) {
                        bIsWhile = false;
                        cond = parseExpression();
                    } else {
                        bIsConditionalAtStart = true;
                    }
                }
                break;
            } else if (want(TOKEN_EOI)) {
                fail("Expected LOOP");
                break;
            } else {
                failAndDumpStatement("Syntax error");
            }
        }
    }

    return new AstDoStatement(position, bIsConditionalAtStart, bIsWhile, collector.newStatementBlock(), cond);
} // Parser::parseDo

AstSelectStatement *Parser::parseSelect()
{
    const SourcePosition position(previousToken.position);
    need(TOKEN_CASE, "Expected CASE");  // keep going if missing.
    AstExpression *test = parseExpression();
    needEndOfStatement();

    CaseCollector casecollector(position);
    bool bSawElse = false;
    while (want(TOKEN_CASE)) {
        const SourcePosition case_position(previousToken.position);
        AstExpressionList *cases = NULL;
        if (bSawElse) {
            fail("Another CASE isn't allowed after a CASE ELSE block");
        }

        if (want(TOKEN_ELSE)) {
            bSawElse = true;
            if (!cases) {
                cases = new AstExpressionList(case_position, NULL, TOKEN_UNKNOWN);
            } else {
                cases->append(NULL, TOKEN_UNKNOWN);
            }
        } else {
            do {
                const SourcePosition expr_position(previousToken.position);
                AstExpression *expr = NULL;
                Token op = TOKEN_UNKNOWN;
                if (want(TOKEN_IS)) {
                    if (want(TOKEN_LESSTHAN) || want(TOKEN_LEQ) || want(TOKEN_GREATERTHAN) || want(TOKEN_GEQ) || want(TOKEN_NEQ) || want(TOKEN_ASSIGN)) {
                        op = previousToken.tokenval;
                        expr = parseExpression();
                    } else {
                        fail("Expected <, <=, >, >=, <>, or =");
                    }
                } else {
                    expr = parseExpression();
                    if (want(TOKEN_TO)) {
                        op = TOKEN_TO;
                        AstExpression *expr2 = parseExpression();
                        if (expr && expr2) {
                            expr = new AstLessThanExpression(expr_position, expr, expr2);  // hack.
                        } else {
                            delete expr2;
                            fail("Expected range");
                        }
                    }
                }

                if (expr && !cases) {
                    cases = new AstExpressionList(case_position, expr, op);
                } else {
                    cases->append(expr, op);
                }
            } while (want(TOKEN_COMMA));
        }
        needEndOfStatement();

        StatementCollector collector(currentToken.position);
        while (true) {
            if (!parseStatements(collector)) {
                if (want(TOKEN_CASE)) {
                    pushback();  // eat this at the top of the loop.
                    break;
                } else if (want(TOKEN_END) && need(TOKEN_SELECT, "Expected END SELECT")) {
                    break;
                } else if (want(TOKEN_EOI)) {
                    fail("Expected END SELECT");
                    break;
                } else {
                    failAndDumpStatement("Syntax error");
                }
            }
        }

        AstCase *astcase = new AstCase(case_position, cases, collector.newStatementBlock());
        casecollector.tail->next = astcase;
        casecollector.tail = astcase;
    }

    return new AstSelectStatement(position, test, casecollector.start.next);
} // Parser::parseSelect

AstIfStatement *Parser::parseIf() {
    const SourcePosition position(previousToken.position);
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

AstSubCallStatement *Parser::parseOpen()
{
    const SourcePosition position(previousToken.position);
    AstExpression *fname = parseExpression();
    if (!fname) {
        fname = new AstStringLiteralExpression(position, "x");
    }
    AstExpressionList *args = new AstExpressionList(position, fname);

    const SourcePosition for_position(currentToken.position);
    Token openFor = TOKEN_RANDOM;
    if (want(TOKEN_FOR)) {
        // !!! FIXME: FOR ISAM
        if (want(TOKEN_RANDOM) || want(TOKEN_BINARY) || want(TOKEN_INPUT) || want(TOKEN_OUTPUT) || want(TOKEN_APPEND)) {
            openFor = previousToken.tokenval;
        } else {
            fail("Expected RANDOM, BINARY, INPUT, OUTPUT, or APPEND");
        }
    }
    args->append(new AstIntLiteralExpression(for_position, (int64) (openFor - TOKEN_RANDOM)));

    const SourcePosition access_position(currentToken.position);
    int openAccess = 1;
    if (want(TOKEN_ACCESS)) {
        openAccess = 0;
        if (want(TOKEN_READ)) {
            openAccess |= 1;
        }
        if (want(TOKEN_WRITE)) {
            openAccess |= 2;
        }
    }
    args->append(new AstIntLiteralExpression(access_position, openAccess));

    const SourcePosition lock_position(currentToken.position);
    int openLock = 0;
    if (want(TOKEN_SHARED)) {
        openLock = 0;
    } else if (want(TOKEN_LOCK)) {
        if (want(TOKEN_READ)) {
            openLock |= 1;
        }
        if (want(TOKEN_WRITE)) {
            openLock |= 2;
        }
    }
    args->append(new AstIntLiteralExpression(lock_position, openLock));

    need(TOKEN_AS, "Expected AS");
    want(TOKEN_HASH);  // optional '#' before file number.

    const SourcePosition filenum_position(currentToken.position);
    AstExpression *filenum = parseExpression();
    if (!filenum) {
        filenum = new AstIntLiteralExpression(filenum_position, 1);
    }
    args->append(filenum);

    const SourcePosition reclen_position(currentToken.position);
    AstExpression *reclen = NULL;
    if (wantIdentifier("LEN")) {  // !!! FIXME: it's messy to make this an actual keyword, but incorrect to not do so.
        need(TOKEN_ASSIGN, "Expected '='");
        reclen = parseExpression();
    }
    if (!reclen) {
        reclen = new AstIntLiteralExpression(reclen_position, -1);
    }
    args->append(reclen);
    return new AstSubCallStatement(position, "OPEN", args);
} // Parser::parseOpen

AstSubCallStatement *Parser::parseClose()
{
    std::vector<AstExpression*> fileids;
    const SourcePosition position(previousToken.position);

    do {
        want(TOKEN_HASH);  // optional '#' before file id.
        AstExpression *expr = parseExpression();
        if (!expr) {
            fail("Expected file number expression");
        } else {
            fileids.push_back(expr);
        }
    } while (want(TOKEN_COMMA));

    AstIntLiteralExpression *numids = new AstIntLiteralExpression(position, fileids.size());
    AstExpressionList *args = new AstExpressionList(position, numids);
    for (std::vector<AstExpression*>::iterator it = fileids.begin(); it != fileids.end(); ++it) {
        args->append(*it);
    }
    return new AstSubCallStatement(position, "CLOSE", args);
} // Parser::parseClose

AstExitStatement *Parser::parseExit() {
    const SourcePosition position(previousToken.position);
    Token type = TOKEN_FUNCTION;
    if (want(TOKEN_DEF) || want(TOKEN_DO) || want(TOKEN_FOR) || want(TOKEN_FUNCTION) || want(TOKEN_SUB)) {
        type = previousToken.tokenval;
    } else {
        fail("Expected DEF, DO, FOR, FUNCTION, or SUB");
    }
    return new AstExitStatement(position, type);
} // Parser::parseExit

AstStatement *Parser::parseEnd() {
    const SourcePosition position(previousToken.position);
    int64 exitcode = 0;
    if (want(TOKEN_NEWLINE) || want(TOKEN_COLON) || want(TOKEN_EOI)) {
        pushback();  // make sure needEndOfStatement() gets this token.
    } else if (want(TOKEN_INT_LITERAL)) {
        exitcode = previousToken.i64;
    } else {
        // might be an "END IF" or whatever, push it back and drop out.
        pushback();
        return NULL;
    }

    AstExpression *expr = new AstIntLiteralExpression(position, exitcode);
    AstExpressionList *args = new AstExpressionList(position, expr);
    return new AstSubCallStatement(position, "END", args);
} // Parser::parseEnd


// BASIC operator precedence: http://qbasic.phatcode.net/TUT/GDE/APCOPER.HTM#B

Token Parser::wantBinaryOperator() {
    const Token t = currentToken.tokenval;
    return (want(TOKEN_EXPONENT) || want(TOKEN_MINUS) || want(TOKEN_STAR) || want(TOKEN_SLASH) || want(TOKEN_BACKSLASH) ||
            want(TOKEN_MOD) || want(TOKEN_PLUS) || want(TOKEN_ASSIGN) || want(TOKEN_LESSTHAN) || want(TOKEN_GREATERTHAN) ||
            want(TOKEN_NEQ) || want(TOKEN_LEQ) || want(TOKEN_GEQ) || want(TOKEN_AND) || want(TOKEN_OR) ||
            want(TOKEN_XOR) || want(TOKEN_EQV) || want(TOKEN_IMP)) ? t : TOKEN_UNKNOWN;
} // Parser::wantBinaryOperator

static AstExpression *newAstExpressionByOperator(const SourcePosition &position, const Token op, AstExpression *l, AstExpression *r) {
    switch (op) {
        case TOKEN_MINUS: return new AstSubtractExpression(position, l, r);
        case TOKEN_STAR: return new AstMultiplyExpression(position, l, r);
        case TOKEN_SLASH: return new AstDivideExpression(position, l, r);
        case TOKEN_BACKSLASH: return new AstIntegerDivideExpression(position, l, r);
        case TOKEN_MOD: return new AstModuloExpression(position, l, r);
        case TOKEN_PLUS: return new AstAddExpression(position, l, r);
        case TOKEN_ASSIGN: return new AstEqualExpression(position, l, r);
        case TOKEN_LESSTHAN: return new AstLessThanExpression(position, l, r);
        case TOKEN_GREATERTHAN: return new AstGreaterThanExpression(position, l, r);
        case TOKEN_NEQ: return new AstNotEqualExpression(position, l, r);
        case TOKEN_LEQ: return new AstLessThanOrEqualExpression(position, l, r);
        case TOKEN_GEQ: return new AstGreaterThanOrEqualExpression(position, l, r);
        case TOKEN_AND: return new AstBinaryAndExpression(position, l, r);
        case TOKEN_OR: return new AstBinaryOrExpression(position, l, r);
        case TOKEN_XOR: return new AstBinaryXorExpression(position, l, r);
        case TOKEN_EQV: return new AstBinaryEqvExpression(position, l, r);
        case TOKEN_IMP: return new AstBinaryImpExpression(position, l, r);
        default: break;
    } // switch

    assert(!"shouldn't hit this");
    return NULL;
} // newAstExpressionByOperator

Token Parser::wantUnaryOperator() {
    const Token t = currentToken.tokenval;
    return (want(TOKEN_PLUS) || want(TOKEN_MINUS) || want(TOKEN_NOT)) ? t : TOKEN_UNKNOWN;
} // Parser::wantUnaryOperator

static AstExpression *newAstExpressionByOperator(const SourcePosition &position, const Token op, AstExpression *e) {
    switch (op) {
        case TOKEN_PLUS: return e; // doesn't mean anything, just drop the operation.
        case TOKEN_MINUS: return new AstNegateExpression(position, e);
        case TOKEN_NOT: return new AstNotExpression(position, e);
        default: break;
    } // switch

    assert(!"shouldn't hit this");
    return NULL;
} // newAstExpressionByOperator



static int operatorPrecedence(const Token op)
{
/*
According to http://www.qb64.net/forum/index.php?topic=5990.0:
()
^
*, / (left to right)
\
MOD
+, - (left to right)
=, <>, <, >, <=, >= (left to right)
NOT
AND
OR
XOR, EQV
IMP
*/
    switch (op) {
        case TOKEN_IMP: return 0;
        case TOKEN_XOR: return 1;
        case TOKEN_EQV: return 1;
        case TOKEN_OR: return 2;
        case TOKEN_AND: return 3;
        case TOKEN_NOT: return 4;
        case TOKEN_ASSIGN: return 5;  // this is '=' comparison operator.
        case TOKEN_NEQ: return 5;
        case TOKEN_LESSTHAN: return 5;
        case TOKEN_GREATERTHAN: return 5;
        case TOKEN_LEQ: return 5;
        case TOKEN_GEQ: return 5;
        case TOKEN_PLUS: return 6;
        case TOKEN_MINUS: return 6;
        case TOKEN_MOD: return 7;
        case TOKEN_BACKSLASH: return 8;
        case TOKEN_STAR: return 9;
        case TOKEN_SLASH: return 9;
        case TOKEN_EXPONENT: return 10;
        default: break;
    } // switch

    return -1;
}

// This is the "Precedence Climbing" algorithm.
// https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
AstExpression *Parser::parseExpression(const int precedence) {
    AstExpression *l = parseSubExpression();
    if (l) {
        Token op;
        while ((op = wantBinaryOperator()) != TOKEN_UNKNOWN) {
            const int thisprecedence = operatorPrecedence(op);
            if (thisprecedence < precedence) {
                pushback();
                break;
            } else {
                AstExpression *r = parseExpression(thisprecedence);
                l = newAstExpressionByOperator(l->position, op, l, r);
            }
        }
    }
    return l;
} // Parser::parseExpression

AstExpression *Parser::parseSubExpression() {
    const SourcePosition position(currentToken.position);
    Token op;
    if ((op = wantUnaryOperator()) != TOKEN_UNKNOWN) {
        const int thisprecedence = operatorPrecedence(op);
        return newAstExpressionByOperator(position, op, parseExpression(thisprecedence));
    } else if (want(TOKEN_LPAREN)) {
        AstExpression *e = parseExpression();
        need(TOKEN_RPAREN, "Expected ')'");
        return e;
    } else if (want(TOKEN_INT_LITERAL)) {
        return new AstIntLiteralExpression(position, previousToken.i64);
    } else if (want(TOKEN_FLOAT_LITERAL)) {
        return new AstFloatLiteralExpression(position, (float) previousToken.dbl);
    } else if (want(TOKEN_STRING_LITERAL)) {
        return new AstStringLiteralExpression(position, previousToken.string);
    } else if (want(TOKEN_IDENTIFIER)) {
        return parseIdentifierExpression(new AstIdentifierExpression(position, previousToken.string));
    }

    return NULL;
} // Parser::parseSubExpression


//assignment_statement
//label

// end of mojobasic_parser.cpp ...

