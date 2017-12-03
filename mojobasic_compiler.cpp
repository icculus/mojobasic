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
#include "mojobasic_internal_llvm.h"

//#include "llvm/IR/Verifier.h"
//#include "llvm/LinkAllPasses.h"

// Compile state, passed around all over the place.

class Compiler
{
public:
    Compiler(MOJOBASIC_program &_program, const char *filename, const char *source, unsigned int sourcelen,
             MOJOBASIC_includeOpen include_open, MOJOBASIC_includeClose include_close)
        : ast(NULL)
        , program(_program)
        , sourcefile(NULL)
        , sourceline(0)
        , isfail(false)
        , initialFilename(filename)
        , initialSource(source)
        , initialSourceLen(sourcelen)
        , includeOpenCallback(include_open)
        , includeCloseCallback(include_close)
    {}

    ~Compiler();

    void run();

    //void failf(const char *fmt, ...) ISPRINTF(2,3);
    //void fail(const char *reason) { failf("%s", reason); }
    //void warnf(const char *fmt, ...) ISPRINTF(2,3);
    //void warn(const char *reason) { warnf("%s", reason); }

    void failLocf(const SourcePosition &pos, const char *fmt, ...) ISPRINTF(3,4);
    void failLocVa(const SourcePosition &pos, const char *fmt, va_list ap);
    void failLoc(const SourcePosition &pos, const char *reason) { failLocf(pos, "%s", reason); }
    void warnLocf(const SourcePosition &pos, const char *fmt, ...) ISPRINTF(3,4);
    void warnLocVa(const SourcePosition &pos, const char *fmt, va_list ap);
    void warnLoc(const SourcePosition &pos, const char *reason) { warnLocf(pos, "%s", reason); }

    bool failed() const { return isfail; }

    const char *getSourceFile() const { return sourcefile; }
    unsigned int getSourceLine() const { return sourceline; }
    const char *getInitialSourceFile() const { return initialFilename; }

    AstProgram *ast;  // Abstract Syntax Tree
    std::vector<AstVariableDeclarationStatement*> inputs;
    std::vector<AstVariableDeclarationStatement*> outputs;
    std::vector<AstVariableDeclarationStatement*> uniforms;
    std::vector<AstVariableDeclarationStatement*> globals;

    MOJOBASIC_program &program;

private:
    StringCache strcache;
    const char *sourcefile;  // current source file that we're parsing.
    unsigned int sourceline; // current line in sourcefile that we're parsing.
    bool isfail;

    const char * const initialFilename;
    const char * const initialSource;
    const unsigned int initialSourceLen;
    MOJOBASIC_includeOpen const includeOpenCallback;
    MOJOBASIC_includeClose const includeCloseCallback;
};

// called from other source files so they don't need a full Compiler definition.
void failCompile(void *c, const char *err, const SourcePosition &pos)
{
    ((Compiler *) c)->failLoc(pos, err);
} // failCompile


static inline char *StrDup(const char *str)
{
    char *retval = new char[strlen(str) + 1];
    strcpy(retval, str);
    return retval;
} // StrDup

static void failVa(std::vector<MOJOBASIC_error> &errvec, const bool isfatal, const SourcePosition &pos, const char *fmt, va_list ap)
{
    MOJOBASIC_error err;
    memset(&err, '\0', sizeof (err));
    err.is_fatal = isfatal ? 1 : 0;
    err.error = mojoavsprintf(fmt, ap);
    err.filename = StrDup(pos.filename);
    err.error_position = pos.line;
    errvec.push_back(err);
} // failVa

#if 0
void Compiler::failf(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    failVa(program.errors, true, sourcepos, fmt, ap);
    va_end(ap);
    isfail = true;
} // Compiler::failf

void Compiler::warnf(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    failVa(program.errors, false, sourcepos, fmt, ap);
    va_end(ap);
} // Compiler::warnf
#endif

void Compiler::failLocf(const SourcePosition &pos, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    failVa(program.errors, true, pos, fmt, ap);
    va_end(ap);
    isfail = true;
} // Compiler::failLocf

void Compiler::warnLocf(const SourcePosition &pos, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    failVa(program.errors, false, pos, fmt, ap);
    va_end(ap);
} // Compiler::warnLocf

void Compiler::failLocVa(const SourcePosition &pos, const char *fmt, va_list ap)
{
    failVa(program.errors, true, pos, fmt, ap);
    isfail = true;
} // Compiler::failLocVa

void Compiler::warnLocVa(const SourcePosition &pos, const char *fmt, va_list ap)
{
    failVa(program.errors, false, pos, fmt, ap);
} // Compiler::warnLocVa


// Abstract Syntax Tree interface...

Compiler::~Compiler()
{
    delete ast;
} // Compiler::~Compiler

AstStatement::~AstStatement()
{
    // it's important not to recurse deeply here, since you may have
    //  thousands of items in this linked list (each line of a massive
    //  function, for example). To avoid this, we iterate the list here,
    //  deleting all children and making them think they have no reason
    //  to recurse in their own delete methods.

    AstStatement *i = this->next;
    this->next = NULL;
    while (i)
    {
        AstStatement *nextstmt = i->next;
        i->next = NULL;
        delete i;
        i = nextstmt;
    } // while
} // AstStatement::~AstStatement


#if DEBUG_COMPILER_AST
class AstPrinterVisitor : public AstVisitor
{
public:
    AstPrinterVisitor() : indent(-1) { printf("PARSER: visiting AST for pretty-printing...\n"); }
    ~AstPrinterVisitor() { printf("PARSER: ...pretty-printing visit complete.\n"); }
    virtual void visit(AstProgram *node) {
        if (node->bOptionExplicit) {
            printf("OPTION EXPLICIT\n");
        }
        if (node->optionBase == 0) {
            printf("OPTION BASE 0\n");
        }
        node->block->accept(this);
    }

    virtual void visit(AstProcedureDeclaration *node) {
        printf("DECLARE ");
        node->signature->accept(this);
    }

    virtual void visit(AstProcedure *node) {
        node->declaration->accept(this);
        node->statements->accept(this);
        printIndent();
        printf("END %s\n", node->declaration->bIsFunction ? "FUNCTION" : "SUB");
    }

    virtual void visit(AstTypeDeclarationStatement *node) {
        printf("TYPE %s\n", node->identifier);
        indent++;
        // this is a hack.
        for (AstVariableDeclaration *decl = node->varlist; decl; decl = decl->next) {
            printIndent();
            AstVariableDeclaration *next = decl->next;
            decl->next = NULL;
            decl->accept(this);
            printf("\n");
            decl->next = next;
        }
        indent--;
        printIndent();
        printf("END TYPE\n");
    }

    virtual void visit(AstDefStatement *node) {
        printf("DEF%s %c-%c\n", node->type, node->a, node->z);
    }

    virtual void visit(AstLineLabel *node) {
        printf("%s:\n", node->identifier);
    }

    virtual void visit(AstNegateExpression *node) { printf("-"); node->operand->accept(this); }
    virtual void visit(AstNotExpression *node) { printf("NOT "); node->operand->accept(this); }
    virtual void visit(AstMultiplyExpression *node) { visitBinaryExpr(node, "*"); }
    virtual void visit(AstDivideExpression *node) { visitBinaryExpr(node, "/"); }
    virtual void visit(AstIntegerDivideExpression *node) { visitBinaryExpr(node, "\\"); }
    virtual void visit(AstExponentExpression *node) { visitBinaryExpr(node, "^"); }
    virtual void visit(AstModuloExpression *node) { visitBinaryExpr(node, "MOD"); }
    virtual void visit(AstAddExpression *node) { visitBinaryExpr(node, "+"); }
    virtual void visit(AstSubtractExpression *node) { visitBinaryExpr(node, "-"); }
    virtual void visit(AstLessThanExpression *node) { visitBinaryExpr(node, "<"); }
    virtual void visit(AstGreaterThanExpression *node) { visitBinaryExpr(node, ">"); }
    virtual void visit(AstLessThanOrEqualExpression *node) { visitBinaryExpr(node, "<="); }
    virtual void visit(AstGreaterThanOrEqualExpression *node) { visitBinaryExpr(node, ">="); }
    virtual void visit(AstEqualExpression *node) { visitBinaryExpr(node, "="); }
    virtual void visit(AstNotEqualExpression *node) { visitBinaryExpr(node, "<>"); }
    virtual void visit(AstBinaryAndExpression *node) { visitBinaryExpr(node, "AND"); }
    virtual void visit(AstBinaryXorExpression *node) { visitBinaryExpr(node, "XOR"); }
    virtual void visit(AstBinaryOrExpression *node) { visitBinaryExpr(node, "OR"); }
    virtual void visit(AstBinaryEqvExpression *node) { visitBinaryExpr(node, "EQV"); }
    virtual void visit(AstBinaryImpExpression *node) { visitBinaryExpr(node, "IMP"); }
    virtual void visit(AstDereferenceArrayExpression *node) { node->left->accept(this); printf("["); node->right->accept(this); printf("]"); }
    virtual void visit(AstIdentifierExpression *node) { printf("%s", node->identifier); }
    virtual void visit(AstIntLiteralExpression *node) { printf("%lld", (long long) node->value); }
    virtual void visit(AstFloatLiteralExpression *node) { printf("%f", node->value); }
    virtual void visit(AstStringLiteralExpression *node) { printf("\"%s\"", node->value); }
    virtual void visit(AstBooleanLiteralExpression *node) { printf("%s", node->value ? "True" : "False"); }
    virtual void visit(AstFunctionCallExpression *node) { node->fn->accept(this); printf("("); if (node->args) { node->args->accept(this); } printf(")"); }
    virtual void visit(AstStructDereferenceExpression *node) { node->parent->accept(this); printf(".%s", node->field); }

    virtual void visit(AstExpressionList *node) {
        AstExpressionListItem *i = node->list;
        while (i) {
            assert(i->op == TOKEN_UNKNOWN);  // this "op" thing is a hack for SELECT statements.
            if (i->expr) {
                i->expr->accept(this);
            }
            i = i->next;
            if (i) {
                printf(", ");
            }
        }
    }

    virtual void visit(AstStatementBlock *node) {
        indent++;
        for (AstStatement *i = node->statements; i; i = i->next) {
            printIndent();
            i->accept(this);
        }
        indent--;
    }

    virtual void visit(AstSubCallStatement *node) {
        printf("%s ", node->identifier); if (node->args) { node->args->accept(this); } printf("\n");
    }

    virtual void visit(AstExitStatement *node) {
        printf("EXIT ");
        switch (node->type) {
            case TOKEN_DEF: printf("DEF"); break;
            case TOKEN_DO: printf("DO"); break;
            case TOKEN_FOR: printf("FOR"); break;
            case TOKEN_FUNCTION: printf("FUNCTION"); break;
            case TOKEN_SUB: printf("SUB"); break;
            default: assert(!"unexpected EXIT type"); printf("[ ? ? ? ]"); break;
        }
        printf("\n");
    }

    virtual void visit(AstAssignmentStatement *node) {
        node->left->accept(this);
        printf(" = ");
        node->right->accept(this);
        printf("\n");
    }

    virtual void visit(AstDoStatement *node) {
        printf("DO");
        if (node->expr && node->bIsConditionalAtStart) {
            printf(" %s ", node->bIsWhile ? "WHILE" : "UNTIL");
            node->expr->accept(this);
        }
        printf("\n");
        node->block->accept(this);
        printIndent();
        printf("LOOP");
        if (node->expr && !node->bIsConditionalAtStart) {
            printf(" %s ", node->bIsWhile ? "WHILE" : "UNTIL");
            node->expr->accept(this);
        }
        printf("\n");
    }

    virtual void visit(AstWhileStatement *node) {
        printf("WHILE ");
        node->expr->accept(this);
        printf("\n");
        node->block->accept(this);
        printIndent();
        printf("WEND\n");
    }

    virtual void visit(AstProcedureSignature *node) {
        printf("%s %s ", node->bIsFunction ? "FUNCTION" : "SUB", node->identifier);

        printf("(");
        if (node->args) {
            node->args->accept(this);
        }
        printf(")");

        if (node->rettype) {
            printf(" AS %s", node->rettype);
        }
        printf("\n");
    }

    virtual void visit(AstConstStatement *node) {
        printf("CONST %s = ", node->identifier);
        node->initializer->accept(this);
        printf("\n");
    }

    virtual void visit(AstIfStatement *node) {
        printf("IF "); node->expr->accept(this); printf(" THEN\n");
        if (node->block) {
            node->block->accept(this);
        }
        if (node->else_block) {
            printIndent();
            printf("ELSE\n");
            node->else_block->accept(this);
        }
        printIndent();
        printf("END IF\n");
    }

    virtual void visit(AstForStatement *node) {
        printf("FOR ");
        node->identifier.accept(this);
        printf(" = ");
        node->initializer->accept(this);
        printf(" TO ");
        node->looptest->accept(this);
        if (node->step) {
            printf(" STEP ");
            node->step->accept(this);
        }
        printf("\n");
        node->block->accept(this);
        printIndent();
        printf("NEXT\n");
    }

    virtual void visit(AstCase *node) {
        while (node) {
            if (node->cases) {
                //node->cases->accept(this);
                printIndent();
                printf("CASE ");
                AstExpressionListItem *i = node->cases->list;
                while (i) {
                    if (!i->expr) {
                        printf("ELSE");
                    } else {
                        switch (i->op) {
                            case TOKEN_TO: {
                                AstLessThanExpression *expr = (AstLessThanExpression *) i->expr;  // hack
                                expr->left->accept(this);
                                printf(" TO ");
                                expr->right->accept(this);
                                break;
                            }
                            case TOKEN_UNKNOWN: i->expr->accept(this); break;
                            case TOKEN_LESSTHAN: printf("IS < "); i->expr->accept(this); break;
                            case TOKEN_LEQ: printf("IS <= "); i->expr->accept(this); break;
                            case TOKEN_GREATERTHAN: printf("IS > "); i->expr->accept(this); break;
                            case TOKEN_GEQ: printf("IS >= "); i->expr->accept(this); break;
                            case TOKEN_NEQ: printf("IS <> "); i->expr->accept(this); break;
                            case TOKEN_ASSIGN: printf("IS = "); i->expr->accept(this); break;
                            default: assert(!"Unknown operator"); printf("IS ? ? ? "); i->expr->accept(this); break;
                        }
                    }
                    i = i->next;
                    if (i) {
                        printf(", ");
                    }
                }
            }
            printf("\n");
            node->block->accept(this);
            node = node->next;
        }
    }

    virtual void visit(AstSelectStatement *node) {
        printf("SELECT CASE ");
        node->test->accept(this);
        printf("\n");
        indent++;
        node->cases->accept(this);
        indent--;
        printIndent();
        printf("END SELECT\n");
    }

    virtual void visit(AstVariableDeclaration *node) {
        const char *sep = "";
        while (node) {
            printf("%s%s", sep, node->identifier);
            if (node->lower || node->upper) {
                printf("(");
                if (node->lower) {
                    node->lower->accept(this);
                    printf(" TO ");
                }
                if (node->upper) {
                    node->upper->accept(this);
                }
                printf(")");
            }

            if (node->datatype) {
                printf(" AS %s", node->datatype);
                if (node->recordsize >= 0) {
                    printf(" * %lld", (long long) node->recordsize);
                }
            }

            sep = ", ";
            node = node->next;
        }
    }

    virtual void visit(AstVariableDeclarationStatement *node) {
        printf("DIM ");
        if (node->bIsShared) {
            printf("SHARED ");
        }
        node->declaration->accept(this);
        printf("\n");
    }

    virtual void visit(AstOnErrorStatement *node) {
        printf("ON %sERROR %s%s\n", node->bIsLocal ? "LOCAL " : "", node->label ? "GOTO " : "RESUME NEXT", node->label ? node->label : "");
    }


#if 0
    static void printDataType(const AstDataType dt)
    {
        printf("%s", datatypes[baseDataType(dt)]);
        const unsigned int veccount = dataTypeVectorCount(dt);
        if (veccount > 1)
            printf("%u", veccount);
        if (isArrayDataType(dt))
            printf("[%llu]", (unsigned long long) dataTypeArrayLen(dt));
    } // printDataType
#endif

private:
    void visitBinaryExpr(const AstBinaryExpression *node, const char *opstr) { printf("("); node->left->accept(this); printf(" %s ", opstr); node->right->accept(this); printf(")"); }
    void printIndent() const { for (int i = 0; i < indent; i++) { printf("    "); } }
    int indent;
};
#endif


#if 0
// Go through the AST and make sure all datatypes check out okay, etc. We do
//  not permit implicit casting, so we fail here on incompatible datatypes.
// This means further processing can assume the AST is sane and not have to
//  spend effort verifying it again.
// This stage will also set every AST node's datatype field, if it is
//  meaningful to do so. This will allow conversion to IR to know what
//  type/size a given node is.
// Since it's easy to check here, we also make sure that break and continue
//  statements only appear inside loops.
// Also, variable references are linked to their declaration statements.
class AstSemanticAnalysisVisitor : public AstVisitor
{
public:
    AstSemanticAnalysisVisitor(Compiler &_compiler) : compiler(_compiler), loopCount(0), symmap(NULL) {}

    virtual void visit(AstProgram *node) {
        node->block->accept(this);

        // append the globals list to the end of the inputs list; they both look like function args, we just need them to be at the end of the list.
        //  (this is sort of hacky, but oh well.)
        for (std::vector<AstVariableDeclarationStatement*>::iterator it = compiler.uniforms.begin(); it != compiler.uniforms.end(); ++it)
            compiler.inputs.push_back(*it);
        compiler.uniforms.clear();

        for (std::vector<AstVariableDeclarationStatement*>::iterator it = compiler.globals.begin(); it != compiler.globals.end(); ++it)
            compiler.inputs.push_back(*it);
        compiler.globals.clear();
    }

    virtual void visit(AstNegateExpression *node) {
        node->operand->accept(this);
        mustBeNumeric(node->operand);
        mustBeSigned(node->operand);
        node->datatype = node->operand->datatype;
    }

    virtual void visit(AstComplementExpression *node) {
        node->operand->accept(this);
        mustBeInteger(node->operand);
        node->datatype = node->operand->datatype;
    }

    virtual void visit(AstNotExpression *node) {
        node->operand->accept(this);
        mustBeBoolean(node->operand);
        node->datatype = node->operand->datatype;
    }

    virtual void visit(AstMultiplyExpression *node) { visitBinaryMathExpression(node); }
    virtual void visit(AstDivideExpression *node) { visitBinaryMathExpression(node); }
    virtual void visit(AstIntegerDivideExpression *node) { visitBinaryMathExpression(node); }
    virtual void visit(AstExponentExpression *node) { visitBinaryMathExpression(node); }
    virtual void visit(AstModuloExpression *node) { visitBinaryMathExpression(node); }
    virtual void visit(AstAddExpression *node) { visitBinaryMathExpression(node); }
    virtual void visit(AstSubtractExpression *node) { visitBinaryMathExpression(node); }
    virtual void visit(AstLeftShiftExpression *node) { visitShiftExpression(node); }
    virtual void visit(AstRightShiftExpression *node) { visitShiftExpression(node); }
    virtual void visit(AstLessThanExpression *node) { visitComparisonExpression(node); }
    virtual void visit(AstGreaterThanExpression *node) { visitComparisonExpression(node); }
    virtual void visit(AstLessThanOrEqualExpression *node) { visitComparisonExpression(node); }
    virtual void visit(AstGreaterThanOrEqualExpression *node) { visitComparisonExpression(node); }
    virtual void visit(AstEqualExpression *node) { visitEqualityExpression(node); }
    virtual void visit(AstNotEqualExpression *node) { visitEqualityExpression(node); }
    virtual void visit(AstBinaryAndExpression *node) { visitBitwiseExpression(node); }
    virtual void visit(AstBinaryXorExpression *node) { visitBitwiseExpression(node); }
    virtual void visit(AstBinaryOrExpression *node) { visitBitwiseExpression(node); }
    virtual void visit(AstLogicalAndExpression *node) { visitLogicalExpression(node); }
    virtual void visit(AstLogicalOrExpression *node) { visitLogicalExpression(node); }

    virtual void visit(AstSwizzleExpression *node) {
        node->vec->accept(this);

        const unsigned int maxElems = dataTypeVectorCount(node->vec->datatype);
        if (maxElems < 2)
            fail(node, "Swizzles can only be used on vector types");

        assert(maxElems != 0);
        const char maxElemChar = (char) ('a' + (maxElems - 1));

        bool badSwizzle = false;
        unsigned int i;
        for (i = 0; (i < 8) && node->swizzlestr[i]; i++)
        {
            const char ch = node->swizzlestr[i];
            if ((ch < 'a') || (ch > maxElemChar))
            {
                if (ch != '\0')  // not end of string? Bogus character.
                    badSwizzle = true;
                break;
            } // if
            node->swizzle[i] = (int8) (ch - 'a');
        } // for

        assert(i > 0);  // Parser shouldn't allow an empty string here.

        unsigned int numElems = i;

        if ((numElems == 0) || (numElems > 8) || (node->swizzlestr[numElems] != '\0'))
            badSwizzle = true;

        if (badSwizzle)
            fail(node, "Invalid swizzle");

        node->datatype = baseDataType(node->vec->datatype) | ((numElems - 1) << AST_DATATYPE_VECTOR_SHIFT);
    }

    virtual void visit(AstDereferenceArrayExpression *node) {
        node->left->accept(this);
        node->right->accept(this);
        mustBeArray(node->left);
        mustBeScalarInteger(node->right);
        node->datatype = (node->left->datatype & (AST_DATATYPE_ARRAY_BIT-1));  // chop off the array bit and any array length information.
        node->constant = node->left->constant;
    }

    virtual void visit(AstConditionalExpression *node) {
        node->left->accept(this);
        node->center->accept(this);
        node->right->accept(this);
        mustBeBoolean(node->left);
        node->datatype = mustBeCompatibleDataTypes(node->center, node->right);
    }

    virtual void visit(AstIdentifierExpression *node) {
        SymbolMap *item;
        for (item = symmap; item; item = item->next)
        {
            if (!item->vardecl)
                continue;  // just moving to a different scope.
            else if (strcmp(item->vardecl->identifier, node->identifier) == 0)
                break;  // found it.
        } // for

        if (!item)
        {
            failf(node, "Variable '%s' not defined", node->identifier);
            node->datatype = AST_DATATYPE_INT;  // oh well.
        } // if
        else
        {
            node->datatype = item->vardecl->datatype;
            node->declaration = item->vardecl;
            node->constant = attributeIsConstant(item->vardecl->attribute);
        } // else
    }

    virtual void visit(AstIntLiteralExpression *node) { assert(node->datatype == AST_DATATYPE_INT); }
    virtual void visit(AstFloatLiteralExpression *node) { assert(node->datatype == AST_DATATYPE_FLOAT); }
    virtual void visit(AstBooleanLiteralExpression *node) { assert(node->datatype == AST_DATATYPE_BOOL); }

    virtual void visit(AstConstructorExpression *node) {
        assert(!isArrayDataType(node->datatype));  // parsing shouldn't allow this.
        const AstDataType baseType = baseDataType(node->datatype);
        const unsigned int neededElems = dataTypeVectorCount(node->datatype);
        node->expressions->accept(this);
        mustNotBeArray(node);
        unsigned int numElems = 0;
        for (AstExpressionListItem *i = node->expressions->list; i; i = i->next)
        {
            const AstDataType dt = i->expr->datatype;
            if (isArrayDataType(dt))
                fail(i->expr, "Cannot construct from an array type");
            else if (isVectorDataType(dt))
                fail(i->expr, "Cannot construct from a vector type"); // !!! FIXME: maybe float4(float3type, 3.0) should work, like HLSL, etc?

            // we treat scalar constructors as a cast operator.
            else if ((neededElems == 1) && (baseType != dt) && (baseType == AST_DATATYPE_BOOL))  // all scalar types will coerce here except bools.
                failf(i->expr, "Cannot construct to a boolean with a %s", datatypes[dt]);
            else if ((neededElems == 1) && (baseType != dt) && (dt == AST_DATATYPE_BOOL)) // all scalar types will coerce here except bools.
                failf(i->expr, "Cannot construct to a %s with a boolean", datatypes[dt]);

            // vector constructors need the exact scalar type (wrap it in a scalar constructor if necessary).
            else if ((neededElems > 1) && (baseType != dt))
                failf(i->expr, "Cannot construct to a %s%u from a %s", datatypes[baseType], neededElems, datatypes[dt]);

            numElems++;
        } // for

        if (numElems != neededElems)
        {
            const char numstr[2] = { (char) (neededElems == 1 ? '\0' : ('0' + neededElems)), '\0' };
            failf(node, "Specifying a %s%s with %d elements", datatypes[baseType], numstr, numElems);
        } // if
    }

    virtual void visit(AstFunctionCallExpression *node) {
        // (currently) we only allow function calls for intrinsic functions; there are no user-defined functions at the moment.
        failf(node, "Unknown function '%s'", node->identifier);
    }

    virtual void visit(AstSubCallStatement *node) {
        write me.
    }

    virtual void visit(AstStructDereferenceExpression *node) {
        write me.
    }

    virtual void visit(AstExpressionList *node) { for (AstExpressionListItem *i = node->list; i; i = i->next) { i->expr->accept(this); } }
    virtual void visit(AstStatementBlock *node) { pushScope(); for (AstStatement *i = node->statements; i; i = i->next) { i->accept(this); } popScope(); }
    virtual void visit(AstExitStatement *node) { if (!loopCount) fail(node, "'break' outside of a loop."); }
    virtual void visit(AstContinueStatement *node) { if (!loopCount) fail(node, "'continue' outside of a loop."); }
    virtual void visit(AstAssignmentStatement *node) { visitAssignmentStatement(node, false); }
    virtual void visit(AstMulAssignmentStatement *node) { visitAssignmentStatement(node); }
    virtual void visit(AstDivAssignmentStatement *node) { visitAssignmentStatement(node); }
    virtual void visit(AstModAssignmentStatement *node) { visitAssignmentStatement(node); }
    virtual void visit(AstAddAssignmentStatement *node) { visitAssignmentStatement(node); }
    virtual void visit(AstSubAssignmentStatement *node) { visitAssignmentStatement(node); }
    virtual void visit(AstLeftShiftAssignmentStatement *node) { visitBitwiseAssignmentStatement(node); }  // shift happens to match bitwise here, for now.
    virtual void visit(AstRightShiftAssignmentStatement *node) { visitBitwiseAssignmentStatement(node); }
    virtual void visit(AstAndAssignmentStatement *node) { visitBitwiseAssignmentStatement(node); }
    virtual void visit(AstXorAssignmentStatement *node) { visitBitwiseAssignmentStatement(node); }
    virtual void visit(AstOrAssignmentStatement *node) { visitBitwiseAssignmentStatement(node); }
    virtual void visit(AstDoStatement *node) { loopCount++; node->block->accept(this); loopCount--; node->expr->accept(this); }

    virtual void visit(AstWhileStatement *node) {
        node->expr->accept(this);
        mustBeBoolean(node->expr);
        loopCount++;
        node->block->accept(this);
        loopCount--;
    }

    virtual void visit(AstIfStatement *node) {
        node->expr->accept(this);
        mustBeBoolean(node->expr);
        node->block->accept(this);
        if (node->else_block) { node->else_block->accept(this); }
    }

    virtual void visit(AstForStatement *node) {
        node->identifier.accept(this);
        mustBeScalarInteger(&node->identifier);
        node->initializer->accept(this);
        mustBeCompatibleDataType(node->initializer, node->identifier.datatype);
        node->looptest->accept(this);
        mustBeCompatibleDataType(node->looptest, node->identifier.datatype);
        if (node->step) {
            node->step->accept(this);
            mustBeCompatibleDataType(node->step, node->identifier.datatype);
        } // if
        loopCount++; 
        node->block->accept(this);
        loopCount--;
    }

    virtual void visit(AstSelectStatement *node) {
sdsdfsdf
    }

    virtual void visit(AstCase *node) {
sdsdfsdf
    }

    virtual void visit(AstVariableDeclarationStatement *node) {
        for (SymbolMap *item = symmap; item && item->vardecl; item = item->next)
        {
            if (strcmp(item->vardecl->identifier, node->identifier) == 0)
            {
                failf(node, "Variable '%s' already defined in this scope", node->identifier);
                fail(item->vardecl, "Previous definition is here");
            } // if
        } // for

        pushSymbol(node);

        if (!node->initializer)
        {
            if (node->attribute == AST_VARATTR_CONST)
                fail(node, "Const symbols must have an initializer");
        } // if

        else
        {
            node->initializer->accept(this);
            mustBeCompatibleDataType(node->initializer, node->datatype);
            if ((node->attribute == AST_VARATTR_INPUT) || (node->attribute == AST_VARATTR_GLOBAL) || (node->attribute == AST_VARATTR_SYSTEM))
                 fail(node, "This variable type can not have an initializer");
        } // else

        // save these off so we know how to build the function signature during IR generation.
        if (node->attribute == AST_VARATTR_INPUT)
            compiler.inputs.push_back(node);
        else if (node->attribute == AST_VARATTR_OUTPUT)
            compiler.outputs.push_back(node);
        else if (node->attribute == AST_VARATTR_UNIFORM)
            compiler.uniforms.push_back(node);
        else if ((node->attribute == AST_VARATTR_SYSTEM) || (node->attribute == AST_VARATTR_GLOBAL))
            compiler.globals.push_back(node);
    }

private:
    void fail(const AstNode *node, const char *reason)
    {
        compiler.failLoc(node->position, reason);
    } // fail

    void failf(const AstNode *node, const char *fmt, ...)
    {
        va_list ap;
        va_start(ap, fmt);
        compiler.failLocVa(node->position, fmt, ap);
        va_end(ap);
    } // fail

    void mustBeNumeric(const AstExpression *expr)
    {
        const AstDataType datatype = expr->datatype;
        if ((baseDataType(datatype) == AST_DATATYPE_BOOL) || isArrayDataType(datatype))
            fail(expr, "Datatype must be numeric");
    } // mustBeNumeric

    void mustBeSigned(const AstExpression *expr)
    {
        const AstDataType datatype = expr->datatype;
        const AstDataType basetype = baseDataType(datatype);
        if (((basetype != AST_DATATYPE_INT) && (basetype != AST_DATATYPE_SHORT) && (basetype != AST_DATATYPE_FLOAT)) || isArrayDataType(datatype))
            fail(expr, "Datatype must be signed");
    } // mustBeSigned

    void mustBeBoolean(const AstExpression *expr)
    {
        if (expr->datatype != AST_DATATYPE_BOOL)
            fail(expr, "Datatype must be boolean");
    } // mustBeBoolean

    inline bool isScalarInteger(const AstDataType dt)
    {
        return (dt == AST_DATATYPE_INT) || (dt == AST_DATATYPE_UINT) || (dt == AST_DATATYPE_SHORT) || (dt == AST_DATATYPE_USHORT);
    } // isScalarInteger

    void mustBeScalarInteger(const AstExpression *expr)
    {
        if (!isScalarInteger(expr->datatype))
            fail(expr, "Datatype must be scalar integer");
    } // mustBeScalarInteger

    void mustBeInteger(const AstExpression *expr)
    {
        const AstDataType datatype = expr->datatype;
        if (!isScalarInteger(baseDataType(datatype)) || isArrayDataType(datatype))
            fail(expr, "Datatype must be integer");
    } // mustBeScalarInteger

    void mustBeArray(const AstExpression *expr)
    {
        if (!isArrayDataType(expr->datatype))
            fail(expr, "Datatype must be array");
    } // mustBeArray

    void mustNotBeArray(const AstExpression *expr)
    {
        if (isArrayDataType(expr->datatype))
            fail(expr, "Datatype must not be array");
    } // mustBeArray

    void mustBeMutable(const AstExpression *expr)
    {
        if (expr->constant)
            fail(expr, "Expression is read-only");
    } // mustBeMutable

    AstDataType mustBeCompatibleDataTypes(AstExpression *l, AstExpression *r)
    {
        assert(l->datatype != AST_DATATYPE_NONE);
        assert(r->datatype != AST_DATATYPE_NONE);

        if (l->datatype != r->datatype)  // we allow no implicit casts.
            fail(r, "Type mismatch");

        return l->datatype;
    } // mustBeCompatibleDataTypes

    AstDataType mustBeCompatibleDataType(AstExpression *r, const AstDataType datatype)
    {
        if (datatype != r->datatype)  // we allow no implicit casts.
            fail(r, "Type mismatch");
        return datatype;
    } // mustBeCompatibleDataType

    void visitAssignmentStatement(AstBaseAssignmentStatement *node, const bool isNumeric=true) {
        node->left->accept(this);
        node->right->accept(this);
        mustBeMutable(node->left);
        if (isNumeric)
        {
            mustBeNumeric(node->left);
            mustBeNumeric(node->right);
        } // if
        mustBeCompatibleDataType(node->right, node->left->datatype);
    }
    void visitBitwiseAssignmentStatement(AstBaseAssignmentStatement *node)
    {
        visitAssignmentStatement(node);
        mustBeInteger(node->left);
        mustBeInteger(node->right);
    } // visitBitwiseAssignmentStatement

    void visitBinaryMathExpression(AstBinaryExpression *node)
    {
        node->left->accept(this);
        node->right->accept(this);
        mustBeNumeric(node->left);
        mustBeNumeric(node->right);
        node->datatype = mustBeCompatibleDataTypes(node->left, node->right);
    } // visitBinaryMathExpression

    void visitBitwiseExpression(AstBinaryExpression *node)
    {
        node->left->accept(this);
        node->right->accept(this);
        mustBeInteger(node->left);
        mustBeInteger(node->right);
        node->datatype = mustBeCompatibleDataTypes(node->left, node->right);
    } // visitBitwiseExpression

    void visitShiftExpression(AstBinaryExpression *node)
    {
        node->left->accept(this);
        node->right->accept(this);
        mustBeInteger(node->left);
        mustBeInteger(node->right);
        node->datatype = node->left->datatype;
    } // visitShiftExpression

    void visitComparisonExpression(AstBinaryExpression *node)
    {
        node->left->accept(this);
        node->right->accept(this);
        mustBeNumeric(node->left);
        mustBeNumeric(node->right);
        mustNotBeArray(node->left);
        mustNotBeArray(node->right);
        mustBeCompatibleDataTypes(node->left, node->right);
        node->datatype = AST_DATATYPE_BOOL;
    } // visitComparisonExpression

    void visitEqualityExpression(AstBinaryExpression *node)
    {
        node->left->accept(this);
        node->right->accept(this);
        mustBeCompatibleDataTypes(node->left, node->right);
        mustNotBeArray(node->left);
        mustNotBeArray(node->right);
        node->datatype = AST_DATATYPE_BOOL;
    } // visitComparisonExpression

    void visitLogicalExpression(AstBinaryExpression *node)
    {
        node->left->accept(this);
        node->right->accept(this);
        mustBeBoolean(node->left);
        mustBeBoolean(node->right);
        node->datatype = AST_DATATYPE_BOOL;
    } // visitComparisonExpression

    struct SymbolMap
    {
        SymbolMap(AstVariableDeclarationStatement *v) : vardecl(v), next(NULL) {}
        AstVariableDeclarationStatement *vardecl;  // copy, don't delete.
        SymbolMap *next;
    };

    void pushSymbol(AstVariableDeclarationStatement *v)
    {
        SymbolMap *item = new SymbolMap(v);
        item->next = symmap;
        symmap = item;
    } // pushSymbol

    void pushScope()
    {
        pushSymbol(NULL);
    } // pushScope

    void popScope()
    {
        bool done = (symmap == NULL);
        while (!done)
        {
            SymbolMap *item = symmap;
            done = !item || (!item->vardecl);
            if (item)
                symmap = item->next;
            delete item;
        } // while
    } // popScope

    Compiler &compiler;
    unsigned int loopCount;
    SymbolMap *symmap;
};

// !!! FIXME: the macro salsa in here is a little nasty.
class AstIRBuilderVisitor : public AstVisitor
{
public:
    AstIRBuilderVisitor(Compiler &_compiler) : compiler(_compiler), context(llvm::getGlobalContext()), builder(context), loopstack(NULL) {}

    ~AstIRBuilderVisitor()
    {
        compiler.program.module->dump();
    }

    virtual void visit(AstProgram *node) {
        const char *fname = compiler.getInitialSourceFile();
        if (!fname)
            fname = "???";
        compiler.program.module = new llvm::Module(fname, context);

        llvm::Type *rettype = nullptr;
        const size_t numoutputs = compiler.outputs.size();
        if (numoutputs == 0)  // no outputs? return void.
            rettype = builder.getVoidTy();
        else if (numoutputs == 1)  // single output.
            rettype = getLLVMType(compiler.outputs.front()->datatype);
        else  // multiple outputs
        {
            std::vector<llvm::Type*> elements;
            for (std::vector<AstVariableDeclarationStatement*>::iterator it = compiler.outputs.begin(); it != compiler.outputs.end(); ++it)
                elements.push_back(getLLVMType((*it)->datatype));
            rettype = llvm::StructType::get(context, elements);
        } // else

        llvm::FunctionType *fntype = nullptr;

        const size_t numinputs = compiler.inputs.size();
        if (numinputs == 0)
            fntype = llvm::FunctionType::get(rettype, false);
        else
        {
            std::vector<llvm::Type*> paramtypes;
            for (std::vector<AstVariableDeclarationStatement*>::iterator it = compiler.inputs.begin(); it != compiler.inputs.end(); ++it)
                paramtypes.push_back(getLLVMType((*it)->datatype));
            fntype = llvm::FunctionType::get(rettype, paramtypes, false);
        } // else

        llvm::Function *mainFunc = llvm::Function::Create(fntype, llvm::Function::InternalLinkage, "", compiler.program.module);
        compiler.program.name = StrDup(node->identifier);
        compiler.program.function = mainFunc;

        size_t idx = 0;
        for (auto &arg : mainFunc->args())
        {
            arg.setName(compiler.inputs[idx]->identifier);
            compiler.inputs[idx]->ir = &arg;
            idx++;
        } // for

        builder.SetInsertPoint(llvm::BasicBlock::Create(context, "localvars", mainFunc));
        localVarInsertPoint = builder.saveIP();

        // Build the entire program in here. Visits every node in the AST.
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
        builder.SetInsertPoint(entry);
        assert(loopstack == NULL);
        node->block->accept(this);
        assert(loopstack == NULL);

        // Set up return, to pass output to the next node in the audio graph.
        if (compiler.outputs.size() == 0)  // no outputs? return void.
            builder.CreateRetVoid();
        else if (compiler.outputs.size() == 1)  // one output? return a value.
            builder.CreateRet(builder.CreateLoad(compiler.outputs.front()->ir->getType()->getPointerElementType(), compiler.outputs.front()->ir));
        else  // return an aggregate value.
        {
            llvm::Value **values = new llvm::Value*[compiler.outputs.size()];
            unsigned int i = 0;
            for (std::vector<AstVariableDeclarationStatement*>::iterator it = compiler.outputs.begin(); it != compiler.outputs.end(); ++it)
            {
                AstVariableDeclarationStatement *node = *it;
                values[i++] = builder.CreateLoad(node->ir->getType()->getPointerElementType(), node->ir);
            } // for
            builder.CreateAggregateRet(values, i);
            delete[] values;
        } // else

        // make sure the the basic block for allocating locals is properly terminated.
        builder.restoreIP(localVarInsertPoint);
        builder.CreateBr(entry);

        llvm::verifyFunction(*mainFunc);
    }

    virtual void visit(AstNegateExpression *node) {
        if (visitExpression(node->operand)) {
            const unsigned int numElems = dataTypeVectorCount(node->datatype);
            const AstDataType basetype = baseDataType(node->datatype);
            if (basetype == AST_DATATYPE_SHORT) {
                node->ir = builder.CreateSub(splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(16, 0, true))), node->operand->ir);
            } else if (basetype == AST_DATATYPE_INT) {
                node->ir = builder.CreateSub(splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(32, 0, true))), node->operand->ir);
            } else if (basetype == AST_DATATYPE_FLOAT) {
                node->ir = builder.CreateFSub(splat(numElems, llvm::ConstantFP::get(context, llvm::APFloat(0.0f))), node->operand->ir);
            }
        }
    }

    virtual void visit(AstComplementExpression *node) {
        if (visitExpression(node->operand)) {
            const unsigned int numElems = dataTypeVectorCount(node->datatype);
            const AstDataType basetype = baseDataType(node->datatype);
            if (basetype == AST_DATATYPE_SHORT) {
                node->ir = builder.CreateXor(node->operand->ir, splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(16, -1, true))));
            } else if (basetype == AST_DATATYPE_USHORT) {
                node->ir = builder.CreateXor(node->operand->ir, splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(16, 0xFFFF, false))));
            } else if (basetype == AST_DATATYPE_INT) {
                node->ir = builder.CreateXor(node->operand->ir, splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(32, -1, true))));
            } else if (basetype == AST_DATATYPE_UINT) {
                node->ir = builder.CreateXor(node->operand->ir, splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t) 0xFFFFFFFF, false))));
            }
        }
    }

    virtual void visit(AstNotExpression *node) {
        if (visitExpression(node->operand)) {
            node->ir = builder.CreateXor(node->operand->ir, llvm::ConstantInt::get(context, llvm::APInt(1, 1, false)));
        }
    }

    #define VISITBINEXPR_INT_ONLY(op) { \
        if (visitBinaryExpression(node)) { \
            node->ir = builder.Create##op(node->left->ir, node->right->ir); \
        } \
    }

    #define VISITBINEXPR(op) { \
        if (visitBinaryExpression(node)) { \
            if (baseDataType(node->datatype) == AST_DATATYPE_FLOAT) { \
                node->ir = builder.CreateF##op(node->left->ir, node->right->ir); \
            } else { \
                node->ir = builder.Create##op(node->left->ir, node->right->ir); \
            } \
        } \
    }

    virtual void visit(AstMultiplyExpression *node) { VISITBINEXPR(Mul); }
    virtual void visit(AstAddExpression *node) { VISITBINEXPR(Add); }
    virtual void visit(AstSubtractExpression *node) { VISITBINEXPR(Sub); }
    virtual void visit(AstLeftShiftExpression *node) { VISITBINEXPR_INT_ONLY(Shl); }
    virtual void visit(AstRightShiftExpression *node) { VISITBINEXPR_INT_ONLY(LShr); }

    #undef VISITBINEXPR_INT_ONLY
    #undef VISITBINEXPR

    #define VISITDIVEXPR(op) { \
        if (visitBinaryExpression(node)) { \
            switch (baseDataType(node->left->datatype)) { \
                case AST_DATATYPE_SHORT: \
                case AST_DATATYPE_INT: \
                    node->ir = builder.CreateS##op(node->left->ir, node->right->ir); \
                    break; \
                case AST_DATATYPE_USHORT: \
                case AST_DATATYPE_UINT: \
                    node->ir = builder.CreateU##op(node->left->ir, node->right->ir); \
                    break; \
                case AST_DATATYPE_FLOAT: \
                    node->ir = builder.CreateF##op(node->left->ir, node->right->ir); \
                    break; \
                default: \
                    assert(!"fixme"); \
                    break; \
            } \
        } \
    }

    virtual void visit(AstDivideExpression *node) { VISITDIVEXPR(Div); }
    virtual void visit(AstModuloExpression *node) { VISITDIVEXPR(Rem); }

    #undef VISITDIVEXPR


    #define VISITCOMPARE(op) { \
        if (visitBinaryExpression(node)) { \
            switch (node->left->datatype) { \
                case AST_DATATYPE_SHORT: \
                case AST_DATATYPE_INT: \
                    node->ir = builder.CreateICmpS##op(node->left->ir, node->right->ir); \
                    break; \
                case AST_DATATYPE_USHORT: \
                case AST_DATATYPE_UINT: \
                    node->ir = builder.CreateICmpU##op(node->left->ir, node->right->ir); \
                    break; \
                case AST_DATATYPE_FLOAT: \
                    node->ir = builder.CreateFCmpO##op(node->left->ir, node->right->ir); \
                    break; \
                default: \
                    assert(!"fixme"); \
                    break; \
            } \
        } \
    }

    #define VISITCOMPARE_EQ(op) { \
        if (visitBinaryExpression(node)) { \
            if (baseDataType(node->left->datatype) == AST_DATATYPE_FLOAT) { \
                node->ir = builder.CreateFCmpO##op(node->left->ir, node->right->ir); \
            } else { \
                node->ir = builder.CreateICmp##op(node->left->ir, node->right->ir); \
            } \
        } \
    }

    virtual void visit(AstLessThanExpression *node) { VISITCOMPARE(LT); }
    virtual void visit(AstGreaterThanExpression *node) { VISITCOMPARE(GT); }
    virtual void visit(AstLessThanOrEqualExpression *node) { VISITCOMPARE(LE); }
    virtual void visit(AstGreaterThanOrEqualExpression *node) { VISITCOMPARE(GE); }
    virtual void visit(AstEqualExpression *node) { VISITCOMPARE_EQ(EQ); }
    virtual void visit(AstNotEqualExpression *node) { VISITCOMPARE_EQ(NE); }

    #undef VISITCOMPARE
    #undef VISITCOMPARE_EQ


    #define VISITBITWISE(op) { \
        if (visitBinaryExpression(node)) { \
            node->ir = builder.Create##op(node->left->ir, node->right->ir); \
        } \
    }

    virtual void visit(AstBinaryAndExpression *node) { VISITBITWISE(And); }
    virtual void visit(AstBinaryXorExpression *node) { VISITBITWISE(Xor); }
    virtual void visit(AstBinaryOrExpression *node)  { VISITBITWISE(Or); }

    virtual void visit(AstLogicalAndExpression *node) {
        if (visitExpression(node->left)) {
            llvm::BasicBlock *startblock = builder.GetInsertBlock();
            llvm::Function *func = startblock->getParent();
            llvm::BasicBlock *trueblock = llvm::BasicBlock::Create(context, "andtrue", func);
            llvm::BasicBlock *finalblock = llvm::BasicBlock::Create(context, "andfinal", func);
            llvm::Value *lval = builder.CreateICmpNE(node->left->ir, llvm::ConstantInt::get(context, llvm::APInt(1, 0, false)));

            builder.CreateCondBr(lval, trueblock, finalblock);
            builder.SetInsertPoint(trueblock);
            if (visitExpression(node->right)) {
                llvm::Value *rval = builder.CreateICmpNE(node->right->ir, llvm::ConstantInt::get(context, llvm::APInt(1, 0, false)));
                builder.CreateBr(finalblock);
                builder.SetInsertPoint(finalblock);
                llvm::PHINode *phi = builder.CreatePHI(builder.getInt1Ty(), 2);
                phi->addIncoming(lval, startblock);
                phi->addIncoming(rval, trueblock);
                node->ir = phi;
            }
        }
    }

    virtual void visit(AstLogicalOrExpression *node) {
        if (visitExpression(node->left)) {
            llvm::BasicBlock *startblock = builder.GetInsertBlock();
            llvm::Function *func = startblock->getParent();
            llvm::BasicBlock *falseblock = llvm::BasicBlock::Create(context, "orfalse", func);
            llvm::BasicBlock *finalblock = llvm::BasicBlock::Create(context, "orfinal", func);
            llvm::Value *lval = builder.CreateICmpNE(node->left->ir, llvm::ConstantInt::get(context, llvm::APInt(1, 0, false)));

            builder.CreateCondBr(lval, finalblock, falseblock);
            builder.SetInsertPoint(falseblock);
            if (visitExpression(node->right)) {
                llvm::Value *rval = builder.CreateICmpNE(node->right->ir, llvm::ConstantInt::get(context, llvm::APInt(1, 0, false)));
                builder.CreateBr(finalblock);
                builder.SetInsertPoint(finalblock);
                llvm::PHINode *phi = builder.CreatePHI(builder.getInt1Ty(), 2);
                phi->addIncoming(lval, startblock);
                phi->addIncoming(rval, falseblock);
                node->ir = phi;
            }
        }
    }

    virtual void visit(AstDereferenceArrayExpression *node) {
        if (visitBinaryExpression(node))
        {
            assert(isArrayDataType(node->left->datatype));  // semantic analysis should have caught this.
            std::vector<llvm::Value*> indices;
            indices.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, 0, false)));
            indices.push_back(node->right->ir);
            llvm::Value *gep = builder.CreateInBoundsGEP(node->left->ir, indices);
            if (node->lvalue)
                node->ir = gep;
            else
                node->ir = builder.CreateLoad(getLLVMType(node->left->datatype)->getPointerElementType(), gep);
        } // if
    }

    virtual void visit(AstSwizzleExpression *node) {
        if (visitExpression(node->vec))
        {
            // if we are swizzling but not changing the datatype, we can use a shufflevector instruction.
            //  if we changed the datatype (like: int2(1,2).aaaaa), we need to build a new vector here.
            if (node->datatype == node->vec->datatype)
            {
                assert(isVectorDataType(node->datatype));
                const unsigned int numElems = dataTypeVectorCount(node->datatype);
                assert(numElems <= STATICARRAYLEN(node->swizzle));
                std::vector<int> shufflemask;
                for (unsigned int i = 0; i < numElems; i++)
                    shufflemask.push_back((int) node->swizzle[i]);
                node->ir = builder.CreateShuffleVector(node->vec->ir, llvm::UndefValue::get(getLLVMType(node->datatype)), shufflemask);
            } // if

            // vector size changed? Build a new object.

            else if (!isVectorDataType(node->datatype))  // build a scalar
                node->ir = builder.CreateExtractElement(node->vec->ir, (uint64_t) node->swizzle[0]);

            else   // build a vector
            {
                const unsigned int numElems = dataTypeVectorCount(node->datatype);
                assert(numElems <= STATICARRAYLEN(node->swizzle));

                // see comments in AstConstructorExpression visitor about constant folding.
                llvm::Value *ir = llvm::UndefValue::get(getLLVMType(node->datatype));
                for (unsigned int i = 0; i < numElems; i++)
                {
                    llvm::Value *val = builder.CreateExtractElement(node->vec->ir, (uint64_t) node->swizzle[i]);
                    ir = builder.CreateInsertElement(ir, val, (uint64_t) i);
                } // for
                node->ir = ir;
            } // else
        } // if
    }

    virtual void visit(AstConditionalExpression *node) {
        if (visitExpression(node->left) && visitExpression(node->center) && visitExpression(node->right))
            node->ir = builder.CreateSelect(node->left->ir, node->center->ir, node->right->ir);
    }

    virtual void visit(AstIdentifierExpression *node) {
        // lvalues need the unmolested llvm::Value*, so they can generate store instructions.
        // Likewise, arrays will do a GEP elsewhere.
        // Most things are locals or function arguments eventually, which aren't necessarily backed by memory, so no CreateLoad for them.
        if ((node->lvalue) || (node->datatype & AST_DATATYPE_ARRAY_BIT) || ((node->declaration->attribute != AST_VARATTR_STATIC) && (node->declaration->attribute != AST_VARATTR_PERSISTENT)))
            node->ir = node->declaration->ir;
        else
            node->ir = builder.CreateLoad(getLLVMType(node->datatype), node->declaration->ir);
    }

    virtual void visit(AstIntLiteralExpression *node) { node->ir = llvm::ConstantInt::get(context, llvm::APInt(32, node->value, true)); }
    virtual void visit(AstFloatLiteralExpression *node) { node->ir = llvm::ConstantFP::get(context, llvm::APFloat(node->value)); }
    virtual void visit(AstBooleanLiteralExpression *node) { node->ir = llvm::ConstantInt::get(context, llvm::APInt(1, node->value ? 1 : 0, false)); }

    virtual void visit(AstConstructorExpression *node) {
        node->expressions->accept(this);
        if (!isVectorDataType(node->datatype))
            node->ir = castExpr(baseDataType(node->datatype), node->expressions->list->expr);  // not a vector, pass it through (as a cast).
        else
        {
            // The IRBuilder will constant-fold this into an inline vector if all values are constant. So this...
            //    a = int4(1,2,3,4);
            // ...becomes this...
            //    store <4 x i32> <i32 1, i32 2, i32 3, i32 4>, <4 x i32>* %a
            // ...but if any values aren't constant, it has to generate insertelement instructions, as expected.
            // However, it can't fold all the constants into a single vector if there's a non-constant in the middle,
            //  as IRBuilder's constant-folder doesn't keep enough state for that between calls:
            //    // "uniform int z; local int4 a = int4(1,2,z,4);"
            //    %0 = load i32, i32* @z
            //    %1 = insertelement <4 x i32> <i32 1, i32 2, i32 undef, i32 undef>, i32 %0, i64 2
            //    %2 = insertelement <4 x i32> %1, i32 4, i64 3
            //
            // The "%1" could have folded into this and %2 removed:
            //    %1 = insertelement <4 x i32> <i32 1, i32 2, i32 undef, i32 4>, i32 %0, i64 2
            // It doesn't look like -O3 optimizations even catch this (but codegen on x86_64, at least, turned out identical, fwiw.)
            //
            // So we take some efforts to insert all the constant elements first, to help the folder, and then insert the non-constants.
            llvm::Value *ir = llvm::UndefValue::get(getLLVMType(node->datatype));
            uint64_t idx = 0;
            for (AstExpressionListItem *i = node->expressions->list; i; i = i->next)
            {
                llvm::Constant *c = llvm::dyn_cast<llvm::Constant>(i->expr->ir);
                if (c != nullptr)
                    ir = builder.CreateInsertElement(ir, c, idx);
                idx++;
            } // for

            idx = 0;
            for (AstExpressionListItem *i = node->expressions->list; i; i = i->next)
            {
                llvm::Constant *c = llvm::dyn_cast<llvm::Constant>(i->expr->ir);
                if (c == nullptr)
                    ir = builder.CreateInsertElement(ir, i->expr->ir, idx);
                idx++;
            } // for
            node->ir = ir;
        } // else
    }

    virtual void visit(AstFunctionCallExpression *node) {  /* !!! FIXME: no actual intrinsic functions yet, and we don't allow user functions. */ }
    virtual void visit(AstSubCallStatement *node) {  /* !!! FIXME: no actual intrinsic functions yet, and we don't allow user functions. */ }
    virtual void visit(AstStructDereferenceExpression *node) { write me. }

    virtual void visit(AstExpressionList *node) { for (AstExpressionListItem *i = node->list; i; i = i->next) { i->expr->accept(this); } }
    virtual void visit(AstStatementBlock *node) { for (AstStatement *i = node->statements; i; i = i->next) { i->accept(this); } }

    virtual void visit(AstVariableDeclarationStatement *node) {
        publishSymbol(node);

        llvm::Type *llvmtype = getLLVMType(node->datatype);

        if ((node->attribute == AST_VARATTR_LOCAL) || (node->attribute == AST_VARATTR_CONST) || (node->attribute == AST_VARATTR_OUTPUT))
        {
            // Move stack allocations to start block of function, so we're not reallocating on each iteration of a loop, etc.
            llvm::IRBuilder<>::InsertPoint currentIP = builder.saveIP();
            builder.restoreIP(localVarInsertPoint);
            node->ir = builder.CreateAlloca(llvmtype, nullptr, node->identifier);
            localVarInsertPoint = builder.saveIP();
            builder.restoreIP(currentIP);
        } // if
        else if (node->attribute == AST_VARATTR_INPUT)
        {
            assert(node->ir != NULL);  // we cheated and set this up in AstProgram's visitor.
        } // else if
        else if ((node->attribute == AST_VARATTR_STATIC) || (node->attribute == AST_VARATTR_PERSISTENT))  // works like C's "static," so the optimizer can know it is only accessed in this module.
        {
            node->ir = new llvm::GlobalVariable(*compiler.program.module, llvmtype, false, llvm::GlobalVariable::InternalLinkage, nullptr, node->identifier);
        } // else
        else  // everything else just looks like a global variable at this level.
        {
            assert(node->ir != NULL);  // we cheated and set this up in AstProgram's visitor.
        } // else

        if (!node->ir)
            return;

        if (node->initializer)
        {
            if (visitExpression(node->initializer))
                builder.CreateStore(node->initializer->ir, node->ir);
        } // if
        else
        {
            const AstDataType dt = node->datatype;
            if ((node->attribute == AST_VARATTR_LOCAL) && (!isArrayDataType(dt)))  // !!! FIXME: memset arrays?
            {
                const unsigned int numElems = dataTypeVectorCount(dt);
                switch (baseDataType(dt))
                {
                    case AST_DATATYPE_BOOL: builder.CreateStore(splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(1, 0, false))), node->ir); break;
                    case AST_DATATYPE_SHORT: builder.CreateStore(splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(16, 0, true))), node->ir); break;
                    case AST_DATATYPE_USHORT: builder.CreateStore(splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(16, 0, false))), node->ir); break;
                    case AST_DATATYPE_INT: builder.CreateStore(splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(32, 0, true))), node->ir); break;
                    case AST_DATATYPE_UINT: builder.CreateStore(splat(numElems, llvm::ConstantInt::get(context, llvm::APInt(32, 0, false))), node->ir); break;
                    case AST_DATATYPE_FLOAT: builder.CreateStore(splat(numElems, llvm::ConstantFP::get(context, llvm::APFloat(0.0f))), node->ir); break;
                    default: assert(!"uhoh"); break;
                } // switch
            } // if
        } // else
    }

    virtual void visit(AstExitStatement *node) {
        assert(loopstack != NULL);  // semantic analysis should have caught this.
        builder.CreateBr(loopstack->breakblock);
    }

    virtual void visit(AstContinueStatement *node) {
        assert(loopstack != NULL);  // semantic analysis should have caught this.
        assert(loopstack->continueblock != NULL);  // semantic analysis should have caught this: a "switch" can't "continue", for example.
        builder.CreateBr(loopstack->continueblock);
    }

    virtual void visit(AstAssignmentStatement *node) {
        if (visitExpression(node->left) && visitExpression(node->right)) {
            builder.CreateStore(node->right->ir, node->left->ir);
        }
    }

    #define VISITOPASSIGN(op) { \
        assert((node->left->datatype & AST_DATATYPE_ARRAY_BIT) == 0);  /* semantic analysis should have caught this. */ \
        if (visitExpression(node->left) && visitExpression(node->right)) { \
            assert(node->left->lvalue);  /* want the visit() to give us the declaration's IR, not a load operation. */ \
            llvm::Value *load = builder.CreateLoad(getLLVMType(node->left->datatype), node->left->ir); \
            llvm::Value *ir = NULL; \
            if (baseDataType(node->left->datatype) == AST_DATATYPE_FLOAT) { \
                ir = builder.CreateF##op(load, node->right->ir); \
            } else { \
                ir = builder.Create##op(load, node->right->ir); \
            } \
            builder.CreateStore(ir, node->left->ir); \
        } \
    }

    #define VISITDIVASSIGN(op) { \
        assert((node->left->datatype & AST_DATATYPE_ARRAY_BIT) == 0);  /* semantic analysis should have caught this. */ \
        if (visitExpression(node->left) && visitExpression(node->right)) { \
            assert(node->left->lvalue);  /* want the visit() to give us the declaration's IR, not a load operation. */ \
            llvm::Value *load = builder.CreateLoad(getLLVMType(node->left->datatype), node->left->ir); \
            llvm::Value *ir = NULL; \
            switch (baseDataType(node->left->datatype)) { \
                case AST_DATATYPE_SHORT: \
                case AST_DATATYPE_INT: \
                    ir = builder.CreateS##op(load, node->right->ir); \
                    break; \
                case AST_DATATYPE_USHORT: \
                case AST_DATATYPE_UINT: \
                    ir = builder.CreateU##op(load, node->right->ir); \
                    break; \
                case AST_DATATYPE_FLOAT: \
                    ir = builder.CreateF##op(load, node->right->ir); \
                    break; \
                default: \
                    assert(!"fixme"); \
                    break; \
            } \
            builder.CreateStore(ir, node->left->ir); \
        } \
    }

    #define VISITBITWISEOPASSIGN(op) { \
        if (visitExpression(node->left) && visitExpression(node->right)) { \
            assert(node->left->lvalue);  /* want the visit() to give us the declaration's IR, not a load operation. */ \
            builder.CreateStore(builder.Create##op(builder.CreateLoad(getLLVMType(node->left->datatype), node->left->ir), node->right->ir), node->left->ir); \
        } \
    }

    virtual void visit(AstMulAssignmentStatement *node) { VISITOPASSIGN(Mul); }
    virtual void visit(AstAddAssignmentStatement *node) { VISITOPASSIGN(Add); }
    virtual void visit(AstSubAssignmentStatement *node) { VISITOPASSIGN(Sub); }
    virtual void visit(AstLeftShiftAssignmentStatement *node) { VISITBITWISEOPASSIGN(Shl); }
    virtual void visit(AstRightShiftAssignmentStatement *node) { VISITBITWISEOPASSIGN(LShr); }
    virtual void visit(AstAndAssignmentStatement *node) { VISITBITWISEOPASSIGN(And); }
    virtual void visit(AstXorAssignmentStatement *node) { VISITBITWISEOPASSIGN(Xor); }
    virtual void visit(AstOrAssignmentStatement *node) { VISITBITWISEOPASSIGN(Or); }
    virtual void visit(AstDivAssignmentStatement *node) { VISITDIVASSIGN(Div); }
    virtual void visit(AstModAssignmentStatement *node) { VISITDIVASSIGN(Rem); }

    #undef VISITOPASSIGN
    #undef VISITDIVASSIGN
    #undef VISITBITWISEOPASSIGN

    virtual void visit(AstIfStatement *node) {
        if (visitExpression(node->expr)) {
            llvm::Function *func = builder.GetInsertBlock()->getParent();
            llvm::BasicBlock *trueblock = llvm::BasicBlock::Create(context, "iftrue", func);
            llvm::BasicBlock *falseblock = (node->else_block != NULL) ? llvm::BasicBlock::Create(context, "iffalse", func) : NULL;
            llvm::BasicBlock *finalblock = llvm::BasicBlock::Create(context, "iffinal", func);
            builder.CreateCondBr(node->expr->ir, trueblock, falseblock ? falseblock : finalblock);
            builder.SetInsertPoint(trueblock);
            node->block->accept(this);
            builder.CreateBr(finalblock);
            if (falseblock) {
                builder.SetInsertPoint(falseblock);
                node->else_block->accept(this);
                builder.CreateBr(finalblock);
            }
            builder.SetInsertPoint(finalblock);
        }
    }

    virtual void visit(AstDoStatement *node) {
        llvm::Function *func = builder.GetInsertBlock()->getParent();
        llvm::BasicBlock *doblock = llvm::BasicBlock::Create(context, "do", func);
        llvm::BasicBlock *testblock = llvm::BasicBlock::Create(context, "dotest", func);
        llvm::BasicBlock *finalblock = llvm::BasicBlock::Create(context, "doend", func);
        builder.CreateBr(doblock);
        builder.SetInsertPoint(doblock);
        pushLoop(finalblock, testblock);
        node->block->accept(this);
        builder.CreateBr(testblock);
        builder.SetInsertPoint(testblock);
        if (visitExpression(node->expr)) {
            builder.CreateCondBr(node->expr->ir, doblock, finalblock);
            builder.SetInsertPoint(finalblock);
        }
        popLoop();
    }

    virtual void visit(AstWhileStatement *node) {
        llvm::Function *func = builder.GetInsertBlock()->getParent();

        llvm::BasicBlock *testblock = llvm::BasicBlock::Create(context, "whiletest", func);
        builder.CreateBr(testblock);  // this block ends, jump into new one.
        builder.SetInsertPoint(testblock);

        llvm::BasicBlock *loopblock = llvm::BasicBlock::Create(context, "while", func);
        llvm::BasicBlock *doneblock = llvm::BasicBlock::Create(context, "whileend", func);

        pushLoop(doneblock, testblock);
        if (visitExpression(node->expr)) {
            builder.CreateCondBr(node->expr->ir, loopblock, doneblock);
            builder.SetInsertPoint(loopblock);
            node->block->accept(this);
            builder.CreateBr(testblock);  // go back to run loop test again.
        }

        builder.SetInsertPoint(doneblock);
        popLoop();
    }

    virtual void visit(AstSelectStatement *node) {
sdfsdf
    }

    virtual void visit(AstCase *node) {
sdfsdf
    }

    virtual void visit(AstForStatement *node) {
        llvm::Function *func = builder.GetInsertBlock()->getParent();

        node->identifier.accept(this);
        if (!node->identifier.ir)
            return;
        else if (!visitExpression(node->initializer))
            return;

        assert(node->identifier.lvalue);  // want the visit() to not generate any load instructions.
        builder.CreateStore(node->initializer->ir, node->identifier.declaration->ir);  // store out the initial value.

        if (!visitExpression(node->looptest))  // this is constant, evaluate it once at startup.
            return;

        llvm::Value *step = nullptr;
        if (!node->step)
            step = llvm::ConstantInt::get(context, llvm::APInt(node->identifier.ir->getType()->getPointerElementType()->getScalarSizeInBits(), 1, true));
        else if (!visitExpression(node->step))  // this is constant, evaluate it once at startup.
            return;
        else
            step = node->step->ir;

        llvm::BasicBlock *testblock = llvm::BasicBlock::Create(context, "fortest", func);
        llvm::BasicBlock *loopblock = llvm::BasicBlock::Create(context, "for", func);
        llvm::BasicBlock *incrblock = llvm::BasicBlock::Create(context, "forincr", func);
        llvm::BasicBlock *doneblock = llvm::BasicBlock::Create(context, "forend", func);

        builder.CreateBr(testblock);   // terminate init code's block by jumping to test block.
        builder.SetInsertPoint(testblock);

        llvm::Value *testvar = builder.CreateLoad(getLLVMType(node->identifier.declaration->datatype), node->identifier.declaration->ir);
        builder.CreateCondBr(builder.CreateICmpSLE(testvar, node->looptest->ir), loopblock, doneblock);  // !!! FIXME: needs to work with unsigned stuff too.

        builder.SetInsertPoint(loopblock);
        pushLoop(doneblock, incrblock);
        node->block->accept(this);
        builder.CreateBr(incrblock);

        builder.SetInsertPoint(incrblock);
        llvm::Value *incrvar = builder.CreateLoad(getLLVMType(node->identifier.declaration->datatype), node->identifier.declaration->ir);
        builder.CreateStore(builder.CreateAdd(incrvar, step), node->identifier.declaration->ir);
        builder.CreateBr(testblock);

        builder.SetInsertPoint(doneblock);
        popLoop();
    }

private:
    Compiler &compiler;
    llvm::LLVMContext &context;
    llvm::IRBuilder<> builder;
    llvm::IRBuilder<>::InsertPoint localVarInsertPoint;

    struct LoopStack
    {
        llvm::BasicBlock *breakblock;
        llvm::BasicBlock *continueblock;
        LoopStack *next;
    };
    LoopStack *loopstack;

    void pushLoop(llvm::BasicBlock *breakblock, llvm::BasicBlock *continueblock)
    {
        LoopStack *item = new LoopStack;
        item->breakblock = breakblock;
        item->continueblock = continueblock;
        item->next = loopstack;
        loopstack = item;
    } // pushLoop

    void popLoop()
    {
        assert(loopstack != NULL);
        LoopStack *loop = loopstack;
        loopstack = loopstack->next;
        delete loop;
    } // popLoop

    llvm::Value *splat(const unsigned int numElems, llvm::Value *val)
    {
        if (numElems > 1)
            return builder.CreateVectorSplat(numElems, val);
        return val;
    } // splat

    void publishSymbol(const AstVariableDeclarationStatement *node)
    {
        if (!attributeIsPublished(node->attribute))
            return;

        MOJOBASIC_variableAttribute attribute;
        switch (node->attribute)
        {
            #define ATTRCASE(x) case AST_VARATTR_##x: attribute = MOJOBASIC_VARATTR_##x; break
            ATTRCASE(UNIFORM);
            ATTRCASE(INPUT);
            ATTRCASE(OUTPUT);
            ATTRCASE(GLOBAL);
            #undef ATTRCASE
            default: assert(!"update attributeIsPublished, please"); return;
        } // switch

        char *name = new char[strlen(node->identifier) + 1];
        strcpy(name, node->identifier);

        const AstDataType dt = node->datatype;

        MOJOBASIC_symbol sym;
        memset(&sym, '\0', sizeof (sym));
        sym.name = name;
        sym.attribute = attribute;
        sym.datatype = (MOJOBASIC_datatype) (baseDataType(dt) - 1);  // these match up.
        sym.vector_elements = dataTypeVectorCount(dt);
        sym.array_elements = dataTypeArrayLen(dt);
        compiler.program.symbols.push_back(sym);
    } // publishSymbol

    llvm::Type *getLLVMType(const AstDataType dt)
    {
        llvm::Type *llvmtype = nullptr;

        const AstDataType basetype = baseDataType(dt);
        const unsigned int numElems = dataTypeVectorCount(dt);

        switch (basetype)
        {
            case AST_DATATYPE_FLOAT: llvmtype = builder.getFloatTy(); break;
            case AST_DATATYPE_BOOL: llvmtype = builder.getInt1Ty(); break;
            case AST_DATATYPE_SHORT: llvmtype = builder.getInt16Ty(); break;
            case AST_DATATYPE_USHORT: llvmtype = builder.getInt16Ty(); break;
            case AST_DATATYPE_INT: llvmtype = builder.getInt32Ty(); break;
            case AST_DATATYPE_UINT: llvmtype = builder.getInt32Ty(); break;
            default: assert(!"uhoh"); return NULL;
        } // switch

        if (numElems > 1)  // it's a vector type.
            llvmtype = llvm::VectorType::get(llvmtype, numElems);

        if (dt & AST_DATATYPE_ARRAY_BIT)
        {
            const uint64 len = (dt & AST_DATATYPE_ARRAY_LENGTH_MASK) >> AST_DATATYPE_ARRAY_LENGTH_SHIFT;
            llvmtype = llvm::ArrayType::get(llvmtype, len);
        } // if

        return llvmtype;
    } // getLLVMType

    void fail(const AstNode *node, const char *reason)
    {
        compiler.failLoc(node->position, reason);
    } // fail

    void failf(const AstNode *node, const char *fmt, ...)
    {
        va_list ap;
        va_start(ap, fmt);
        compiler.failLocVa(node->position, fmt, ap);
        va_end(ap);
    } // fail

    bool visitExpression(AstExpression *node)
    {
        node->accept(this);
        if (node->ir == NULL)
        {
            fail(node, "LLVM IR generation failed here");
            return false;
        } // if
        return true;
    }

    llvm::Value *castExpr(const AstDataType castTo, AstExpression *expr)
    {
        llvm::Value *retval = nullptr;
        const AstDataType castFrom = expr->datatype;

        assert(!isArrayDataType(castTo));  // semantic analysis should have caught this.
        assert(!isArrayDataType(castFrom));  // semantic analysis should have caught this.

        const AstDataType srcBase = baseDataType(castFrom);
        const AstDataType dstBase = baseDataType(castTo);
        const unsigned int srcElems = dataTypeVectorCount(castFrom);
        const unsigned int dstElems = dataTypeVectorCount(castTo);

        assert(srcElems == dstElems);  // semantic analysis should have caught this.
        assert(srcBase != AST_DATATYPE_BOOL);  // semantic analysis should have caught this.
        assert(dstBase != AST_DATATYPE_BOOL);  // semantic analysis should have caught this.
        assert(srcBase != AST_DATATYPE_NONE);  // semantic analysis should have caught this.
        assert(dstBase != AST_DATATYPE_NONE);  // semantic analysis should have caught this.

        llvm::Type *dstLLVMType = getLLVMType(castTo);
        llvm::Value *ir = expr->ir;

        if (castTo == castFrom)
            retval = ir;  // just pass it through without generating any conversion instructions.

        else if (srcBase == AST_DATATYPE_FLOAT)
        {
            if ((dstBase == AST_DATATYPE_SHORT) || (dstBase == AST_DATATYPE_INT))
                retval = builder.CreateFPToSI(ir, dstLLVMType);
            else if ((dstBase == AST_DATATYPE_USHORT) || (dstBase == AST_DATATYPE_UINT))
                retval = builder.CreateFPToUI(ir, dstLLVMType);
        } // if

        // source must be integer type, then.
        else if ((srcBase == AST_DATATYPE_SHORT) || (srcBase == AST_DATATYPE_INT))
        {
            if (dstBase == AST_DATATYPE_FLOAT)
                retval = builder.CreateSIToFP(ir, dstLLVMType);
            else // dest must be integer type, then.
                retval = builder.CreateSExtOrTrunc(ir, dstLLVMType);
        } // else if

        else if ((srcBase == AST_DATATYPE_USHORT) || (srcBase == AST_DATATYPE_UINT))
        {
            if (dstBase == AST_DATATYPE_FLOAT)
                retval = builder.CreateUIToFP(ir, dstLLVMType);
            else // dest must be integer type, then.
                retval = builder.CreateZExtOrTrunc(ir, dstLLVMType);
        } // else if

        assert(retval != NULL);  // uhoh, did we forget a conversion?
        return retval;
    }

    bool visitBinaryExpression(AstBinaryExpression *node)
    {
        return visitExpression(node->left) && visitExpression(node->right);
    } // visitBinaryExpression
};
#endif

void Compiler::run()
{
    ast = parseSource(this, strcache, initialFilename, initialSource, initialSourceLen, includeOpenCallback, includeCloseCallback);
    if (ast)  // parsing didn't totally failed?
    {
        #if DEBUG_COMPILER_AST
        { AstPrinterVisitor visitor; ast->accept(&visitor); }  // debug builds print out original AST.
        #endif
        
        #if 0
        { AstSemanticAnalysisVisitor visitor(*this); ast->accept(&visitor); }

        if (!failed())  // don't generate IR if we had other problems.
        {
            AstIRBuilderVisitor visitor(*this);
            ast->accept(&visitor);
        } // if
        #endif
    } // if
} // Compiler::run


MOJOBASIC_program *MOJOBASIC_compileProgram(const char *filename, const char *source, unsigned int sourcelen,
                                            MOJOBASIC_includeOpen include_open, MOJOBASIC_includeClose include_close)
{
    MOJOBASIC_program *retval = new MOJOBASIC_program;
    // !!! FIXME: llvm-config --cxxflags is turning off exceptions and rtti, so we can't try/catch here.
    //try {
        Compiler compiler(*retval, filename, source, sourcelen, include_open, include_close);
        compiler.run();
    //} catch (std::bad_alloc) {
    //    return NULL;  // uhoh, ran out of memory.
    //}
    return retval;
} // MOJOBASIC_compileProgram


MOJOBASIC_program::~MOJOBASIC_program()
{
    delete[] name;
    //delete module;

    //for (std::vector<MOJOBASIC_symbol>::iterator it = symbols.begin(); it != symbols.end(); ++it)
    //    delete[] (*it).name;

    for (std::vector<MOJOBASIC_error>::iterator it = errors.begin(); it != errors.end(); ++it)
    {
        delete[] (*it).error;
        delete[] (*it).filename;
    } // for
} // MOJOBASIC_program::~MOJOBASIC_program


void MOJOBASIC_freeProgram(MOJOBASIC_program *program)
{
    if (program)
        delete program;
} // MOJOBASIC_freeProgram


unsigned int MOJOBASIC_getProgramErrorCount(const MOJOBASIC_program *program)
{
    return program->errors.size();
} // MOJOBASIC_getProgramErrorCount

const MOJOBASIC_error *MOJOBASIC_getProgramError(const MOJOBASIC_program *program, const unsigned int idx)
{
    return (idx < program->errors.size()) ? &program->errors[idx] : NULL;
} // MOJOBASIC_getProgramError

// end of mojobasic_compiler.cpp ...

