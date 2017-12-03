/**
 * MojoBASIC; a modern reimplementation of QuickBASIC.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// This file is the same as mojobasic_internal.h, but adds things that need to
//  talk to LLVM, since LLVM's headers greatly increase compile time.
//  That means the AST is in here, so the parser and compiler need it.

#ifndef _INCLUDE_MOJOBASIC_INTERNAL_LLVM_H_
#define _INCLUDE_MOJOBASIC_INTERNAL_LLVM_H_

#include "mojobasic_internal.h"

//#include "llvm/ADT/ArrayRef.h"
//#include "llvm/IR/LLVMContext.h"
//#include "llvm/IR/Module.h"
//#include "llvm/IR/Function.h"
//#include "llvm/IR/BasicBlock.h"
//#include "llvm/IR/IRBuilder.h"

// !!! FIXME: llvm-config --cxxflags turns off assertions with -DNDEBUG !!
#undef assert
#if 1
#define assert(cond) do { if (!(cond)) { fprintf(stderr, "ASSERT FAILED: %s (%s:%d)\n", #cond, __FILE__, __LINE__); fflush(stderr); __asm__ __volatile__ ("int $3\n\t"); } } while (0)
#else
#define assert(cond) do { (void) sizeof (cond); } while (0)
#endif

typedef uint64 AstDataType;  // !!! FIXME
#define AST_DATATYPE_NONE 0
#define AST_DATATYPE_BOOL 1
#define AST_DATATYPE_INT 2
#define AST_DATATYPE_UINT 3
#define AST_DATATYPE_SHORT 4
#define AST_DATATYPE_USHORT 5
#define AST_DATATYPE_FLOAT 6
#define AST_DATATYPE_DOUBLE 7
#define AST_DATATYPE_STRING 8

struct MOJOBASIC_program
{
    MOJOBASIC_program() : name(NULL), module(NULL) {}
    ~MOJOBASIC_program();
    const char *name;
    std::vector<MOJOBASIC_error> errors;
    void *module; //llvm::Module *module;
};

struct AstProgram;
struct AstProcedureSignature;
struct AstProcedureDeclaration;
struct AstProcedure;
struct AstNegateExpression;
struct AstNotExpression;
struct AstMultiplyExpression;
struct AstDivideExpression;
struct AstIntegerDivideExpression;
struct AstExponentExpression;
struct AstModuloExpression;
struct AstAddExpression;
struct AstSubtractExpression;
struct AstLessThanExpression;
struct AstGreaterThanExpression;
struct AstLessThanOrEqualExpression;
struct AstGreaterThanOrEqualExpression;
struct AstEqualExpression;
struct AstNotEqualExpression;
struct AstBinaryAndExpression;
struct AstBinaryXorExpression;
struct AstBinaryOrExpression;
struct AstBinaryEqvExpression;
struct AstBinaryImpExpression;
struct AstDereferenceArrayExpression;
struct AstIdentifierExpression;
struct AstIntLiteralExpression;
struct AstFloatLiteralExpression;
struct AstStringLiteralExpression;
struct AstBooleanLiteralExpression;
struct AstFunctionCallExpression;
struct AstStructDereferenceExpression;
struct AstExpressionList;
struct AstStatementBlock;
struct AstExitStatement;
struct AstSubCallStatement;
struct AstAssignmentStatement;
struct AstIfStatement;
struct AstDoStatement;
struct AstWhileStatement;
struct AstForStatement;
struct AstCase;
struct AstSelectStatement;
struct AstConstStatement;
struct AstVariableDeclaration;
struct AstVariableDeclarationStatement;
struct AstTypeDeclarationStatement;
struct AstDefStatement;
struct AstLineLabel;
struct AstOnErrorStatement;

class AstVisitor
{
public:
    virtual void visit(AstProgram *node) = 0;
    virtual void visit(AstProcedureSignature *node) = 0;
    virtual void visit(AstProcedureDeclaration *node) = 0;
    virtual void visit(AstProcedure *node) = 0;
    virtual void visit(AstNegateExpression *node) = 0;
    virtual void visit(AstNotExpression *node) = 0;
    virtual void visit(AstMultiplyExpression *node) = 0;
    virtual void visit(AstDivideExpression *node) = 0;
    virtual void visit(AstIntegerDivideExpression *node) = 0;
    virtual void visit(AstExponentExpression *node) = 0;
    virtual void visit(AstModuloExpression *node) = 0;
    virtual void visit(AstAddExpression *node) = 0;
    virtual void visit(AstSubtractExpression *node) = 0;
    virtual void visit(AstLessThanExpression *node) = 0;
    virtual void visit(AstGreaterThanExpression *node) = 0;
    virtual void visit(AstLessThanOrEqualExpression *node) = 0;
    virtual void visit(AstGreaterThanOrEqualExpression *node) = 0;
    virtual void visit(AstEqualExpression *node) = 0;
    virtual void visit(AstNotEqualExpression *node) = 0;
    virtual void visit(AstBinaryAndExpression *node) = 0;
    virtual void visit(AstBinaryXorExpression *node) = 0;
    virtual void visit(AstBinaryOrExpression *node) = 0;
    virtual void visit(AstBinaryEqvExpression *node) = 0;
    virtual void visit(AstBinaryImpExpression *node) = 0;
    virtual void visit(AstDereferenceArrayExpression *node) = 0;
    virtual void visit(AstIdentifierExpression *node) = 0;
    virtual void visit(AstIntLiteralExpression *node) = 0;
    virtual void visit(AstFloatLiteralExpression *node) = 0;
    virtual void visit(AstBooleanLiteralExpression *node) = 0;
    virtual void visit(AstStringLiteralExpression *node) = 0;
    virtual void visit(AstFunctionCallExpression *node) = 0;
    virtual void visit(AstStructDereferenceExpression *node) = 0;
    virtual void visit(AstExpressionList *node) = 0;
    virtual void visit(AstStatementBlock *node) = 0;
    virtual void visit(AstExitStatement *node) = 0;
    virtual void visit(AstSubCallStatement *node) = 0;
    virtual void visit(AstAssignmentStatement *node) = 0;
    virtual void visit(AstIfStatement *node) = 0;
    virtual void visit(AstDoStatement *node) = 0;
    virtual void visit(AstWhileStatement *node) = 0;
    virtual void visit(AstForStatement *node) = 0;
    virtual void visit(AstCase *node) = 0;
    virtual void visit(AstSelectStatement *node) = 0;
    virtual void visit(AstConstStatement *node) = 0;
    virtual void visit(AstVariableDeclaration *node) = 0;
    virtual void visit(AstVariableDeclarationStatement *node) = 0;
    virtual void visit(AstTypeDeclarationStatement *node) = 0;
    virtual void visit(AstDefStatement *node) = 0;
    virtual void visit(AstOnErrorStatement *node) = 0;
    virtual void visit(AstLineLabel *node) = 0;
};


// You can cast any AST node pointer to this.
class AstNode
{
public:
    virtual ~AstNode() {}
    const SourcePosition position;
    virtual void accept(AstVisitor *visitor) = 0;

protected:
    AstNode(const SourcePosition &pos) : position(pos) {}
};

struct AstStatement : public AstNode
{
    AstStatement(const SourcePosition &pos) : AstNode(pos), next(NULL) {}
    virtual ~AstStatement();
    AstStatement *next;
};

struct AstStatementBlock : public AstStatement  // a list of statements
{
    AstStatementBlock(const SourcePosition &pos, AstStatement *first) : AstStatement(pos), statements(first) {}
    virtual ~AstStatementBlock() { delete statements; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstStatement *statements;  // list of child statements.
};

struct AstExpression : public AstNode
{
    AstExpression(const SourcePosition &pos) : AstNode(pos), datatype(AST_DATATYPE_NONE), constant(true), ir(NULL), lvalue(false) {}
    AstDataType datatype;  // mostly not known until semantic analysis.
    bool constant;  // mostly not known until semantic analysis.
    void *ir; //llvm::Value *ir;  // NULL until IR generation.
    bool lvalue;
};

struct AstVariableDeclaration : public AstNode
{
    AstVariableDeclaration(const SourcePosition &pos, const char *_identifier, const char *_datatype, const int64 _recordsize, AstExpression *_lower, AstExpression *_upper)
        : AstNode(pos), identifier(_identifier), datatype(_datatype), recordsize(_recordsize), lower(_lower), upper(_upper), next(NULL)
    {}
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const char *identifier;  //  not a copy, don't delete.
    const char *datatype;  //  not a copy, don't delete.
    AstExpression *lower;
    AstExpression *upper;
    const int64 recordsize;
    AstVariableDeclaration *next;
};

struct AstVariableDeclarationStatement : public AstStatement
{
    AstVariableDeclarationStatement(const SourcePosition &pos, const bool _bIsShared, AstVariableDeclaration *_declaration)
        : AstStatement(pos), declaration(_declaration), bIsShared(_bIsShared), ir(NULL)
    {}
    virtual ~AstVariableDeclarationStatement() { delete declaration; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstVariableDeclaration *declaration;
    const bool bIsShared;
    void *ir; //llvm::Value *ir;  // NULL until IR generation.
};

struct AstTypeDeclarationStatement : public AstStatement
{
    AstTypeDeclarationStatement(const SourcePosition &pos, const char *_identifier, AstVariableDeclaration *_varlist)
        : AstStatement(pos), identifier(_identifier), varlist(_varlist) {}
    virtual ~AstTypeDeclarationStatement() { delete varlist; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const char *identifier;  //  not a copy, don't delete.
    AstVariableDeclaration *varlist;
};

struct AstDefStatement : public AstStatement
{
    AstDefStatement(const SourcePosition &pos, const char *_type, const char _a, const char _z)
        : AstStatement(pos), type(_type), a(_a), z(_z) {}
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const char *type;  //  not a copy, don't delete.
    const char a;
    const char z;
};

struct AstProcedureSignature : public AstNode
{
    AstProcedureSignature(const SourcePosition &pos, const bool bIsFunc, const char *name, AstVariableDeclaration *_args, const char *_rettype) : AstNode(pos), bIsFunction(bIsFunc), identifier(name), args(_args), rettype(_rettype) {}
    virtual ~AstProcedureSignature() { delete args; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const bool bIsFunction;
    const char *identifier;
    AstVariableDeclaration *args;
    const char *rettype;
};

struct AstProcedureDeclaration : public AstStatement
{
    AstProcedureDeclaration(const SourcePosition &pos, AstProcedureSignature *sig) : AstStatement(pos), signature(sig) {}
    virtual ~AstProcedureDeclaration() { delete signature; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstProcedureSignature *signature;
};

// Strictly speaking, these aren't statements, but they can appear in the middle of a pile of statements in the mainline.
//  AstProcedure covers both FUNCTION and SUB subroutines.
struct AstProcedure : public AstStatement
{
    AstProcedure(const SourcePosition &pos, AstProcedureSignature *decl, AstStatementBlock *stmts) : AstStatement(pos), declaration(decl), statements(stmts) {}
    virtual ~AstProcedure() { delete declaration; delete statements; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstProcedureSignature *declaration;
    AstStatementBlock *statements;
};

struct AstUnaryExpression : public AstExpression
{
    AstUnaryExpression(const SourcePosition &pos, AstExpression *_operand) : AstExpression(pos), operand(_operand) {}
    virtual ~AstUnaryExpression() { delete operand; }
    AstExpression *operand;
};

#define AST_UNARY_EXPR_STRUCT(name, op) struct Ast##name##Expression : public AstUnaryExpression { \
    Ast##name##Expression(const SourcePosition &pos, AstExpression *_operand) : AstUnaryExpression(pos, _operand) {} \
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); } \
}

AST_UNARY_EXPR_STRUCT(Negate, NEGATE);
AST_UNARY_EXPR_STRUCT(Not, NOT);

#undef AST_UNARY_EXPR_STRUCT


struct AstBinaryExpression : public AstExpression
{
    AstBinaryExpression(const SourcePosition &pos, AstExpression *_left, AstExpression *_right) : AstExpression(pos), left(_left), right(_right) {}
    virtual ~AstBinaryExpression() { delete left; delete right; }
    AstExpression *left;
    AstExpression *right;
};

#define AST_BINARY_EXPR_STRUCT(name, op) struct Ast##name##Expression : public AstBinaryExpression { \
    Ast##name##Expression(const SourcePosition &pos, AstExpression *_left, AstExpression *_right) : AstBinaryExpression(pos, _left, _right) {} \
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); } \
}

AST_BINARY_EXPR_STRUCT(Multiply, MULTIPLY);
AST_BINARY_EXPR_STRUCT(Divide, DIVIDE);
AST_BINARY_EXPR_STRUCT(IntegerDivide, DIVIDE);
AST_BINARY_EXPR_STRUCT(Exponent, EXPONENT);
AST_BINARY_EXPR_STRUCT(Modulo, MODULO);
AST_BINARY_EXPR_STRUCT(Add, ADD);
AST_BINARY_EXPR_STRUCT(Subtract, SUBTRACT);
AST_BINARY_EXPR_STRUCT(LessThan, LESSTHAN);
AST_BINARY_EXPR_STRUCT(GreaterThan, GREATERTHAN);
AST_BINARY_EXPR_STRUCT(LessThanOrEqual, LESSTHANOREQUAL);
AST_BINARY_EXPR_STRUCT(GreaterThanOrEqual, GREATERTHANOREQUAL);
AST_BINARY_EXPR_STRUCT(Equal, EQUAL);
AST_BINARY_EXPR_STRUCT(NotEqual, NOTEQUAL);
AST_BINARY_EXPR_STRUCT(BinaryAnd, BINARYAND);
AST_BINARY_EXPR_STRUCT(BinaryXor, BINARYXOR);
AST_BINARY_EXPR_STRUCT(BinaryOr, BINARYOR);
AST_BINARY_EXPR_STRUCT(BinaryEqv, BINARYEQV);
AST_BINARY_EXPR_STRUCT(BinaryImp, BINARYIMP);
AST_BINARY_EXPR_STRUCT(DereferenceArray, DEREF_ARRAY);

#undef AST_BINARY_EXPR_STRUCT

struct AstIdentifierExpression : public AstExpression
{
    AstIdentifierExpression(const SourcePosition &pos, const char *_identifier) : AstExpression(pos), identifier(_identifier), declaration(NULL) {}
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const char *identifier;  //  not a copy, don't delete.
    AstVariableDeclaration *declaration;
};

struct AstIntLiteralExpression : public AstExpression
{
    AstIntLiteralExpression(const SourcePosition &pos, const int64 _value) : AstExpression(pos), value(_value) { datatype = AST_DATATYPE_INT; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    int64 value;
};

struct AstFloatLiteralExpression : public AstExpression
{
    AstFloatLiteralExpression(const SourcePosition &pos, const float _value) : AstExpression(pos), value(_value) { datatype = AST_DATATYPE_FLOAT; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    float value;
};

struct AstBooleanLiteralExpression : public AstExpression
{
    AstBooleanLiteralExpression(const SourcePosition &pos, const bool _value) : AstExpression(pos), value(_value) { datatype = AST_DATATYPE_BOOL; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    bool value;
};

struct AstStringLiteralExpression : public AstExpression
{
    AstStringLiteralExpression(const SourcePosition &pos, const char *_value) : AstExpression(pos), value(_value) { datatype = AST_DATATYPE_STRING; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const char *value;  // this is strcache()'d, don't delete.
};

struct AstExpressionListItem
{
    AstExpressionListItem(AstExpression *_expr, const Token _op=TOKEN_UNKNOWN) : expr(_expr), next(NULL), op(_op) {}
    ~AstExpressionListItem() { delete expr; delete next; }
    AstExpression *expr;
    AstExpressionListItem *next;
    const Token op;  // this is used for CASE statements.
};

// !!! FIXME: turn this into a Collector, since we aren't doing a parser grammar?
struct AstExpressionList : public AstNode
{
    AstExpressionList(const SourcePosition &pos, AstExpression *first, const Token op=TOKEN_UNKNOWN) : AstNode(pos), list(NULL), tail(NULL) { tail = new AstExpressionListItem(first, op); list = tail; }
    virtual ~AstExpressionList() { delete list; }
    void append(AstExpression *expr, const Token op=TOKEN_UNKNOWN) { assert(tail != NULL); tail->next = new AstExpressionListItem(expr, op); tail = tail->next; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpressionListItem *list;
    AstExpressionListItem *tail;
};

struct AstFunctionCallExpression : public AstExpression
{
    AstFunctionCallExpression(const SourcePosition &pos, AstExpression *_fn, AstExpressionList *_args) : AstExpression(pos), fn(_fn), args(_args) {}
    virtual ~AstFunctionCallExpression() { delete args; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpression *fn;
    AstExpressionList *args;
};

struct AstStructDereferenceExpression : public AstExpression
{
    AstStructDereferenceExpression(const SourcePosition &pos, AstExpression *_parent, const char *_field) : AstExpression(pos), parent(_parent), field(_field) {}
    virtual ~AstStructDereferenceExpression() { delete parent; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpression *parent;
    const char *field;
};

struct AstExitStatement : public AstStatement
{
    AstExitStatement(const SourcePosition &pos, const Token _type) : AstStatement(pos), type(_type) {}
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const Token type;
};

struct AstSubCallStatement : public AstStatement
{
    AstSubCallStatement(const SourcePosition &pos, const char *_identifier, AstExpressionList *_args) : AstStatement(pos), identifier(_identifier), args(_args) {}
    virtual ~AstSubCallStatement() { delete args; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const char *identifier;
    AstExpressionList *args;
};

struct AstAssignmentStatement : public AstStatement
{
    AstAssignmentStatement(const SourcePosition &pos, AstExpression *_left, AstExpression *_right) : AstStatement(pos), left(_left), right(_right) {}
    virtual ~AstAssignmentStatement() { delete left; delete right; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpression *left;
    AstExpression *right;
};

struct AstIfStatement : public AstStatement
{
    AstIfStatement(const SourcePosition &pos, AstExpression *_expr, AstStatementBlock *_block, AstStatementBlock *_else_block) : AstStatement(pos), expr(_expr), block(_block), else_block(_else_block) {}
    virtual ~AstIfStatement() { delete expr; delete block; delete else_block; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpression *expr;
    AstStatementBlock *block;
    AstStatementBlock *else_block;
};

struct AstWhileStatement : public AstStatement
{
    AstWhileStatement(const SourcePosition &pos, AstExpression *_expr, AstStatementBlock *_block) : AstStatement(pos), expr(_expr), block(_block) {}
    virtual ~AstWhileStatement() { delete expr; delete block; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpression *expr;
    AstStatementBlock *block;
};

struct AstDoStatement : public AstStatement
{
    AstDoStatement(const SourcePosition &pos, const bool _bIsConditionalAtStart, const bool _bIsWhile, AstStatementBlock *_block, AstExpression *_expr) : AstStatement(pos), expr(_expr), block(_block), bIsConditionalAtStart(_bIsConditionalAtStart), bIsWhile(_bIsWhile) {}
    virtual ~AstDoStatement() { delete expr; delete block; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpression *expr;
    AstStatementBlock *block;
    const bool bIsConditionalAtStart;
    const bool bIsWhile;
};

struct AstForStatement : public AstStatement
{
    AstForStatement(const SourcePosition &pos, const char *_identifier, AstExpression *_initializer, AstExpression *_looptest, AstExpression *_step, AstStatementBlock *_block) : AstStatement(pos), identifier(pos, _identifier), initializer(_initializer), looptest(_looptest), step(_step), block(_block) { identifier.lvalue = true; }
    virtual ~AstForStatement() { delete initializer; delete looptest; delete step; delete block; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstIdentifierExpression identifier;
    AstExpression *initializer;
    AstExpression *looptest;
    AstExpression *step;
    AstStatementBlock *block;
};

struct AstCase : public AstNode
{
    AstCase(const SourcePosition &pos, AstExpressionList *_cases, AstStatementBlock *_block) : AstNode(pos), cases(_cases), block(_block), next(NULL) {}
    virtual ~AstCase() { delete cases; delete block; delete next; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpressionList *cases;
    AstStatementBlock *block;
    AstCase *next;
};

struct AstSelectStatement : public AstStatement
{
    AstSelectStatement(const SourcePosition &pos, AstExpression *_test, AstCase *_cases) : AstStatement(pos), test(_test), cases(_cases) {}
    virtual ~AstSelectStatement() { delete test; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstExpression *test;
    AstCase *cases;
};

struct AstConstStatement : public AstStatement
{
    AstConstStatement(const SourcePosition &pos, const char *_identifier, AstExpression *_initializer) : AstStatement(pos), identifier(_identifier), initializer(_initializer) {}
    virtual ~AstConstStatement() { delete initializer; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const char *identifier;
    AstExpression *initializer;
};

struct AstLineLabel : public AstStatement
{
    AstLineLabel(const SourcePosition &pos, const char *_identifier) : AstStatement(pos), identifier(_identifier) {}
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const char *identifier;
};

struct AstOnErrorStatement : public AstStatement
{
    AstOnErrorStatement(const SourcePosition &pos, const bool _bIsLocal, const char *_label) : AstStatement(pos), bIsLocal(_bIsLocal), label(_label) {}
    virtual ~AstOnErrorStatement() {}
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    const bool bIsLocal;
    const char *label;
};


struct AstProgram : public AstNode
{
    AstProgram(const SourcePosition &pos, AstStatementBlock *_block, const bool _bOptionExplicit, const int _optionBase) : AstNode(pos), block(_block), bOptionExplicit(_bOptionExplicit), optionBase(_optionBase) {}
    virtual ~AstProgram() { delete block; }
    virtual void accept(AstVisitor *visitor) { visitor->visit(this); }
    AstStatementBlock *block;
    const bool bOptionExplicit;
    const int optionBase;
};

void failCompile(void *c, const char *err, const SourcePosition &pos);
AstProgram *parseSource(void *ctx, StringCache &strcache, const char *filename, const char *source, unsigned int sourcelen, MOJOBASIC_includeOpen include_open, MOJOBASIC_includeClose include_close);

#endif  // _INCLUDE_MOJOBASIC_INTERNAL_LLVM_H_

// end of mojobasic_internal_llvm.h ...

