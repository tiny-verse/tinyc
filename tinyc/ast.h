#pragma once

#include <unordered_map>
#include <memory>

#include "common/helpers.h"
#include "common/ast.h"
#include "common/lexer.h"
#include "common/symbol.h"

namespace tinyc {
    class Type;

    using Token = tiny::Token;
    using Symbol = tiny::Symbol;
    using ParserError = tiny::ParserError;
    using ASTBase = tiny::ASTBase;
    using ASTPrettyPrinter = tiny::ASTPrettyPrinter;

    class ASTVisitor;

    class AST : public ASTBase {
    public:

        /** Returns the backend type of the AST expression.

            After a successful type checking this must never be nullptr.
         */
        Type * type() const {
            return type_;
        }

        /** Sets the type for the expression in the AST node. 
         
            The type must *not* be nullptr. Setting type twice is an error unless the type is identical.
         */
        void setType(Type * t) {
            if (t == nullptr)
                throw ParserError("Incorrect types", location());
            if (type_ != nullptr && type_ != t)
                throw ParserError("Different type already set", location());
            type_ = t;
        }

        /** Returns true if the result of the expression has an address.

            A bit simplified version of l-values from C++. This is important for two things:

            1) an address (ASTAddress) can only be obtained of expressions that have address themselves.
            2) only elements that have address can be assigned to
         */
        virtual bool hasAddress() const {
            return false;
        }

    protected:

        friend class ASTVisitor;

        AST(Token const & t):
            ASTBase{t} {
        }

        Type * type_ = nullptr;

        virtual void accept(ASTVisitor * v) = 0;
    };

    class ASTInteger : public AST {
    public:
        int64_t value;

        ASTInteger(Token const & t):
            AST{t},
            value{t.valueInt()} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.numberLiteral << value;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTDouble : public AST {
    public:
        double value;

        ASTDouble(Token const & t):
            AST{t},
            value{t.valueDouble()} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.numberLiteral << value;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTChar : public AST {
    public:
        char value;

        ASTChar(Token const & t):
            AST{t} {
            std::string const & s{t.valueString()};
            if (t == Token::Kind::StringDoubleQuoted)
                throw ParserError(STR("Expected character (single quote), but string \"" << s << "\" (double quote) found"), t.location(), false);
            if (s.size() != 1)
                throw ParserError(STR("Expected single character, but " << s.size() << " characters found in '" << s << "'"), t.location(), false);
            value = s[0];
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.charLiteral << value;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTString : public AST {
    public:
        std::string value;

        ASTString(Token const & t):
            AST{t} {
            std::string const & s{t.valueString()};
            if (t == Token::Kind::StringSingleQuoted)
                throw ParserError(STR("Expected string (double quote), but character '" << s << "' (single quote) found"), t.location(), false);
            value = s;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.stringLiteral << value;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTIdentifier : public AST {
    public:
        Symbol name;

        ASTIdentifier(Token const & t):
            AST{t},
            name{t.valueSymbol()} {
        }

        /** An identifier in read position is a variable read and all variables have addresses.
         */
        bool hasAddress() const override {
            return true;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.identifier << name.name();
        }

        void accept(ASTVisitor * v) override;

    };

    /** Base class for all types.

     */
    class ASTType : public AST {
    public:
        std::string toString() const {
            std::stringstream ss;
            buildStringRepresentation(ss);
            return ss.str();
        }

    protected:
        ASTType(Token const & t):
            AST{t} {
        }

    protected:
        virtual void buildStringRepresentation(std::ostream & s) const = 0;

        // friendship is not inherited, but methods are
        void toString(ASTType const * type, std::ostream & s) const {
            type->buildStringRepresentation(s);
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTPointerType : public ASTType {
    public:
        std::unique_ptr<ASTType> base;

        ASTPointerType(Token const & t, std::unique_ptr<ASTType> base):
            ASTType{t},
            base{std::move(base)} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << (*base) << p.symbol << "*";
        }

        void buildStringRepresentation(std::ostream & s) const override {
            toString(base.get(), s);
            s << "*";
        };

        void accept(ASTVisitor * v) override;

    };

    class ASTArrayType : public ASTType {
    public:
        std::unique_ptr<ASTType> base;
        std::unique_ptr<AST> size;

        ASTArrayType(Token const & t, std::unique_ptr<ASTType> base, std::unique_ptr<AST> size):
            ASTType{t},
            base{std::move(base)},
            size{std::move(size)} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << (*base) << p.symbol << "[" << *size << p.symbol << "]";
        }

        void buildStringRepresentation(std::ostream & s) const override {
            toString(base.get(), s);
            s << "[]";
        };

        void accept(ASTVisitor * v) override;

    };

    class ASTNamedType : public ASTType {
    public:
        Symbol name;

        ASTNamedType(Token const & t) :
            ASTType{t},
            name{t.valueSymbol()} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.type << name.name();
        }

        void buildStringRepresentation(std::ostream & s) const override {
            s << name.name();
        };

        void accept(ASTVisitor * v) override;

    };

    // comma separated, single line
    class ASTSequence : public AST {
    public:
        std::vector<std::unique_ptr<AST>> body;

        ASTSequence(Token const & t):
            AST{t} {
        }

        /** The result of sequence has address if its last element has address as the last element is what is returned.
         */
        bool hasAddress() const override {
            if (body.empty())
                return false;
            return body.back()->hasAddress();
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            auto i = body.begin();
            if (i != body.end()) {
                p << **i;
                while (++i != body.end())
                    p << p.symbol << ", " << **i;
            }
        }

        void accept(ASTVisitor * v) override;

    };

    // new line separated with {}
    class ASTBlock : public ASTSequence {
    public:
        ASTBlock(Token const & t):
            ASTSequence{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.symbol << "{";
            p.indent();
            for (auto & i : body) {
                p.newline();
                p << *i;
            }
            p.dedent();
            p.newline();
            p << p.symbol << "}";
            p.newline();
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTVarDecl : public AST {
    public:
        std::unique_ptr<ASTType> type;
        std::unique_ptr<ASTIdentifier> name;
        std::unique_ptr<AST> value;

        ASTVarDecl(Token const & t, std::unique_ptr<ASTType> type):
            AST{t},
            type{std::move(type)} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << (*type) << " " << (*name);
            if (value.get() != nullptr)
                p << p.symbol << " = " << (*value);
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTFunDecl : public AST {
    public:
        std::unique_ptr<ASTType> typeDecl;
        Symbol name;
        std::vector<std::pair<std::unique_ptr<ASTType>, std::unique_ptr<ASTIdentifier>>> args;
        std::unique_ptr<AST> body;

        ASTFunDecl(Token const & t, std::unique_ptr<ASTType> type):
            AST{t},
            typeDecl{std::move(type)},
            name{t.valueSymbol()} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << (*typeDecl) << " " << p.identifier << name.name() << p.symbol << "(";
            auto i = args.begin();
            if (i != args.end()) {
                p << *(i->first) << " " << *(i->second);
                while (++i != args.end())
                    p << p.symbol << ", " << *(i->first) << " " << *(i->second);
            }
            p << p.symbol << ")" << (*body);
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTStructDecl : public AST {
    public:
        Symbol name;
        std::vector<std::pair<std::unique_ptr<ASTIdentifier>, std::unique_ptr<ASTType>>> fields;

        /** If true the struct has also field definitions. Extra flag is necessary because empty fields can also mean an empty struct.
         */
        bool isDefinition = false;

        ASTStructDecl(Token const & t, Symbol name):
            AST{t},
            name{name} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "struct " << name.name();
            if (isDefinition) {
                p << p.symbol << "{";
                p.indent();
                for (auto & i : fields) {
                    p.newline();
                    p << *(i.first) << " " << *(i.second) << p.symbol << ";";
                }
                p.dedent();
                p.newline();
                p << p.symbol << "}";
            }
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTFunPtrDecl : public AST {
    public:
        std::unique_ptr<ASTIdentifier> name;
        std::vector<std::unique_ptr<ASTType>> args;
        std::unique_ptr<ASTType> returnType;

        ASTFunPtrDecl(Token const & t, std::unique_ptr<ASTIdentifier> name, std::unique_ptr<ASTType> returnType):
            AST{t},
            name{std::move(name)},
            returnType{std::move(returnType)} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "typedef " << (*returnType) << p.symbol << "( *" << (*name) << p.symbol << ")(";
            auto i = args.begin();
            if (i != args.end()) {
                p << **i;
                while (++i != args.end())
                    p << p.symbol << ", " << **i;
            }
            p << p.symbol << ")";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTIf : public AST {
    public:
        std::unique_ptr<AST> cond;
        std::unique_ptr<AST> trueCase;
        std::unique_ptr<AST> falseCase;

        ASTIf(Token const & t):
            AST{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "if " << p.symbol << "(" << *cond << p.symbol << ")" << *trueCase;
            if (falseCase.get() != nullptr)
                p << p.keyword << "else" << *falseCase;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTSwitch : public AST {
    public:
        std::unique_ptr<AST> cond;
        AST *  defaultCase;
        std::vector<std::pair<int, std::unique_ptr<AST>>> cases;

        ASTSwitch(Token const & t):
            AST{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "switch " << p.symbol << "(" << *cond << p.symbol << ") {";
            p.indent();
            for (auto & i : cases) {
                p.newline();
                p << p.keyword << "case " << p.numberLiteral << i.first << p.symbol << ":" << *i.second;
            }
            if (defaultCase != nullptr) {
                p.newline();
                p << p.keyword << "default" << p.symbol << ":" << *defaultCase;
            }
            p.dedent();
            p.newline();
            p << p.symbol << "}";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTWhile : public AST {
    public:
        std::unique_ptr<AST> cond;
        std::unique_ptr<AST> body;

        ASTWhile(Token const & t):
            AST{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "while " << p.symbol << "(" << *cond << p.symbol << ")" << *body;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTDoWhile : public AST {
    public:
        std::unique_ptr<AST> body;
        std::unique_ptr<AST> cond;

        ASTDoWhile(Token const & t):
            AST{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "do" << *body << p.keyword << "while " << p.symbol << "(" << *cond << p.symbol << ")";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTFor : public AST {
    public:
        std::unique_ptr<AST> init;
        std::unique_ptr<AST> cond;
        std::unique_ptr<AST> increment;
        std::unique_ptr<AST> body;

        ASTFor(Token const & t):
            AST{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "for " << p.symbol << "(";
            if (init.get() != nullptr)
                p << *init;
            p << p.symbol << ";";
            if (cond.get() != nullptr)
                p << *cond;
            p << p.symbol << ";";
            if (increment.get() != nullptr)
                p << *increment;
            p << p.symbol << ")";
            p << *body;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTBreak : public AST {
    public:
        ASTBreak(Token const & t):
            AST{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "break";

        }

        void accept(ASTVisitor * v) override;

    };

    class ASTContinue : public AST {
    public:
        ASTContinue(Token const & t):
            AST{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "continue";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTReturn : public AST {
    public:
        std::unique_ptr<AST> value;
        ASTReturn(Token const & t):
            AST{t} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "return";
            if (value.get() != nullptr)
                p << " " << *value;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTBinaryOp : public AST {
    public:
        Symbol op;
        std::unique_ptr<AST> left;
        std::unique_ptr<AST> right;

        ASTBinaryOp(Token const & t, std::unique_ptr<AST> left, std::unique_ptr<AST> right):
            AST{t},
            op{t.valueSymbol()},
            left{std::move(left)},
            right{std::move(right)} {
        }

        /** Whether a result of binary operator has an address depends on the operation and operands.
         */
        bool hasAddress() const override;

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << *left << " " << p.symbol << op.name() << " " << *right;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTAssignment : public AST {
    public:
        Symbol op;
        std::unique_ptr<AST> lvalue;
        std::unique_ptr<AST> value;

        ASTAssignment(Token const & t, std::unique_ptr<AST> lvalue, std::unique_ptr<AST> value):
            AST{t},
            op{t.valueSymbol()},
            lvalue{std::move(lvalue)},
            value{std::move(value)} {
        }

        /** Assignment result always has address of the lvalue it assigns to.
         */
        bool hasAddress() const override {
            return true;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << *lvalue << " " << p.symbol << op.name() << " " << *value;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTUnaryOp : public AST {
    public:
        Symbol op;
        std::unique_ptr<AST> arg;

        ASTUnaryOp(Token const & t, std::unique_ptr<AST> arg):
            AST{t},
            op{t.valueSymbol()},
            arg{std::move(arg)} {
        }

        /** Whether a result of unary operator has an address depends on the operation and operands.
         */
        bool hasAddress() const override;

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.symbol << op.name() << *arg;

        }

        void accept(ASTVisitor * v) override;

    };

    /** Post-increment and post-decrement.

     */
    class ASTUnaryPostOp : public AST {
    public:
        Symbol op;
        std::unique_ptr<AST> arg;

        ASTUnaryPostOp(Token const & t, std::unique_ptr<AST> arg):
            AST{t},
            op{t.valueSymbol()},
            arg{std::move(arg)} {
        }

        /** As the result of post-increment or decrement is the previous value, it is now a temporary and therefore does not have an address.

            NOTE: The default behavior is identical, but this method is included for clarity.
         */
        bool hasAddress() const override {
            return false;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << *arg << p.symbol << op.name();
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTAddress : public AST {
    public:
        std::unique_ptr<AST> target;

        ASTAddress(Token const & t, std::unique_ptr<AST> target):
            AST{t},
            target{std::move(target)} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.symbol << "&" << *target;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTDeref : public AST {
    public:
        std::unique_ptr<AST> target;

        ASTDeref(Token const & t, std::unique_ptr<AST> target):
            AST{t},
            target{std::move(target)} {
        }

        /** The dereferenced item always has address as it was obtained by following a pointer in the first place.
         */
        bool hasAddress() const override {
            return true;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.symbol << "*" << *target;
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTIndex : public AST {
    public:
        std::unique_ptr<AST> base;
        std::unique_ptr<AST> index;

        ASTIndex(Token const & t, std::unique_ptr<AST> base, std::unique_ptr<AST> index):
            AST{t},
            base{std::move(base)},
            index{std::move(index)} {
        }

        /** If the base has address, then its element must have address too.
         */
        bool hasAddress() const override {
            return base->hasAddress();
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << *base << p.symbol << "[" << *index << p.symbol << "]";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTMember : public AST {
    public:
        std::unique_ptr<AST> base;
        Symbol member;

        ASTMember(Token const & t, std::unique_ptr<AST> base, Symbol member):
            AST{t},
            base{std::move(base)},
            member(member) {
        }

        /** If the base has address, then its element must have address too.
         */
        bool hasAddress() const override {
            return base->hasAddress();
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << *base << p.symbol << "." << p.identifier << member.name();
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTMemberPtr : public AST {
    public:
        std::unique_ptr<AST> base;
        Symbol member;

        ASTMemberPtr(Token const & t, std::unique_ptr<AST> base, Symbol member):
            AST{t},
            base{std::move(base)},
            member(member) {
        }

        /** Since member of pointer's rhs must have an adrress (it's a pointer), the element must have address as well.
         */
        bool hasAddress() const override {
            return true;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << *base << p.symbol << "->" << p.identifier << member.name();
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTCall : public AST {
    public:
        std::unique_ptr<AST> function;
        std::vector<std::unique_ptr<AST>> args;

        ASTCall(Token const & t, std::unique_ptr<AST> function):
            AST{t},
            function{std::move(function)} {
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << *function << p.symbol << "(";
            auto i = args.begin();
            if (i != args.end()) {
                p << **i;
                while (++i != args.end())
                    p << p.symbol << **i;
            }
            p << p.symbol << ")";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTCast : public AST {
    public:
        std::unique_ptr<AST> value;
        std::unique_ptr<ASTType> type;

        ASTCast(Token const & t, std::unique_ptr<AST> value, std::unique_ptr<ASTType> type):
            AST{t},
            value{std::move(value)},
            type{std::move(type)} {
        }

        /** Casts can only appear on right hand side of assignments.
         */
        bool hasAddress() const override {
            return false;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "cast" << p.symbol << "<" << (*type) << p.symbol << ">(" << *value << p.symbol << ")";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTWrite : public AST {
    public:
        std::unique_ptr<AST> value;

        ASTWrite(Token const & t, std::unique_ptr<AST> value):
            AST{t},
            value{std::move(value)}{
        }

        /** Writes can only appear on right hand side of assignments.
         */
        bool hasAddress() const override {
            return false;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "print(" << *value << p.symbol << ")";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTRead : public AST {
    public:
        ASTRead(Token const & t):
            AST{t}{
        }

        /** Reads can only appear on right hand side of assignments.
         */
        bool hasAddress() const override {
            return false;
        }

    protected:

        void print(ASTPrettyPrinter & p) const override {
            p << p.keyword << "write()";
        }

        void accept(ASTVisitor * v) override;

    };

    class ASTVisitor {
    public:

        virtual void visit(AST * ast) = 0;
        virtual void visit(ASTInteger * ast) = 0;
        virtual void visit(ASTDouble * ast) = 0;
        virtual void visit(ASTChar * ast) = 0;
        virtual void visit(ASTString * ast) = 0;
        virtual void visit(ASTIdentifier * ast) = 0;
        virtual void visit(ASTType * ast) = 0;
        virtual void visit(ASTPointerType * ast) = 0;
        virtual void visit(ASTArrayType * ast) = 0;
        virtual void visit(ASTNamedType * ast) = 0;
        virtual void visit(ASTSequence * ast) = 0;
        virtual void visit(ASTBlock * ast) = 0;
        virtual void visit(ASTVarDecl * ast) = 0;
        virtual void visit(ASTFunDecl * ast) = 0;
        virtual void visit(ASTStructDecl * ast) = 0;
        virtual void visit(ASTFunPtrDecl * ast) = 0;
        virtual void visit(ASTIf * ast) = 0;
        virtual void visit(ASTSwitch * ast) = 0;
        virtual void visit(ASTWhile * ast) = 0;
        virtual void visit(ASTDoWhile * ast) = 0;
        virtual void visit(ASTFor * ast) = 0;
        virtual void visit(ASTBreak * ast) = 0;
        virtual void visit(ASTContinue * ast) = 0;
        virtual void visit(ASTReturn * ast) = 0;
        virtual void visit(ASTBinaryOp * ast) = 0;
        virtual void visit(ASTAssignment * ast) = 0;
        virtual void visit(ASTUnaryOp * ast) = 0;
        virtual void visit(ASTUnaryPostOp * ast) = 0;
        virtual void visit(ASTAddress * ast) = 0;
        virtual void visit(ASTDeref * ast) = 0;
        virtual void visit(ASTIndex * ast) = 0;
        virtual void visit(ASTMember * ast) = 0;
        virtual void visit(ASTMemberPtr * ast) = 0;
        virtual void visit(ASTCall * ast) = 0;
        virtual void visit(ASTCast * ast) = 0;
        virtual void visit(ASTWrite * ast) = 0;
        virtual void visit(ASTRead * ast) = 0;

    protected:

        void visitChild(AST * child) {
            child->accept(this);
        }

    }; // tinyc::ASTVisitor

    inline void AST::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTInteger::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTDouble::accept(ASTVisitor* v) { v->visit(this); }
    inline void ASTChar::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTString::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTIdentifier::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTType::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTPointerType::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTArrayType::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTNamedType::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTSequence::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTBlock::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTVarDecl::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTFunDecl::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTStructDecl::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTFunPtrDecl::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTIf::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTSwitch::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTWhile::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTDoWhile::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTFor::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTBreak::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTContinue::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTReturn::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTBinaryOp::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTAssignment::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTUnaryOp::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTUnaryPostOp::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTAddress::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTDeref::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTIndex::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTMember::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTMemberPtr::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTCall::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTCast::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTRead::accept(ASTVisitor * v) { v->visit(this); }
    inline void ASTWrite::accept(ASTVisitor * v) { v->visit(this); }

} // namespace tinyc
