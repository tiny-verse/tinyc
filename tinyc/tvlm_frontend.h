#pragma once
#if (defined OPTIMIZER_tvlm)


#include "tvlm/il_builder.h"

#include "ast.h"
#include "types.h"

#include "typechecker.h"


namespace tinyc {

    class TvlmFrontend : public ASTVisitor {
    public:
        TvlmFrontend(Frontend &frontend) : frontend_{frontend} , b_(){}

        static tvlm::Program translate(AST *ast, Frontend &frontend) {
            TvlmFrontend b(frontend);
            b.visit(ast);
            std::stringstream ss;
            auto printer = tiny::ASTPrettyPrinter(ss);

            b.b_.finalize();
            b.b_.print(printer);

            std::cerr << tiny::color::lightBlue << "IL:\n" << ss.str() << std::endl;

            return std::move(b.b_.finish());
        }

        void visit(AST *ast) override;

        void visit(ASTInteger *ast) override;

        void visit(ASTDouble *ast) override;

        void visit(ASTChar *ast) override;

        void visit(ASTString *ast) override;

        void visit(ASTIdentifier *ast) override;

        void visit(ASTType *ast) override;

        void visit(ASTPointerType *ast) override;

        void visit(ASTArrayType *ast) override;

        void visit(ASTNamedType *ast) override;

        void visit(ASTSequence *ast) override;

        void visit(ASTBlock *ast) override;

        void visit(ASTVarDecl *ast) override;

        void visit(ASTFunDecl *ast) override;

        void visit(ASTStructDecl *ast) override;

        void visit(ASTFunPtrDecl *ast) override;

        void visit(ASTIf *ast) override;

        void visit(ASTSwitch *ast) override;

        void visit(ASTWhile *ast) override;

        void visit(ASTDoWhile *ast) override;

        void visit(ASTFor *ast) override;

        void visit(ASTBreak *ast) override;

        void visit(ASTContinue *ast) override;

        void visit(ASTReturn *ast) override;

        void visit(ASTBinaryOp *ast) override;

        void visit(ASTAssignment *ast) override;

        void visit(ASTUnaryOp *ast) override;

        void visit(ASTUnaryPostOp *ast) override;

        void visit(ASTAddress *ast) override;

        void visit(ASTDeref *ast) override;

        void visit(ASTIndex *ast) override;

        void visit(ASTMember *ast) override;

        void visit(ASTMemberPtr *ast) override;

        void visit(ASTCall *ast) override;

        void visit(ASTCast *ast) override;

        void visit(ASTRead *ast) override;

        void visit(ASTWrite *ast) override;

    protected:
        tvlm::Instruction *visitChild(AST *ast, bool lvalue = false) {
            lvalue_ = lvalue;
            ASTVisitor::visitChild(ast);
            return lastIns_;
        }

        template<typename T>
        tvlm::Instruction *visitChild(std::unique_ptr<T> const &ptr, bool lvalue = false) {
            return visitChild(ptr.get(), lvalue);
        }

    private:

        tvlm::Instruction *append(tvlm::Instruction *ins) {
            b_.add(ins);
            lastIns_ = ins;
            return ins;
        }

        Type * registerType(Type * type){
            allocated_types_.emplace_back(type);
            return type;
        }

        Frontend &frontend_;
        tvlm::ILBuilder b_;
        tvlm::Instruction *lastIns_;
        std::vector<std::unique_ptr<Type>> allocated_types_;
        bool lvalue_ = false;

    };

    static size_t staticalyResolve(AST * ast) {
        auto integer = dynamic_cast<ASTInteger *>(ast);
        auto binaryOperator = dynamic_cast<ASTBinaryOp *>(ast);
        auto unaryOperator = dynamic_cast<ASTUnaryOp *>(ast);
        if (integer) {
            return integer->value;
        } else if (binaryOperator) {
            size_t l = staticalyResolve(binaryOperator->left.get());
            size_t r = staticalyResolve(binaryOperator->right.get());
            if (binaryOperator->op == Symbol::Add) {
                return l + r;
            } else if (binaryOperator->op == Symbol::Sub) {
                return l - r;
            } else if (binaryOperator->op == Symbol::Mul) {
                return l * r;
            } else if (binaryOperator->op == Symbol::Div) {
                return l / r;
            } else if (binaryOperator->op == Symbol::ShiftLeft) {
                return l << r;
            } else if (binaryOperator->op == Symbol::ShiftRight) {
                return l >> r;
            } else if (binaryOperator->op == Symbol::Mod) {
                return l % r;
            } else if (binaryOperator->op == Symbol::And) {
                return l && r;
            } else if (binaryOperator->op == Symbol::Or) {
                return l || r;
            } else if (binaryOperator->op == Symbol::BitAnd) {
                return l & r;
            } else if (binaryOperator->op == Symbol::BitOr) {
                return l | r;
            } else if (binaryOperator->op == Symbol::Xor) {
                return l ^ r;
            } else if (binaryOperator->op == Symbol::Lt) {
                return l < r;
            } else if (binaryOperator->op == Symbol::Lte) {
                return l <= r;
            } else if (binaryOperator->op == Symbol::Gt) {
                return l > r;
            } else if (binaryOperator->op == Symbol::Gte) {
                return l >= r;
            } else if (binaryOperator->op == Symbol::Eq) {
                return l == r;
            } else if (binaryOperator->op == Symbol::NEq) {
                return l != r;
            }
        } else if (unaryOperator) {
            size_t operand = staticalyResolve(unaryOperator->arg.get());

            if (binaryOperator->op == Symbol::Sub) {
                return -operand;
            } else if (binaryOperator->op == Symbol::Not) {
                return !operand;
            } else if (binaryOperator->op == Symbol::Neg) {
                return ~operand;
            }
        }
        throw "Is not statically known size";
    }


} // namespace tinyc

#endif