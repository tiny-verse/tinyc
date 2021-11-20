#pragma once
#if (defined OPTIMIZER_tvlm)


#include "tvlm/il_builder.h"

#include "ast.h"
#include "types.h"

#include "typechecker.h"


namespace tinyc {

    class TvlmFrontend : public ASTVisitor {
    public:
        TvlmFrontend(Frontend & frontend): frontend_{frontend}{}

        static tvlm::Program translate(AST * ast, Frontend & frontend){
            TvlmFrontend b(frontend);
            b.visit(ast);
            std::stringstream ss;
            auto printer = tiny::ASTPrettyPrinter(ss);

            b.b_.print(printer);

            std::cerr << "tinyc:\n" << ss.str() << std::endl;

            return b.b_.finish();
        }

        void visit(AST * ast) override;
        void visit(ASTInteger * ast) override;
        void visit(ASTDouble * ast) override;
        void visit(ASTChar * ast) override;
        void visit(ASTString * ast) override;
        void visit(ASTIdentifier * ast) override;
        void visit(ASTType * ast) override;
        void visit(ASTPointerType * ast) override;
        void visit(ASTArrayType * ast) override;
        void visit(ASTNamedType * ast) override;
        void visit(ASTSequence * ast) override;
        void visit(ASTBlock * ast) override;
        void visit(ASTVarDecl * ast) override;
        void visit(ASTFunDecl * ast) override;
        void visit(ASTStructDecl * ast) override;
        void visit(ASTFunPtrDecl * ast) override;
        void visit(ASTIf * ast) override;
        void visit(ASTSwitch * ast) override;
        void visit(ASTWhile * ast) override;
        void visit(ASTDoWhile * ast) override;
        void visit(ASTFor * ast) override;
        void visit(ASTBreak * ast) override;
        void visit(ASTContinue * ast) override;
        void visit(ASTReturn * ast) override;
        void visit(ASTBinaryOp * ast) override;
        void visit(ASTAssignment * ast) override;
        void visit(ASTUnaryOp * ast) override;
        void visit(ASTUnaryPostOp * ast) override;
        void visit(ASTAddress * ast) override;
        void visit(ASTDeref * ast) override;
        void visit(ASTIndex * ast) override;
        void visit(ASTMember * ast) override;
        void visit(ASTMemberPtr * ast) override;
        void visit(ASTCall * ast) override;
        void visit(ASTCast * ast) override;

    protected:
        tvlm::Instruction * visitChild(AST * ast) {
            ASTVisitor::visitChild(ast);
            return lastIns_;
        }

        template<typename T>
        tvlm::Instruction * visitChild(std::unique_ptr<T> const & ptr) {
            return visitChild(ptr.get());
        }
    private:

        tvlm::Instruction * append(tvlm::Instruction * ins) {
            b_.add(ins);
            lastIns_ = ins;
            return ins;
        }

        Frontend & frontend_;
        tvlm::ILBuilder b_;
        tvlm::Instruction * lastIns_;
        bool lvalue_ = false;

    };


} // namespace tinyc

#endif