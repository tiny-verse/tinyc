#if (defined OPTIMIZER_tvlm)

#include "tvlm_frontend.h"

namespace tinyc {

    void TvlmFrontend::visit(AST * ast) { }

    void TvlmFrontend::visit(ASTInteger * ast) { 
        append(new tvlm::LoadImm{ast->value, ast});
    }

    void TvlmFrontend::visit(ASTDouble * ast) {
        // TODO
    }

    void TvlmFrontend::visit(ASTChar * ast) {
        append(new tvlm::LoadImm{static_cast<int64_t>(ast->value), ast});
    }

    void TvlmFrontend::visit(ASTString * ast) { 
        lastIns_ = b_.getStringLiteral(ast->value, ast);
    }

    void TvlmFrontend::visit(ASTIdentifier * ast) { }
    void TvlmFrontend::visit(ASTType * ast) { }
    void TvlmFrontend::visit(ASTPointerType * ast) { }
    void TvlmFrontend::visit(ASTArrayType * ast) { }
    void TvlmFrontend::visit(ASTNamedType * ast) { }
    void TvlmFrontend::visit(ASTSequence * ast) { }
    void TvlmFrontend::visit(ASTBlock * ast) { }
    void TvlmFrontend::visit(ASTVarDecl * ast) { }
    void TvlmFrontend::visit(ASTFunDecl * ast) { }
    void TvlmFrontend::visit(ASTStructDecl * ast) { }
    void TvlmFrontend::visit(ASTFunPtrDecl * ast) { }
    void TvlmFrontend::visit(ASTIf * ast) { }
    void TvlmFrontend::visit(ASTSwitch * ast) { }
    void TvlmFrontend::visit(ASTWhile * ast) { }
    void TvlmFrontend::visit(ASTDoWhile * ast) { }
    void TvlmFrontend::visit(ASTFor * ast) { }
    void TvlmFrontend::visit(ASTBreak * ast) { }
    void TvlmFrontend::visit(ASTContinue * ast) { }
    void TvlmFrontend::visit(ASTReturn * ast) { }
    void TvlmFrontend::visit(ASTBinaryOp * ast) { }
    void TvlmFrontend::visit(ASTAssignment * ast) { }
    void TvlmFrontend::visit(ASTUnaryOp * ast) { }
    void TvlmFrontend::visit(ASTUnaryPostOp * ast) { }
    void TvlmFrontend::visit(ASTAddress * ast) { }
    void TvlmFrontend::visit(ASTDeref * ast) { }
    void TvlmFrontend::visit(ASTIndex * ast) { }
    void TvlmFrontend::visit(ASTMember * ast) { }
    void TvlmFrontend::visit(ASTMemberPtr * ast) { }
    void TvlmFrontend::visit(ASTCall * ast) { }
    void TvlmFrontend::visit(ASTCast * ast) { }

} // namespace tinyc

#endif