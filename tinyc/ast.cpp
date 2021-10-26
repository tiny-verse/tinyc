#include "ast.h"

namespace tinyc {

    bool ASTBinaryOp::hasAddress() const {
        return false;
    }

    bool ASTUnaryOp::hasAddress() const {
        return false;
    }

}