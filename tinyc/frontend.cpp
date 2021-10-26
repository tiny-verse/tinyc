#include "frontend.h"
//#include "types.h"
//#include "ir.h"
//#include "backend.h"

#ifdef HAHA

namespace tiny {

#if (defined BACKEND_dummy)

    Backend::IR Frontend::translate(AST & ast) {
        std::stringstream ss;
        ASTPrettyPrinter p{ss};
        p << *ast;
        return ss.str();
    }

#elif (defined BACKEND_tinyc)

    Backend::IR Frontend::translate(AST & ast, Backend & backend) {
        tiny::AST * rootNode = dynamic_cast<tiny::AST*>(ast.get());
        // typecheck the ast and store type declarations
        TypeChecker tc{backend};
        rootNode->typeCheck(tc);
        // compile the program
        IRBuilder b{backend};
        rootNode->compileToIR(b, false);
        return b;
    }

#else
    #error "Selected backend not supported"
#endif

} // namespace tiny

#endif