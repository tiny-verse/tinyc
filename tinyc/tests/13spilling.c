#include <stdlib.h>                // AST                          ->  IL
#include <stdio.h>                // AST                          ->  IL

int main(){     // ASTFunDecl                   ->  Function
    int a1 = 1;  // ASTVarDecl   // ASTType      ->  addr  = AllocL(4 : size)
    int a2 = 2*2;  // ASTVarDecl   // ASTType      ->  addr  = AllocL(4 : size)
    int a3 = 3*3;  // ASTVarDecl   // ASTType      ->  addr  = AllocL(4 : size)
    int a4 = 4*4;  // ASTVarDecl   // ASTType      ->  addr  = AllocL(4 : size)
    int a5 = 5*5;  // ASTVarDecl   // ASTType      ->  addr  = AllocL(4 : size)
    int a6 = 6*6;  // ASTVarDecl   // ASTType      ->  addr  = AllocL(4 : size)
    int a7 = 7*7; // ASTVarDecl   // ASTType      ->  addr  = AllocL(4 : size)
                //              // ASTInteger   ->  v     = LoadImm(5 : value)
                //                              ->  Store(v : value, addr : address)
    printf("%d\n", a1 + a2 *a3 /a4 + a5 + a6 + a7);   // ASTReturn   // ASTIdentifier ->  a     = Load(addr, Integer)
                              //                ->  Return(a : retValue)
}
    //!
