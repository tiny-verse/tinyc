#include "typechecker.h"

#include "frontend.h"

namespace tinyc {

     TypeChecker::TypeChecker(Frontend & frontend):
        frontend_{frontend} {
        // create the global context
        context_.push_back(Context{getType(Symbol::KwVoid), {}});
    }


    Type * TypeChecker::getArithmeticResult(Type * lhs, Type * rhs) {
        if (lhs == getTypeDouble() && (rhs == getTypeInt() || rhs == getTypeChar() || rhs == getTypeDouble()))
            return getTypeDouble();
        if (rhs == getTypeDouble() && (lhs == getTypeInt() || lhs == getTypeChar() || lhs == getTypeDouble()))
            return getTypeDouble();
        if (lhs == getTypeInt() && (rhs == getTypeChar() || rhs == getTypeInt()))
            return getTypeInt();
        if (rhs == getTypeInt() && (lhs == getTypeChar() || lhs == getTypeInt()))
            return getTypeInt();
        if (lhs == getTypeChar() && getTypeChar())
            return getTypeChar();
        return nullptr;
    }


    Type * TypeChecker::getType(Symbol symbol) {
        return frontend_.getType(symbol);
    }

    Type * TypeChecker::getTypeVoid() {
        return frontend_.getTypeVoid();
    }

    Type * TypeChecker::getTypeInt() {
        return frontend_.getTypeInt();
    }

    Type * TypeChecker::getTypeDouble() {
        return frontend_.getTypeDouble();
    }

    Type * TypeChecker::getTypeChar() {
        return frontend_.getTypeChar();
    }

    Type::Fun * TypeChecker::isFunctionPointer(Type * t) {
        Type::Pointer * p = dynamic_cast<Type::Pointer*>(t);
        if (p != nullptr)
            t = p->base();
        return dynamic_cast<Type::Fun*>(t);
    }


    bool TypeChecker::isPointer(Type * t) {
        return frontend_.isPointer(t);
    }

    bool TypeChecker::isPOD(Type * t) {
        return frontend_.isPOD(t);
    }

    bool TypeChecker::convertsToBool(Type * t) {
        return frontend_.convertsToBool(t);
    }

    Type::Struct * TypeChecker::getOrCreateStructType(Symbol name) {
        return frontend_.getOrCreateStructType(name);
    }

    Type::Fun * TypeChecker::getOrCreateFunctionType(std::unique_ptr<Type::Fun> type) {
        return frontend_.getOrCreateFunctionType(std::move(type));

    }

    Type::Alias * TypeChecker::createTypeAlias(Symbol name, Type * base) {
        return frontend_.createTypeAlias(name, base);

    }

    Type * TypeChecker::getOrCreatePointerType(Type * base) {
        return frontend_.getOrCreatePointerType(base);

    }

    void TypeChecker::visit(AST * ast) { 

    }

    void TypeChecker::visit(ASTInteger * ast) { 
        return ast->setType(getTypeInt());
    }

    void TypeChecker::visit(ASTDouble * ast) { 
        return ast->setType(getType(Symbol::KwDouble));
    }

    void TypeChecker::visit(ASTChar * ast) { 
        return ast->setType(getType(Symbol::KwChar));
    }

    void TypeChecker::visit(ASTString * ast) { 
        return ast->setType(getOrCreatePointerType(getType(Symbol::KwChar)));
    }

    void TypeChecker::visit(ASTIdentifier * ast) { 
        Type * t = getVariable(ast->name);
        if (t == nullptr)
            throw ParserError(STR("Unknown variable " << ast->name.name()), ast->location());
        return ast->setType(t);
    }

    void TypeChecker::visit(ASTType * ast) { 
        // unreachable
    }

    void TypeChecker::visit(ASTPointerType * ast) {
        return ast->setType(getOrCreatePointerType(visitChild(ast->base)));
    }

    void TypeChecker::visit(ASTArrayType * ast) { 
        return ast->setType(getOrCreatePointerType(visitChild(ast->base)));
    }

    void TypeChecker::visit(ASTNamedType * ast) { 
        return ast->setType(getType(ast->name));
    }

    void TypeChecker::visit(ASTSequence * ast) { 
        Type * t = nullptr;
        for (auto & i : ast->body)
            t = visitChild(i);
        return ast->setType(t);;
    }

    void TypeChecker::visit(ASTBlock * ast) { 
        enterBlockContext();
        Type * t = getTypeVoid();
        for (auto & i : ast->body) {
            Type * tt = visitChild(i);
            if (dynamic_cast<ASTReturn*>(i.get())) {
                t = tt;
                ast->setType(t);
            }
        }
        leaveContext();
    }

    void TypeChecker::visit(ASTVarDecl * ast) { 
        Type * t = visitChild(ast->type);
        if (!t->isFullyDefined())
            throw ParserError(STR("Type " << t->toString() << " is not fully defined yet"), ast->location());
        if (ast->value != nullptr) {
            Type * valueType = visitChild(ast->value);
            if (valueType != t)
                throw ParserError(STR("Value of type " << valueType->toString() << " cannot be assigned to variable of type " << t->toString()), ast->location());
        }
        addVariable(ast->name->name, t);
        return ast->setType(t);
    }

    void TypeChecker::visit(ASTFunDecl * ast) { 
        // first get the function type
        std::unique_ptr<Type::Fun> ftype{new Type::Fun{visitChild(ast->typeDecl)}};
        if (! ftype->returnType()->isFullyDefined())
            throw ParserError{STR("Return type " << ftype->returnType()->toString() << " is not fully defined"), ast->typeDecl->location()};
        // typecheck all arguments, make sure the argument types are fully defined and add the arguments as local variables
        for (auto & i : ast->args) {
            Type * argType = visitChild(i.first);
            if (!argType->isFullyDefined())
                throw ParserError(STR("Type " << argType->toString() << " is not fully defined"), i.first->location());
            ftype->addArgument(argType);
        }
        // enter the context and add all arguments as local variables
        enterFunctionContext(ftype->returnType());
        for (auto & i : ast->args)
            addVariable(i.second->name, i.first->type());
        // set own type as the function type itself
        Type * t = getOrCreateFunctionType(std::move(ftype));
        // this is a trick to allow storing the functions in variables, we create a new global variable of the name of the function and its type. We must do this before the body typecheck so that recursive calls are possible
        if (!addGlobalVariable(ast->name, t))
            throw ParserError{STR("Name " << ast->name.name() << " already used"), ast->location()};
        // typecheck the function body
        Type * actualReturn = visitChild(ast->body);
        if (actualReturn != dynamic_cast<Type::Fun*>(t)->returnType())
            throw ParserError{STR("Invalid function return type: " << actualReturn->toString()), ast->location()};
        // leave the function context
        leaveContext();
        return ast->setType(t);
    }

    /** Type checking a structure declaration creates the type.
     */
    void TypeChecker::visit(ASTStructDecl * ast) { 
        Type::Struct * type = getOrCreateStructType(ast->name);
        if (type == nullptr)
            throw ParserError{STR("Type " << ast->name.name() << " already defined and is not struct"), ast->location()};
        if (ast->isDefinition) {
            for (auto & i : ast->fields) {
                Type * argType = visitChild(i.second);
                type->addField(i.first->name, argType, i.first.get());
            }
        }
        // we have to do this *after* the types so that the struct type itself remains not fully defined while typing its fields
        if (type->ast() == nullptr || ! type->ast()->isDefinition)
            type->updateDefinition(ast);
        else
            throw ParserError{STR("Type " << ast->name.name() << " already fully defined"), ast->location()};
        return ast->setType(type);
    }

    /** Typechecking a function pointer declaration creates the type.
        But here is a catch - we can have multiple definitions of the same function type, each time under a different name. In fact we can have a type definition and then a function of that type. These must not be different types, for which the type alias class is needed.
     */
    void TypeChecker::visit(ASTFunPtrDecl * ast) { 
        // check if type with given name already exists
        if (getType(ast->name->name) != nullptr)
            throw ParserError{STR("Type " << ast->name->name.name() << " already exists"), ast->location()};
        // first get the function type
        std::unique_ptr<Type::Fun> ftype{new Type::Fun{visitChild(ast->returnType)}};
        if (! ftype->returnType()->isFullyDefined())
            throw ParserError{STR("Return type " << ftype->returnType()->toString() << " is not fully defined"), ast->returnType->location()};
        // typecheck all arguments, make sure the argument types are fully defined and add the arguments as local variables
        for (auto & i : ast->args) {
            Type * argType = visitChild(i);
            if (!argType->isFullyDefined())
                throw ParserError(STR("Type " << argType->toString() << " is not fully defined"), i->location());
            ftype->addArgument(argType);
        }
        // now get the proper type and create type alias
        Type * fptr = getOrCreatePointerType(getOrCreateFunctionType(std::move(ftype)));
        return ast->setType(createTypeAlias(ast->name->name, fptr));
    }

    void TypeChecker::visit(ASTIf * ast) { 
        if (! convertsToBool(visitChild(ast->cond)))
            throw ParserError{STR("Condition must convert to bool, but " << ast->cond->type()->toString() << " found"), ast->cond->location()};
        visitChild(ast->trueCase);
        if (ast->falseCase != nullptr) 
            visitChild(ast->falseCase);
        return ast->setType(getType(Symbol::KwVoid));
    }

    void TypeChecker::visit(ASTSwitch * ast) { 
        if (! convertsToBool(visitChild(ast->cond)))
            throw ParserError{STR("Condition must convert to bool, but " << ast->cond->type()->toString() << " found"), ast->cond->location()};
        if (ast->defaultCase != nullptr)
            visitChild(ast->defaultCase);
        for (auto & i : ast->cases)
            visitChild(i.second);
        return ast->setType(getType(Symbol::KwVoid));
    }

    void TypeChecker::visit(ASTWhile * ast) { 
        if (! convertsToBool(visitChild(ast->cond)))
            throw ParserError{STR("Condition must convert to bool, but " << ast->cond->type()->toString() << " found"), ast->cond->location()};
        visitChild(ast->body);
        return ast->setType(getType(Symbol::KwVoid));
    }

    void TypeChecker::visit(ASTDoWhile * ast) { 
        if (! convertsToBool(visitChild(ast->cond)))
            throw ParserError{STR("Condition must convert to bool, but " << ast->cond->type()->toString() << " found"), ast->cond->location()};
        visitChild(ast->body);
        return ast->setType(getType(Symbol::KwVoid));
    }

    void TypeChecker::visit(ASTFor * ast) { 
        visitChild(ast->init);
        if (! convertsToBool(visitChild(ast->cond)))
            throw ParserError{STR("Condition must convert to bool, but " << ast->cond->type()->toString() << " found"), ast->cond->location()};
        visitChild(ast->increment);
        visitChild(ast->body);
        return ast->setType(getType(Symbol::KwVoid));
    }

    void TypeChecker::visit(ASTBreak * ast) { 
        return ast->setType(getType(Symbol::KwVoid));
    }

    void TypeChecker::visit(ASTContinue * ast) { 
        return ast->setType(getType(Symbol::KwVoid));
    }

    void TypeChecker::visit(ASTReturn * ast) { 
        Type * t = nullptr;
        if (ast->value == nullptr)
            t = getType(Symbol::KwVoid);
        else
            t = visitChild(ast->value);
        if (t != currentReturnType())
            throw ParserError(STR("Invalid return type, expected " << currentReturnType()->toString() << ", but " << t->toString() << " found"), ast->location());
        return ast->setType(t);
    }

    void TypeChecker::visit(ASTBinaryOp * ast) { 
        Type * leftType = visitChild(ast->left);
        Type * rightType = visitChild(ast->right);
        Type * t = nullptr;
        // works on all PODs and pointers
        if (ast->op == Symbol::Add || ast->op == Symbol::Sub) {
            if (isPointer(leftType) && rightType == getTypeInt())
                t = leftType;
            else
                t = getArithmeticResult(leftType, rightType);
        // works on all PODs
        } else if (ast->op == Symbol::Mul || ast->op == Symbol::Div) {
            t = getArithmeticResult(leftType, rightType);
        // works on char and int, result is the type itself, the types must be identical
        } else if (ast->op == Symbol::Mod) {
            if (leftType == rightType && (leftType == getTypeInt() || leftType == getTypeChar()))
                t = leftType;
        // lhs must be char or int, rhs can be char or int, result is lhs
        } else if (ast->op == Symbol::ShiftRight || ast->op == Symbol::ShiftLeft) {
            if ((leftType == getTypeInt() || leftType == getTypeChar()) &&
                (rightType == getTypeInt() || rightType == getTypeChar()))
                t = leftType;
        // works on types convertible to boolean and result is integer (we do not have bool)
        } else if (ast->op == Symbol::And || ast->op == Symbol::Or || ast->op == Symbol::Xor) {
            if (convertsToBool(leftType) && convertsToBool(rightType))
                t = getTypeInt();
        // bitwise and and or operations only work on chars and its, result is the same type
        } else if (ast->op == Symbol::BitAnd || ast->op == Symbol::BitOr) {
            if (leftType == rightType && (leftType == getTypeInt() || leftType == getTypeChar()))
                t = leftType;
        // relational operators work on int, chars,doubles and pointers of same type result is int
        } else if (ast->op == Symbol::Lt || ast->op == Symbol::Gt || ast->op == Symbol::Lte || ast->op == Symbol::Gte) {
            if (leftType == rightType && (isPointer(leftType) || isPOD(leftType)))
                t = getTypeInt();
        // equality and inequality work on all types as long as they match
        } else if(ast->op == Symbol::Eq || ast->op == Symbol::NEq) {
            if (leftType == rightType)
                t = getTypeInt();
        }
        return ast->setType(t);
    }

    void TypeChecker::visit(ASTAssignment * ast) { 
        Type * lvalueType = visitChild(ast->lvalue);
        Type * valueType = visitChild(ast->value);
        if (!ast->lvalue->hasAddress())
            throw ParserError{"Assignment target must have address", ast->location()};
        // we are working with exact types only
        if (lvalueType == valueType)
            return ast->setType(lvalueType);
        else
            return ast->setType(nullptr); // error
    }

    void TypeChecker::visit(ASTUnaryOp * ast) { 
        Type * argt = visitChild(ast->arg);
        Type * t = nullptr;
        // works on all numeric types
        if (ast->op == Symbol::Add || ast->op == Symbol::Sub) {
            t = getArithmeticResult(argt, argt);
        // negation works on char and int only
        } else if (ast->op == Symbol::Neg) {
            if (argt == getTypeInt() || argt == getTypeChar())
                t = argt;
        // not works on any type convertible to boolean
        } else if (ast->op == Symbol::Not) {
            if (convertsToBool(argt))
                t = getTypeInt();
        // works on pointers and arithmetic types
        } else if (ast->op == Symbol::Inc || ast->op == Symbol::Dec) {
            if (!ast->arg->hasAddress())
                throw ParserError("Cannot increment or decrement non l-value", ast->location());
            // not all types can be incremented
            if (isPointer(argt) || isPOD(argt))
                t = argt;
        }
        return ast->setType(t);
    }

    /** For increment and decrement, the result type is identical to its argument, but not all types can be incremented.
     */
    void TypeChecker::visit(ASTUnaryPostOp * ast) { 
        if (!ast->arg->hasAddress())
            throw ParserError("Cannot increment or decrement non l-value", ast->location());
        Type * argt = visitChild(ast->arg);
        // not all types can be incremented
        if (isPointer(argt) || isPOD(argt))
            return ast->setType(argt);
        else
            return ast->setType(nullptr); // error
    }

    /** The interesting feature of the address operator is that not every value in tinyC has an address (only local variables do)
     */
    void TypeChecker::visit(ASTAddress * ast) { 
        Type * targetType = visitChild(ast->target);
        if (!ast->target->hasAddress())
            throw ParserError("Address can only be taken from a non-temporary value (l-value)", ast->location());
        return ast->setType(getOrCreatePointerType(targetType));
    }

    void TypeChecker::visit(ASTDeref * ast) { 
        Type * t = visitChild(ast->target);
        Type::Pointer * p = dynamic_cast<Type::Pointer*>(t);
        if (p == nullptr)
            throw ParserError{STR("Cannot dereference a non-pointer type " << t->toString()), ast->location()};
        return ast->setType(p->base());
    }

    void TypeChecker::visit(ASTIndex * ast) { 
        Type * baseType = visitChild(ast->base);
        if (! isPointer(baseType))
            throw ParserError{STR("Expected pointer, but " << baseType->toString() << " found"), ast->location()};
        Type * indexType = visitChild(ast->index);
        if (indexType != getTypeInt() && indexType != getTypeChar())
            throw ParserError{STR("Expected int or char, but " << indexType->toString() << " found"), ast->location()};
        return ast->setType(dynamic_cast<Type::Pointer*>(baseType)->base());
    }

    void TypeChecker::visit(ASTMember * ast) { 
        Type * baseType = visitChild(ast->base);
        Type::Struct * type = dynamic_cast<Type::Struct*>(baseType);
        if (type == nullptr || ! type->isFullyDefined())
            throw ParserError{STR("Only fully defined struct types can have members extracted, but " << baseType->toString() << " found"), ast->location()};
        Type * t  = type->getFieldType(ast->member);
        if (t == nullptr)
            throw ParserError{STR("Field " << ast->member.name() << " not defined in struct " << type->toString()), ast->location()};
        return ast->setType(t);    
    }

    /** The member ptr is similar to member, but we must also check that the base is a pointer first.
     */
    void TypeChecker::visit(ASTMemberPtr * ast) { 
        Type * baseType = visitChild(ast->base);
        Type::Pointer * basePtr = dynamic_cast<Type::Pointer*>(baseType);
        if (basePtr == nullptr)
            throw ParserError{STR("Only pointers can appear on left side of -> operator, but " << baseType->toString() << " found"), ast->location()};
        Type::Struct * type = dynamic_cast<Type::Struct*>(basePtr->base());
        if (type == nullptr || ! type->isFullyDefined())
            throw ParserError{STR("Only fully defined struct types can have members extracted, but " << basePtr->base()->toString() << " found"), ast->location()};
        Type * t  = type->getFieldType(ast->member);
        if (t == nullptr)
            throw ParserError{STR("Field " << ast->member.name() << " not defined in struct " << type->toString()), ast->location()};
        return ast->setType(t);
    }

    void TypeChecker::visit(ASTCall * ast) { 
        visitChild(ast->function);
        Type::Fun * f = isFunctionPointer(ast->function->type());
        if (f == nullptr)
            throw ParserError{STR("Expected function, but value of " << ast->function->type()->toString() << " found"), ast->location()};
        if (ast->args.size() != f->numArgs())
            throw ParserError{STR("Function of type " << f->toString() << " requires " << f->numArgs() << " arguments, but " << ast->args.size() << " given"), ast->location()};
        for (size_t i = 0; i < ast->args.size(); ++i)
            if (visitChild(ast->args[i]) != f->argType(i))
                throw ParserError{STR("Type " << f->argType(i)->toString() << " expected for argument " << (i + 1) << ", but " << ast->args[i]->type()->toString() << " found"), ast->args[i]->location()};
        return ast->setType(f->returnType());    
    }

    /** Makes sure that incompatible types are not casted.
        The following casts are legal in tinyC:
        - any pointer to any other pointer
        - any pod to any pod
        - integer to any pointer and any pointer to integer
     */
    void TypeChecker::visit(ASTCast * ast) { 
        Type * valueType = visitChild(ast->value);
        Type * castType = visitChild(ast->type);
        Type * t = nullptr;
        if (isPointer(castType)) {
            if (isPointer(valueType) || valueType == getTypeInt())
                t = castType;
        } else if (castType == getTypeInt()) {
            if (isPointer(valueType) || isPOD(valueType))
                t = castType;
        } else if (isPOD(castType) && isPOD(valueType))
            t = castType;
        return ast->setType(t);
    }

} // namespace tinyc
