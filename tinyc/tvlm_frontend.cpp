#if (defined OPTIMIZER_tvlm)

#include "tvlm_frontend.h"
#include "typechecker.h"

namespace tinyc {

    void TvlmFrontend::visit(AST * ast) {
        visitChild(ast);
    }

    void TvlmFrontend::visit(ASTInteger * ast) {
        append(new tvlm::LoadImm{ast->value, ast});
    }

    void TvlmFrontend::visit(ASTDouble * ast) {
        append(new tvlm::LoadImm{ast->value, ast});
    }

    void TvlmFrontend::visit(ASTChar * ast) {
        append(new tvlm::LoadImm{static_cast<int64_t>(ast->value), ast});
    }

    void TvlmFrontend::visit(ASTString * ast) {
        lastIns_ = b_.getStringLiteral(ast->value, ast);
    }

    void TvlmFrontend::visit(ASTIdentifier * ast) {
        if (lvalue_) {
            lastIns_ = b_.getVariableAddress(ast->name);
        } else {
            tvlm::Instruction * addr = b_.getVariableAddress(ast->name);
            if(/*is double*/ast->type() ){
                append(new tvlm::Load{addr, tvlm::ResultType::Double, ast});
            }else{
                append(new tvlm::Load{addr, tvlm::ResultType::Integer, ast});
            }
        }
    }
    void TvlmFrontend::visit(ASTType * ast) {
        //TODO nothing?
    }
    void TvlmFrontend::visit(ASTPointerType * ast) {
        //TODO
    }
    void TvlmFrontend::visit(ASTArrayType * ast) {
        //TODO
    }
    void TvlmFrontend::visit(ASTNamedType * ast) {
        //TODO
    }
    void TvlmFrontend::visit(ASTSequence * ast) {
        for (auto & i : ast->body){
            visitChild(i);
        }
    }
    void TvlmFrontend::visit(ASTBlock * ast) {
        for (auto & i : ast->body){
            visitChild(i);
        }
        lastIns_ = nullptr;
    }
    void TvlmFrontend::visit(ASTVarDecl * ast) {
//        if(ast->type->type()->isFullyDefined()){
        if(b_.globalEnv()){
            size_t varSize = 4; // b_.sizeOfType(ast->type->type()); // TODO
            tvlm::Instruction * addr = append(new tvlm::AllocG{varSize, ast});
            tvlm::Instruction * val = nullptr;
            if(ast->value != nullptr){
                lvalue_ = false;
                visitChild(ast->value);
                val = lastIns_;
                append(new tvlm::Store{addr, val, ast});
            }
            b_.addGlobalVariable(ast->name->name, addr);
            lastIns_ = val;
//            return val;
        }else{
            size_t varSize = 4; // b_.sizeOfType(ast->type->type()); // TODO
            tvlm::Instruction * addr = append(new tvlm::AllocL{varSize, ast});
            tvlm::Instruction * val = nullptr;
            if(ast->value != nullptr){
//                val = ast->value->compileToIR(b_, false);
                lvalue_ = false;
                visitChild(ast->value);
                val = lastIns_;
                append(new tvlm::Store{addr, val, ast});
            }
            b_.addVariable(ast->name->name, addr);
            lastIns_ = val; // return val;
        }
//        }else{
//                lastIns_ = nullptr
//            // return nullptr;
//        }

//    throw "not implemented";

    }
    void TvlmFrontend::visit(ASTFunDecl * ast) {
        tvlm::Function * f = new tvlm::Function{ast};
        b_.addGlobalVariable(f->name(), append(new tvlm::LoadImm{(int64_t )0, ast}));
        b_.enterFunction(f);
        for (size_t i = 0, e = ast->args.size(); i != e; ++i) {
            tvlm::Instruction * arg = append(new tvlm::ArgAddr{i, ast->args[i].first.get()});
            b_.addVariable(ast->args[i].second->name.name(), arg);
        }
        visitChild(ast->body); //->compileToIR(b, false);
        b_.leaveFunction();

    }
    size_t resolveStructSize(std::vector<std::pair<std::unique_ptr<ASTIdentifier>, std::unique_ptr<ASTType>>> & fields){
        size_t acc = 0;
        for (int i = 0; i < fields.size(); i++) {
            acc += 4;//fields[i].second->type()
        }
        return acc ? acc : 1;
    }

    void TvlmFrontend::visit(ASTStructDecl * ast) {

        if(ast->isDefinition){
            size_t varSize = resolveStructSize(ast->fields) ;
            tvlm::Instruction * addr = append(new tvlm::AllocL{varSize, ast});
            b_.addVariable(ast->name, addr);

        }
        //TODO
    }
    void TvlmFrontend::visit(ASTFunPtrDecl * ast) {

        //TODO
    }
    void TvlmFrontend::visit(ASTIf * ast) {

        tvlm::Instruction * condVal = visitChild(ast->cond);//->compileToIR(b, false);
        tvlm::BasicBlock * bbTrue = b_.createBasicBlock();
        tvlm::BasicBlock * bbFalse = b_.createBasicBlock();
        tvlm::BasicBlock * bbAfter = b_.createBasicBlock();
        append(new tvlm::CondJump{condVal, ast});
        dynamic_cast<tvlm::CondJump*>(lastIns_)->addTarget(bbFalse);
        dynamic_cast<tvlm::CondJump*>(lastIns_)->addTarget(bbTrue);
        b_.enterBasicBlock(bbTrue);
        visitChild(ast->trueCase); //->compileToIR(b, false);
        append(new tvlm::Jump{bbAfter, nullptr});
        b_.enterBasicBlock(bbFalse);
        visitChild(ast->falseCase); //->compileToIR(b, false);
        append(new tvlm::Jump{bbAfter, nullptr});
        b_.enterBasicBlock(bbAfter);
        lastIns_ = nullptr;
//        return nullptr;
    }
    void TvlmFrontend::visit(ASTSwitch * ast) {
        if (!ast->cases.empty()) {
            tvlm::BasicBlock *bAfter = b_.createBasicBlock("bb_after");
            tvlm::Instruction *condVal = visitChild(ast->cond);//->compileToIR(b, false);
            tvlm::ILBuilder::Context oldContext = b_.enterContext(tvlm::ILBuilder::Context::Switch(bAfter));

            tvlm::BasicBlock *bCase = b_.createBasicBlock("bb_case" + std::to_string(ast->cases.begin()->first));
            append(new tvlm::Jump(bCase, ast));

            tvlm::BasicBlock *bSuccessCmp =
                    b_.createBasicBlock("bb_succ" + std::to_string(ast->cases.begin()->first));
            tvlm::BasicBlock * defaultCaseBB = nullptr;
            for (auto it = ast->cases.begin(); it != ast->cases.end(); it++) {
                auto &c = *it;
                if(c.second == ast->defaultCase){

                    tvlm::BasicBlock *bSuccessCmpNext;
                    auto nextCaseIt = it++;
                    if (nextCaseIt != ast->cases.end()) {
                        bCase ->setName("bb_case_" + std::to_string(nextCaseIt->first));
                        bSuccessCmpNext = b_.createBasicBlock("bb_succ" + std::to_string(nextCaseIt->first));
                    } else {
                        b_.enterBasicBlock(bCase);
                        append(new tvlm::Jump(bAfter, ast));

                        bSuccessCmpNext = bAfter;
                    }
                    defaultCaseBB = b_.enterBasicBlock(bSuccessCmp);
                    bSuccessCmp->setName("bb_default");
                    visitChild(c.second);//->compileToIR(b, false);
                    append(new tvlm::Jump{bSuccessCmpNext, ast});

                    bSuccessCmp = bSuccessCmpNext;
                }else{

                    b_.enterBasicBlock(bCase);

                    tvlm::Instruction *caseVal = append(new tvlm::LoadImm((int64_t)c.first, c.second.get()));
                    tvlm::Instruction *jmpVal = append(new tvlm::BinOp{Symbol::NEq, condVal, caseVal, ast});

                    tvlm::BasicBlock *bSuccessCmpNext;
                    auto nextCaseIt = it++;
                    if (nextCaseIt != ast->cases.end()) {
                        bCase = b_.createBasicBlock("bb_case_" + std::to_string(nextCaseIt->first));
                        bSuccessCmpNext = b_.createBasicBlock("bb_succ_ " + std::to_string(nextCaseIt->first));
                    } else {
                        if(!ast->defaultCase){
                            bCase = bAfter;
                        }else{
                            bCase = defaultCaseBB;
                        }
                        bSuccessCmpNext = bAfter;
                    }
                    tvlm::CondJump * condJump = new tvlm::CondJump{
                            jmpVal,
                            ast
                    };
                    condJump->addTarget(bSuccessCmp);
                    condJump->addTarget(bCase);
                    append(condJump);
                    b_.enterBasicBlock(bSuccessCmp);
                    visitChild(c.second);//->compileToIR(b, false);
                    b_.add(new tvlm::Jump{bSuccessCmpNext, ast});

                    bSuccessCmp = bSuccessCmpNext;
                }
            }
            b_.leaveContext(oldContext);
            b_.enterBasicBlock(bAfter);
        }
        lastIns_ = nullptr; //return nullptr;
    }
    void TvlmFrontend::visit(ASTWhile * ast) {
        tvlm::BasicBlock * bCond = b_.createBasicBlock();
        tvlm::BasicBlock * bBody = b_.createBasicBlock();
        tvlm::BasicBlock * bAfter = b_.createBasicBlock();
        append(new tvlm::Jump{bCond, ast});
        b_.enterBasicBlock(bCond);
        tvlm::ILBuilder::Context oldContext = b_.enterContext(tvlm::ILBuilder::Context::Loop(bAfter, bCond));
        tvlm::Instruction * condVal = visitChild(ast->cond); // ->compileToIR(b, false);
        append(new tvlm::CondJump{condVal, ast});
        dynamic_cast<tvlm::CondJump*>(lastIns_) ->addTarget(bAfter);
        dynamic_cast<tvlm::CondJump*>(lastIns_) ->addTarget(bBody);
        b_.enterBasicBlock(bBody);
        visitChild(ast->body); // ->compileToIR(b, false);
        append(new tvlm::Jump{bCond, ast});
        b_.leaveContext(oldContext);
        b_.enterBasicBlock(bAfter);
        lastIns_ = nullptr; // return nullptr;

    }
    void TvlmFrontend::visit(ASTDoWhile * ast) {

        tvlm::BasicBlock * bCond = b_.createBasicBlock();
        tvlm::BasicBlock * bBody = b_.createBasicBlock();
        tvlm::BasicBlock * bAfter = b_.createBasicBlock();
        tvlm::ILBuilder::Context oldContext = b_.enterContext(tvlm::ILBuilder::Context::Loop(bAfter, bCond));
        b_.enterBasicBlock(bBody);
        visitChild(ast->body); //->compileToIR(b, false);
        b_.enterBasicBlock(bCond);
        tvlm::Instruction * condVal = visitChild(ast->cond); //->compileToIR(b, false);
        append(new tvlm::CondJump{condVal, ast});
        dynamic_cast<tvlm::CondJump*>(lastIns_) ->addTarget(bAfter);
        dynamic_cast<tvlm::CondJump*>(lastIns_) ->addTarget(bBody);

        b_.leaveContext(oldContext);
        b_.enterBasicBlock(bAfter);
        lastIns_ = nullptr; // return nullptr;
    }

    void TvlmFrontend::visit(ASTFor * ast) {

        tvlm::BasicBlock * bCond = b_.createBasicBlock();
        tvlm::BasicBlock * bBody = b_.createBasicBlock();
        tvlm::BasicBlock * bAfter = b_.createBasicBlock();
        tvlm::BasicBlock * bInc = b_.createBasicBlock();
        visitChild(ast->init);//->compileToIR(b, false);
        append(new tvlm::Jump{bCond, ast->init.get()});
        b_.enterBasicBlock(bCond);
        tvlm::Instruction * condVal = visitChild(ast->cond); //->compileToIR(b, false);
        append(new tvlm::CondJump{condVal, ast->cond.get()});
        dynamic_cast<tvlm::CondJump*>(lastIns_) ->addTarget(bAfter);
        dynamic_cast<tvlm::CondJump*>(lastIns_) ->addTarget(bBody);

        tvlm::ILBuilder::Context oldContext = b_.enterContext(tvlm::ILBuilder::Context::Loop(bAfter, bInc));
        b_.enterBasicBlock(bBody);
        visitChild(ast->body); //->compileToIR(b, false);
        append(new tvlm::Jump{bInc, ast->increment.get()});
        b_.leaveContext(oldContext);
        b_.enterBasicBlock(bInc);
        visitChild(ast->increment); //->compileToIR(b, false);
        append(new tvlm::Jump{bCond, ast});
        b_.enterBasicBlock(bAfter);
        lastIns_ = nullptr; //return nullptr;
    }
    void TvlmFrontend::visit(ASTBreak * ast) {
        tvlm::BasicBlock * target = b_.context().breakTarget;
        if (target == nullptr)
            throw ParserError{"Break can only be used inside loop or switch", ast->location(), false};
        append(new tvlm::Jump{target, ast});
        // we have closed the basic block, so a new one has to be created
        b_.enterBasicBlock(b_.createBasicBlock());
        lastIns_ = nullptr; //return nullptr;
    }
    void TvlmFrontend::visit(ASTContinue * ast) {
        tvlm::BasicBlock * target = b_.context().continueTarget;
        if (target == nullptr)
            throw ParserError{"Continue can only be used inside loop", ast->location(), false};
        append(new tvlm::Jump{target, ast});
        // we have closed the basic block, so a new one has to be created
        b_.enterBasicBlock(b_.createBasicBlock());
        lastIns_ = nullptr; //return nullptr;
    }
    void TvlmFrontend::visit(ASTReturn * ast) {
        tvlm::Instruction * resultValue = visitChild(ast->value);//->compileToIR(b, false);
        append(new tvlm::Return{resultValue, ast});
        // we have closed the basic block, so a new one has to be created
        b_.enterBasicBlock(b_.createBasicBlock());
        lastIns_ = nullptr; //return nullptr;
    }
    void TvlmFrontend::visit(ASTBinaryOp * ast) {
        visitChild(ast->left);
        tvlm::Instruction * lhs= lastIns_;
        //ShortCircuiting
        if(ast->op == Symbol::And){
            //TODO
            tvlm::Instruction * what = nullptr;
            tvlm::BasicBlock * lhsTrue = b_.createBasicBlock();
            tvlm::BasicBlock * bbFalse = b_.createBasicBlock();
            tvlm::BasicBlock * bbTrue = b_.createBasicBlock();
            tvlm::BasicBlock * bbAfter = b_.createBasicBlock();
            tvlm::Phi * phi = new tvlm::Phi(tvlm::ResultType::Integer, ast);
            tvlm::CondJump * tmp = new tvlm::CondJump{lhs, ast};
            tmp->addTarget(bbFalse);
            tmp->addTarget(lhsTrue);
            append(tmp);
            b_.enterBasicBlock(lhsTrue);
            tvlm::Instruction * rhs = visitChild(ast->right);//->compileToIR(b, false);
            tmp = new tvlm::CondJump{rhs, ast};
            tmp->addTarget(bbFalse);
            tmp->addTarget(bbTrue);
            append(tmp);
            b_.enterBasicBlock(bbFalse);
            what = b_.add(new tvlm::LoadImm{(int64_t) 0, ast});
            phi->addIncomming(what, bbFalse);
//                b.add(new Instruction::Store(resAddr, what));
            append(new tvlm::Jump{bbAfter, nullptr});
            b_.enterBasicBlock(bbTrue);
            what = append(new tvlm::LoadImm{(int64_t)1, ast});
            phi->addIncomming(what, bbTrue);
//                b.add(new Instruction::Store(resAddr, what));
            append(new tvlm::Jump{bbAfter, nullptr});
            b_.enterBasicBlock(bbAfter);
//                return b.add(new Instruction::Load(resAddr,Instruction::ResultType::Integer, this));
            append(phi); //return
        }else if(ast->op == Symbol::Or) {
            //TODO
        }else{
            visitChild(ast->right);
            tvlm::Instruction * rhs = lastIns_;
            //TODO operator check? so we can throw unsupported bin operator?
            append(new tvlm::BinOp(ast->op, lhs, rhs, ast));

        }
    }
    tvlm::Instruction * resolveAssignment(tvlm::ILBuilder & b, Type * type, tvlm::Instruction * addr, tvlm::Instruction * val, AST const * ast){
        if(dynamic_cast<tinyc::Type::Alias*>(type)){
            return resolveAssignment(b, dynamic_cast<tinyc::Type::Alias*>(type)->base(), addr, val, ast);
        }else if(dynamic_cast<tinyc::Type::POD* >(type)){
            b.add(new tvlm::Store{addr, val, ast});
            return val;
        }else if(dynamic_cast<tinyc::Type::Pointer* >(type)){
            b.add(new tvlm::Store{addr, val, ast});
            return val;
        }else if(dynamic_cast<tinyc::Type::Struct* >(type)){
            //TODO
            return val;


//                return b.getVariableAddress(val->);
        }else if(dynamic_cast<Type::Fun* >(type)){
            b.add(new tvlm::Store{addr, val, ast});
            return val;
        }
        throw "not implemented";
        return nullptr;
    }

    void TvlmFrontend::visit(ASTAssignment * ast) {
        tvlm::Instruction * addr = visitChild(ast->lvalue);//->compileToIR(b, true);
        tvlm::Instruction * val = visitChild(ast->value);//->compileToIR(b, false);
        if(!ast->lvalue->type()->isFullyDefined()){
            throw ParserError(STR("assignment to not fully defined type " << ast->lvalue->type()->toString() ), ast->location(), false);
        }
        resolveAssignment(b_,ast->lvalue->type(), addr, val, ast);
        lastIns_ = val; // return val;
    }

    void TvlmFrontend::visit(ASTUnaryOp * ast) {

        tvlm::Instruction * operand = visitChild(ast->arg);//->compileToIR(b, true);
        if (ast->op == Symbol::Sub) {
            append(new tvlm::BinOp{ast->op, append(new tvlm::LoadImm{(int64_t)0, ast}), operand, ast});
        } else if (ast->op == Symbol::Not || ast->op == Symbol::Neg) {
            append(new tvlm::UnOp{ast->op, operand, ast});
        }else if (ast->op == Symbol::Inc) {
            tvlm::Instruction * res = append(new tvlm::BinOp{Symbol::Add, operand, append(new tvlm::LoadImm{(int64_t)1, ast}), ast});
            append(new tvlm::Store{visitChild(ast->arg)/*->compileToIR(b, true)*/,res, ast});
            lastIns_ = res; //return res;
        } else if (ast->op == Symbol::Dec) {
            tvlm::Instruction * res = append(new tvlm::BinOp{Symbol::Sub, operand, append(new tvlm::LoadImm{(int64_t)1, ast}), ast});
            append(new tvlm::Store{visitChild(ast->arg)/*->compileToIR(b, true)*/,res, ast});
            lastIns_ = res; //return res;
        } else {
            throw ParserError{STR("Unsupported unary operator: " << ast->op), ast->location(), false};
        }
    }

    void TvlmFrontend::visit(ASTUnaryPostOp * ast) {

        tvlm::Instruction * operand = visitChild(ast->arg);//->compileToIR(b, false);
        if (ast->op == Symbol::Inc) {
            tvlm::Instruction * cpy = append(new tvlm::Copy{operand, ast});
            tvlm::Instruction * x = append(new tvlm::BinOp{Symbol::Add, operand, append(new tvlm::LoadImm{(int64_t)1, ast}), ast});
            append(new tvlm::Store( visitChild(ast->arg) /*->compileToIR(b, true)*/ , x, ast));
            lastIns_ = cpy; // return cpy;
        } else if (ast->op == Symbol::Dec) {
            tvlm::Instruction * cpy = append(new tvlm::Copy{operand, ast});
            tvlm::Instruction * x = append(new tvlm::BinOp{Symbol::Sub, operand, append(new tvlm::LoadImm{(int64_t)1, ast}), ast});
            append(new tvlm::Store( visitChild(ast->arg)/*->compileToIR(b, true)*/ , x, ast));
            lastIns_ = cpy; //return cpy;
        } else {
            throw ParserError{STR("Unsupported unary operator: " << ast->op), ast->location(), false};
        }

        throw "not implemented";
    }
    void TvlmFrontend::visit(ASTAddress * ast) {
        visitChild(ast->target);//->compileToIR(b, true);
//        return
    }
    void TvlmFrontend::visit(ASTDeref * ast) {
        if (lvalue_ ) {
            visitChild(ast->target); //->compileToIR(b, false);
//            return
        } else {
            // TODO how about other types:)



            tvlm::Instruction * addr = visitChild(ast->target);//->compileToIR(b, false);
            if(/*double TODO*/ast->type()){
                append(new tvlm::Load{addr, tvlm::ResultType::Double, ast});
            }else{
                append(new tvlm::Load{addr, tvlm::ResultType::Integer, ast});
            }
        }
    }
    void TvlmFrontend::visit(ASTIndex * ast) {
        //TODO
        ASTNamedType * base= dynamic_cast<ASTNamedType *>(ast->base.get());
//        tvlm::ElemAddr * elem = new tvlm::ElemAddr(base, ast);
//        append(elem);
        throw "not implemented ASTIndex";
    }
    void TvlmFrontend::visit(ASTMember * ast) {

        Type * baseType = ast->base->type();
        Type::Struct * type = dynamic_cast<Type::Struct*>(baseType);

        ASTNamedType * base= dynamic_cast<ASTNamedType *>(ast->base.get());
        if(base){
            tvlm:: Instruction * basestruct =b_.getVariableAddress( base->name);

        }

        //TODO
        throw "not implemented ASTMember";
    }
    void TvlmFrontend::visit(ASTMemberPtr * ast) {
        //TODO
        throw "not implemented ASTMemberPtr";
    }
    void TvlmFrontend::visit(ASTCall * ast) {

        std::vector<tvlm::Instruction *> argValues;
        for (auto &i : ast->args){
            argValues.push_back(visitChild(i));//->compileToIR(b, false));
        }
        if (auto i = dynamic_cast<ASTIdentifier*>(ast->function.get())) {
            auto it = b_.functions().find(i->name);
            tvlm::Function * f = b_.findFnc(i->name);
            if(!f){
                throw "idk: ASTCall -compile to IR";
            }

            append(new tvlm::CallStatic{f,std::move(argValues), ast});
//            return
        }else{

            tvlm::Instruction *f = visitChild(ast->function); //->compileToIR(b, false);
            append(new tvlm::Call{f, std::move(argValues), ast});
            //return
//            throw "function pointers not implemented";
        }
    }
    void TvlmFrontend::visit(ASTCast * ast) {
        tvlm::Instruction * val = visitChild(ast->value); // ->compileToIR(b, false);

        if(dynamic_cast<ASTNamedType* >(ast->type.get()) != nullptr){
            if(dynamic_cast<ASTNamedType* >(ast->type.get())->name == Symbol::KwDouble){
                append(new tvlm::Extend(val, ast));
            } else if(dynamic_cast<ASTNamedType* >(ast->type.get())->name == Symbol::KwInt){
                append(new tvlm::Truncate(val, ast));
            }
        }else{
            throw STR("casting "<< val->name() << " to " << ast->type->toString());
        }
    }

} // namespace tinyc

#endif