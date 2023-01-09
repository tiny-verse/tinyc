#if (defined OPTIMIZER_tvlm)

#include "tvlm_frontend.h"
#include "typechecker.h"
#include "frontend.h"

namespace tinyc {
    using CType = tinyc::Type;
    using ILType = tvlm::Type;

    void TvlmFrontend::visit(AST *ast) {
        visitChild(ast);
    }

    void TvlmFrontend::visit(ASTInteger *ast) {
        append(new tvlm::LoadImm{ast->value, ast});
    }

    void TvlmFrontend::visit(ASTDouble *ast) {
        append(new tvlm::LoadImm{ast->value, ast});
    }

    void TvlmFrontend::visit(ASTChar *ast) {
        append(new tvlm::LoadImm{static_cast<int64_t>(ast->value), ast});
    }
  
    void TvlmFrontend::visit(ASTString *ast) {
        lastIns_ = b_.getStringLiteral(ast->value, ast);
    }

    void TvlmFrontend::visit(ASTIdentifier *ast) {
        if (lvalue_) {
            lastIns_ = b_.getVariableAddress(ast->name);
        } else {
            tvlm::Instruction *addr = b_.getVariableAddress(ast->name);
            append(new tvlm::Load{addr, getILType(ast->type()), ast});

        }
    }

    void TvlmFrontend::visit(ASTType *ast) {
        std::cerr << "visited 'ASTType'" << std::endl;
    }

    void TvlmFrontend::visit(ASTPointerType *ast) {
        std::cerr << "visited 'ASTPointerType'" << std::endl;
    }

    void TvlmFrontend::visit(ASTArrayType *ast) {
        std::cerr << "visited 'ASTArrayType'" << std::endl;
    }

    void TvlmFrontend::visit(ASTNamedType *ast) {
        std::cerr << "visited 'ASTNamedType'" << std::endl;
    }

    void TvlmFrontend::visit(ASTSequence *ast) {
        for (auto &i: ast->body) {
            visitChild(i);
        }
    }

    void TvlmFrontend::visit(ASTBlock *ast) {
        for (auto &i: ast->body) {
            visitChild(i);
        }
        lastIns_ = nullptr;
    }

    size_t resolveStructSize(std::vector<std::pair<Symbol, Type *>> fields, Frontend & frontend) {
        size_t acc = 0;
        for (int i = 0; i < fields.size(); i++) {
            Type::Struct * strct = dynamic_cast<tinyc::Type::Struct * >(fields[i].second );
            if(strct){
                acc += resolveStructSize(strct->fields(), frontend);//fields[i].second->type()
            }else if ( dynamic_cast<tinyc::Type::POD *>(fields[i].second ) && fields[i].second == frontend.getTypeDouble()){
                acc += 8;
            }else if (dynamic_cast<tinyc::Type::POD *>(fields[i].second ) && fields[i].second == frontend.getTypeInt()){
                acc += 4;
            }else if (dynamic_cast<tinyc::Type::POD *>(fields[i].second ) && fields[i].second == frontend.getTypeChar()){
                acc += 1;
            }
        }
        return acc ? acc : 1;
    }


    tvlm::Instruction * TvlmFrontend::resolveAccessToMember(ILType::Struct * strct,
                                              tvlm::Instruction * loadAddr, const Symbol & member,
                                              bool lvalue,
                                              const AST * ast){
        tvlm::Instruction * offset = append(new tvlm::LoadImm((int64_t)strct->getFieldOffset(member), ast));
        tvlm::Instruction * addr = append(new tvlm::ElemAddrOffset(loadAddr, offset, ast));
        auto memberType = strct->getFieldType(member);
        auto memberStruct = dynamic_cast<ILType::Type::Struct * >(memberType );
        if(!lvalue && !(memberStruct) ){ // lvalue means we need only address of member, otherwise load the value
            // member of type struct is always address -> no load necessary
            //            tvlm::ResultType resType = ( strct->getFieldType(member)->registerType() );
            return append(new tvlm::Load((tvlm::Instruction *)addr, memberType, ast));
        }
        return addr;
    }

    //returns compiled value
    tvlm::Instruction * TvlmFrontend::resolveValue(Type *type,
                                     AST *srcVal, AST *ast) {
        if (dynamic_cast<tinyc::Type::Alias *>(type)) {
            return resolveValue(dynamic_cast<tinyc::Type::Alias *>(type)->base(),  srcVal, ast);
        } else if (auto pod = dynamic_cast<tinyc::Type::POD * >(type)) {
            tvlm::Instruction * val = visitChild(srcVal, false);
//            append(new tvlm::Store{val, dstAddr, ast});
            return val;
        } else if (auto ptr = dynamic_cast<tinyc::Type::Pointer * >(type)) {
            tvlm::Instruction * val = visitChild(srcVal, false);
//            append(new tvlm::Store{val, dstAddr, ast});
            return val;
        } else if (auto struc = dynamic_cast<tinyc::Type::Struct * >(type)) {
            tvlm::Instruction * val = visitChild(srcVal, true);
//            CType::Struct * strct = dynamic_cast<CType::Struct * >(type);
//               ILType::Struct * tvlmStruct = dynamic_cast<ILType::Struct *>(getILType(strct));
//             return append(new tvlm::StructAssign(val, dstAddr, tvlmStruct, ast));

            return val;

        } else if (auto fun = dynamic_cast<Type::Fun * >(type)) {
            tvlm::Instruction * val = visitChild(srcVal, false);
//            append(new tvlm::Store{val, dstAddr, ast});
            return val;
        }
        throw "not implemented type";
        return nullptr;
    }
    //returns ast value
    tvlm::Instruction * TvlmFrontend::resolveAssignment(Type *type, tvlm::Instruction *dstAddr,
                                                        tvlm::Instruction *val, AST *ast) {
        if (dynamic_cast<tinyc::Type::Alias *>(type)) {
            return resolveAssignment(dynamic_cast<tinyc::Type::Alias *>(type)->base(), dstAddr, val, ast);
        } else if (auto pod = dynamic_cast<tinyc::Type::POD * >(type)) {
//            tvlm::Instruction * val = visitChild(srcVal, false);
            return append(new tvlm::Store{val, dstAddr, ast});
        } else if (auto ptr = dynamic_cast<tinyc::Type::Pointer * >(type)) {
//            tvlm::Instruction * val = visitChild(srcVal, false);
            return append(new tvlm::Store{val, dstAddr, ast});
        } else if (auto struc = dynamic_cast<tinyc::Type::Struct * >(type)) {
//            tvlm::Instruction * val = visitChild(srcVal, true);
            CType::Struct * strct = dynamic_cast<CType::Struct * >(type);
               ILType::Struct * tvlmStruct = dynamic_cast<ILType::Struct *>(getILType(strct));

            return append(new tvlm::StructAssign(val, dstAddr, tvlmStruct, ast));

        } else if (auto fun = dynamic_cast<Type::Fun * >(type)) {
//            tvlm::Instruction * val = visitChild(srcVal, false);
            return append(new tvlm::Store{val, dstAddr, ast});
        }
        throw "not implemented type";
        return nullptr;
    }

    void TvlmFrontend::visit(ASTVarDecl *ast) {

        if (b_.globalEnv()) {
            size_t varSize = ast->type->type()->size();
            auto arrayType = dynamic_cast<ASTArrayType*>(ast->type.get());
            tvlm::Instruction *val = nullptr;
            if (ast->value != nullptr) {
//                val = visitChild(ast->value);
                val = resolveValue(ast->value->type(), ast->value.get(), ast);

            }
            tvlm::Instruction *addr;
            if(arrayType){
                auto ptrType = dynamic_cast<CType::Pointer*>(ast->type->type());
                auto arrSize =  visitChild(arrayType->size);
                auto arr = b_.registerType(new ILType::Array(getILType(ptrType->base()), arrSize));
                addr = append(new tvlm::AllocG{arr,arrSize, ast});
            }else {
                addr = append(new tvlm::AllocG{getILType(ast->type->type()), nullptr, ast});
            }

            resolveAssignment(ast->value->type(), addr, val, ast);
            b_.addGlobalVariable(ast->name->name, addr);
            lastIns_ = val;

        } else {
            size_t varSize = ast->type->type()->size();
            auto arrayType = dynamic_cast<ASTArrayType*>(ast->type.get());
                tvlm::Instruction *addr;
            ILType*  ilType = getILType(ast->type->type());
            tvlm::Instruction *val = nullptr;
            if (ast->value != nullptr) {

//                val = visitChild(ast->value, false);
                val = resolveValue(ast->value->type(), ast->value.get(), ast);
            }
            if(arrayType){
                auto ptrType = dynamic_cast<CType::Pointer*>(ast->type->type());
                auto arrSize =  visitChild(arrayType->size);
//                auto arr = b_.registerType(new ILType::Array(getILType(ptrType->base()), arrSize));
//                addr = append(new tvlm::AllocL{arr,arrSize, ast});
//                auto arr = getILType(ptrType->base());
                auto arr = b_.registerType(new ILType::Array(getILType(ptrType->base()), arrSize));
                auto arrayAddr = append(new tvlm::AllocL{arr,arrSize, ast});
                addr = append(new tvlm::AllocL{ilType, nullptr, ast});
                append(new tvlm::Store{ arrayAddr, addr, ast});

            }else {
                addr = append(new tvlm::AllocL{ilType ,nullptr, ast});
            }

            resolveAssignment(ast->value->type(), addr, val, ast);
            b_.addVariable(ast->name->name, addr);

            lastIns_ = val;
        }


    }

    void TvlmFrontend::visit(ASTFunDecl *ast) {
        tvlm::Function *f = new tvlm::Function{ast};
        f->setName(ast->name.name());
        f->setType(getILType(ast->typeDecl->type()));
        if( frontend_.getTypeDouble() == ast->typeDecl->type() ){
            f->setResultType(tvlm::ResultType::Double);
        }else if (frontend_.getTypeVoid() == ast->typeDecl->type() ){
            f->setResultType(tvlm::ResultType::Void);
        }else{
            f->setResultType(tvlm::ResultType::Integer);
        }
        b_.addGlobalVariable(f->name(), append(new tvlm::LoadImm{(int64_t) 0, ast}));
        b_.enterFunction(f);
//        std::vector<tvlm::Instruction *> arguments;
        for (size_t i = 0, e = ast->args.size(); i != e; ++i) {
            auto ctype = ast->args[i].first.get()->type();
            auto type = getILType(ctype);
            tvlm::Instruction * arg = append(new tvlm::ArgAddr{i, type, ast->args[i].first.get()});
            if(dynamic_cast< ILType::Struct*>(type )){
                tvlm::Instruction * argValue = append(new tvlm::Load{arg, type,ast->args[i].first.get()});
                arg = argValue;
            }

//            arguments.push_back(arg);
            b_.addVariable(ast->args[i].second->name.name(), arg);
        }
        visitChild(ast->body);
        b_.leaveFunction();

    }


    void TvlmFrontend::visit(ASTStructDecl *ast) {
// maybe register type so it s clear which field is an array, so we know precise size
//  ---> no need arrays are sot supported inside struct;
// insufficient info from type analysis cuz array decl = pointer;
//        if (ast->isDefinition) {
//            // add types to IL?
//            // il register new type
//        }
    }

    void TvlmFrontend::visit(ASTFunPtrDecl *ast) {
        //nothing needs to be done already registered by globals?
    }

    void TvlmFrontend::visit(ASTIf *ast) {

        tvlm::Instruction *condVal = visitChild(ast->cond);
        tvlm::BasicBlock *bbTrue = b_.createBasicBlock();
        if (ast->falseCase) {
            tvlm::BasicBlock *bbFalse = b_.createBasicBlock();
            tvlm::BasicBlock *bbAfter = b_.createBasicBlock();

            append(new tvlm::CondJump{condVal, bbTrue, bbFalse, ast});
//            dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bbFalse);
//            dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bbTrue);

            b_.enterBasicBlock(bbTrue);
            visitChild(ast->trueCase);
            append(new tvlm::Jump{bbAfter, nullptr});

            b_.enterBasicBlock(bbFalse);
            visitChild(ast->falseCase);

            append(new tvlm::Jump{bbAfter, nullptr});
            b_.enterBasicBlock(bbAfter);
            lastIns_ = nullptr;

        } else {
            tvlm::BasicBlock *bbAfter = b_.createBasicBlock();

            append(new tvlm::CondJump{condVal, bbTrue, bbAfter, ast});
//            dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bbAfter);
//            dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bbTrue);

            b_.enterBasicBlock(bbTrue);
            visitChild(ast->trueCase);
            append(new tvlm::Jump{bbAfter, nullptr});
            b_.enterBasicBlock(bbAfter);
            lastIns_ = nullptr;

        }


    }

    void TvlmFrontend::visit(ASTSwitch *ast) {
        if (!ast->cases.empty()) {
            tvlm::BasicBlock *bAfter = b_.createBasicBlock("bb_after");
            tvlm::Instruction *condVal = visitChild(ast->cond);
            tvlm::ILBuilder::Context oldContext = b_.enterContext(tvlm::ILBuilder::Context::Switch(bAfter));

            tvlm::BasicBlock *bCase = b_.createBasicBlock("bb_case" + std::to_string(ast->cases.begin()->first));
            append(new tvlm::Jump(bCase, ast));

            tvlm::BasicBlock *bSuccessCmp =
                    b_.createBasicBlock("bb_succ" + std::to_string(ast->cases.begin()->first));
            tvlm::BasicBlock *defaultCaseBB = nullptr;
            for (auto it = ast->cases.begin(); it != ast->cases.end(); it++) {
                auto &c = *it;
                if (c.second.get() == ast->defaultCase) { // DEFAULT CASE
                    auto tmp_bCase = bCase;

                    tvlm::BasicBlock *bSuccessCmpNext;
                    auto nextCaseIt = it+1;
                    if (nextCaseIt != ast->cases.end()) {
                        bCase = b_.createBasicBlock("bb_case_default");
                        tmp_bCase->setName("bb_case_" + std::to_string(nextCaseIt->first) );
                        bSuccessCmpNext = b_.createBasicBlock("bb_succ_" + std::to_string(nextCaseIt->first));
                    } else {
                        b_.enterBasicBlock(bCase);
                        append(new tvlm::Jump(bSuccessCmp, ast));
                        bSuccessCmpNext = bAfter;
                    }
                    defaultCaseBB = b_.enterBasicBlock(bSuccessCmp);
                    bSuccessCmp->setName("bb_default");
                    visitChild(c.second);
                    append(new tvlm::Jump{bSuccessCmpNext, ast});

                    bCase = tmp_bCase;
                    bSuccessCmp = bSuccessCmpNext;
                } else {                        // ORDINARY CASE

                    b_.enterBasicBlock(bCase);

                    tvlm::Instruction *caseVal = append(new tvlm::LoadImm((int64_t) c.first, c.second.get()));
                    tvlm::Instruction * cpy  = append(new tvlm::Copy{condVal, ast});
                    tvlm::Instruction *jmpVal = append(new tvlm::BinOp{tvlm::BinOpType::NEQ, tvlm::Instruction::Opcode::NEQ, cpy, caseVal, ast});

                    tvlm::BasicBlock *bSuccessCmpNext = nullptr;
                    auto nextCaseIt = it+1;
                    if (nextCaseIt != ast->cases.end()) {
                        bCase = b_.createBasicBlock("bb_case_" + std::to_string(nextCaseIt->first));
                        bSuccessCmpNext = b_.createBasicBlock("bb_succ_" + std::to_string(nextCaseIt->first));
                    } else {
                        if (!ast->defaultCase) {
                            bCase = bAfter;
                        } else {
                            if(defaultCaseBB) bCase = defaultCaseBB;
                            else throw "WTF";
                        }
                        bSuccessCmpNext = bAfter;
                    }
                    tvlm::CondJump *condJump = new tvlm::CondJump{
                            jmpVal
                            ,bCase, bSuccessCmp
                            ,ast
                    };
//                    condJump->addTarget(bSuccessCmp);
//                    condJump->addTarget(bCase);
                    append(condJump);
                    b_.enterBasicBlock(bSuccessCmp);
                    visitChild(c.second);
                    b_.add(new tvlm::Jump{bSuccessCmpNext, ast});

                    bSuccessCmp = bSuccessCmpNext;
                }
            }
            b_.leaveContext(oldContext);
            b_.enterBasicBlock(bAfter);
        }
        lastIns_ = nullptr;
    }

    void TvlmFrontend::visit(ASTWhile *ast) {
        tvlm::BasicBlock *bCond = b_.createBasicBlock();
        tvlm::BasicBlock *bBody = b_.createBasicBlock();
        tvlm::BasicBlock *bAfter = b_.createBasicBlock();
        append(new tvlm::Jump{bCond, ast});
        b_.enterBasicBlock(bCond);
        tvlm::ILBuilder::Context oldContext = b_.enterContext(tvlm::ILBuilder::Context::Loop(bAfter, bCond));
        tvlm::Instruction *condVal = visitChild(ast->cond);
        append(new tvlm::CondJump{condVal, bBody, bAfter, ast});
//        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bAfter);
//        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bBody);
        b_.enterBasicBlock(bBody);
        visitChild(ast->body);
        append(new tvlm::Jump{bCond, ast});
        b_.leaveContext(oldContext);
        b_.enterBasicBlock(bAfter);
        lastIns_ = nullptr;

    }

    void TvlmFrontend::visit(ASTDoWhile *ast) {

        tvlm::BasicBlock *bCond = b_.createBasicBlock();
        tvlm::BasicBlock *bBody = b_.createBasicBlock();
        tvlm::BasicBlock *bAfter = b_.createBasicBlock();
        tvlm::ILBuilder::Context oldContext = b_.enterContext(tvlm::ILBuilder::Context::Loop(bAfter, bCond));
        b_.add(new tvlm::Jump(bBody, ast));
        b_.enterBasicBlock(bBody);
        visitChild(ast->body);
        b_.add(new tvlm::Jump(bCond, ast));
        b_.enterBasicBlock(bCond);
        tvlm::Instruction *condVal = visitChild(ast->cond);
        append(new tvlm::CondJump{condVal, bBody, bAfter, ast});
//        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bAfter);
//        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bBody);

        b_.leaveContext(oldContext);
        b_.enterBasicBlock(bAfter);
        lastIns_ = nullptr;
    }

    void TvlmFrontend::visit(ASTFor *ast) {

        tvlm::BasicBlock *bCond = b_.createBasicBlock();
        tvlm::BasicBlock *bBody = b_.createBasicBlock();
        tvlm::BasicBlock *bAfter = b_.createBasicBlock();
        tvlm::BasicBlock *bInc = b_.createBasicBlock();
        visitChild(ast->init);
        append(new tvlm::Jump{bCond, ast->init.get()});
        b_.enterBasicBlock(bCond);
        tvlm::Instruction *condVal = visitChild(ast->cond);
        append(new tvlm::CondJump{condVal, bBody, bAfter, ast->cond.get()});
//        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bAfter);
//        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bBody);

        tvlm::ILBuilder::Context oldContext = b_.enterContext(tvlm::ILBuilder::Context::Loop(bAfter, bInc));
        b_.enterBasicBlock(bBody);
        visitChild(ast->body);
        append(new tvlm::Jump{bInc, ast->increment.get()});
        b_.leaveContext(oldContext);
        b_.enterBasicBlock(bInc);
        visitChild(ast->increment);
        append(new tvlm::Jump{bCond, ast});
        b_.enterBasicBlock(bAfter);
        lastIns_ = nullptr;
    }

    void TvlmFrontend::visit(ASTBreak *ast) {
        tvlm::BasicBlock *target = b_.context().breakTarget;
        if (target == nullptr)
            throw ParserError{"Break can only be used inside loop or switch", ast->location(), false};
        append(new tvlm::Jump{target, ast});
        // we have closed the basic block, so a new one has to be created
        b_.enterBasicBlock(b_.createBasicBlock());
        lastIns_ = nullptr;
    }

    void TvlmFrontend::visit(ASTContinue *ast) {
        tvlm::BasicBlock *target = b_.context().continueTarget;
        if (target == nullptr)
            throw ParserError{"Continue can only be used inside loop", ast->location(), false};
        append(new tvlm::Jump{target, ast});
        // we have closed the basic block, so a new one has to be created
        b_.enterBasicBlock(b_.createBasicBlock());
        lastIns_ = nullptr;
    }

    void TvlmFrontend::visit(ASTReturn *ast) {
        if(ast->value){
            tvlm::Instruction *resultValue ;
            if(dynamic_cast<CType::Struct*>(ast->value->type())){
                resultValue = visitChild(ast->value, true);
            }else{
                resultValue = visitChild(ast->value);
            }
            append(new tvlm::Return{resultValue, getILType(ast->value->type()), ast});
        }else{
            append(new tvlm::Return{nullptr, getILType(frontend_.getTypeVoid()), ast});
        }
        // we have closed the basic block, so a new one has to be created
        b_.enterBasicBlock(b_.createBasicBlock());
        lastIns_ = nullptr;
    }

    void TvlmFrontend::visit(ASTBinaryOp *ast) {
        visitChild(ast->left);
        tvlm::Instruction *lhs = lastIns_;
        //ShortCircuiting
        if (ast->op == Symbol::And) {
            tvlm::Instruction *what = nullptr;
            tvlm::BasicBlock *lhsTrue = b_.createBasicBlock();
            tvlm::BasicBlock *bbFalse = b_.createBasicBlock();
            tvlm::BasicBlock *bbTrue = b_.createBasicBlock();
            tvlm::BasicBlock *bbAfter = b_.createBasicBlock();
            tvlm::Phi *phi = new tvlm::Phi(tvlm::ResultType::Integer, ast);//phi node
            tvlm::CondJump *tmp = new tvlm::CondJump{lhs, lhsTrue, bbFalse, ast};
//            tmp->addTarget(bbFalse); //if L false --> bbFalse
//            tmp->addTarget(lhsTrue); //if L true --> lhsTrue
            append(tmp);
            b_.enterBasicBlock(lhsTrue);
            tvlm::Instruction *rhs = visitChild(ast->right);
            tmp = new tvlm::CondJump{rhs, bbTrue, bbFalse, ast};
//            tmp->addTarget(bbFalse); //if R false --> bbFalse
//            tmp->addTarget(bbTrue);  //if R true --> bbTrue
            append(tmp);

            b_.enterBasicBlock(bbFalse); // results in 0
            what = b_.add(new tvlm::LoadImm{(int64_t) 0, ast});
            phi->addIncomming(what, bbFalse);
            append(new tvlm::Jump{bbAfter, nullptr});

            b_.enterBasicBlock(bbTrue);// results in 1
            what = append(new tvlm::LoadImm{(int64_t) 1, ast});
            phi->addIncomming(what, bbTrue);
            append(new tvlm::Jump{bbAfter, nullptr});

            b_.enterBasicBlock(bbAfter);
            append(phi);

        } else if (ast->op == Symbol::Or) {
            // carry through memory -- unfriendly for optimization
            // but friendly for register allocation

/*
            tvlm::BasicBlock * bbTrue = b_.createBasicBlock();
            tvlm::BasicBlock * lhsFalse = b_.createBasicBlock();
            tvlm::BasicBlock * bbFalse = b_.createBasicBlock();
            tvlm::BasicBlock * bbAfter = b_.createBasicBlock();
            tvlm::Instruction * resAddr = b_.add(new tvlm::AllocL(4, nullptr, ast));
            tvlm::Instruction * what = nullptr;
            tvlm::CondJump *tmp = new tvlm::CondJump{lhs, ast};
            tmp->addTarget(lhsFalse); // if L false --> lhsFalse
            tmp->addTarget(bbTrue);   // if L true -->  bbTrue
            append(tmp);

            b_.enterBasicBlock(lhsFalse);
            tvlm::Instruction * rhs = visitChild(ast->right, false);
            tmp = new tvlm::CondJump{rhs, ast};
            tmp->addTarget(bbFalse); // if R false --> bbFalse
            tmp->addTarget(bbTrue);  // if R true --> bbTrue

            b_.enterBasicBlock(bbFalse);
            what =  b_.add(new tvlm::LoadImm{(int64_t) 0, ast});
            b_.add(new tvlm::Store(resAddr, what, ast));
            b_.add(new tvlm::Jump{bbAfter, nullptr});

            b_.enterBasicBlock(bbTrue);
            what = b_.add(new tvlm::LoadImm{(int64_t) 1, ast});
            b_.add(new tvlm::Store(resAddr, what, ast));
            b_.add(new tvlm::Jump{bbAfter, nullptr});

            b_.enterBasicBlock(bbAfter);
            append(new tvlm::Load(resAddr,tvlm::ResultType::Integer, ast));

            //---------------------------------------------------------------/
/*/
            tvlm::Instruction *what = nullptr;
            tvlm::BasicBlock *lhsFalse = b_.createBasicBlock();
            tvlm::BasicBlock *bbTrue = b_.createBasicBlock();
            tvlm::BasicBlock *bbFalse = b_.createBasicBlock();
            tvlm::BasicBlock *bbAfter = b_.createBasicBlock();
            tvlm::Phi *phi = new tvlm::Phi(tvlm::ResultType::Integer, ast);//phi node
            tvlm::CondJump *tmp = new tvlm::CondJump{lhs, bbTrue, lhsFalse, ast};
//            tmp->addTarget(lhsFalse); //if L true --> lhsTrue
//            tmp->addTarget(bbTrue); //if L false --> bbTrue
            append(tmp);
            b_.enterBasicBlock(lhsFalse);
            tvlm::Instruction *rhs = visitChild(ast->right);
            tmp = new tvlm::CondJump{rhs, bbTrue, bbFalse, ast};
//            tmp->addTarget(bbFalse); //if R false --> bbFalse
//            tmp->addTarget(bbTrue);  //if R true --> bbTrue
            append(tmp);

            b_.enterBasicBlock(bbFalse); // results in 0
            what = b_.add(new tvlm::LoadImm{(int64_t) 0, ast});
            phi->addIncomming(what, bbFalse);
            append(new tvlm::Jump{bbAfter, nullptr});

            b_.enterBasicBlock(bbTrue);// results in 1
            what = append(new tvlm::LoadImm{(int64_t) 1, ast});
            phi->addIncomming(what, bbTrue);
            append(new tvlm::Jump{bbAfter, nullptr});

            b_.enterBasicBlock(bbAfter);
            append(phi);

            /**/
        } else {
            tvlm::BinOpType opcode;
            tvlm::Instruction::Opcode opc;
                if (ast->op == tvlm::Symbol::Add){
                    opcode = tvlm::BinOpType::ADD;
                    opc = tvlm::Instruction::Opcode::ADD;
                }else if (ast->op == tvlm::Symbol::Sub){
                    opcode = tvlm::BinOpType::SUB;
                    opc = tvlm::Instruction::Opcode::SUB;
                }else if (ast->op == tvlm::Symbol::Mul){
                    opcode = tvlm::BinOpType::MUL;
                    opc = tvlm::Instruction::Opcode::MUL;
                }else if (ast->op == tvlm::Symbol::Mod){
                    opcode = tvlm::BinOpType::MOD;
                    opc = tvlm::Instruction::Opcode::MOD;
                }else if (ast->op == tvlm::Symbol::Div){
                    opcode = tvlm::BinOpType::DIV;
                    opc = tvlm::Instruction::Opcode::DIV;
                }else if (ast->op == tvlm::Symbol::ShiftLeft){
                    opcode = tvlm::BinOpType::LSH;
                    opc = tvlm::Instruction::Opcode::LSH;
                }else if (ast->op == tvlm::Symbol::ShiftRight){
                    opcode = tvlm::BinOpType::RSH;
                    opc = tvlm::Instruction::Opcode::RSH;
                }else if (ast->op == tvlm::Symbol::BitAnd){
                    opcode = tvlm::BinOpType::AND;
                    opc = tvlm::Instruction::Opcode::AND;
                }else if (ast->op == tvlm::Symbol::BitOr){
                    opcode = tvlm::BinOpType::OR;
                    opc = tvlm::Instruction::Opcode::OR;
                }else if (ast->op == tvlm::Symbol::Xor) {
                    opcode = tvlm::BinOpType::XOR;
                    opc = tvlm::Instruction::Opcode::XOR;
                }else if (ast->op == tvlm::Symbol::Eq){
                    opcode = tvlm::BinOpType::EQ;
                    opc = tvlm::Instruction::Opcode::EQ;
                }else if (ast->op == tvlm::Symbol::NEq){
                    opcode = tvlm::BinOpType::NEQ;
                    opc = tvlm::Instruction::Opcode::NEQ;
                }else if (ast->op == tvlm::Symbol::Lt){
                    opcode = tvlm::BinOpType::LT;
                    opc = tvlm::Instruction::Opcode::LT;
                }else if (ast->op == tvlm::Symbol::Lte){
                    opcode = tvlm::BinOpType::LTE;
                    opc = tvlm::Instruction::Opcode::LTE;
                }else if (ast->op == tvlm::Symbol::Gt) {
                    opcode = tvlm::BinOpType::GT;
                    opc = tvlm::Instruction::Opcode::GT;
                }else if (ast->op == tvlm::Symbol::Gte) {
                    opcode = tvlm::BinOpType::GTE;
                    opc = tvlm::Instruction::Opcode::GTE;
                }else{
                    throw ParserError(STR("Unsupported binary operator " << ast->op), ast->location());
                }
            visitChild(ast->right);
            tvlm::Instruction *rhs = lastIns_;
            tvlm::Instruction *finalRhs = nullptr;
            tvlm::Instruction *finalLhs = nullptr;
            if(ast->type() == frontend_.getTypeDouble()){
                if(lhs->resultType() == tvlm::ResultType::Integer){
                    finalLhs = append(new tvlm::Extend(lhs, ast));
                }else{
                    finalLhs = lhs;
                }


                if(rhs->resultType() == tvlm::ResultType::Integer){
                    finalRhs = append(new tvlm::Extend(rhs, ast));
                }else{
                    finalRhs = rhs;
                }
            }else{
                finalLhs = lhs;
                finalRhs = rhs;
            }
//            append(new tvlm::BinOp(opcode, tvlm::Instruction::Opcode::BinOp, lhs, rhs, ast));
            append(new tvlm::BinOp(opcode, opc, finalLhs, finalRhs, ast));

        }
    }

    void TvlmFrontend::visit(ASTAssignment *ast) {

        Type *identifierType =
                ast->lvalue->type();
        tvlm::Instruction *val = resolveValue(identifierType, ast->value.get(), ast);

        tvlm::Instruction *addr = visitChild(ast->lvalue, true);


        if (identifierType && !identifierType->isFullyDefined()) {
            throw ParserError(STR("assignment to not fully defined type " << ast->lvalue->type()->toString()),
                              ast->location(), false);
        }
        resolveAssignment(identifierType, addr, val, ast);
//        tvlm::Instruction * res =

        lastIns_ = val;
    }

    void TvlmFrontend::visit(ASTUnaryOp *ast) {

        tvlm::Instruction *operand = visitChild(ast->arg, true);
        if (ast->op == Symbol::Sub) {
            append(new tvlm::UnOp{tvlm::UnOpType::UNSUB, tvlm::Instruction::Opcode::UNSUB, operand, ast});
        } else if (ast->op == Symbol::Not ) {
            append(new tvlm::BinOp{tvlm::BinOpType::EQ, tvlm::Instruction::Opcode::EQ, append(new tvlm::LoadImm{(int64_t) 0, ast}), operand, ast});
        }else if (ast->op == Symbol::Neg){
            //operator ~
            append(new tvlm::UnOp{tvlm::UnOpType::NOT, tvlm::Instruction::Opcode::NOT, operand, ast});
        } else if (ast->op == Symbol::Inc) {
            tvlm::Instruction *res = append(
                    new tvlm::UnOp{tvlm::UnOpType::INC, tvlm::Instruction::Opcode::INC, operand, ast});
            append(new tvlm::Store{res, visitChild(ast->arg, true), ast});
            lastIns_ = res;
        } else if (ast->op == Symbol::Dec) {
            tvlm::Instruction *res = append(
                    new tvlm::UnOp{tvlm::UnOpType::DEC, tvlm::Instruction::Opcode::DEC, operand,ast});
            append(new tvlm::Store{res, visitChild(ast->arg, true), ast});
            lastIns_ = res;
        } else {
            throw ParserError{STR("Unsupported unary operator: " << ast->op), ast->location(), false};
        }
    }

    void TvlmFrontend::visit(ASTUnaryPostOp *ast) {

        tvlm::Instruction *operand = visitChild(ast->arg);
        if (ast->op == Symbol::Inc) {
            tvlm::Instruction *cpy = append(new tvlm::Copy{operand, ast});
            tvlm::Instruction *x = append(
                    new tvlm::UnOp{tvlm::UnOpType::INC, tvlm::Instruction::Opcode::INC, operand, ast});
            append(new tvlm::Store(x, visitChild(ast->arg, true) ,  ast));
            lastIns_ = cpy;
        } else if (ast->op == Symbol::Dec) {
            tvlm::Instruction *cpy = append(new tvlm::Copy{operand, ast});
            tvlm::Instruction *x = append(
                    new tvlm::UnOp{tvlm::UnOpType::DEC, tvlm::Instruction::Opcode::DEC, operand, ast});
            append(new tvlm::Store(x, visitChild(ast->arg, true),  ast));
            lastIns_ = cpy;
        } else {
            throw ParserError{STR("Unsupported unary operator: " << ast->op), ast->location(), false};
        }
    }

    void TvlmFrontend::visit(ASTAddress *ast) {
        visitChild(ast->target, true);

    }

    void TvlmFrontend::visit(ASTDeref *ast) {
        if (lvalue_) {
            visitChild(ast->target);

        } else {
            tvlm::Instruction *addr = visitChild(ast->target);
//            if (frontend_.isPointer(ast->type()))  {
//                lastIns_ = addr;
//            } else {
                append(new tvlm::Load{addr, getILType(ast->type()), ast});
//            }
        }
    }

    void TvlmFrontend::visit(ASTIndex *ast) {
        bool lvalue = lvalue_;
        Type::Pointer * pointer = dynamic_cast<Type::Pointer* >(ast->base->type());
        if(!pointer){
            throw "ASTIndex called on non-pointer type";
        }

        tvlm::Instruction * addrInstr = visitChild(ast->base, true);
        tvlm::Instruction * indexInstr = visitChild(ast->index);
        tvlm::Instruction * offsetInstr = append(new tvlm::LoadImm((int64_t) getILType(pointer->base())->size(), ast));
        tvlm::Instruction * addr =
        append(new tvlm::ElemAddrIndex( addrInstr,offsetInstr,  indexInstr , ast));

        if(lvalue){
            lastIns_ = addr;
        }else{
            lastIns_ = append(new tvlm::Load(addr, getILType(pointer->base()), ast));
        }
    }

    void TvlmFrontend::visit(ASTMember *ast) {
        bool lvalue = lvalue_;
        Type::Struct *type = dynamic_cast<Type::Struct *>(ast->base->type());

        tvlm::Instruction * addrInstr = visitChild(ast->base, true);
        lastIns_ = resolveAccessToMember(dynamic_cast<ILType::Struct *>(getILType(type)),
                                         addrInstr, ast->member, lvalue, ast);
    }

    void TvlmFrontend::visit(ASTMemberPtr *ast) {
        bool lvalue = lvalue_;
        Type *baseType = ast->base->type();

        Type::Pointer *ptype = dynamic_cast<Type::Pointer *>(baseType);
        Type::Struct *type = dynamic_cast<Type::Struct *>(ptype->base());

        tvlm::Instruction * res;
        //resolve address from Pointer
        tvlm::Instruction * loadAddr = append(new tvlm::Load( visitChild(ast->base, true), getILType(type),ast));

        //resolve access to member
        lastIns_ = resolveAccessToMember(
                dynamic_cast<ILType::Struct *>(getILType(type)),
                loadAddr, ast->member, lvalue, ast);
    }


    void TvlmFrontend::visit(ASTCall *ast) {
        std::vector<std::pair< tvlm::Instruction *, tvlm::Type*>> argValues;
        for (auto &i: ast->args) {
            auto ilType = getILType(i->type());
            if(auto strctType= dynamic_cast<ILType::Struct*>(ilType)){
                //copy of a struct
                tvlm::Instruction * addr = visitChild(i, true);
                tvlm::Instruction * cpy =
                        append(new tvlm::AllocL{ilType ,nullptr, ast});
                        append(new tvlm::StructAssign{addr,cpy, strctType , ast});
                argValues.push_back(std::make_pair(cpy, getILType(i->type()) ));
            }else{
                argValues.push_back(std::make_pair(visitChild(i), getILType(i->type()) ));
            }
        }
        if (auto i = dynamic_cast<ASTIdentifier *>(ast->function.get())) {

            tvlm::Function *f = b_.findFnc(i->name);
            if (f) {
                append(new tvlm::CallStatic{f, std::move(argValues), ast});
            }else{
                tvlm::Instruction *f = visitChild(ast->function);
                append(new tvlm::Call{f, getILType(ast->function->type()), std::move(argValues), ast});
            }
        } else {

                throw "ASTCall - failed to compile to IL";

        }
    }

    void TvlmFrontend::visit(ASTCast *ast) {
        tvlm::Instruction *val = visitChild(ast->value);
        ASTNamedType * newType = dynamic_cast<ASTNamedType * >(ast->type.get());

        if(ast->type->type() == ast->value->type()){ //casting T to T
            append(new tvlm::Copy(val, ast)); //to preserve SSA
//            lastIns_ = val; //no cast necessary
        }else if (ast->type->type() == frontend_.getTypeDouble() && ast->value->type() != frontend_.getTypeDouble()) {
            append(new tvlm::Extend(val, ast)); // needs extension to float type
        } else if (ast->type->type() == frontend_.getTypeInt() && ast->value->type() == frontend_.getTypeDouble()) {
            append(new tvlm::Truncate(val, ast)); // needs truncation from float type
        }
    }

    void TvlmFrontend::visit(ASTRead *ast) {
        append(new tvlm::GetChar(ast));
    }

    void TvlmFrontend::visit(ASTWrite *ast) {
        tvlm::Instruction *val = visitChild(ast->value);
        append(new tvlm::PutChar(val, ast));
    }

    ILType *TvlmFrontend::getILType(const Type *pType) {
        auto cAliasType = dynamic_cast<const CType::Alias*>(pType);
        if(cAliasType) {
            return getILType(cAliasType->base());
        }
        auto cPODType =  dynamic_cast<const CType::POD*>(pType);
        if(cPODType && frontend_.getTypeInt() == pType) {
            return b_.registerType(new ILType::Integer());

        }else if(cPODType && frontend_.getTypeDouble() == pType) {
            return b_.registerType(new ILType::Double());

        }else if(cPODType && frontend_.getTypeChar() == pType) {
            return b_.registerType(new ILType::Char());

        }else if(cPODType && frontend_.getTypeVoid() == pType) {
            return b_.registerType(new ILType::Void());

        }
        auto cStructType = dynamic_cast<const CType::Struct*>(pType);
        if(cStructType) {
            std::vector<std::pair<Symbol, ILType *>> fields;
            for(auto & f : cStructType->fields()) {
                fields.emplace_back(f.first, getILType(f.second));
            }
            return b_.registerType(new ILType::Struct(cStructType->ast()->name, fields));
        }
        auto cFunType = dynamic_cast<const CType::Fun*>(pType);
        if(cFunType) {
            return b_.registerType(new ILType::Integer());//no need
        }
        auto cPointerType = dynamic_cast<const CType::Pointer*>(pType);
        if(cPointerType) {
            return b_.registerType(new ILType::Pointer(getILType(cPointerType->base())));
        }

        throw "cannot resolve Type";
        return nullptr;
    }

    tvlm::Instruction *TvlmFrontend::append(tvlm::Instruction *ins)  {
        b_.add(ins);
        lastIns_ = ins;

        return ins;
    }

} // namespace tinyc

#endif