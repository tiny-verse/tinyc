#if (defined OPTIMIZER_tvlm)

#include "tvlm_frontend.h"
#include "typechecker.h"
#include "frontend.h"

namespace tinyc {

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
            if (/*is double*/ ast->type() == frontend_.getTypeDouble()) {
                append(new tvlm::Load{addr, tvlm::ResultType::Double, ast});
            } else {
                append(new tvlm::Load{addr, tvlm::ResultType::Integer, ast});
            }


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


    tvlm::Instruction * resolveAssignment(tvlm::ILBuilder &b, Type *type, tvlm::Instruction *addr,
                                          tvlm::Instruction *val, AST const *ast) {
        if (dynamic_cast<tinyc::Type::Alias *>(type)) {
            return resolveAssignment(b, dynamic_cast<tinyc::Type::Alias *>(type)->base(), addr, val, ast);
        } else if (dynamic_cast<tinyc::Type::POD * >(type)) {
            b.add(new tvlm::Store{val, addr,ast});
            return val;
        } else if (dynamic_cast<tinyc::Type::Pointer * >(type)) {
            b.add(new tvlm::Store{val, addr,ast});
            return val;
        } else if (dynamic_cast<tinyc::Type::Struct * >(type)) {
            Type::Struct * strct = dynamic_cast<tinyc::Type::Struct * >(type);
            //TODO dummy linear memcpy ? or represent with store and resolve by compiling to target
            tvlm::Instruction * off = b.add(new tvlm::LoadImm((int64_t) 1, ast));
            for (auto & f : strct->fields()) {
                tvlm::ResultType fieldType = f.second->size() == 8 ? tvlm::ResultType::Double : tvlm::ResultType::Integer;
                tvlm::ElemAddr * valElem =  new tvlm::ElemAddr(val, ast);
                valElem->addIndex(off, strct->getFieldOffset(f.first));
                tvlm::Instruction * valAddr = b.add(valElem);
                tvlm::Instruction * tmp = b.add(new tvlm::Load( valAddr,fieldType, ast));

                tvlm::ElemAddr * resElem =  new tvlm::ElemAddr( addr, ast);
                resElem->addIndex(off, strct->getFieldOffset(f.first));
                tvlm::Instruction * resAddr = b.add(resElem);

                b.add(new tvlm::Store(tmp, resAddr, ast ));
            }

            return val;

        } else if (dynamic_cast<Type::Fun * >(type)) {
            b.add(new tvlm::Store{val, addr, ast});
            return val;
        }
        throw "not implemented type";
        return nullptr;
    }

    void TvlmFrontend::visit(ASTVarDecl *ast) {

        if (b_.globalEnv()) {
            size_t varSize = ast->type->type()->size();
            auto arrayType = dynamic_cast<ASTArrayType*>(ast->type.get());
            tvlm::Instruction *addr;
            if(arrayType){
                addr = append(new tvlm::AllocG{varSize, visitChild(arrayType->base), ast});
            }else {
                addr = append(new tvlm::AllocG{varSize,nullptr, ast});
            }
            tvlm::Instruction *val = nullptr;
            if (ast->value != nullptr) {
                val = visitChild(ast->value);
                resolveAssignment(b_, ast->value->type(), addr, val, ast);
            }
            b_.addGlobalVariable(ast->name->name, addr);
            lastIns_ = val;

        } else {
            size_t varSize = ast->type->type()->size();
            auto arrayType = dynamic_cast<ASTArrayType*>(ast->type.get());
                tvlm::Instruction *addr;
            if(arrayType){
                varSize = arrayType->base->type()->size();
                addr = append(new tvlm::AllocL{varSize * staticalyResolve(arrayType->size.get()), visitChild(arrayType->size), ast});
            }else {
                addr = append(new tvlm::AllocL{varSize,nullptr, ast});
            }
            tvlm::Instruction *val = nullptr;
            if (ast->value != nullptr) {

                val = visitChild(ast->value);
                resolveAssignment(b_, ast->value->type(), addr, val, ast);
            }
            b_.addVariable(ast->name->name, addr);

            lastIns_ = val;
        }


    }

    void TvlmFrontend::visit(ASTFunDecl *ast) {
        tvlm::Function *f = new tvlm::Function{ast};
        f->setName(ast->name.name());
        b_.addGlobalVariable(f->name(), append(new tvlm::LoadImm{(int64_t) 0, ast}));
        b_.enterFunction(f);
        for (size_t i = 0, e = ast->args.size(); i != e; ++i) {
            tvlm::Instruction *arg = append(new tvlm::ArgAddr{i, ast->args[i].first.get()});
            b_.addVariable(ast->args[i].second->name.name(), arg);
        }
        visitChild(ast->body);
        b_.leaveFunction();

    }

    size_t resolveStructSize(std::vector<std::pair<std::unique_ptr<ASTIdentifier>, std::unique_ptr<ASTType>>> &fields) {
        size_t acc = 0;
        for (int i = 0; i < fields.size(); i++) {
            acc += 4;//fields[i].second->type()
        }
        return acc ? acc : 1;
    }

    void TvlmFrontend::visit(ASTStructDecl *ast) {

        if (ast->isDefinition) {
            //TODO add types to IL?
            // il register new type
        }
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

            append(new tvlm::CondJump{condVal, ast});
            dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bbFalse);
            dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bbTrue);

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

            append(new tvlm::CondJump{condVal, ast});
            dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bbAfter);
            dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bbTrue);

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
                if (c.second == ast->defaultCase) {

                    tvlm::BasicBlock *bSuccessCmpNext;
                    auto nextCaseIt = it++;
                    if (nextCaseIt != ast->cases.end()) {
                        bCase->setName("bb_case_" + std::to_string(nextCaseIt->first));
                        bSuccessCmpNext = b_.createBasicBlock("bb_succ" + std::to_string(nextCaseIt->first));
                    } else {
                        b_.enterBasicBlock(bCase);
                        append(new tvlm::Jump(bAfter, ast));

                        bSuccessCmpNext = bAfter;
                    }
                    defaultCaseBB = b_.enterBasicBlock(bSuccessCmp);
                    bSuccessCmp->setName("bb_default");
                    visitChild(c.second);
                    append(new tvlm::Jump{bSuccessCmpNext, ast});

                    bSuccessCmp = bSuccessCmpNext;
                } else {

                    b_.enterBasicBlock(bCase);

                    tvlm::Instruction *caseVal = append(new tvlm::LoadImm((int64_t) c.first, c.second.get()));
                    tvlm::Instruction *jmpVal = append(new tvlm::BinOp{Symbol::NEq, tvlm::Instruction::Opcode::NEQ, condVal, caseVal, ast});

                    tvlm::BasicBlock *bSuccessCmpNext;
                    auto nextCaseIt = it++;
                    if (nextCaseIt != ast->cases.end()) {
                        bCase = b_.createBasicBlock("bb_case_" + std::to_string(nextCaseIt->first));
                        bSuccessCmpNext = b_.createBasicBlock("bb_succ_ " + std::to_string(nextCaseIt->first));
                    } else {
                        if (!ast->defaultCase) {
                            bCase = bAfter;
                        } else {
                            bCase = defaultCaseBB;
                        }
                        bSuccessCmpNext = bAfter;
                    }
                    tvlm::CondJump *condJump = new tvlm::CondJump{
                            jmpVal,
                            ast
                    };
                    condJump->addTarget(bSuccessCmp);
                    condJump->addTarget(bCase);
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
        append(new tvlm::CondJump{condVal, ast});
        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bAfter);
        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bBody);
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
        b_.enterBasicBlock(bBody);
        visitChild(ast->body);
        b_.enterBasicBlock(bCond);
        tvlm::Instruction *condVal = visitChild(ast->cond);
        append(new tvlm::CondJump{condVal, ast});
        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bAfter);
        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bBody);

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
        append(new tvlm::CondJump{condVal, ast->cond.get()});
        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bAfter);
        dynamic_cast<tvlm::CondJump *>(lastIns_)->addTarget(bBody);

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
        tvlm::Instruction *resultValue = visitChild(ast->value, false);
        append(new tvlm::Return{resultValue, ast});
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
            tvlm::CondJump *tmp = new tvlm::CondJump{lhs, ast};
            tmp->addTarget(bbFalse); //if L false --> bbFalse
            tmp->addTarget(lhsTrue); //if L true --> lhsTrue
            append(tmp);
            b_.enterBasicBlock(lhsTrue);
            tvlm::Instruction *rhs = visitChild(ast->right);
            tmp = new tvlm::CondJump{rhs, ast};
            tmp->addTarget(bbFalse); //if R false --> bbFalse
            tmp->addTarget(bbTrue);  //if R true --> bbTrue
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
            // but friendly for register allocation TODO
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
        } else {
            tvlm::Instruction::Opcode opcode;
                if (ast->op == tvlm::Symbol::Add){
                    opcode = tvlm::Instruction::Opcode::ADD;
                }else if (ast->op == tvlm::Symbol::Sub){
                    opcode = tvlm::Instruction::Opcode::SUB;
                }else if (ast->op == tvlm::Symbol::Mul){
                    opcode = tvlm::Instruction::Opcode::MUL;
                }else if (ast->op == tvlm::Symbol::Mod){
                    opcode = tvlm::Instruction::Opcode::MOD;
                }else if (ast->op == tvlm::Symbol::Div){
                    opcode = tvlm::Instruction::Opcode::DIV;
                }else if (ast->op == tvlm::Symbol::ShiftLeft){
                    opcode = tvlm::Instruction::Opcode::LSH;
                }else if (ast->op == tvlm::Symbol::ShiftRight){
                    opcode = tvlm::Instruction::Opcode::RSH;
                }else if (ast->op == tvlm::Symbol::BitAnd){
                    opcode = tvlm::Instruction::Opcode::AND;
                }else if (ast->op == tvlm::Symbol::BitOr){
                    opcode = tvlm::Instruction::Opcode::OR;
                }else if (ast->op == tvlm::Symbol::Xor) {
                    opcode = tvlm::Instruction::Opcode::XOR;
                }else if (ast->op == tvlm::Symbol::Eq){
                    opcode = tvlm::Instruction::Opcode::EQ;
                }else if (ast->op == tvlm::Symbol::NEq){
                    opcode = tvlm::Instruction::Opcode::NEQ;
                }else if (ast->op == tvlm::Symbol::Lt){
                    opcode = tvlm::Instruction::Opcode::LT;
                }else if (ast->op == tvlm::Symbol::Lte){
                    opcode = tvlm::Instruction::Opcode::LTE;
                }else if (ast->op == tvlm::Symbol::Gt) {
                    opcode = tvlm::Instruction::Opcode::GT;
                }else if (ast->op == tvlm::Symbol::Gte) {
                    opcode = tvlm::Instruction::Opcode::GTE;
                }else{
                    throw ParserError(STR("Unsupported binary operator " << ast->op), ast->location());
                }
            visitChild(ast->right);
            tvlm::Instruction *rhs = lastIns_;
            append(new tvlm::BinOp(ast->op, opcode, lhs, rhs, ast));

        }
    }

    void TvlmFrontend::visit(ASTAssignment *ast) {
        tvlm::Instruction *addr = visitChild(ast->lvalue, true);
        tvlm::Instruction *val = visitChild(ast->value);

        Type *identifierType =
                ast->lvalue->type();

        if (identifierType && !identifierType->isFullyDefined()) {
            throw ParserError(STR("assignment to not fully defined type " << ast->lvalue->type()->toString()),
                              ast->location(), false);
        }
        resolveAssignment(b_, identifierType, addr, val, ast);
        lastIns_ = val;
    }

    void TvlmFrontend::visit(ASTUnaryOp *ast) {

        tvlm::Instruction *operand = visitChild(ast->arg, true);
        if (ast->op == Symbol::Sub) {
            append(new tvlm::BinOp{ast->op, tvlm::Instruction::Opcode::UNSUB, append(new tvlm::LoadImm{(int64_t) 0, ast}), operand, ast});
        } else if (ast->op == Symbol::Not ) {
            append(new tvlm::BinOp{Symbol::Eq, tvlm::Instruction::Opcode::EQ, append(new tvlm::LoadImm{(int64_t) 0, ast}), operand, ast});
        }else if (ast->op == Symbol::Neg){
            //operator ~
            append(new tvlm::UnOp{ast->op, tvlm::Instruction::Opcode::NOT, operand, ast});
        } else if (ast->op == Symbol::Inc) {
            tvlm::Instruction *res = append(
                    new tvlm::UnOp{ast->op, tvlm::Instruction::Opcode::INC, operand, ast});
            append(new tvlm::Store{res, visitChild(ast->arg, true), ast});
            lastIns_ = res;
        } else if (ast->op == Symbol::Dec) {
            tvlm::Instruction *res = append(
                    new tvlm::UnOp{ast->op, tvlm::Instruction::Opcode::DEC, operand,ast});
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
                    new tvlm::UnOp{ast->op, tvlm::Instruction::Opcode::INC, operand, ast});
            append(new tvlm::Store(x, visitChild(ast->arg, true) ,  ast));
            lastIns_ = cpy;
        } else if (ast->op == Symbol::Dec) {
            tvlm::Instruction *cpy = append(new tvlm::Copy{operand, ast});
            tvlm::Instruction *x = append(
                    new tvlm::UnOp{ast->op, tvlm::Instruction::Opcode::DEC, operand, ast});
            append(new tvlm::Store(x, visitChild(ast->arg, true),  ast));
            lastIns_ = cpy;
        } else {
            throw ParserError{STR("Unsupported unary operator: " << ast->op), ast->location(), false};
        }

        throw STR("not implemented ASTUnaryPostOp op:" << ast->op);
    }

    void TvlmFrontend::visit(ASTAddress *ast) {
        visitChild(ast->target, true);

    }

    void TvlmFrontend::visit(ASTDeref *ast) {
        if (lvalue_) {
            visitChild(ast->target);

        } else {
            tvlm::Instruction *addr = visitChild(ast->target);
            if (ast->type() == frontend_.getTypeDouble()) {
                append(new tvlm::Load{addr, tvlm::ResultType::Double, ast});
            } else if (frontend_.isPointer(ast->type()))  {
                lastIns_ = addr;
            } else {

                append(new tvlm::Load{addr, tvlm::ResultType::Integer, ast});
            }
        }
    }

    void TvlmFrontend::visit(ASTIndex *ast) {
        bool lvalue = lvalue_;
        Type::Pointer * pointer = dynamic_cast<Type::Pointer* >(ast->base->type());
        if(!pointer){
            throw "ASTIndex called on non-pointer type";
        }
        tvlm::ElemAddr * addr = new tvlm::ElemAddr(visitChild(ast->base, true), ast);
        addr->addIndex(visitChild(ast->index), pointer->base()->size() );
        append(addr);
        if(lvalue){
            lastIns_ = addr;
        }else{
            if(ast->base->type() == frontend_.getTypeDouble()){
                lastIns_ = append(new tvlm::Load(addr, tvlm::ResultType::Double, ast));
            }else{
                lastIns_ = append(new tvlm::Load(addr, tvlm::ResultType::Integer, ast));
            }
        }
    }


    void TvlmFrontend::visit(ASTMember *ast) {
        bool lvalue = lvalue_;
        Type::Struct *type = dynamic_cast<Type::Struct *>(ast->base->type());

         tvlm::Instruction * res;
        //resolve access to member
        tvlm::ElemAddr * addr = new tvlm::ElemAddr(visitChild(ast->base, true), ast);
        tvlm::Instruction * off = append(new tvlm::LoadImm((int64_t)1, ast));
        addr->addIndex( off, type->getFieldOffset(ast->member));
        res = append( addr );
        if(!lvalue){ // lvalue means we need only address of member, otherwise load the value
            tvlm::ResultType resType = ( type->getFieldType(ast->member) == frontend_.getTypeDouble() ) ?
                    tvlm::ResultType::Double : tvlm::ResultType::Integer;
            append(new tvlm::Load(res, resType, ast));
        }

    }



    void TvlmFrontend::visit(ASTMemberPtr *ast) {
        bool lvalue = lvalue_;
        Type *baseType = ast->base->type();

        Type::Pointer *ptype = dynamic_cast<Type::Pointer *>(baseType);
        Type::Struct *type = dynamic_cast<Type::Struct *>(ptype->base());

        tvlm::Instruction * res;
        //resolve address from Pointer
        tvlm::Instruction * loadAddr = append(new tvlm::Load( visitChild(ast->base, true),tvlm::ResultType::Integer,ast));

        //resolve access to member
        tvlm::ElemAddr * elem = new tvlm::ElemAddr(loadAddr, ast);
        tvlm::Instruction * off = append(new tvlm::LoadImm((int64_t)1, ast));
        elem->addIndex(off, type->getFieldOffset(ast->member));
        res = append(elem);
        if(!lvalue){ // lvalue means we need only address of member, otherwise load the value
            tvlm::ResultType resType = ( type->getFieldType(ast->member) == frontend_.getTypeDouble() ) ?
                                       tvlm::ResultType::Double : tvlm::ResultType::Integer;
            append(new tvlm::Load(res, resType, ast));
        }
    }

    void TvlmFrontend::visit(ASTCall *ast) {
        std::vector<tvlm::Instruction *> argValues;
        for (auto &i: ast->args) {
            argValues.push_back(visitChild(i));
        }
        if (auto i = dynamic_cast<ASTIdentifier *>(ast->function.get())) {
            auto it = b_.functions().find(i->name);
            tvlm::Function *f = b_.findFnc(i->name);
            if (f) {
                append(new tvlm::CallStatic{f, std::move(argValues), ast});
            }else{
                tvlm::Instruction *f = visitChild(ast->function);
                append(new tvlm::Call{f, std::move(argValues), ast});
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
        }else if (ast->type->type() == frontend_.getTypeDouble() && ast->value->type() != frontend_.getTypeDouble()) {
            append(new tvlm::Extend(val, ast)); // needs extension to float type
        } else if (ast->type->type() == frontend_.getTypeInt() && ast->value->type() == frontend_.getTypeDouble()) {
            append(new tvlm::Truncate(val, ast)); // needs truncation from float type
        }
        lastIns_ = val; //no cast necessary
    }

    void TvlmFrontend::visit(ASTRead *ast) {
        append(new tvlm::GetChar(ast));
    }

    void TvlmFrontend::visit(ASTWrite *ast) {
        tvlm::Instruction *val = visitChild(ast->value);
        append(new tvlm::PutChar(val, ast));
    }

} // namespace tinyc

#endif