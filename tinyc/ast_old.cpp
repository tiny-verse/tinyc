#ifdef HAHA

#include "ir.h"
#include "ast.h"
#include "backend.h"
#include "types.h"

namespace tiny {

        Instruction * ASTInteger::compileToIR(IRBuilder & b, bool lvalue) {
            return b.add(new Instruction::LoadImm{value, this});
        }

        Instruction * ASTDouble::compileToIR(IRBuilder & b, bool lvalue) {
            return b.add(new Instruction::LoadImm{value, this});
        }

        Instruction * ASTChar::compileToIR(IRBuilder & b, bool lvalue) {
            return b.add(new Instruction::LoadImm{static_cast<int64_t>(value), this});
        }

        Instruction * ASTString::compileToIR(IRBuilder & b, bool lvalue) {
            return b.getStringLiteral(value);
        }

        Instruction * ASTIdentifier::compileToIR(IRBuilder & b, bool lvalue) {
            if (lvalue) {
                return b.getVariableAddress(name);
            } else {
                Instruction * addr = b.getVariableAddress(name);
                return b.add(new Instruction::Load{addr, Instruction::ResultType::Integer, this});
            }
            /*
            // int :                                 Int*                   int
            // double :                              Double *               double
            // int*:                                 Int **                 int*
            // struct:                               struct *
            if (type()->isStruct() || type()->isArray() {
                return b.getLocalVariable(name);
            } else {
                return b.add(new Instruction::Load{b.getLocalvariable(name), type()->registerType(), this});
            }
            */
        }

        Instruction * ASTSequence::compileToIR(IRBuilder & b, bool lvalue) {
            Instruction * result = nullptr;
            for (auto & i : body)
                result = i->compileToIR(b, false);
            return result;
        }

        Instruction * ASTBlock::compileToIR(IRBuilder & b, bool lvalue) {
            ASTSequence::compileToIR(b, false);
            return nullptr;
        }

        Instruction * ASTVarDecl::compileToIR(IRBuilder & b, bool lvalue) {
            size_t varSize = b.sizeOfType(type->type());
            Instruction * val = value->compileToIR(b, false);
            Instruction * addr = b.add(new Instruction::AllocL{varSize, this});
            b.addVariable(name->name, addr);
            b.add(new Instruction::Store{addr, val, this});
            return val;
        }

        /** Compiles a function.

            Adds the function name to global namespace. Creates environment for the function.

         */
        Instruction * ASTFunDecl::compileToIR(IRBuilder & b, bool lvalue) {
            Function * f = new Function{this};
            b.addGlobalVariable(f->name(), b.add(new Instruction::LoadImm{0, this}));
            b.enterFunction(f);
            for (size_t i = 0, e = args.size(); i != e; ++i) {
                Instruction * arg = b.add(new Instruction::ArgAddr{i, args[i].first.get()});
                b.addVariable(args[i].second->name.name(), arg);
            }
            body->compileToIR(b, false);
            b.leaveFunction();
            return nullptr;
        }

        Instruction * ASTIf::compileToIR(IRBuilder & b, bool lvalue) {
            Instruction * condVal = cond->compileToIR(b, false);
            BasicBlock * bbTrue = b.createBasicBlock();
            BasicBlock * bbFalse = b.createBasicBlock();
            BasicBlock * bbAfter = b.createBasicBlock();
            b.add(new Instruction::CondJump{condVal, bbTrue, bbFalse, this});
            b.enterBasicBlock(bbTrue);
            trueCase->compileToIR(b, false);
            b.add(new Instruction::Jump{bbAfter, nullptr});
            b.enterBasicBlock(bbFalse);
            if (falseCase != nullptr)
                falseCase->compileToIR(b, false);
            b.add(new Instruction::Jump{bbAfter, nullptr});
            b.enterBasicBlock(bbAfter);
            return nullptr;
        }

        Instruction * ASTSwitch::compileToIR(IRBuilder & b, bool lvalue) {
            BasicBlock * bAfter = b.createBasicBlock();
            Instruction * condVal = cond->compileToIR(b, false);
            IRBuilder::Context oldContext = b.enterContext(IRBuilder::Context::Switch(bAfter));

            throw "not implemented";

            b.leaveContext(oldContext);
            b.enterBasicBlock(bAfter);
            return nullptr;
        }

        Instruction * ASTWhile::compileToIR(IRBuilder & b, bool lvalue) {
            BasicBlock * bCond = b.createBasicBlock();
            BasicBlock * bBody = b.createBasicBlock();
            BasicBlock * bAfter = b.createBasicBlock();
            b.add(new Instruction::Jump{bCond});
            b.enterBasicBlock(bCond);
            IRBuilder::Context oldContext = b.enterContext(IRBuilder::Context::Loop(bAfter, bCond));
            Instruction * condVal = cond->compileToIR(b, false);
            b.add(new Instruction::CondJump{condVal, bBody, bAfter, this});
            b.enterBasicBlock(bBody);
            body->compileToIR(b, false);
            b.add(new Instruction::Jump{bCond});
            b.leaveContext(oldContext);
            b.enterBasicBlock(bAfter);
            return nullptr;
        }

        Instruction * ASTDoWhile::compileToIR(IRBuilder & b, bool lvalue) {
            BasicBlock * bCond = b.createBasicBlock();
            BasicBlock * bBody = b.createBasicBlock();
            BasicBlock * bAfter = b.createBasicBlock();
            IRBuilder::Context oldContext = b.enterContext(IRBuilder::Context::Loop(bAfter, bCond));
            b.enterBasicBlock(bBody);
            body->compileToIR(b, false);
            b.enterBasicBlock(bCond);
            Instruction * condVal = cond->compileToIR(b, false);
            b.add(new Instruction::CondJump{condVal, bBody, bAfter, this});
            b.leaveContext(oldContext);
            b.enterBasicBlock(bAfter);
            return nullptr;
        }

        Instruction * ASTFor::compileToIR(IRBuilder & b, bool lvalue) {
            BasicBlock * bCond = b.createBasicBlock();
            BasicBlock * bBody = b.createBasicBlock();
            BasicBlock * bAfter = b.createBasicBlock();
            BasicBlock * bInc = b.createBasicBlock();
            init->compileToIR(b, false);
            b.add(new Instruction::Jump{bCond});
            b.enterBasicBlock(bCond);
            Instruction * condVal = cond->compileToIR(b, false);
            b.add(new Instruction::CondJump{condVal, bBody, bAfter, this});
            IRBuilder::Context oldContext = b.enterContext(IRBuilder::Context::Loop(bAfter, bInc));
            b.enterBasicBlock(bBody);
            body->compileToIR(b, false);
            b.add(new Instruction::Jump{bInc});
            b.leaveContext(oldContext);
            b.enterBasicBlock(bInc);
            increment->compileToIR(b, false);
            b.add(new Instruction::Jump{bCond});
            b.enterBasicBlock(bAfter);
            return nullptr;
        }

        Instruction * ASTBreak::compileToIR(IRBuilder & b, bool lvalue) {
            BasicBlock * target = b.context().breakTarget;
            if (target == nullptr)
                throw ParserError{"Break can only be used inside loop or switch", location(), false};
            b.add(new Instruction::Jump{target, this});
            // we have closed the basic block, so a new one has to be created
            b.enterBasicBlock(b.createBasicBlock());
            return nullptr;
        }

        Instruction * ASTContinue::compileToIR(IRBuilder & b, bool lvalue){
            BasicBlock * target = b.context().continueTarget;
            if (target == nullptr)
                throw ParserError{"Continue can only be used inside loop", location(), false};
            b.add(new Instruction::Jump{target, this});
            // we have closed the basic block, so a new one has to be created
            b.enterBasicBlock(b.createBasicBlock());
            return nullptr;
        }

        Instruction * ASTReturn::compileToIR(IRBuilder & b, bool lvalue) {
            if (value != nullptr) {
                Instruction * resultValue = value->compileToIR(b, false);
                b.add(new Instruction::Return{resultValue, this});
            } else {
                b.add(new Instruction::Return{this});
            }
            // we have closed the basic block, so a new one has to be created
            b.enterBasicBlock(b.createBasicBlock());
            return nullptr;
        }

        bool ASTBinaryOp::hasAddress() const {
            return false;
        }

        Instruction * ASTBinaryOp::compileToIR(IRBuilder & b, bool lvalue) {
            Instruction * lhs = left->compileToIR(b, false);
            Instruction * rhs = right->compileToIR(b, false);
            if (op == Symbol::Add) {
               return  b.add(new Instruction::Add{lhs, rhs, this});
            } else if (op == Symbol::Sub) {
                return b.add(new Instruction::Sub{lhs, rhs, this});
            } else if (op == Symbol::Mul) {
                return b.add(new Instruction::Mul{lhs, rhs, this});
            } else if (op == Symbol::Div) {
                return b.add(new Instruction::Div{lhs, rhs, this});
            } else if (op == Symbol::Mod) {
                return b.add(new Instruction::Mod{lhs, rhs, this});
            } else if (op == Symbol::ShiftRight) {
                return b.add(new Instruction::Shr{lhs, rhs, this});
            } else if (op == Symbol::ShiftLeft) {
                return b.add(new Instruction::Shl{lhs, rhs, this});
            } else if (op == Symbol::And) {
                return b.add(new Instruction::And{lhs, rhs, this});
            } else if (op == Symbol::Or) {
                return b.add(new Instruction::Or{lhs, rhs, this});
            } else if (op == Symbol::Xor) {
                return b.add(new Instruction::Xor{lhs, rhs, this});
            } else {
                throw ParserError{STR("Unsupported binary operator: " << op), location(), false};
            }
        }

        Instruction * ASTAssignment::compileToIR(IRBuilder & b, bool lvalue) {
            // TODO
            Instruction * addr = this->lvalue->compileToIR(b, true);
            Instruction * val = value->compileToIR(b, false);
            b.add(new Instruction::Store{addr, val, this});
            return val;
        }

        bool ASTUnaryOp::hasAddress() const {
            return false;
        }

        Instruction * ASTUnaryOp::compileToIR(IRBuilder & b, bool lvalue) {
            /*
            Instruction * operand = arg->compileToIR(b);
            if (op == Symbol::Not) {
               return b.add(new Instruction::Not{operand, this});
            } if (op == Symbol::Inc) {
               return b.add(new Instruction::Add{operand, b.add(new Instruction::LoadImm{1}), this});
            } if (op == Symbol::Dec) {
               return b.add(new Instruction::Sub{operand, b.add(new Instruction::LoadImm{1}), this});
            } else {
                throw ParserError{STR("Unsupported unary operator: " << op), location(), false};
            }
            */
            throw "not implemented";

        }

        Instruction * ASTUnaryPostOp::compileToIR(IRBuilder & b, bool lvalue) {
            /*
            Instruction * operand = arg->compileToIR(b);
            if (op == Symbol::Inc) {
               Instruction * x = b.add(new Instruction::Add{operand, b.add(new Instruction::LoadImm{1}), this});
               b.add(new Instruction::Store{, x});
            } if (op == Symbol::Dec) {
               b.add(new Instruction::Sub{operand, b.add(new Instruction::LoadImm{1}), this});
            } else {
                throw ParserError{STR("Unsupported unary operator: " << op), location(), false};
            }
            return operand;
            */
            throw "not implemented";
        }

        Instruction * ASTAddress::compileToIR(IRBuilder & b, bool lvalue) {
            return target->compileToIR(b, true);
        }

        Instruction * ASTDeref::compileToIR(IRBuilder & b, bool lvalue) {
            if (lvalue) {
                return target->compileToIR(b, false);
            } else {
                // TODO how about other types:)
                Instruction * addr = target->compileToIR(b, false);
                return b.add(new Instruction::Load{addr, Instruction::ResultType::Integer, this});
            }
        }

        Instruction * ASTIndex::compileToIR(IRBuilder & b, bool lvalue) {
            throw "not implemented";
        }

        Instruction * ASTMember::compileToIR(IRBuilder & b, bool lvalue) {
            throw "not implemented";
        }

        Instruction * ASTMemberPtr::compileToIR(IRBuilder & b, bool lvalue) {
            throw "not implemented";
        }

        Instruction * ASTCall::compileToIR(IRBuilder & b, bool lvalue) {
            Instruction * f = function->compileToIR(b, false);
            std::vector<Instruction *> argValues;
            for (auto & i : args)
                argValues.push_back(i->compileToIR(b, false));
            return b.add(new Instruction::Call{f, std::move(argValues), this});
        }

        Instruction * ASTCast::compileToIR(IRBuilder & b, bool lvalue) {
            throw "not implemented";
        }

}

#endif