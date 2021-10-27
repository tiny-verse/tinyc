#pragma once

#include "common/ast.h"

namespace tinyc {
namespace il {

    using ASTBase = tiny::ASTBase;

    class BasicBlock;
    class Function;

    /** Result type of an instruction. 
     */ 
    enum class ResultType {
        Integer,
        Double,
        Void,
    }; // tinyc::il::ResultType 


    /** Base class for intermediate language instructions. 
     */ 
    class Instruction {
    public:
        class ImmSize;
        class ImmIndex;
        class ImmValue;
        class BinaryOperator;
        class UnaryOperator;
        class LoadAddress;
        class StoreAddress;
        class Terminator;
        class Terminator0;
        class Terminator1;
        class TerminatorN;

        /** Returns the result type of the instruction. 
         */
        ResultType resultType() const {
            return resultType_;
        }

        std::string const & name() const {
            return name_;
        }

        void setName(std::string const & value) {
            name_ = value;
        }

    protected:

        /** Creates the instruction with given return type and corresponding abstract syntax tree. 
         */
        Instruction(ResultType resultType, ASTBase const * ast):
            resultType_{resultType},
            ast_{ast} {
        }    

    private:


        ResultType resultType_;
        ASTBase const * ast_;
        std::string name_;

    }; // tinyc::il::Instruction

    class Instruction::ImmSize : public Instruction {
    public:

        size_t size() {
            return size_;   
        }

    protected:

        ImmSize(size_t size, ASTBase const * ast):
            Instruction{ResultType::Integer, ast},
            size_{size} {
        }
        size_t size_;
    };

    class Instruction::ImmIndex : public Instruction {
    public:

        size_t index() {
            return index_;   
        }

    protected:

        ImmIndex(size_t index, ASTBase const * ast):
            Instruction{ResultType::Integer, ast},
            index_{index} {
        }

        size_t index_;
    };

    class Instruction::ImmValue : public Instruction {
    public:

        int value() {
            return value_;   
        }

    protected:

        ImmValue(int value, ASTBase const * ast):
            Instruction{ResultType::Integer, ast},
            value_{value} {
        }

        int value_;
    };

    class Instruction::BinaryOperator : public Instruction {
    public:
        Instruction * lhs() const {
            return lhs_;
        }

        Instruction * rhs() const {
            return rhs_;
        }

    protected:
        BinaryOperator(Instruction * lhs, Instruction * rhs, ASTBase const * ast):
            Instruction{GetResultType(lhs, rhs), ast},
            lhs_{lhs},
            rhs_{rhs} {
        }        

        static ResultType GetResultType(Instruction * lhs, Instruction * rhs) {
            assert(lhs->resultType() != ResultType::Void && rhs->resultType() != ResultType::Void);
            if (lhs->resultType() == ResultType::Double || rhs->resultType() == ResultType::Double)
                return ResultType::Double;
            else
                return ResultType::Integer;
        }

    private:
        Instruction * lhs_;
        Instruction * rhs_;    
    };

    class Instruction::UnaryOperator : public Instruction {
    public:
        Instruction * operand() const {
            return operand_;
        }
    protected:
        UnaryOperator(Instruction * operand, ASTBase const * ast):
            Instruction{operand->resultType(), ast},
            operand_{operand} {
        }

    private:
        Instruction * operand_;
    }; 

    class Instruction::LoadAddress : public Instruction {
    public:

        Instruction * address() const { return address_; }

    protected:

        LoadAddress(Instruction * address, ASTBase const * ast):
            Instruction{ResultType::Integer, ast},
            address_{address} {
        }
    private:

        Instruction * address_;

    }; // Instruction::LoadAddress

    class Instruction::StoreAddress : public Instruction {
    public:

        Instruction * value() const { return value_; }

        Instruction * address() const { return address_; }

    protected:

        StoreAddress(Instruction * value, Instruction * address, ASTBase const * ast):
            Instruction{ResultType::Integer, ast},
            value_{value},
            address_{address} {
        }
    private:

        Instruction * value_;
        Instruction * address_;

    }; // Instruction::StoreAddress

    class Instruction::Terminator : public Instruction {
    public:

        virtual size_t numTargets() const = 0;

        virtual BasicBlock * getTarget(size_t i) const = 0;        

    protected:
        Terminator(ASTBase const * ast):
            Instruction{ResultType::Void, ast} {
        }
    };

    class Instruction::Terminator0 : public Instruction::Terminator {
    public:
        size_t numTargets() const override { return 0; }

        BasicBlock * getTarget(size_t i) const override { return nullptr; }

    protected:

        Terminator0(ASTBase const * ast):
            Terminator{ast} {
        }
    };

    class Instruction::Terminator1 : public Instruction::Terminator {
    public:
        size_t numTargets() const override { return 1; }

        BasicBlock * getTarget(size_t i) const override { return i == 1 ? target_ : nullptr; }

    protected:

        Terminator1(BasicBlock * target, ASTBase const * ast):
            Terminator{ast},
            target_{target} {
        }

    private:
        BasicBlock * target_;
    };

    class Instruction::TerminatorN : public Instruction::Terminator {
    public:

        Instruction * condition() const { return cond_; }

        size_t numTargets() const override { return targets_.size(); }

        BasicBlock * getTarget(size_t i) const override { return targets_[i]; }

        void addTarget(BasicBlock * target) {
            targets_.push_back(target);
        }

    protected:

        TerminatorN(Instruction * cond, ASTBase const * ast):
            Terminator{ast},
            cond_{cond} {
        }

    private:
        Instruction * cond_;
        std::vector<BasicBlock *> targets_;
    };






#define INS(NAME, ENCODING) class NAME : public Instruction::ENCODING { \
    public: \
        NAME ENCODING(ENCODING) \
};
#define ImmSize(ENCODING) (size_t size, ASTBase const * ast) : Instruction::ENCODING{size, ast} {}
#define ImmIndex(ENCODING) (size_t index, ASTBase const * ast) : Instruction::ENCODING{index, ast} {}
#define ImmValue(ENCODING) (int value, ASTBase const * ast) : Instruction::ENCODING{value, ast} {}
#define BinaryOperator(ENCODING) (Instruction * lhs, Instruction * rhs, ASTBase const * ast): Instruction::ENCODING{lhs, rhs, ast} {}
#define UnaryOperator(ENCODING) (Instruction * operand, ASTBase const * ast): Instruction::ENCODING{operand, ast} {}
#define Terminator0(ENCODING) (ASTBase const * ast): Instruction::ENCODING{ast} {}
#define Terminator1(ENCODING) (BasicBlock * target, ASTBase const * ast): Instruction::ENCODING{target, ast} {}
#define TerminatorN(ENCODING) (Instruction * condition, ASTBase const * ast): Instruction::ENCODING{condition, ast} {}
#define LoadAddress(ENCODING) (Instruction * address, ASTBase const * ast): Instruction::ENCODING{address, ast} {}
#define StoreAddress(ENCODING) (Instruction * value, Instruction * address, ASTBase const * ast): Instruction::ENCODING{value, address, ast} {}

#include "il_insns.h"


} // namespace tinyc::il
} // namespace tinyc