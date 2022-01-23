#pragma once

#include "common/symbol.h"

#include "ast.h"

namespace tinyc {

    /** Representation of a tinyc type.
     */
    class Type {
    public:

        class Alias;
        class POD;
        class Pointer;
        class Struct;
        class Fun;

        virtual ~Type() = default;

        /** Converts the type to a printable string.
         */
        std::string toString() const {
            std::stringstream ss;
            toStream(ss);
            return ss.str();
        }

        /** Determines if the type is fully defined.
            A fully defined type can be instantiated. In general all types with the exception of forward declared structures are fully defined.
         */
        virtual bool isFullyDefined() const {
            return true;
        }


        virtual int size()const = 0;
    private:

        virtual void toStream(std::ostream & s) const = 0;

    }; // tinyc::Type

    /** A type alias, i.e. a different name for same type.
     */
    class Type::Alias : public Type {
    public:
        Alias(Symbol name, Type * base):
            name_{name},
            base_{base} {
        }

        bool isFullyDefined() const override {
            return base_->isFullyDefined();
        }

        Type * base() const {
            return base_;
        }
        int size() const override{
            return base_->size();
        }
    private:

        void toStream(std::ostream & s) const override {
            s << name_.name();
        }

        Symbol name_;
        Type * base_;

    }; // tinyc::Type::Alias

    /** Plain old data type declaration.
        These are created automatically by the backend for the primitive types supported by the language. In the case of tinyC, these are:
        - int
        - double
        - char
        - void ??
     */
    class Type::POD : public Type {
    public:

        POD(Symbol name):
            name_{name} {
        }
        int size() const override{
            if(name_ == Symbol::KwInt){
                return 4;
            }else if (name_ == Symbol::KwDouble){
                return 8;
            } else if (name_ == Symbol::KwChar){
                return 1;
            }

            std::cerr << "unknown type " << name_.name() << ": counting with size 4" <<std::endl;
            return 4;
        }

    private:

        void toStream(std::ostream & s) const override {
            s << name_.name();
        }

        Symbol name_;
    }; // tinyc::Type::POD


    /** Pointer to a type. 
     */
    class Type::Pointer : public Type {
    public:
        Pointer(Type * base):
            base_{base} {
        }

        Type * base() const {
            return base_;
        }
        int size() const override{
            return 4;
        }

    private:

        void toStream(std::ostream & s) const override {
            base_->toStream(s);
            s << "*";
        }

        Type * base_;
    };

    /** Structure declaration.
     * 
        Keeps a mapping from the fields to their types and the AST where the type was declared.
     */
    class Type::Struct : public Type {
    public:

        Struct(ASTStructDecl * ast):
            ast_{ast} {
        }

        ASTStructDecl * ast() const {
            return ast_;
        }

        /** Struct type is fully defined if its ast the definition, not just forward declaration of the type.
         */
        bool isFullyDefined() const override {
            return ast_->isDefinition;
        }

        void updateDefinition(ASTStructDecl * ast) {
            assert(ast_ == nullptr || ! ast_->isDefinition);
            ast_ = ast;
        }

        void addField(Symbol name, Type * type, AST * ast) {
            if (!type->isFullyDefined())
                throw ParserError{STR("Field " << name.name() << " has not fully defined type " << type->toString()), ast->location()};
            for (auto & i : fields_)
                if (i.first == name)
                    throw ParserError{STR("Field " << name.name() << " already defined "), ast->location()};
            fields_.push_back(std::make_pair(name, type));
        }

        Type * getFieldType(Symbol name) const {
            for (auto & i : fields_)
                if (i.first == name)
                    return i.second;
            return nullptr;
        }


        int size()const{
            int size = 0;
            for(auto & i : fields_){
                size += i.second->size();
            }
            return size ? size : 4; //every struct has to have a memory footprint
        }
       const std::vector<std::pair<Symbol, Type *>> & fields()const{
            return fields_;
        }
    private:

        friend class TypeChecker;

        void toStream(std::ostream & s) const override {
            s << "struct " << ast_->name.name();
        }

        ASTStructDecl * ast_;
        std::vector<std::pair<Symbol, Type *>> fields_;
    }; // tinyc::Type::Struct

    /** Function type declaration.
     * 
        A function type. Determines the types of the arguments (we do not need names for the type as tinyC does not support keyword arguments or other fancy things) and the return type.
     */
    class Type::Fun : public Type {
    public:

        Fun(Type * returnType):
            returnType_{returnType} {
        }

        Type * returnType() const {
            return returnType_;
        }

        void addArgument(Type * type) {
            args_.push_back(type);
        }

        size_t numArgs() const {
            return args_.size();
        }

        Type * argType(size_t i) const {
            return args_[i];
        }
        int size()const{
            std::cerr << "calling size at function type" << std::endl;
            return 4;
        }

    private:

        void toStream(std::ostream & s) const override {
            returnType_->toStream(s);
            s << " (";
            auto i = args_.begin();
            auto e = args_.end();
            if (i != e) {
                (*i)->toStream(s);
                while (++i != e) {
                    s << ", ";
                    (*i)->toStream(s);
                }
            }
            s << ")";
        }

        Type * returnType_;

        std::vector<Type *> args_;
    };


    // tinyc::Type::Fun


} // namespace tinyc