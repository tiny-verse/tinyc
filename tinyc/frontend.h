#pragma once

#include <string>

#include "ast.h"
#include "parser.h"
#include "types.h"

#if (defined OPTIMIZER_tvlm)
#include "tvlm/tvlm.h"
#include "tvlm_frontend.h"
#include "tvlm/il_builder.h"
#include "typechecker.h"

#endif

namespace tinyc {

    /** The TinyC frontend.

     */
    class Frontend {
    public:

//        using AST = std::string;
        using AST = std::unique_ptr<ASTBase>;

        /** Parses the given string containing tinyC source and returns its AST.
         */
        AST parse(std::string const & source) {
            return Parser::Parse(source,"", *this);
//            return std::string{""};
        }

        /** Opens file specified by the filename and parses the tinyC source within.
         */
        AST parseFile(std::string const & filename) {
            return Parser::ParseFile(filename, *this);
//            return std::string{""};
        }

#if (defined OPTIMIZER_dummy)
        /** When using the dummy optimizer, the backend should work directly from the ASTs themselves and therefore the compileToIl method simply returns the given AST.
         */
        using IL = std::string;
        IL compileToIl(AST && ast) {
            return ast;
        }
#elif (defined OPTIMIZER_tvlm)
        /** The Tiny Virtual Low-level Machine Optimizer translator.
         */
        using IL = tvlm::Program;
        IL compileToIl(AST && ast) {
            tinyc::AST * rootNode = dynamic_cast<tinyc::AST*>(ast.get());
            // typecheck the ast and store type declarations
            tinyc::TypeChecker tc{*this};
            tc.visit(rootNode);
            // compile the program
            // rootNode->compileToIR(b, false);
            auto tmp = TvlmFrontend::translate(rootNode, *this);
            std::stringstream ss;
            auto printer = tiny::ASTPrettyPrinter(ss);

            tvlm::ILBuilder b;

            return tmp;
            //throw "not_implemented";
        }
#else
#error "Selected optimizer not supported by tinyC frontend"
#endif

        Frontend() {
            int_ = types_.insert(std::make_pair(Symbol::KwInt.name(), std::unique_ptr<Type>{new Type::POD{Symbol::KwInt}})).first->second.get();
            double_ = types_.insert(std::make_pair(Symbol::KwDouble.name(), std::unique_ptr<Type>{new Type::POD{Symbol::KwDouble}})).first->second.get();
            char_ = types_.insert(std::make_pair(Symbol::KwChar.name(), std::unique_ptr<Type>{new Type::POD{Symbol::KwChar}})).first->second.get();
            void_ = types_.insert(std::make_pair(Symbol::KwVoid.name(), std::unique_ptr<Type>{new Type::POD{Symbol::KwVoid}})).first->second.get();
        }

        /** Determines whether given name is a known type name.

            It's a typename if we find it in the type declarations.
         */
        bool isTypeName(Symbol name) const {
            // TODO this is wrong for REPL-like invocations
            return false;
        }

        Type * getTypeInt() {
            return int_;
        }

        Type * getTypeDouble() {
            return double_;
        }

        Type * getTypeChar() {
            return char_;
        }

        Type * getTypeVoid() {
            return void_;
        }

        Type * getType(Symbol symbol) {
            auto i = types_.find(symbol.name());
            if (i == types_.end())
                return nullptr;
            Type * result = i->second.get();
            // check if it is a type alias, and if so, return the base type
            Type::Alias * alias = dynamic_cast<Type::Alias*>(result);
            if (alias != nullptr)
                return alias->base();
            else
                return result;
        }

        bool isPointer(Type * t) {
            assert(dynamic_cast<Type::Alias*>(t) == nullptr);
            return dynamic_cast<Type::Pointer *>(t) != nullptr;
        }

        bool isPOD(Type * t) {
            assert(dynamic_cast<Type::Alias*>(t) == nullptr);
            return t == char_ || t == int_ || t == double_;
        }

        bool convertsToBool(Type * t) {
            assert(dynamic_cast<Type::Alias*>(t) == nullptr);
            return isPointer(t) || isPOD(t);
        }

        Type::Struct * getOrCreateStructType(Symbol name) {
            // type structs can't have aliases
            auto i = types_.find(name.name());
            if (i == types_.end()) {
                Type::Struct * result = new Type::Struct{nullptr};
                types_.insert(std::make_pair(name.name(), std::unique_ptr<Type>{result}));
                return result;
            } else {
                Type::Struct * result = dynamic_cast<Type::Struct*>(i->second.get());
                return result;
            }
        }

        Type::Fun * getOrCreateFunctionType(std::unique_ptr<Type::Fun> type) {
            std::string typeName = type->toString();
            auto i = types_.find(typeName);
            if (i == types_.end())
                i = types_.insert(std::make_pair(typeName, type.release())).first;
            Type::Fun * result = dynamic_cast<Type::Fun*>(i->second.get());
            assert(result != nullptr && "The type existed, but was something else");
            return result;
        }

        Type::Alias * createTypeAlias(Symbol name, Type * base) {
            assert(types_.find(name.name()) == types_.end());
            Type::Alias * result = new Type::Alias(name, base);
            types_.insert(std::make_pair(name.name(), std::unique_ptr<Type>{ result }));
            return result;
        }

        /** Returns a pointer type to the given base.
         */
        Type * getOrCreatePointerType(Type * base) {
            std::string typeName = base->toString() + "*";
            auto i = types_.find(typeName);
            if (i == types_.end())
                i = types_.insert(std::make_pair(typeName, std::unique_ptr<Type>(new Type::Pointer{base}))).first;
            return i->second.get();
        }


    private:

        std::unordered_map<std::string, std::unique_ptr<Type>> types_;

        Type * int_;
        Type * double_;
        Type * char_;
        Type * void_;

    }; // tinyc::Frontend
}

