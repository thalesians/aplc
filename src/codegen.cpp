#include "codegen.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Transforms/Utils/Cloning.h>

#include <llvm/IR/ValueMap.h>
#include <llvm/IR/PassManager.h>
#include <llvm/LinkAllPasses.h>
#include <llvm/Passes/PassBuilder.h>

#include <llvm/ExecutionEngine/Interpreter.h>

#include <cstdio>
#include <map>

#include "node.h"
#include "parser.h"

using namespace llvm;

using namespace llvm;

static LLVMContext my_context;

llvm::Value *ErrorV(const char *str)
{
    std::cerr << "Error: " << str << std::endl;
    return 0;
}

static llvm::Module *mod;
static llvm::IRBuilder<> builder(my_context);

typedef std::map<std::string, llvm::Value *> symbolTable;
static symbolTable named_values;

namespace llvm { struct UnresolvedType; }
static std::map<int, llvm::UnresolvedType *> unresolved_type_map;

namespace llvm {
struct UnresolvedType : public Type {
    int x;
    UnresolvedType(int x) : Type(mod->getContext(), getTypeID()), x(x) {}

    static UnresolvedType *get(int x) {
        UnresolvedType *ret = unresolved_type_map[x];
        if (!ret) {
            ret = unresolved_type_map[x] = new UnresolvedType(x);
        }
        return ret;
    }

    virtual TypeID getTypeID() const { return MetadataTyID; }
    static inline bool classof(const Type *b) {
        return b->getTypeID() == MetadataTyID;
        return false; // see above
    }
};
}

llvm::Function *generate_identity()
{
    std::cerr << "In generate_identity()..." << std::endl;
    std::vector<Type *> func_args;
    func_args.push_back(builder.getInt64Ty());
    func_args.push_back(builder.getInt64Ty());
    FunctionType *func_type = FunctionType::get(StructType::create(func_args), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "ι");

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(my_context);
    builder.SetInsertPoint(block);

    std::vector<Value *> retvals;
    for (Function::arg_iterator I = func->arg_begin(), E = func->arg_end(); I != E; ++I) {
        retvals.push_back(I);
    }
    builder.CreateAggregateRet(&retvals[0], retvals.size());

    return func;
}

llvm::Function *generate_alpha(Type *type_ptr)
{
    std::cerr << "In generate_alpha(...)..." << std::endl;
    PointerType *type = cast<PointerType>(type_ptr);
    std::vector<Type *> func_args;
    func_args.push_back(type);
    FunctionType *func_type = FunctionType::get(cast<StructType>(type->getArrayElementType())->getContainedType(0), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "α", mod);

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(my_context);
    builder.SetInsertPoint(block);
    Value *el = builder.CreateStructGEP(func->arg_begin()->getParamByValType(), 0, 0);
    builder.CreateRet(builder.CreateLoad(func->arg_begin()->getParamByValType(), el));

    return func;
}

llvm::Function *generate_beta(Type *type_ptr)
{
    std::cerr << "In generate_alpha(...)..." << std::endl;
    PointerType *type = cast<PointerType>(type_ptr);
    std::vector<Type *> func_args;
    func_args.push_back(type);
    FunctionType *func_type = FunctionType::get(cast<StructType>(type->getArrayElementType())->getContainedType(1), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "β", mod);

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(my_context);
    builder.SetInsertPoint(block);
    
    Value *el = builder.CreateStructGEP(func->arg_begin()->getParamByValType(), func->arg_begin(), 1);
    builder.CreateRet(builder.CreateLoad(func->arg_begin()->getParamByValType(), el));

    return func;
}

llvm::Function *generate_putchar()
{
    std::cerr << "In generate_putchar(...)..." << std::endl;
    std::vector<Type *> func_args;
    func_args.push_back(builder.getInt64Ty());
    FunctionType *func_type = FunctionType::get(builder.getVoidTy(), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "putchar", mod);

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(my_context);
    builder.SetInsertPoint(block);

    static Value* const_ptr_12 = builder.CreateGlobalStringPtr("%lc");

    Value *args_[] = { const_ptr_12, func->arg_begin() };
    builder.CreateCall(func_type, named_values["__printf"], ArrayRef<Value *>(args_), "", llvm::MDNode::get(mod->getContext(), nullptr));
    builder.CreateRetVoid();

    return func;
}

llvm::Value * NControl::codeGen() {
    std::cerr << "In NControl::codeGen()..." << std::endl;
    return ErrorV("NControl codegen not implemented yet");
}

static bool is_aplc_array(Value *val)
{
    std::cerr << "In is_aplc_array(Value *val)..." << std::endl;
    PointerType *tmp1;
    ArrayType *tmp;
    return (tmp1 = dyn_cast<PointerType>(val->getType())) &&
           tmp1->getArrayElementType()->isStructTy() &&
           tmp1->getArrayElementType()->getNumContainedTypes() == 2 &&
           (tmp = dyn_cast<ArrayType>(tmp1->getArrayElementType()->getContainedType(1))) &&
           tmp->getNumElements() == 0;
}

llvm::Value * NAssign::codeGen() {
    std::cerr << "In NAssign::codeGen()..." << std::endl;
    Value *id = l.codeGen();
    if (isa<Argument>(id)) {
        named_values[id->getName().str()] = id;
    }
    llvm::StringRef ident = id->getName();
    std::cerr << ident.str() << std::endl;
    llvm::Value *R = r.codeGen();
    if (!R) return NULL;

    // TODO special case aplc array
    std::cerr << "NAssign" << ident.str() << std::endl;
    if (named_values[ident.str()]->getType()->isArrayTy() && is_aplc_array(R)) {
    } else if (named_values[ident.str()]->getType()->isArrayTy() && R->getType()->isArrayTy()) {
      // skip
    } else if (named_values[ident.str()]->getType() != R->getType()) {
        raw_fd_ostream o(fileno(stderr), false);
        named_values[ident.str()]->getType()->print(o);
        std::cerr << std::endl;
        R->getType()->print(o);
        std::cerr << std::endl;
        return ErrorV("Types don't match in assignment");
    }

    named_values[ident.str()] = R;
    return R;
}

llvm::Value * NLambda::codeGen() {
    std::cerr << "In NLambda::codeGen()..." << std::endl;
    Value* lhs = l.codeGen();
    Value* mhs = m.codeGen();

    Function *F = dyn_cast<Function>(lhs);
    std::vector<Type*> ArgTypes;
    std::map<std::string, Argument *> func_args;
    for (Function::arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
        func_args[I->getName().str()] = I;
        ArgTypes.push_back(I->getType());
    }

    FunctionType *FTy = FunctionType::get(mhs->getType(), ArgTypes, F->getFunctionType()->isVarArg());
    Function *NewF = Function::Create(FTy, F->getLinkage(), F->getName(), mod);
    for (Function::arg_iterator I = F->arg_begin(), I2 = NewF->arg_begin(), E = F->arg_end(); I != E; ++I, ++I2) {
        I2->setName(I->getName());
    }

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", NewF, 0);
    BasicBlock* old = builder.GetInsertBlock();
    builder.SetInsertPoint(block);
    Value* rhs = r.codeGen();
    builder.CreateRet(rhs);
    builder.SetInsertPoint(old);
    return NewF;
}

llvm::Value * NComparisonOperator::codeGen() {
    std::cerr << "In NComparisonOperator::codeGen()..." << std::endl;
    return ErrorV("NComparisonOperator codegen not implemented yet");
}

static bool is_resolved(llvm::Function *F)
{
    std::cerr << "In is_resolved(llvm::Function *F)..." << std::endl;
    std::vector<Type *> ArgTypes;
    for (llvm::Function::arg_iterator I = F->arg_begin(), E = F->arg_end();
        I != E; ++I) {
        if (llvm::isa<llvm::UnresolvedType>(I->getType())) {
            return false;
        }
    }
    Type *ret_type = F->getFunctionType()->getReturnType();
    if (llvm::isa<llvm::UnresolvedType>(ret_type)) {
        return false;
    }

    return true;
}

static llvm::Function *replace_unresolved(llvm::Function *F, int index, Type *type, bool put_in_module)
{
    std::cerr << "In replace_unresolved(llvm::Function *F, int index, Type *type, bool put_in_module)..." << std::endl;
    std::vector<Type*> ArgTypes;
    for (Function::arg_iterator I = F->arg_begin(), E = F->arg_end();
        I != E; ++I) {
        const UnresolvedType *t;
        if ((t = llvm::dyn_cast<UnresolvedType>(I->getType())) && t->x == index) {
            ArgTypes.push_back(type);
        } else {
            ArgTypes.push_back(I->getType());
        }
    }

    // Create a new function type...
    Type *ret_type = F->getFunctionType()->getReturnType();
    if (llvm::isa<UnresolvedType>(ret_type)) {
        if (llvm::cast<UnresolvedType>(ret_type)->x == index) {
            ret_type = type;
        } else if (llvm::cast<UnresolvedType>(ret_type)->x >= 200) {
        }
    }
    FunctionType *FTy = FunctionType::get(ret_type, ArgTypes, F->getFunctionType()->isVarArg());

    // Create the new function...
    Function *NewF = Function::Create(FTy, F->getLinkage(), F->getName(), put_in_module ? mod : NULL);

    ValueToValueMapTy vmap;
    Function::arg_iterator DestI = NewF->arg_begin();
    for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
        DestI->setName(I->getName());
        vmap[I] = DestI++;
    }
    SmallVector<ReturnInst *, 0> ri;
    CloneFunctionInto(NewF, F, vmap, CloneFunctionChangeType::LocalChangesOnly, ri, "");

    return NewF;
}

llvm::Value * NUnaryOperator::codeGen() {
    std::cerr << "In NUnaryOperator::codeGen()..." << std::endl;
    return ErrorV("NUnaryOperator codegen not implemented yet");
}

llvm::Value * NApply::codeGen() {
    std::cerr << "In NApply::codeGen()..." << std::endl;
    static std::map<std::string, int> projection_operators;
    static bool projection_operators_generated;
    if (!projection_operators_generated) {
        projection_operators["α"] = 0;
        projection_operators["β"] = 1;
        projection_operators_generated = true;
    }

    llvm::Value *func = lhs.codeGen();
    llvm::Value *apply = rhs.codeGen();
    if (isa<Argument>(func) && projection_operators.find(func->getName().str()) != projection_operators.end()) {
        if (apply->getType()->isStructTy()) {
            return builder.CreateExtractValue(apply, projection_operators[func->getName().str()]);
        } else if (apply->getType()->isArrayTy()) {
            std::vector<Constant *> result;
            for (size_t i = 0; i < cast<ArrayType>(apply->getType())->getNumElements(); ++i) {
                result.push_back(cast<Constant>(builder.CreateExtractValue(builder.CreateExtractValue(apply, i), projection_operators[func->getName().str()])));
            }
            return ConstantArray::get(ArrayType::get(result[0]->getType(), result.size()), result);
        }
    }
    if (isa<Argument>(func) || (isa<ConstantStruct>(func) && isa<Argument>(builder.CreateExtractValue(func, 0)))) {
        if (isa<Argument>(apply)) {
            if (!isa<NIdentifier>(rhs)) {
                return ErrorV("Expected identifier in declaration");
            }
            std::cerr << "apply getname: " << apply->getName().str() << std::endl;
            return new Argument(func->getType(), apply->getName());
        }
    } else if (is_aplc_array(func) && apply->getType()->isIntegerTy()) {
        Value *array_data = builder.CreateStructGEP(func->getType(), func, 1);
        Value *Idxs[] = { builder.getInt64(0), apply };
        auto inner = builder.CreateGEP(array_data->getType(), array_data, ArrayRef<Value *>(Idxs));
        return builder.CreateLoad(inner->getType(), inner);
    } else if (func->getType()->isArrayTy() && isa<ConstantInt>(apply)) {
        return builder.CreateExtractValue(func, cast<ConstantInt>(apply)->getZExtValue());
    } else if (apply->getType()->isArrayTy()) {
        std::vector<Value *> results;
        for (size_t i = 0; i < cast<ArrayType>(apply->getType())->getNumElements(); ++i) {
            Value *arg = builder.CreateExtractValue(apply, i);
            if (arg->getType()->isStructTy()) {
                std::vector<Value *> args;
                std::vector<Type *> arg_types;
                for (size_t i = 0; i < cast<StructType>(arg->getType())->getNumElements(); ++i) {
                    args.push_back(builder.CreateExtractValue(arg, i));
                    arg_types.push_back(arg->getType());
                }
                results.push_back(builder.CreateCall(llvm::FunctionType::get(func->getType(), ArrayRef<Type *>(arg_types), false), func, args));
            } else {
                auto inner = builder.CreateExtractValue(apply, i);
                results.push_back(builder.CreateCall(llvm::FunctionType::get(func->getType(), false), func, inner));
            }
        }
        if (!cast<Function>(func)->getReturnType()->isVoidTy()) {
            Value *array_alloc = builder.CreateAlloca(cast<Function>(func)->getReturnType(), builder.getInt64(results.size()));
            for (size_t i = 0; i < results.size(); ++i) {
                Value *Idxs[] = { builder.getInt64(i) };
                builder.CreateStore(results[i], builder.CreateGEP(array_alloc->getType(), array_alloc, ArrayRef<Value *>(Idxs)));
            }
            auto load = builder.CreateBitCast(array_alloc,
                        PointerType::getUnqual(ArrayType::get(cast<Function>(func)->getReturnType(), results.size())));
            return builder.CreateLoad(load->getType(), load);
        } else {
            return NULL;
        }
    } else {
        llvm::Function *func_func;
        std::cerr << "I was here func func" << std::endl;
        if (!(func_func = llvm::dyn_cast<llvm::Function>(func))) {
            return ErrorV("Non-function left arguments to NApply not supported yet");
        }

        if (is_aplc_array(apply)) {
            auto inner1 = builder.CreateStructGEP(apply->getType(), apply, 0);
            Value *array_size = builder.CreateLoad(inner1->getType(), inner1);
            Value *array_data = builder.CreateStructGEP(apply->getType(), apply, 1);
            Type *element_type = cast<ArrayType>(cast<PointerType>(array_data->getType())
                                                 ->getArrayElementType())->getContainedType(0);
            if (!is_resolved(func_func)) {
                func_func = replace_unresolved(func_func, 0, element_type, true);
            }

            Value *ret = NULL;
            if (!func_func->getReturnType()->isVoidTy()) {
                Type *types[] = { builder.getInt64Ty(), ArrayType::get(func_func->getReturnType(), 0)};
                Type *array_real = StructType::get(mod->getContext(), types, false);
                Value *alloca_size = ConstantExpr::getSizeOf(array_real);
                alloca_size = builder.CreateAdd(alloca_size, builder.CreateMul(array_size, ConstantExpr::getSizeOf(element_type)));
                Value *mem = builder.CreateAlloca(builder.getInt8Ty(), alloca_size);
                ret = builder.CreateBitCast(mem, PointerType::getUnqual(array_real));
                builder.CreateStore(array_size, builder.CreateStructGEP(ret->getType(), ret, 0));
            }

            BasicBlock* label_loop = BasicBlock::Create(mod->getContext(), "", builder.GetInsertBlock()->getParent(), 0);
            BasicBlock* label_loop_exit = BasicBlock::Create(mod->getContext(), "", builder.GetInsertBlock()->getParent(), 0);
            builder.CreateBr(label_loop);
            BasicBlock* old = builder.GetInsertBlock();
            builder.SetInsertPoint(label_loop);
            PHINode* indvar = builder.CreatePHI(builder.getInt64Ty(), 2);
            indvar->addIncoming(builder.getInt64(0), old);
            Value *Idxs[] = { builder.getInt64(0), indvar };
            auto gep = builder.CreateGEP(array_data->getType(), array_data, ArrayRef<Value *>(Idxs));
            Value *arg = builder.CreateLoad(gep->getType(), gep);
            Value *result = builder.CreateCall(func_func, arg);
            if (ret) {
                auto inner = builder.CreateStructGEP(ret->getType(), ret, 1);
                builder.CreateStore(result, builder.CreateGEP(inner->getType(), inner, ArrayRef<Value *>(Idxs)));
            }
            Value* nextindvar = builder.CreateBinOp(Instruction::Add, indvar, builder.getInt64(1));
            indvar->addIncoming(nextindvar, label_loop);
            builder.CreateCondBr(builder.CreateICmpEQ(nextindvar, array_size), label_loop_exit, label_loop);
            builder.SetInsertPoint(label_loop_exit);

            return ret;
        } else if (apply->getType()->isStructTy()) {
            std::vector<Value *> args;
            for (size_t i = 0; i < cast<StructType>(apply->getType())->getNumElements(); ++i) {
                args.push_back(builder.CreateExtractValue(apply, i));
                if (!is_resolved(func_func)) {
                    func_func = replace_unresolved(func_func, 0, args.back()->getType(), true);
                }
            }
            func_func = replace_unresolved(func_func, 0, builder.getVoidTy(), true);
            return builder.CreateCall(func_func, args);
        } else {
            return builder.CreateCall(func_func, apply);
        }
    }
    return ErrorV("Something bad happened in NApply codegen");
}

llvm::Value * NFuncType::codeGen() {
    std::cerr << "In NFuncType::codeGen()..." << std::endl;
    Value *l = lhs.codeGen();
    Value *r = rhs.codeGen();

    std::vector<Type *> arg_types;
    if (l->getType()->isStructTy()) {
        for (size_t i = 0; i < cast<StructType>(l->getType())->getNumElements(); ++i) {
            arg_types.push_back(cast<StructType>(l->getType())->getContainedType(i));
        }
    } else {
        arg_types.push_back(l->getType());
    }
    Function *dummy = Function::Create(FunctionType::get(r->getType(), arg_types, false), GlobalValue::InternalLinkage);
    return new Argument(dummy->getType());
}

llvm::Value * NInteger::codeGen() {
    std::cerr << "In NInteger::codeGen()..." << std::endl;
    return ConstantInt::get(builder.getInt64Ty(), value);
}

llvm::Value * NIdentifier::codeGen() {
    std::cerr << "In NIdentifier::codeGen()..." << std::endl;
    if (name == "int") {
        return new Argument(builder.getInt64Ty());
    } else {
        symbolTable::iterator it = named_values.find(name);
        if (named_values.find(name) == named_values.end()) {
            return new Argument(builder.getInt64Ty(), name);
        }
        return it->second;
    }
}

llvm::Value * NTuple::codeGen() {
    std::cerr << "In NTuple::codeGen()..." << std::endl;
    std::vector<Value *> values;
    std::vector<Constant *> constants;
    std::vector<Type *> types;
    int isTypeExpr = 0;

    for (size_t i = 0; i < l.size(); ++i) {
        llvm::Value *val = l[i]->codeGen();
        if (!val) { return ErrorV("Bad Tuple"); }
        if (!isa<Constant>(val) && !isa<Argument>(val)) { return ErrorV("Tuple elements must be constants or arguments"); }
        if (!isTypeExpr) {
            if (isa<Argument>(val)) {
                isTypeExpr = 1;
            } else {
                isTypeExpr = 2;
            }
        } else if ((isa<Argument>(val) && isTypeExpr != 1) ||
                  (!isa<Argument>(val) && isTypeExpr != 2)) {
            return ErrorV("mixed types and non-types in tuple");
        }
        values.push_back(val);
        constants.push_back(dyn_cast<Constant>(val));
        types.push_back(val->getType());
    }

    if (isTypeExpr == 1 && cast<Argument>(values[0])->hasName()) {  /* Function arguments */
        FunctionType *ft = FunctionType::get(builder.getVoidTy(), types, false);
        Function *f = Function::Create(ft, GlobalValue::InternalLinkage);
        Function::arg_iterator args = f->arg_begin();

        for (size_t i = 0; i < l.size(); ++i) {
            args++->setName(cast<Argument>(values[i])->getName());
        }
        return f;
    } else if (isTypeExpr == 1) {
        return new Argument(StructType::get(mod->getContext(), types, false));
    }

    Constant *tuple = ConstantStruct::get(llvm::StructType::get(mod->getContext(), types, false), constants);

    // GlobalVariable* gvar = new GlobalVariable(*mod, tuple->getType(), true, GlobalValue::PrivateLinkage, 0, "struct");
    // gvar->setInitializer(tuple);
    // return gvar;
    return tuple;
}

llvm::Value * NArray::codeGen() {
    std::cerr << "In NArray::codeGen()..." << std::endl;
    std::vector<Constant *> values;
    Type *type = NULL;
    for (size_t i = 0; i < l.size(); ++i) {
        Value *val = l[i]->codeGen();
        if (!val) { return ErrorV("Bad Tuple"); }
        if (!isa<Constant>(val) && !isa<Argument>(val)) return ErrorV("Tuple elements must be constants or arguments");
        values.push_back(cast<Constant>(val));
        if (i == 0) type = val->getType();
        else if (val->getType() != type) return ErrorV("Types of array elements must be the same");
    }
    if (values.size() && isa<Argument>(values[0])) {
        return new Argument(ArrayType::get(values[0]->getType(), 0));
    }

    ArrayType *array = ArrayType::get(type, values.size());
    return ConstantArray::get(array, values);
    Type *types[] = { builder.getInt64Ty(), array };
    Type *array_real = StructType::get(mod->getContext(), types, false);
    Constant *alloca_size = ConstantExpr::getSizeOf(array_real);

    types[1] = ArrayType::get(type, 0);
    Type *array_type = StructType::get(mod->getContext(), types, false);

    Value *mem = builder.CreateAlloca(builder.getInt8Ty(), alloca_size);
    Value *ret = builder.CreateBitCast(mem, PointerType::getUnqual(array_type));

    builder.CreateStore(builder.getInt64(values.size()), builder.CreateStructGEP(ret->getType(), ret, 0));
    builder.CreateStore(ConstantArray::get(array, values), builder.CreateBitCast(builder.CreateStructGEP(ret->getType(), ret, 1), PointerType::getUnqual(array)));

    return ret;
}

llvm::Value * NBinaryOperator::codeGen() {
    std::cerr << "In NBinaryOperator::codeGen()..." << std::endl;
    Value *L = lhs.codeGen();
    Value *R = rhs.codeGen();
    if (L == 0 || R == 0) return NULL;

    switch (op) {
    case TPLUS:
        std::cerr << "TPLUS" << std::endl;
        return builder.CreateAdd(L, R);
    case TMINUS:
        std::cerr << "TMINUS" << std::endl;
        return builder.CreateSub(L, R);
    case TMUL:
        std::cerr << "TMUL" << std::endl;
        return builder.CreateMul(L, R);
    default:
        return ErrorV("invalid binary operator");
    }

    return NULL;
}


static void print_value(llvm::Function *printf, llvm::Value *val)
{
    std::cerr << "In print_value(llvm::Function *printf, llvm::Value *val)..." << std::endl;
    static bool strings_generated = false;
    static Value *format_int, *format_str, *format_newline, *struct_beg, *struct_del, *struct_end, *array_beg, *array_end;
    if (!strings_generated) {
        format_int = builder.CreateGlobalStringPtr("%d");
        format_str = builder.CreateGlobalStringPtr("%s");
        format_newline = builder.CreateGlobalStringPtr("\n");
        struct_beg = builder.CreateGlobalStringPtr("(");
        struct_del = builder.CreateGlobalStringPtr(", ");
        struct_end = builder.CreateGlobalStringPtr(")");
        array_beg = builder.CreateGlobalStringPtr("[");
        array_end = builder.CreateGlobalStringPtr("]");
        strings_generated = true;
    }

    if (val->getType()->isIntegerTy()) {
        Value *args_[] = { format_int, val };
        builder.CreateCall(printf, ArrayRef<Value *>(args_), "", llvm::MDNode::get(mod->getContext(), nullptr));
    } else if (val->getType()->isPointerTy()) {
        if (is_aplc_array(val)) {
            auto gep__ = builder.CreateStructGEP(val->getType(), val, 0);
            Value *array_size = builder.CreateLoad(gep__->getType(), gep__);
            Value *array_data = builder.CreateStructGEP(val->getType(), val, 1);

            builder.CreateCall(printf, array_beg);

            BasicBlock* label_loop =        BasicBlock::Create(mod->getContext(), "", builder.GetInsertBlock()->getParent(), 0);
            BasicBlock* label_loop_exit =   BasicBlock::Create(mod->getContext(), "", builder.GetInsertBlock()->getParent(), 0);

            builder.CreateBr(label_loop);
            BasicBlock* old = builder.GetInsertBlock();
            builder.SetInsertPoint(label_loop);

            // Block  (label_loop)
            PHINode* indvar = builder.CreatePHI(builder.getInt64Ty(), 2);
            indvar->addIncoming(builder.getInt64(0), old);

            Value *Idxs[] = { builder.getInt64(0), indvar };
            auto gep_ = builder.CreateGEP(array_data->getType(), array_data, ArrayRef<Value *>(Idxs));
            Value *args_[] = { format_int, builder.CreateLoad(gep_->getType(), gep_, "") };
            builder.CreateCall(printf, ArrayRef<Value *>(args_), "", llvm::MDNode::get(mod->getContext(), nullptr));
            builder.CreateCall(printf, struct_del);

            Value* nextindvar = builder.CreateBinOp(Instruction::Add, indvar, builder.getInt64(1));
            indvar->addIncoming(nextindvar, label_loop);
            builder.CreateCondBr(builder.CreateICmpEQ(nextindvar, array_size), label_loop_exit, label_loop);
            builder.SetInsertPoint(label_loop_exit);

            builder.CreateCall(printf, array_end);
        } else if (builder.CreateLoad(val->getType(), val, "")->getType()->isStructTy()) {
            builder.CreateCall(printf, struct_beg);
            size_t size = cast<StructType>(builder.CreateLoad(val->getType(), val, "")->getType())->getNumElements();
            for (size_t i = 0; i < size; ++i) {
                Value *args_[] = { format_int, builder.CreateStructGEP(val->getType(), val, i) };
                builder.CreateCall(printf, ArrayRef<Value *>(args_), "", llvm::MDNode::get(mod->getContext(), nullptr));
                if (i != size - 1) builder.CreateCall(printf, struct_del);
            }
            builder.CreateCall(printf, struct_end);
        }
    } else if (val->getType()->isArrayTy()) {
        builder.CreateCall(printf, array_beg);
        size_t size = cast<ArrayType>(val->getType())->getNumElements();
        for (size_t i = 0; i < size; ++i) {
            Value *args_[] = { format_int, builder.CreateExtractValue(val, i) };
            builder.CreateCall(printf, ArrayRef<Value *>(args_), "", llvm::MDNode::get(mod->getContext(), nullptr));
            if (i != size - 1) builder.CreateCall(printf, struct_del);
        }
        builder.CreateCall(printf, array_end);
        return;
    } else if (val->getType()->isStructTy()) {
        builder.CreateCall(printf, struct_beg);
        size_t size = cast<StructType>(val->getType())->getNumElements();
        for (size_t i = 0; i < size; ++i) {
            Value *args_[] = { format_int, builder.CreateExtractValue(val, i) };
            builder.CreateCall(printf, ArrayRef<Value *>(args_), "", llvm::MDNode::get(mod->getContext(), nullptr));
            if (i != size - 1) builder.CreateCall(printf, struct_del);
        }
        builder.CreateCall(printf, struct_end);
    }
    builder.CreateCall(printf, format_newline);
}

void generate_code(ExpressionList *exprs, LLVMContext &context, IRBuilder<> &builder, Module &module)
{
    std::cerr << "In generate_code(ExpressionList *exprs, LLVMContext &context, IRBuilder<> &builder, Module &module)..." << std::endl;

    mod = &module;

    // main
    Type *func_main_args[] = { builder.getInt32Ty(), PointerType::get(builder.getInt8PtrTy(), 0) };
    FunctionType* func_main_type = FunctionType::get(builder.getInt32Ty(), func_main_args, false);
    Function *func_main = Function::Create(func_main_type, GlobalValue::ExternalLinkage, "main", mod);

    // printf
    FunctionType *func_printf_type = FunctionType::get(builder.getInt32Ty(), builder.getInt8PtrTy(), true);
    named_values["__printf"] = Function::Create(func_printf_type, GlobalValue::ExternalLinkage, "printf", mod);

    // setlocale
    Type *func_setlocale_args[] = { builder.getInt32Ty(), builder.getInt8PtrTy() };
    FunctionType *func_setlocale_type = FunctionType::get(builder.getInt8PtrTy(), func_setlocale_args, false);
    named_values["__setlocale"] = Function::Create(func_setlocale_type, GlobalValue::ExternalLinkage, "setlocale", mod);

    // main block
    BasicBlock *bblock = BasicBlock::Create(mod->getContext(), "", func_main, 0);
    builder.SetInsertPoint(bblock);

    static Value* zero_initializer = builder.CreateGlobalStringPtr("");
    Value *args_[] = { builder.getInt32(LC_ALL), zero_initializer };
    Type *arg_types_[] = { args_[0]->getType(), args_[1]->getType() };
    builder.CreateCall(llvm::FunctionType::get(named_values["__setlocale"]->getType(), ArrayRef<Type *>(arg_types_), false), named_values["__setlocale"], ArrayRef<Value *>(args_), "", llvm::MDNode::get(mod->getContext(), nullptr));

    // globals
    named_values["ι"] = generate_identity();
    // named_values["α"] = generate_alpha();
    // named_values["β"] = generate_beta();
    named_values["putchar"] = generate_putchar();

    for (size_t i = 0; i < exprs->size(); ++i) {
        Value *val = (*exprs)[i]->codeGen();
        if (!isa<NAssign>((*exprs)[i]) && val && !val->getType()->isVoidTy() &&
                !(val->getType()->isArrayTy() && cast<ArrayType>(val->getType())->getContainedType(0)->isVoidTy())) {
            print_value(cast<Function>(named_values["__printf"]), val);
        }
    }
    builder.CreateRet(builder.getInt32(0));

    std::cerr << "Code is generated." << std::endl;

    std::cerr << "Creating LoopAnalysisManager..." << std::endl;
    LoopAnalysisManager lam;
    std::cerr << "Creating FunctionAnalysisManager..." << std::endl;
    FunctionAnalysisManager fam;
    std::cerr << "Creating CGSCCAnalysisManager..." << std::endl;
    CGSCCAnalysisManager cgam;
    std::cerr << "Creating ModuleAnalysisManager..." << std::endl;
    ModuleAnalysisManager mam;
    
    // std::cerr << "Creating PrintModulePass..." << std::endl;
    // PrintModulePass pmp;

    std::cerr << "Creating PassBuilder..." << std::endl;
    PassBuilder pb;
    
    std::cerr << "Registering stuff with the PassBuilder..." << std::endl;
    pb.registerModuleAnalyses(mam);
    pb.registerCGSCCAnalyses(cgam);
    pb.registerFunctionAnalyses(fam);
    pb.registerLoopAnalyses(lam);
    pb.crossRegisterProxies(lam, fam, cgam, mam);
    pb.buildInlinerPipeline(llvm::OptimizationLevel::O0, llvm::ThinOrFullLTOPhase::None);
    // pb.registerPrintModulePass(pmp);

    std::cerr << "Creating ModulePassManager..." << std::endl;
    ModulePassManager pm = pb.buildPerModuleDefaultPipeline(OptimizationLevel::O2);

    // std::cerr << "Printing pass names..." << std::endl;
    // pm.printPassNames(std::cerr);

    std::cerr << "Running ModulePassManager..." << std::endl;
    pm.run(*mod, mam);




  EngineBuilder ebuilder{std::unique_ptr<llvm::Module>(mod)};
  std::string ErrorMsg;
  //ebuilder.setMArch(MArch);
  //ebuilder.setMCPU(MCPU);
  //ebuilder.setMAttrs(MAttrs);
  //ebuilder.setRelocationModel(RelocModel);
  //ebuilder.setCodeModel(CMModel);
  ebuilder.setErrorStr(&ErrorMsg);
  /*
  ebuilder.setEngineKind(ForceInterpreter
                        ? EngineKind::Interpreter
                        : EngineKind::JIT);
                        */

  CodeGenOpt::Level OLvl = CodeGenOpt::Default;
  ebuilder.setOptLevel(OLvl);

  ExecutionEngine * EE = ebuilder.create();
  if (!EE) {
    if (!ErrorMsg.empty())
      errs() << ": error creating EE: " << ErrorMsg << "\n";
    else
      errs() << ": unknown error creating EE!\n";
    exit(1);
  }

  // EE->DisableLazyCompilation(NoLazyCompilation);

  Function *EntryFn = func_main;
  if (!EntryFn) {
    errs() << "Entry function not found in module.\n";
    return;
  }

  // If the program doesn't explicitly call exit, we will need the Exit 
  // function later on to make an explicit call, so get the function now. 
  /*
  Constant *Exit = mod->getOrInsertFunction("exit", Type::getVoidTy(context),
                                                    Type::getInt32Ty(context),
                                                    NULL);
  */
  
  // Reset errno to zero on entry to main.
  errno = 0;
 
  // Run static constructors.
  EE->runStaticConstructorsDestructors(false);

  for (Module::iterator I = mod->begin(), E = mod->end(); I != E; ++I) {
    Function *Fn = &*I;
    if (Fn != EntryFn && !Fn->isDeclaration())
      EE->getPointerToFunction(Fn);
  }

  // Run main.
  char * const *envp;
  std::vector<std::string> InputArgv;
  int Result = EE->runFunctionAsMain(EntryFn, InputArgv, envp);
  std::cout << "Result: " << Result << std::endl;

  // Run static destructors.
  EE->runStaticConstructorsDestructors(true);
  
  // If the program didn't call exit explicitly, we should call it now. 
  // This ensures that any atexit handlers get called correctly.
  /*
  if (Function *ExitF = dyn_cast<Function>(Exit)) {
    std::vector<GenericValue> Args;
    GenericValue ResultGV;
    ResultGV.IntVal = APInt(32, Result);
    Args.push_back(ResultGV);
    EE->runFunction(ExitF, Args);
    errs() << "ERROR: exit(" << Result << ") returned!\n";
    abort();
  } else {
    errs() << "ERROR: exit defined with wrong prototype!\n";
    abort();
  }
  */






    std::cerr << "All done!" << std::endl;
}
