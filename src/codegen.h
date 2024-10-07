#ifndef CODEGEN_H
#define CODEGEN_H

#include "node.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

using namespace llvm;

void generate_code(ExpressionList *exprs, LLVMContext &context, IRBuilder<> &builder, Module &module);

#endif /* end of include guard: CODEGEN_H */
