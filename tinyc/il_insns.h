#ifndef INS
#define INS(...)
#endif

INS(LoadImm, ImmValue)
INS(AllocG, ImmSize)
INS(AllocL, ImmSize)
INS(ArgAddr, ImmIndex)
INS(Add, BinaryOperator)
INS(Sub, BinaryOperator)
INS(Mul, BinaryOperator)
INS(Div, BinaryOperator)
INS(Mod, BinaryOperator)
INS(Shr, BinaryOperator)
INS(Shl, BinaryOperator)
INS(And, BinaryOperator)
INS(Or, BinaryOperator)
INS(Xor, BinaryOperator)
INS(Not, UnaryOperator)

INS(Return, Terminator0)
INS(CondJump, TerminatorN)
INS(Jump, Terminator1)
INS(Halt, Terminator0)

INS(Load, LoadAddress)
INS(Store, StoreAddress)

//INS(Copy, )
//INS(Call,)
//INS(CallStatic,)

#undef INS

#ifdef ImmSize
#undef ImmSize
#endif
#ifdef ImmIndex
#undef ImmIndex
#endif
#ifdef ImmValue
#undef ImmValue
#endif
#ifdef BinaryOperator
#undef BinaryOperator
#endif
#ifdef UnaryOperator
#undef UnaryOperator
#endif
#ifdef Terminator0
#undef Terminator0
#endif
#ifdef Terminator1
#undef Terminator1
#endif
#ifdef TerminatorN
#undef TerminatorN
#endif
#ifdef LoadAddress
#undef LoadAddress
#endif
#ifdef StoreAddress
#undef StoreAddress
#endif
