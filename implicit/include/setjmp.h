
#ifdef __HLC_COMPILE_TO_OBJECT__
// @cleanup: Microsotf passes rsp as the second parameter (somemthing like __read_rsp()),
//           but for me that crashes somewhere inside RtlUnwindEx.
//           It seems it does not do unwinding if the second parameter is 0.
//                                                              - Pascal Beyer 10.11.2024
#define setjmp(env) setjmp(env, 0)
#endif

#pragma compilation_unit("setjmp.c")


