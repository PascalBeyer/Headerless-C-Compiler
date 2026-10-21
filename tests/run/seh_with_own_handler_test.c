// os windows
// run
// check "__C_specific_handler"
// check "exception!"

#include <excpt.h>
#include <stdarg.h>

typedef unsigned int  ULONG;   // 32-bit
typedef unsigned int  DWORD;   // 32-bit
typedef unsigned __int64 ULONG64;

struct _CONTEXT;                                  // only ever used as a pointer
typedef struct _IMAGE_RUNTIME_FUNCTION_ENTRY *PRUNTIME_FUNCTION;

struct _EXCEPTION_RECORD {
    DWORD ExceptionCode;
    DWORD ExceptionFlags;
    struct _EXCEPTION_RECORD *ExceptionRecord;
    void *ExceptionAddress;
    DWORD NumberParameters;
    ULONG64 ExceptionInformation[15];
};

struct _EXCEPTION_POINTERS {
    struct _EXCEPTION_RECORD *ExceptionRecord;
    struct _CONTEXT *ContextRecord;
};

struct _DISPATCHER_CONTEXT {
    ULONG64 ControlPc;
    ULONG64 ImageBase;
    PRUNTIME_FUNCTION FunctionEntry;
    ULONG64 EstablisherFrame;
    ULONG64 TargetIp;
    struct _CONTEXT *ContextRecord;
    void *LanguageHandler;
    void *HandlerData;
    void *HistoryTable;
    DWORD ScopeIndex;
    DWORD Fill0;
};

__declspec(dllimport) void __stdcall RtlUnwindEx(void *TargetFrame, void *TargetIp,
        struct _EXCEPTION_RECORD *ExceptionRecord, void *ReturnValue,
        struct _CONTEXT *ContextRecord, void *HistoryTable);

__declspec(dllimport) void *__cdecl __acrt_iob_func(unsigned _Ix);
__declspec(dllimport) int __cdecl __stdio_common_vfprintf(unsigned __int64 _Options,
        void *_Stream, char const *_Format, void *_Locale, va_list _ArgList);

int printf(char const *format, ...){
    va_list va;
    va_start(va, format);
    int ret = __stdio_common_vfprintf(0, __acrt_iob_func(1), format, 0, va);
    va_end(va);
    return ret;
}

void exception_function(){
    int *a = 0;
    *a = 1337;
}

void other_function(){
}

int _start(){
    other_function();
    
    
    int a = 1;
    __try{
        exception_function();
        int arst  = 1;
    } __except(EXCEPTION_EXECUTE_HANDLER) {
        printf("exception!\n");
        a = 0;
        other_function();
    }
    
    other_function();
    
    return a;
}

// MSDN via Wayback Machine:
// ControlPc        is the value of RIP within this function. This is either an exception address or the address at which control left the establishing function. This is the RIP that will be used to determine if control is within some guarded construct within this function (for example, a __try block for __try/__except or __try/__finally).
// ImageBase        is the image base (load address) of the module containing this function, to be added to the 32-bit offsets used in the function entry and unwind info to record relative addresses.
// FunctionEntry    supplies a pointer to the RUNTIME_FUNCTION function entry holding the function and unwind info image-base relative addresses for this function.
// EstablisherFrame is the address of the base of the fixed stack allocation for this function.
// TargetIp         Supplies an optional instruction address that specifies the continuation address of the unwind. This address is ignored if EstablisherFrame is not specified.
// ContextRecord    points to the exception context, for use by the system exception dispatch/unwind code.
// LanguageHandler  points to the language-specific language handler routine being called.
// HandlerData      points to the language-specific handler data for this function.

// defintions from 'https://blog.talosintelligence.com/2014/06/exceptional-behavior-windows-81-x64-seh.html'
struct _C_SCOPE_TABLE_ENTRY {
    ULONG Begin;        // +0x00 - Begin of guarded code block
    ULONG End;          // +0x04 - End of target code block
    ULONG Handler;      // +0x08 - Exception filter function (or '__finally' handler)
    ULONG Target;       // +0x0C - Exception handler pointer (the code inside __except block)
};

struct _C_SCOPE_TABLE {
    ULONG NumEntries;              // +0x00 - Number of entries
    struct _C_SCOPE_TABLE_ENTRY Table[];   // +0x04 - Scope table array
};


EXCEPTION_DISPOSITION __C_specific_handler(struct _EXCEPTION_RECORD *exception_record, void *EstablisherFrame, struct _CONTEXT *context, struct _DISPATCHER_CONTEXT *dispatcher_context){
    
    printf("__C_specific_handler\n");
    
    ULONG64 image_base  = dispatcher_context->ImageBase;
    ULONG64 relative_pc = dispatcher_context->ControlPc - image_base;
    struct _C_SCOPE_TABLE *scope_table = dispatcher_context->HandlerData;
    
    struct _EXCEPTION_POINTERS exception_pointers = {
        .ExceptionRecord = (void *)exception_record,
        .ContextRecord   = context,
    };
    
    if((exception_record->ExceptionFlags & /*EXCEPTION_UNWIND*/ 0x66) == 0){
        for(DWORD scope_index = dispatcher_context->ScopeIndex; scope_index < scope_table->NumEntries; scope_index++){
            if(scope_table->Table[scope_index].Target  == 0) continue;
            
            if(scope_table->Table[scope_index].Begin <= relative_pc && relative_pc < scope_table->Table[scope_index].End){
                if(scope_table->Table[scope_index].Handler != 1){
                    int (*handler)(struct _EXCEPTION_POINTERS *, void *) =
                            (void *)(image_base + scope_table->Table[scope_index].Handler);
                    
                    int code = (*handler)(&exception_pointers, EstablisherFrame);
                    if(code  < 0) return -1;
                    if(code == 0) continue;
                }
                
                RtlUnwindEx(EstablisherFrame, (void *)(image_base + scope_table->Table[scope_index].Target),
                        exception_record, (void*)(__int64)exception_record->ExceptionCode, context, dispatcher_context->HistoryTable);
            }
        }
    }else{
        
        ULONG64 relative_target = dispatcher_context->TargetIp - image_base;
        
        for(DWORD scope_index = dispatcher_context->ScopeIndex; scope_index < scope_table->NumEntries; scope_index++){
            if(scope_table->Table[scope_index].Begin > relative_pc || relative_pc >= scope_table->Table[scope_index].End) continue;
            
            if((exception_record->ExceptionFlags & /*EXCEPTION_TARGET_UNWIND*/ 0x20) != 0){
                DWORD target_index = 0;
                for(; target_index < scope_table->NumEntries; target_index++){
                    if(scope_table->Table[target_index].Begin <= relative_target && relative_target < scope_table->Table[target_index].End){
                        if(scope_table->Table[target_index].Handler == scope_table->Table[scope_index].Handler){
                            if(scope_table->Table[target_index].Target == scope_table->Table[scope_index].Target){
                                break;
                            }
                        }
                    }
                }
                
                if(target_index != scope_table->NumEntries){
                    return 1;
                }
            }
            
            if(scope_table->Table[scope_index].Target == 0){
                dispatcher_context->ScopeIndex = scope_index + 1;
                void (*handler)(char, void *) = (void *)(image_base + scope_table->Table[scope_index].Handler);
                (*handler)(1, EstablisherFrame);
                
            }else if(relative_target == scope_table->Table[scope_index].Target){
                return 1;
            }
        }
    }
    
    return 1;
}

