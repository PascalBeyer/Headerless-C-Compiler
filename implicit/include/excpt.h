
typedef enum _EXCEPTION_DISPOSITION
{
    ExceptionContinueExecution,
    ExceptionContinueSearch,
    ExceptionNestedException,
    ExceptionCollidedUnwind
} EXCEPTION_DISPOSITION;


enum{
    EXCEPTION_CONTINUE_SEARCH = 0,
    EXCEPTION_EXECUTE_HANDLER = 1,
    EXCEPTION_CONTINUE_EXECUTION = -1,
};

// https://learn.microsoft.com/en-us/cpp/cpp/try-except-statement?view=msvc-170#structured-exception-handling-intrinsic-functions
#define GetExceptionCode()          _exception_code()
#define GetExceptionInformation()   ((struct _EXCEPTION_POINTERS *)_exception_info())
#define AbnormalTermination()       _abnormal_termination()

