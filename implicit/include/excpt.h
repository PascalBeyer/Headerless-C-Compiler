
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
