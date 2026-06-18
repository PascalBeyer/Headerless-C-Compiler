
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#if _WIN32
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <intrin.h>
#else

#include <dirent.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/wait.h>
#include <wordexp.h>
#include <errno.h>
#include <sys/eventfd.h>
#include <sys/ptrace.h>

#endif

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;


typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef float f32;
typedef double f64;

#define array_count(a) (sizeof(a) / sizeof(*(a)))

#define kilo_bytes(a) ((a) * 1024ULL)
#define mega_bytes(a) ((kilo_bytes(a)) * 1024ULL)
#define giga_bytes(a) ((mega_bytes(a)) * 1024ULL)

#define true  1
#define false 0
#define null ((void *)0)

#if __HLC__
__declspec(printlike)
#endif
int print(char *format, ...){
    va_list va;
    va_start(va, format);
    int ret = vprintf(format, va);
    va_end(va);
    
    fflush(stdout); // I always want my prints to flush, but you can remove this if you don't :)
    
    return ret;
}

struct memory_arena{
    u8 *base;
    u64 allocated;
    u64 committed;
    u64 reserved;
};

struct memory_arena create_memory_arena(u64 size_to_reserve){
    struct memory_arena ret = {0};
    ret.reserved = size_to_reserve;
#if _WIN32
    ret.base = VirtualAlloc(/*DesiredBase*/NULL, size_to_reserve, MEM_RESERVE, PAGE_READWRITE);
#else
    ret.base = mmap(NULL, size_to_reserve, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#endif
    if(!ret.base){
        print("Error: Allocation failiure.");
        _exit(1);
    }
    return ret;
}

__declspec(noinline) void grow_arena(struct memory_arena *arena, u64 grow_to){
    
    if(!arena->base){
        // 
        // If the 'memory_arena' is not yet initialized, initialize it 
        // to have with a 'reserved' size of 64 GiB.
        // 
        *arena = create_memory_arena(/*size_to_reserve*/giga_bytes(64));
    }
    
    
    // 
    // If we can fit 'grow_to', we should always succeed.
    // Otherwise, panic.
    // 
    if(grow_to > arena->reserved){
        print("Error: Ran out of memory.");
        _exit(1);
    }
    
    // 
    // If we can fit an additional mega byte, we should.
    // 
    if(grow_to + mega_bytes(1) <= arena->reserved){
        grow_to += mega_bytes(1);
    }
    
    // 
    // Page align 'grow_to'.
    // 
    grow_to = (grow_to + 0xfff) & ~0xfff;
    
    // 
    // If this was called manually, we might not have to commit anything.
    // 
    if(grow_to < arena->committed) return;
    
    // 
    // Commit the bytes.
    // 
#if _WIN32
    void *Success = VirtualAlloc(arena->base, grow_to, MEM_COMMIT, PAGE_READWRITE);
    if(Success != arena->base){
        print("Error: Allocation failiure.");
        _exit(1);
    }
#else
    int error = mprotect(arena->base, grow_to, PROT_READ | PROT_WRITE);
    if(error){
        print("Error: Allocation failiure.");
        _exit(1);
    }
#endif
    
    arena->committed = grow_to;
}

void *memory_arena_allocate_bytes(struct memory_arena *arena, u64 size, u64 alignment){
    
    // Make sure the alignment is a power of two.
    assert(alignment && ((alignment & (alignment - 1)) == 0));
    
    // 
    // Allocate bytes to reach the 'alignment'.
    // 
    u64 allocated = arena->allocated;
    allocated = (allocated + (alignment - 1)) & ~(alignment - 1);
    
    u64 allocation_base = allocated;
    
    // 
    // Allocate 'size' bytes.
    // 
    allocated += size;
    
    assert(arena->allocated <= allocated);
    
    if(allocated > arena->committed){
        grow_arena(arena, allocated);
    }
    
    arena->allocated = allocated;
    
    memset(arena->base + allocation_base, 0, size);
    
    return arena->base + allocation_base;
}

u8 *arena_current(struct memory_arena *arena){
    return arena->base + arena->allocated;
}

#define push_struct(arena, type) ((type *)memory_arena_allocate_bytes((arena), sizeof(type), _Alignof(type)))
#define push_array(arena, type, count) ((type *)memory_arena_allocate_bytes((arena), sizeof(type) * (count), _Alignof(type)))

struct temporary_memory{
    struct memory_arena *arena;
    u64 saved_allocated;
};

struct temporary_memory begin_temporary_memory(struct memory_arena *arena){
    struct temporary_memory ret = {
        .arena = arena,
        .saved_allocated = arena->allocated,
    };
    return ret;
}

void end_temporary_memory(struct temporary_memory *temp){
    struct memory_arena *arena = temp->arena;
    
    u8 *reset_to   = arena->base + temp->saved_allocated;
    u64 reset_size = arena->allocated - temp->saved_allocated;
    
    memset(reset_to, 0, reset_size);
    
    arena->allocated = temp->saved_allocated;
}

char *push_format_cstring(struct memory_arena *arena, char *format, ...){
    va_list va, va2;
    
    va_start(va, format);
    
    va_copy(va2, va);
    int string_size = vsnprintf(NULL, 0, format, va2);
    va_end(va2);
    
    if(string_size < 0) return 0; // @cleanup: not sure.
    
    char *data = push_array(arena, char, string_size + 1);
    
    vsnprintf(data, (size_t)string_size + 1, format, va);
    
    return data;
}

struct string{
    union{
        u64 size;
        u64 length;
        u64 amount;
    };
    union{
        char *data;
        char *memory;
    };
};

#define string(a) ((struct string){.data = a, .amount = (sizeof(a) - 1)})
#define const_string(a) {.data = a, .amount = (sizeof(a) - 1)}

struct string create_string(char *data, u64 size){
    struct string ret;
    ret.data = data;
    ret.size = size;
    return ret;
}

int string_match(struct string a, struct string b){
    if(a.size != b.size) return 0;
    
    return memcmp(a.data, b.data, a.size) == 0;
}

struct string string_eat_front(struct string *string, u64 amount_to_eat){
    struct string front = create_string(string->data, amount_to_eat);
    string->data += amount_to_eat;
    string->size -= amount_to_eat;
    return front;
}

struct string string_eat_line(struct string *string){
    
    u64 index = 0;
    for(; index < (string->size - 1); index++){
        if(string->data[index] == '\n') break;
    }
    
    return string_eat_front(string, index + 1);
}


int string_front_match_eat(struct string *string, char *front){
    
    u64 index = 0;
    for(; (index < string->amount) && *front; index++, front++){
        if(string->data[index] != *front) return false;
    }
    
    if(*front) return false;
    
    string_eat_front(string, index);
    return true;
    
}

struct string string_eat_whitespace(struct string *string){
    
    u64 index = 0;
    for(; index < string->length; index++){
        if(string->data[index] != ' ') break;
    }
    
    return string_eat_front(string, index);
}

struct string string_eat_until_whitespace(struct string *string){
    
    u64 index = 0;
    for(; index < string->length; index++){
        if(string->data[index] == ' ') break;
    }
    
    return string_eat_front(string, index);
}

int string_is_substring(struct string substring, struct string string){
    // @cleanup: there are faster algorithms.
    
    // We cannot be a substring if we are bigger.
    if(substring.size > string.size) return false;
    
    for(u64 offset = 0; offset <= string.size - substring.size; offset += 1){
        if(memcmp(substring.data, string.data + offset, substring.size) == 0) return true;
    }
    return false;
} 

struct string strip_file_name(struct string path){
    u64 one_past_last_slash = 0;
    for(s64 i = path.amount - 1; i >= 0; i--){
        if(path.data[i] == '/' || path.data[i] == '\\'){
            one_past_last_slash = i + 1;
            break;
        }
    }
    
    path.amount = one_past_last_slash;
    return path;
}

struct string load_file(struct memory_arena *arena, char *file_name){
    struct string ret = {0};
    
    FILE *handle = fopen(file_name, "rb");
    
    if(!handle){
        print("could not fopen '%s'\n", file_name);
        return ret;
    }
    
    fseek(handle, 0, SEEK_END);
    int size = ftell(handle);
    if(size < 0){
        print("could not ftell '%s'\n", file_name);
        return ret;
    }
    fseek(handle, 0, SEEK_SET);
    
    char *memory = push_array(arena, char, (size_t)size + 1);
    memory[size] = 0;
    
    fread(memory, 1, (size_t)size, handle);
    
    fclose(handle);
    
    ret.memory = memory;
    ret.size   = (size_t)size;
    
    return ret;
}

struct ticket_spinlock{
    s64 tickets_given_out;
    s64 ticket_in_work;
};


#define atomic_load(type, value) *((volatile type *)&(value))
#define atomic_store(type, address, value) (*(volatile type *)(address) = (value))


void ticket_spinlock_lock(struct ticket_spinlock *mutex){
    s64 my_ticket = _InterlockedIncrement64(&mutex->tickets_given_out) - 1;
    while(atomic_load(s64, mutex->ticket_in_work) != my_ticket){
        _mm_pause();
    }
}

void ticket_spinlock_unlock(struct ticket_spinlock *mutex){
    _InterlockedIncrement64(&mutex->ticket_in_work);
}

struct work{
    struct test{
        char *file_name;
        char *directory;
        
        enum test_result_kind{
            TEST_RESULT_success,
            TEST_RESULT_fail,
            TEST_RESULT_broken,
        } result;
        
        // These are only set for fails.
        u8 *log_start;
        u8 *log_end;
    } *tests;
    
    // @note: these are longs because Windows cares about that.
    long test_at;
    long amount_of_tests;
    long skipped; // supplimentary .c files can be marked 'skip', these are not tests and thus should not count in the final statistic.
    long failed_tests;
    long broken_tests;
    
    long thread_index_allocator;
    
    char *extra_options;
};

void print_log(struct test *test){
    for(u8 *log_iterator = test->log_start; log_iterator < test->log_end;){
        
        char *log_entry = (char *)log_iterator;
        size_t log_entry_length = strlen(log_entry);
        
        print("%.*s", log_entry_length, log_entry);
        
        log_iterator += (log_entry_length + 1);
    }
}

#if _WIN32

static f64 os_get_time_in_seconds(void){
    LARGE_INTEGER performance_frequency;
    QueryPerformanceFrequency(&performance_frequency);
    
    LARGE_INTEGER time;
    QueryPerformanceCounter(&time);
    
    return (f64)time.QuadPart / (f64)performance_frequency.QuadPart;
}

struct string execute_command_output(struct memory_arena *arena, char *command_line, char *working_directory, u32 *exit_code){
    
    // 
    // Create a pipe to redirect the input and output.
    // Make sure this pipe has 'SecurityAttributes', 
    // otherwise the handles are not _inheritable_.
    // 
    HANDLE ReadHandle;
    HANDLE WriteHandle;
    
    SECURITY_ATTRIBUTES SecurityAttributes = { 
        .nLength = sizeof(SECURITY_ATTRIBUTES), 
        .lpSecurityDescriptor = NULL, 
        .bInheritHandle = TRUE,
    };
    
    if(!CreatePipe(&ReadHandle, &WriteHandle, &SecurityAttributes, /*Size = default*/0)){
        *exit_code = GetLastError();
        return string("CreatePipe failed\n");
    }
    
    // 
    // Redirect both 'stdout' and 'stderr' to a pipe we can read afterwards.
    // 
    STARTUPINFOA StartupInformation = {
        .cb = sizeof(StartupInformation),
        .dwFlags = STARTF_USESTDHANDLES,
        .hStdInput  = GetStdHandle(STD_INPUT_HANDLE),
        .hStdOutput = WriteHandle,
        .hStdError  = WriteHandle,
    };
    
    PROCESS_INFORMATION ProcessInformation;
    
    if(!CreateProcessA(NULL, command_line, /*ProcessAttributes*/NULL, /*ThreadAttributes*/NULL, /*InheritHandles*/TRUE, /*Flags*/0, /*Environment*/NULL, working_directory, &StartupInformation, &ProcessInformation)){
        print("Error: 'CreateProcess' failed with error %u (%s)\n", GetLastError(), command_line);
        *exit_code = GetLastError();
        CloseHandle(WriteHandle);
        CloseHandle(ReadHandle);
        return string("CreateProcess failed\n");
    }
    
    // 
    // Close all the handles we do not need anymore.
    // Because we close the 'WriteHandle', 'ReadFile' will fail,
    // once the child process has exited.
    // 
    CloseHandle(WriteHandle);
    CloseHandle(ProcessInformation.hThread);
    
    // 
    // Read all output from the child into the arena.
    // 
    DWORD read_size = 0x100;
    char *read_buffer = push_array(arena, char, read_size);
    char *read_at = read_buffer;
    
    DWORD bytes_read;
    while(ReadFile(ReadHandle, read_at, read_size, &bytes_read, NULL)){
        // 
        // Make sure there is space in the arena for the next bytes.
        // 
        push_array(arena, u8, read_size - bytes_read);
        
        read_at += bytes_read;
    }
    
    CloseHandle(ReadHandle);
    
    DWORD ExitCode;
    if(!GetExitCodeProcess(ProcessInformation.hProcess, &ExitCode)){
        print("Error: Failed to get ExitCode.\n");
        ExitCode = (DWORD)-1;
    }
    
    if(ExitCode == STILL_ACTIVE){
        // 
        // If the process is still active, it means it has closed its 
        // stdout handle prior to exiting. Wait for it, then get the exit code.
        // 
        
        WaitForSingleObject(ProcessInformation.hProcess, /*.5s*/500);
        if(!GetExitCodeProcess(ProcessInformation.hProcess, &ExitCode)){
            print("Error: Failed to get ExitCode.\n");
            ExitCode = (DWORD)-1;
        }
    }
    
    CloseHandle(ProcessInformation.hProcess);
    
    *exit_code = ExitCode;
    
    struct string string = {
        .data = read_buffer,
        .size = read_at - read_buffer,
    };
    return string;
}


struct string os_get_working_directory(struct memory_arena *arena){
    struct string working_directory = {0};
    // GetCurrentDirectory with 0 returns the size of the buffer including the null terminator
    working_directory.size = GetCurrentDirectoryA(0, null) - 1;
    working_directory.data = push_array(arena, char, working_directory.length + 1); // @cleanup: What about utf-8?
    GetCurrentDirectoryA((u32)working_directory.length + 1, working_directory.data);
    return working_directory;
}

int os_get_number_of_processors(void){
    SYSTEM_INFO system_info;
    GetSystemInfo(&system_info); // this cannot fail apperantly
    return system_info.dwNumberOfProcessors;
}

int path_is_directory(char *path){
    u32 file_attributes = GetFileAttributesA(path);
    return (file_attributes != INVALID_FILE_ATTRIBUTES) && (file_attributes & FILE_ATTRIBUTE_DIRECTORY);
}


struct os_file_iterator{
    WIN32_FIND_DATAA find_data;
    HANDLE handle;
};

struct os_file_iterator os_file_iterator_initialize(char *search_string){
    struct os_file_iterator ret = {0};
    ret.handle = FindFirstFileA(search_string, &ret.find_data);
    return ret;
}

// returns a file_iterator that will return 'false' on 'os_file_iterator_valid'
// and can later be initialized by just overwriting it
struct os_file_iterator os_file_iterator_invalid(void){
    struct os_file_iterator ret = {0};
    ret.handle = INVALID_HANDLE_VALUE;
    return ret;
}

int os_file_iterator_valid(struct os_file_iterator *iterator){
    return (iterator->handle != INVALID_HANDLE_VALUE);
}

struct string os_file_iterator_get(struct os_file_iterator *iterator){
    return (struct string){.data = iterator->find_data.cFileName, .size = strlen(iterator->find_data.cFileName)};
}

int os_file_iterator_is_directory(struct os_file_iterator *iterator){
    return (iterator->find_data.dwFileAttributes & /*FILE_ATTRIBUTE_DIRECTORY*/0x10);
}

void os_file_iterator_free(struct os_file_iterator *iterator){
    FindClose(iterator->handle);
    iterator->handle = INVALID_HANDLE_VALUE;
}

int os_file_iterator_next(struct os_file_iterator *iterator){
    int ret = 1;
    if(!FindNextFileA(iterator->handle, &iterator->find_data)){
        os_file_iterator_free(iterator);
        ret = 0;
    }
    return ret;
}

#else

#define MAX_PATH PATH_MAX

struct os_file_iterator{
    DIR *directory_handle;
    struct dirent *directory_entry;
    char *file_name_wildcard;
};

void os_file_iterator_free(struct os_file_iterator *iterator){
    closedir(iterator->directory_handle);
    iterator->directory_handle = null;
}

int os_file_iterator_is_directory(struct os_file_iterator *iterator){
    return (iterator->directory_entry->d_type == DT_DIR);
}

// stupid recursive version as I do not wanna allocate
// @cleanup: I vaguely remember this having some bugs...
int wildcard_cstring_match(char *pattern, char *string){
    
    if(*pattern == 0){
        // empty pattern only matches with empty string
        if(*string == 0) return true;
        return false;
    }
    
    if(*string == 0){
        if(*pattern == '*') return wildcard_cstring_match(pattern + 1, string);
        return false;
    }
    
    if(pattern[0] == '?' || *pattern == *string){
        return wildcard_cstring_match(pattern + 1, string + 1);
    }
    
    if(*pattern == '*'){
        return wildcard_cstring_match(pattern + 1, string) || wildcard_cstring_match(pattern, string + 1);
    }
    
    return false;
}

int linux_os_file_iterator_next_directory_entry_wildcard_match(struct os_file_iterator *iterator){
    while(true){
        struct dirent *directory_entry = readdir(iterator->directory_handle);
        if(!directory_entry){
            os_file_iterator_free(iterator);
            return 0;
        }
        
        if(wildcard_cstring_match(iterator->file_name_wildcard, directory_entry->d_name)){
            iterator->directory_entry = directory_entry;
            return 1;
        }
    }
}

struct os_file_iterator os_file_iterator_initialize(char *search_string){
    struct os_file_iterator ret = {0};
    
    s64 length = strlen(search_string);
    s64 last_slash = -1;
    for(s64 i = length - 1; i >= 0; i--){
        if(search_string[i] == '/' || search_string[i] == '\\'){
            last_slash = i;
            break;
        }
    }
    assert(last_slash >= 0);
    
    // hacky zero termination of the search string 
    char saved = search_string[last_slash];
    search_string[last_slash] = 0; 
    
    // DIR *opendir(char *filename);
    // 'return value' - on error NULL else a pointer to a directory stream
    
    ret.directory_handle = opendir(search_string);
    search_string[last_slash] = saved;
    
    if(!ret.directory_handle) return ret;
    
    ret.file_name_wildcard = search_string + (last_slash + 1);
    linux_os_file_iterator_next_directory_entry_wildcard_match(&ret);
    return ret;
}

struct os_file_iterator os_file_iterator_invalid(){
    struct os_file_iterator ret = {0};
    return ret;
}

int os_file_iterator_valid(struct os_file_iterator *iterator){
    return (iterator->directory_handle != null);
}

struct string os_file_iterator_get(struct os_file_iterator *iterator){
    return (struct string){.data = iterator->directory_entry->d_name, .size = strlen(iterator->directory_entry->d_name)};
}

// returns the file_name, i.e 'file' for 'C:/path/to/file'
int os_file_iterator_next(struct os_file_iterator *iterator){
    return linux_os_file_iterator_next_directory_entry_wildcard_match(iterator);
}

static int os_get_number_of_processors(){
    return sysconf(_SC_NPROCESSORS_ONLN);
}

static f64 os_get_time_in_seconds(void){
    struct timespec ts;
    
    if(clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return -1.0;
    
    return (f64)ts.tv_sec + (f64)ts.tv_nsec * 1e-9;
}


struct string execute_command_output(struct memory_arena *arena, char *command_line, char *working_directory, u32 *exit_code){
    
    // 
    // Create a pipe to redirect the input and output.
    // 
    int pipefd[2];
    if(pipe(pipefd) != 0){
        *exit_code = errno;
        return string("pipe failed");
    }
    
    // 
    // fork/exec the executable.
    // 
    
    pid_t pid = fork();
    
    if(pid < 0){
        close(pipefd[0]);
        close(pipefd[1]);
        
        *exit_code = errno;
        return string("fork failed");
    }
    
    if(pid == 0){
        
        // 
        // We are the child, dup the pipe file descriptors and exec.
        // 
        
        close(pipefd[0]);
        
        dup2(pipefd[1], STDOUT_FILENO);
        dup2(pipefd[1], STDERR_FILENO);
        
        close(pipefd[1]);
        
        if(chdir(working_directory) != 0){
            _exit(127);
        }
        
        execl("/bin/sh", "sh", "-c", command_line, (char *)NULL);
        
        _exit(127);
    }
    
    // 
    // We are the parent!
    // We don't need the write handle anymore.
    // 
    
    close(pipefd[1]);
    
    // 
    // Read all output from the child into the arena.
    // 
    size_t read_size = 0x100;
    char *read_buffer = push_array(arena, char, read_size);
    char *read_at = read_buffer;
    
    while(1){
        ssize_t bytes_read = read(pipefd[0], read_at, read_size);
        
        read_at += (size_t)bytes_read;
        
        if(bytes_read <= 0) break;
        
        push_array(arena, u8, read_size - bytes_read);
    }
    
    close(pipefd[0]);
    
    int status;
    waitpid(pid, &status, 0);
    
    if(WIFEXITED(status)){
        *exit_code = (u32)WEXITSTATUS(status);
    }else{
        *exit_code = (u32)-1;
    }
    
    struct string string = {
        .data = read_buffer,
        .size = read_at - read_buffer,
    };
    
    return string;
}

static struct string os_get_working_directory(struct memory_arena *arena){
    char *cwd = push_array(arena, char, PATH_MAX);
    
    if(getcwd(cwd, PATH_MAX) != NULL){
        struct string ret = {.data = cwd, .size = strlen(cwd)};
        arena->allocated -= PATH_MAX - (ret.size + 1);
        return ret;
    }
    
    arena->allocated -= PATH_MAX;
    return (struct string){0};
}

int path_is_directory(char *path){
    struct stat file_info;
    int stat_error = stat(path, &file_info);
    if(stat_error < 0) return false; // does not exits or some thing
    return (file_info.st_mode & S_IFDIR) ? true : false;
}

#endif


unsigned int test_thread_entry(void *thread_parameter){
    
    struct work *work = thread_parameter;
    
    // 
    // Allocate arenas.
    // 
    struct memory_arena arena = {0};
    struct memory_arena log   = create_memory_arena(giga_bytes(64)); // @note: this needs to be initialized so we can call 'arena_current' for the log start for the first entry.
    
    // 
    // Generate a unique file path/name for the output file.
    // 
    
    // @note: We were using the 'GetCurrentThreadId()' function here, but that gives big random indices
    //        and thus we did not overwrite the executables of previous test runs.
    //        Also we are putting the executables in the 'tests' directory, as it is excluded from
    //        Defender, and thus we _might_ gain some perf.
    
    struct string working_directory = os_get_working_directory(&arena);
    
    long thread_index = _InterlockedIncrement(&work->thread_index_allocator);
    char *output_file_name = push_format_cstring(&arena, "%.*s/tests/test_%x", working_directory.size, working_directory.data, thread_index);
    
    char *out_command = push_format_cstring(&arena, "-out \"%s\"", output_file_name);
    
    while(true){
        struct temporary_memory arena_temp = begin_temporary_memory(&arena);
        struct temporary_memory log_temp   = begin_temporary_memory(&log);
        
        u8 *log_start = arena_current(&log);
        
        int test_index = _InterlockedIncrement(&work->test_at) - 1;
        
        // Check if we are done.
        if(test_index >= work->amount_of_tests) break;
        
        struct test *test = &work->tests[test_index];
        char *file_name = test->file_name;
        char *directory = test->directory;
        char *file_path = push_format_cstring(&arena, "%s/%s", directory, file_name);
        
        {
            f64 start_time = os_get_time_in_seconds();
            
            // 
            // Start running the test.
            // 
            
            int error = 0;
            
            // 
            // Load the file into memory, so we can parse the "test-header".
            // 
            struct string file = load_file(&arena, file_path);
            
            struct{
                struct command_node{
                    struct command_node *next;
                    
                    enum command_type{
                        COMMAND_compile,
                        COMMAND_check,
                        COMMAND_reject,
                        COMMAND_fail,
                        COMMAND_run,
                        COMMAND_dump,
                    } type;
                    
                    struct string data;
                } *first, *last;
            } command_list = {0};
            
            int skip = false;
            
            while(file.size){
                struct string line = string_eat_line(&file);
                
                // Strip the newline and trailling whitespace.
                while(line.size && (line.data[line.size-1] == '\n' || line.data[line.size-1] == '\r' || line.data[line.size-1] == ' ')) line.size -= 1;
                
                if(!string_front_match_eat(&line, "//")) break;
                
                string_eat_whitespace(&line);
                
                if(line.size == 0) continue; // ALlow empty lines.
                
                // 
                // The lines are comments at the start of the file with a bunch of commands:
                //     
                //     compile [-compile_flags]
                //     check "string to check"
                //     fail ["string to check"]
                //     run [-run_flags]
                //     dump [-dump_flags] (@incomplete)
                //     
                // There are also two commands to indicate that we should not run the file:
                //     broken - This is a known bug.
                //     skip   - This is a supplimentary file (e.g.: an other.c file for a main.c).
                // 
                
                struct string command = string_eat_until_whitespace(&line);
                string_eat_whitespace(&line);
                
                struct command_node *command_node = null;
                
                if(string_match(command, string("compile"))){
                    command_node = push_struct(&arena, struct command_node);
                    command_node->type = COMMAND_compile;
                    command_node->data = line;
                }else if(string_match(command, string("check"))){
                    if(line.size >= 2 && line.data[0] == '"' && line.data[line.size-1] == '"'){
                        line.size -= 2;
                        line.data += 1;
                    }
                    
                    command_node = push_struct(&arena, struct command_node);
                    command_node->type = COMMAND_check;
                    command_node->data = line;
                }else if(string_match(command, string("reject"))){
                    if(line.size >= 2 && line.data[0] == '"' && line.data[line.size-1] == '"'){
                        line.size -= 2;
                        line.data += 1;
                    }
                    
                    command_node = push_struct(&arena, struct command_node);
                    command_node->type = COMMAND_reject;
                    command_node->data = line;
                }else if(string_match(command, string("fail"))){
                    if(line.size >= 2 && line.data[0] == '"' && line.data[line.size-1] == '"'){
                        line.size -= 2;
                        line.data += 1;
                    }
                    
                    command_node = push_struct(&arena, struct command_node);
                    command_node->type = COMMAND_fail;
                    command_node->data = line;
                }else if(string_match(command, string("run"))){
                    command_node = push_struct(&arena, struct command_node);
                    command_node->type = COMMAND_run;
                    command_node->data = line;
                }else if(string_match(command, string("broken"))){
                    push_format_cstring(&log, "%s: Skipping because it was marked 'broken'.\n", file_path);
                    error = true;
                }else if(string_match(command, string("skip"))){
                    skip = true;
                }else if(string_match(command, string("dump"))){
                    push_format_cstring(&log, "%s: Error: @incomplete 'dump' not yet implemented.\n", file_path);
                    error = true;
                }else{
                    push_format_cstring(&log, "%s: Error: unhandled command '%.*s'\n", file_path, command.size, command.data);
                    error = true;
                }
                
                if(command_node){
                    if(command_list.first){
                        command_list.last = command_list.last->next = command_node;
                    }else{
                        command_list.first = command_list.last = command_node;
                    }
                }
            }
            
            // 
            // Validate the 'command_list':
            //    
            //    The 'compile' command should only ever be the first command.
            //    The 'fail'    command should only ever be the last  command.
            //    If 'fail' is used, we should not have 'run' or 'dump' commands.
            //    
            // And infer some basic information:
            // 
            //    Needs executable - true if any 'run' or 'dump' is present.
            //    Should fail      - true if the last command is 'fail'.
            // 
            
            int needs_executable = false;
            
            for(struct command_node *command = command_list.first; command; command = command->next){
                
                if(command->type == COMMAND_compile && command != command_list.first){
                    push_format_cstring(&log, "Error: The command 'compile' should only ever be the first command.\n");
                    error = 1;
                }
                
                if(command->type == COMMAND_fail && command != command_list.last){
                    push_format_cstring(&log, "Error: The command 'fail' should only ever be the last command.\n");
                    error = 1;
                }
                
                if(command->type == COMMAND_run || command->type == COMMAND_dump){
                    needs_executable = true;
                }
            }
            
            if(needs_executable && command_list.last->type == COMMAND_fail){
                push_format_cstring(&log, "Error: The 'fail' command should only be used for compilation failiures.\n");
                error = 1;
            }
            
            // 
            // If we encountered an error, or the test was marked 'broken' or 'skip',
            // do not execute it, instead save the log and mark the result accordingly.
            // 
            if(error || skip){ 
                if(skip){
                    // This is not a failiure case.
                    // Hence, we don't need a log.
                    _InterlockedIncrement(&work->skipped);
                    end_temporary_memory(&log_temp);
                }else{
                    test->result = TEST_RESULT_broken;
                    test->log_start = log_start;
                    test->log_end = arena_current(&log);
                    
                    _InterlockedIncrement(&work->broken_tests);
                }
                
                end_temporary_memory(&arena_temp);
                continue;
            }
            
            // 
            // First execute the compile command:
            // 
            //    There are default options to minimize the amount of work necessary.
            //        -dont_print_the_files (if !needs_executable)
            // 
            // This used to be longer :)
            
            char *default_command = "hlc";
            char *output_file = needs_executable ? out_command : "-dont_print_the_files";
            
            struct string test_compile_options = string("");
            if(command_list.first && command_list.first->type == COMMAND_compile){
                test_compile_options = command_list.first->data;
            }
            
            char *compile_command_line = push_format_cstring(&arena, "%s %s %s %.*s%s", default_command, output_file, file_name, test_compile_options.size, test_compile_options.data, work->extra_options);
            
            u32 compile_exit_code = 0;
            struct string output = execute_command_output(&arena, compile_command_line, directory, &compile_exit_code);
            
            push_format_cstring(&log, "%s> %s:\n", directory, compile_command_line);
            push_format_cstring(&log, "Exited with code 0x%x\n", compile_exit_code);
            push_format_cstring(&log, "%.*s\n", output.size, output.data);
            
            if(compile_exit_code != 0 && compile_exit_code != 1){
                push_format_cstring(&log, "Error: Compiler crashed. (ExitCode 0x%x)\n", compile_exit_code);
                error = 1;
            }
            
            int should_fail = (command_list.last && command_list.last->type == COMMAND_fail);
            
            int expected_exit_code = should_fail ? 1 : 0;
            if(compile_exit_code != expected_exit_code){
                if(compile_exit_code == 0) push_format_cstring(&log, "Error: Compilation was supposed to fail, but succeeded.\n");
                if(compile_exit_code == 1) push_format_cstring(&log, "Error: Compilation failed.\n");
                error = 1;
            }
            
            // 
            // Iterate and execute the commands.
            // 
            for(struct command_node *command = command_list.first; !error && command; command = command->next){
                
                switch(command->type){
                    case COMMAND_compile: continue; // This was handled above.
                    
                    case COMMAND_fail:
                    case COMMAND_check:{
                        if(!string_is_substring(command->data, output)){
                            error = 1;
                            push_format_cstring(&log, "Error: Expected '%.*s' in the output.\n", command->data.size, command->data.data);
                        }
                    }break;
                    
                    case COMMAND_reject:{
                        if(string_is_substring(command->data, output)){
                            error = 1;
                            push_format_cstring(&log, "Error: Found rejected string '%.*s' in the output.\n", command->data.size, command->data.data);
                        }
                    }break;
                    
                    case COMMAND_run:{
                        u32 run_exit_code;
                        
                        char *run_command_line = push_format_cstring(&arena, "%s.exe%s%.*s", output_file_name, command->data.size ? " " : "", command->data.size, command->data.data);
                        
                        // 
                        // We overwrite output here, this means subsequent 'check' commands check against this output.
                        // 
                        output = execute_command_output(&arena, run_command_line, directory, &run_exit_code);
                        
                        push_format_cstring(&log, "%s:\n", run_command_line);
                        push_format_cstring(&log, "Exited with code 0x%x\n", run_exit_code);
                        push_format_cstring(&log, "%.*s\n", output.size, output.data);
                        
                        if(run_exit_code != 0){
                            error = 1;
                            push_format_cstring(&log, "Error: Run '%s' failed.\n", run_command_line);
                        }
                    }break;
                    
                    case COMMAND_dump:{
                        // @cleanup: @incomplete: execute a dump command here.
                    }break;
                }
            }
            
            if(error){
                // 
                // If there was an error, print the log.
                // 
                test->result = TEST_RESULT_fail;
                test->log_start = log_start;
                test->log_end   = arena_current(&log);
                
                static struct ticket_spinlock log_print_lock = {0};
                ticket_spinlock_lock(&log_print_lock);
                
                print_log(test); // Because this is more then a single call to print, we need to have a spinlock.
                
                ticket_spinlock_unlock(&log_print_lock);
                _InterlockedIncrement(&work->failed_tests);
            }else{
                // 
                // It was a success! Clear the log, we don't need it.
                // 
                end_temporary_memory(&log_temp);
                
                char *test_type = "compile";
                if(needs_executable) test_type = "run"; // @cleanup: dump?
                if(should_fail) test_type = "fail";
                
                print("%s: %s success! (%f seconds)\n", file_path, test_type, os_get_time_in_seconds() - start_time);
            }
        }
        
        // Always reset the 'arena', but only reset the 'log' if the test succeeded.
        end_temporary_memory(&arena_temp);
    }
    
    return 0;
}

int main(int argument_count, char *argument_values[]){
    
    // 
    // Options for the test runner:
    //    -threads <thread-count>
    //    <folder>
    //    <test-file>
    //    -- <global-options>
    // 
    
    char *targets[0x100] = {0};
    int amount_of_targets = 0;
    
    int thread_count = -1;
    
    struct memory_arena arena = {0};
    char *extra_options = "";
    
    
    for(int argument_index = 1; argument_index < argument_count; argument_index++){
        char *argument = argument_values[argument_index];
        
        if(argument[0] == '-' || argument[0] == '/'){
            argument += 1;
            
            if(strcmp(argument, "threads") == 0){
                argument_index += 1;
                
                if(argument_index == argument_count){
                    print("Error: Expected an argument after '-threads'.\n");
                    return -1;
                }
                
                thread_count = atoi(argument_values[argument_index]);
                
                if(thread_count < 1){
                    print("Error: Invalid thread count '%d' specified.\n", thread_count);
                    return -1;
                }
            }else if(strcmp(argument, "-") == 0){
                // 
                // Argument delimiter, everything afterwards is for the compile of each test.
                // 
                
                for(argument_index += 1; argument_index < argument_count; argument_index++){
                    extra_options = push_format_cstring(&arena, "%s %s", extra_options, argument_values[argument_index]);
                }
                
                break;
            }else{
                print("Error: Unknown argument '%s'\n", argument);
                return -1;
            }
        }else{
            
            if(amount_of_targets >= array_count(targets)){
                print("Error: Too many targets specified. Maximally %llu allowed.\n", array_count(targets));
                return -1;
            }
            
            targets[amount_of_targets++] = argument;
        }
    }
    
    if(thread_count == -1){
        // 
        // Detect the amount of logical processors.
        // 
        
        thread_count = os_get_number_of_processors()/2;
        if(thread_count < 1) thread_count = 1;
    }
    
    print("Running with %d threads.\n", thread_count);
    
    if(amount_of_targets == 0){
        // 
        // If no target was specified, default to the target of 'tests'.
        // 
        targets[amount_of_targets++] = "tests";
    }
    
    struct file_name_entry{
        struct file_name_entry *next;
        char *file_name;
        char *directory;
    } *file_names = 0;
    
    // 
    // Gather all files for each target.
    // Targets can be either folders or files.
    // 
    for(int target_index = 0; target_index < amount_of_targets; target_index++){
        char *target = targets[target_index];
        
        int is_directory = path_is_directory(target);
        
        if(!is_directory){
            // 
            // If it's a single file, add it to the list.
            // 
            
            struct file_name_entry *entry = push_struct(&arena, struct file_name_entry);
            entry->file_name = target;
            
            struct string target_string = create_string(target, strlen(target));
            struct string path = strip_file_name(target_string);
            if(path.size){
                entry->file_name += path.size;            
                entry->directory = push_format_cstring(&arena, "%.*s", path.size, path.data);
            }else{
                entry->directory = ".";
            }
            
            entry->next = file_names;
            file_names = entry;
            continue;
        }
        
        // 
        // The 'target' is a directory, add all files in the directory to the list.
        // We do this recursively, so we get all the files we care about.
        // 
        
        struct file_name_entry initial_directory = {
            .file_name = target,
        };
        
        struct file_name_entry *directory_list = &initial_directory;
        
        while(directory_list){
            // 
            // Pop the path off of the 'directory_list'.
            // 
            char *directory_path = directory_list->file_name;
            size_t directory_path_length = strlen(directory_path);
            
            directory_list = directory_list->next;
            
            // 
            // Build the 'SearchBuffer' as "<directory_path>\*".
            // 
            char SearchBuffer[MAX_PATH];
            if(directory_path_length + 3 >= MAX_PATH){
                print("WARNING: Directory path '%s' exceeds MAX_PATH. Ignoring it.\n", directory_path);
                continue;
            }
            memcpy(SearchBuffer, directory_path, directory_path_length);
            SearchBuffer[directory_path_length + 0] = '/';
            SearchBuffer[directory_path_length + 1] = '*';
            SearchBuffer[directory_path_length + 2] = 0;
            
            struct os_file_iterator file_iterator = os_file_iterator_initialize(SearchBuffer);
            
            if(!os_file_iterator_valid(&file_iterator)) continue; // I don't think this can happen.
            
            do{
                struct string file_name = os_file_iterator_get(&file_iterator);
                
                if(os_file_iterator_is_directory(&file_iterator)){
                    if(strcmp(file_name.data, ".") == 0 || strcmp(file_name.data, "..") == 0){
                        continue;
                    }
                    
                    // 
                    // If its a directory, allocate a new path and push it to the 'directory_list'.
                    // The new path is of the form <directory_path>\<FileName>
                    // 
                    
                    char *new_directory_path = push_array(&arena, char, directory_path_length + 1 + file_name.length + 1);
                    memcpy(new_directory_path, directory_path, directory_path_length);
                    new_directory_path[directory_path_length] = '/';
                    memcpy(new_directory_path + directory_path_length + 1, file_name.data, file_name.length);
                    new_directory_path[directory_path_length + 1 + file_name.length] = 0;
                    
                    struct file_name_entry *new_directory_entry = push_struct(&arena, struct file_name_entry);
                    new_directory_entry->file_name = new_directory_path;
                    new_directory_entry->next = directory_list;
                    directory_list = new_directory_entry;
                    
                    continue;
                }
                
                // 
                // Check that the FileName ends in '.c'.
                // 
                if((file_name.length < 2) || (file_name.data[file_name.length-1] != 'c') || (file_name.data[file_name.length-2] != '.')){
                    continue;
                }
                
                // 
                // Add the file to the 'file_names' list.
                // 
                
                char *new_file_path = push_array(&arena, char, file_name.length + 1);
                memcpy(new_file_path, file_name.data, file_name.length);
                new_file_path[file_name.length] = 0;
                
                struct file_name_entry *new_file_entry = push_struct(&arena, struct file_name_entry);
                new_file_entry->file_name = new_file_path;
                new_file_entry->directory = directory_path;
                new_file_entry->next = file_names;
                file_names = new_file_entry;
                
            }while(os_file_iterator_next(&file_iterator));
        }
    }
    
    // 
    // Count the amount of tests, so we can allocate storage for them.
    // 
    int amount_of_tests = 0;
    for(struct file_name_entry *it = file_names; it; it = it->next){
        amount_of_tests += 1;
    }
    
    struct test *tests = push_array(&arena, struct test, amount_of_tests);
    {
        int test_index = 0;
        for(struct file_name_entry *it = file_names; it; it = it->next, test_index++){
            tests[test_index].file_name = it->file_name;
            tests[test_index].directory = it->directory;
        }
    }
    
    struct work work = {
        .tests = tests,
        .amount_of_tests = amount_of_tests,
        .extra_options = extra_options,
    };
    
#ifdef _WIN32
    HANDLE *ThreadHandles = push_array(&arena, HANDLE, thread_count);
    
    for(u32 ThreadIndex = 0; ThreadIndex < thread_count; ThreadIndex++){
        
        ThreadHandles[ThreadIndex] = CreateThread(/*SecurityAttributes*/NULL, /*StackSize*/0, test_thread_entry, &work, /*flags*/0, /*optional-out-ThreadId*/NULL);
        if(ThreadHandles[ThreadIndex] == NULL){
            print("Error: CreateThread failed with error %u\n", GetLastError());
            return -1;
        }
    }
    
    WaitForMultipleObjects(thread_count, ThreadHandles, /*WaitAll*/TRUE, INFINITE);
#else
    
    pthread_t *threads = push_array(&arena, pthread_t , thread_count);
    
    for(u32 index = 0; index < thread_count; index++){
        int pthread_error = pthread_create(&threads[index], NULL, (void *(*)(void *))test_thread_entry, &work);
        if(pthread_error != 0){
            perror("pthread_create");
            return -1;
        }
    }
    
    for(int index = 0; index < thread_count; index++){
        pthread_join(threads[index], NULL);
    }
    
#endif
    
    if(work.broken_tests){
        print("\n\n");
        print("broken tests:\n\n");
        
        for(int test_index = 0; test_index < amount_of_tests; test_index++){
            struct test *test = &tests[test_index];
            if(test->result == TEST_RESULT_broken){
                // 
                // Print the log!
                // 
                print_log(test);
                print("\n\n");
            }
        }
    }
    
    
    if(work.failed_tests){
        print("\n\n");
        print("Failed tests:\n\n");
        
        for(int test_index = 0; test_index < amount_of_tests; test_index++){
            struct test *test = &tests[test_index];
            if(test->result == TEST_RESULT_fail){
                // 
                // Print the log!
                // 
                print_log(test);
                print("\n\n");
            }
        }
    }
    
    print("\n");
    print("%d tests run.\n", amount_of_tests - work.skipped);
    print("%d tests failed.\n", work.failed_tests);
    print("%d tests marked broken skipped.\n", work.broken_tests);
    
    return 0;
}

