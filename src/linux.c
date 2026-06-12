
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

typedef int HANDLE;

#define MAX_PATH PATH_MAX

#define stdin  0
#define stdout 1
#define stderr 2

struct parsed_command_line{
    int argc;
    char **argv;
};

struct parsed_command_line parse_command_line(char *line){
    struct parsed_command_line parsed_command_line = {0};
    
    wordexp_t p;
    
    if(wordexp(line, &p, 0) == 0){
        parsed_command_line.argc = p.we_wordc;
        parsed_command_line.argv = p.we_wordv;
    }
    
    return parsed_command_line;
}

static struct string os_get_working_directory(struct memory_arena *arena){
    char *cwd = push_uninitialized_data(arena, u8, PATH_MAX);
    
    if(getcwd(cwd, PATH_MAX) != NULL){
        struct string ret = cstring_to_string(cwd);
        arena->current -= PATH_MAX - (ret.size + 1);
        return ret;
    }
    
    arena->current -= PATH_MAX;
    return (struct string){0};
}

static struct string os_get_executable_path(struct memory_arena *arena){
    char *path = push_uninitialized_data(arena, u8, PATH_MAX + 1);
    
    struct string ret = {0};
    
    ssize_t len = readlink("/proc/self/exe", path, PATH_MAX);
    if (len != -1) {
        ret.data = path;
        ret.size = len;
    }
    
    arena->current -= (PATH_MAX + 1) - (ret.size + 1);
    return ret;
}


u32 os_print_string(char *string, smm length){
    // ssize_t write(int fd, const void *buf, size_t count);
    // 'return value' - number of bytes written, might be smaller then 'buffer_size' in edge cases,
    //                  '-1' on error
    // 'fd'           - file descriptor of the file to write
    // 'buf'          - buffer to write
    // 'count'        - size of buffer to write
    
    return write(STDOUT_FILENO, string, length);
}

struct os_virtual_buffer os_reserve_memory(void *desired_base, smm reserve_size){
    // void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);
    // 'return_value' - pointer to mapped area or '(void *)-1' on error
    // 'addr'         - hint for the starting address for the mapping, if zero the kernel chooses
    // 'length'       - desired_length 
    // 'prot'         - protections; bitwise or of 'PROT_EXEC', 'PROT_READ', 'PROT_WRITE' or 'PROT_NONE'
    // 'flags'        - fancy options. Most important 'MAP_ANONYMOUS' meaning no file fd or offset.
    // 'fd'           - for us always '-1'.
    // 'offset'       - for us always '0'.
    
    void *memory = mmap(desired_base, reserve_size, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    
    struct os_virtual_buffer ret = zero_struct;
    if(memory == (void *)-1){
        return ret;
    }
    
    ret.memory   = memory;
    ret.reserved = reserve_size;
    ret.committed = 0;
    return ret;
}

void os_free_memory_all(void *memory_to_free, size_t size){
    munmap(memory_to_free, size);
}

struct os_thread_info{
    pthread_t thread_identifier;
};

// @cleanup: duplicated
typedef u32 (*os_thread_proc)(void *param);

static struct os_thread_info os_create_thread(os_thread_proc proc, void *data){
    struct os_thread_info info = zero_struct;
    
    // int pthread_create(pthread_t *thread, const pthread_attr_t *attr, 
    //                    void *(*start_routine)(void *), void *arg);
    // 'return value' - on success '0' an error otherwise, then 'thread' will be undefined
    // 'thread'       - return argument, will receive the ID of the new thread
    // 'attr'         - attributes, NULL means default.
    
    pthread_t thread_identifier = 0;
    int pthread_error = pthread_create(&thread_identifier, NULL, (void *(*)(void *))proc, data);
    if(pthread_error == 0){
        info.thread_identifier = thread_identifier;
    }
    
    return info;
}


b32 path_is_directory(char *path){
    struct stat file_info;
    int stat_error = stat(path, &file_info);
    if(stat_error < 0) return false; // does not exits or some thing
    return (file_info.st_mode & S_IFDIR) ? true : false;
}

struct os_virtual_buffer os_commit_memory(void *desired_base, smm commit_size){
    // void *mprotect(void *addr, size_t len, int prot);
    // 'return_value' - '0' on success '-1' on error
    
    int error = mprotect(desired_base, commit_size, PROT_READ | PROT_WRITE);
    
    struct os_virtual_buffer ret = zero_struct;
    
    if(error != 0){
        return ret;
    }
    
    ret.base = desired_base;
    ret.committed = commit_size;
    ret.reserved = 0; // @cleanup: this is kinda terrible
    return ret;
}

void os_panic(u32 exit_code){
    _exit((int)exit_code);
}

void os_debug_break(void){
    __asm__("int3");
}

func s64 atomic_postincrement(s64 *val){
    return __sync_fetch_and_add(val, 1);
}

func s64 atomic_preincrement(s64 *val){
    return __sync_add_and_fetch(val, 1);
}

func s64 atomic_postdecrement(s64 *val){
    return __sync_fetch_and_sub(val, 1);
}

func s64 atomic_predecrement(s64 *val){
    return __sync_sub_and_fetch(val, 1);
}

func s64 atomic_add(s64 *val, s64 to_add){
    return __sync_fetch_and_add(val, to_add);
}


func void *atomic_compare_and_swap(void *dest, void *source, void *comparand){
    return (void *)_InterlockedCompareExchange64((s64 *)dest, (s64)source, (s64)comparand);
}

func smm atomic_compare_and_swap_smm(smm *dest, smm source, smm comparand){
    return _InterlockedCompareExchange64((s64 *)dest, (s64)source, (s64)comparand);
}

func u64 atomic_compare_and_swap_u64(u64 *dest, u64 source, u64 comparand){
    return _InterlockedCompareExchange64((s64 *)dest, (s64)source, (s64)comparand);
}

// returns 1 on success
// returns 0 on fail and overrides *comparand with *dest
func b32 atomic_compare_and_swap_128(m128 *dest, m128 source, m128 *comparand){
    assert(((umm)dest & 15) == 0);
    return _InterlockedCompareExchange128((__int64 *)dest, source.ptr2, source.ptr1, (__int64 *)comparand);
}


struct os_file os_load_file(char *file_name, void *buffer, smm buffer_size){
    
    struct os_file ret = zero_struct;
    
    // int open(const char *pathname, int flags, mode_t mode);
    // 'return value' - file descriptor (non-negative) or '-1' on error
    // 'pathname'     - file name ? only full path?
    // 'flags'        - One of a lot, we only need Read-only here (must include one of RDONLY,WRONLY, RDWR)
    // 'mode'         - specifies the permissions if the file is created, in our case this does not matter
    int file_descriptor = open(file_name, O_RDONLY, 0);
    if(file_descriptor < 0){
        ret.file_does_not_exist = true;
        return ret; // no need to close the fd here.
    }
    
    
    // int fstat(int fd, struct stat *buf);
    // 'return value' - '0' on success '-1' on error
    // 'fd'           - the file descriptor to get a 'stat' for.
    // 'buf'          - out parameter that contains information about the file.
    struct stat file_information;
    int stat_error = fstat(file_descriptor, &file_information);
    if(stat_error != 0){
        goto cleanup;
    }
    
    ret.size = file_information.st_size;
    
    // @note: time tracked in 100ns-intervalls since Unix epoch
    ret.access_time       = file_information.st_atim.tv_sec * 10000000ull + file_information.st_atim.tv_nsec/100;
    ret.modification_time = file_information.st_atim.tv_sec * 10000000ull + file_information.st_atim.tv_nsec/100;
    ret.creation_time     = file_information.st_atim.tv_sec * 10000000ull + file_information.st_atim.tv_nsec/100;
    
    
    if(buffer_size < ret.size){
        goto cleanup;
    }
    
    ssize_t bytes_read = 0;
    do{
        
        // ssize_t read(int fd, void *buf, size_t count);
        // 'return value' - on success number of bytes read, might be less then requested because read() was
        //                  interrupted by a signal. '-1' on error.
        // 'fd'           - file descriptor of the file to read
        // 'buf'          - buffer that receives the file
        // 'count'        - size of the buffer that receives the file
        ssize_t size_read = read(file_descriptor, buffer, buffer_size);
        if(size_read < 0) goto cleanup; // error
        if(size_read == 0) break; // we are done
        
        bytes_read += size_read;
    }while(bytes_read < ret.size);
    
    ret.memory = buffer;
    
    cleanup:;
    // int close(int fd);
    // 'return value' - on error '-1' on success '0'. 
    // 'fd'           - the file descriptor to close.
    // NOTES: should not be retried on failiure!
    
    close(file_descriptor);
    return ret;
}

b32 os_write_file(char *file_name, void *buffer, smm buffer_size){
    
    // int open(const char *pathname, int flags, mode_t mode);
    // 'return value' - file descriptor (non-negative) or '-1' on error
    // 'pathname'     - file name ? only full path?
    // 'flags'        - 'O_WRONLY' open the file for writing, 'O_CREAT' creat if id does not exist
    // 'mode'         - specifies the permissions if the file is created, we set read write for user
    int file_descriptor = open(file_name, O_RDONLY, S_IRUSR | S_IWUSR);
    if(file_descriptor < 0){
        return false;
    }
    
    // ssize_t write(int fd, const void *buf, size_t count);
    // 'return value' - number of bytes written, might be smaller then 'buffer_size' in edge cases,
    //                  '-1' on error
    // 'fd'           - file descriptor of the file to write
    // 'buf'          - buffer to write
    // 'count'        - size of buffer to write
    ssize_t bytes_written = write(file_descriptor, buffer, buffer_size);
    
    return (bytes_written == buffer_size);
}

HANDLE os_open_file(char *file_name){
    return open(file_name, O_RDONLY, 0);
}

int os_file_write(HANDLE file_handle, void *buffer, smm buffer_size){
    
    // ssize_t write(int fd, const void *buf, size_t count);
    // 'return value' - number of bytes written, might be smaller then 'buffer_size' in edge cases,
    //                  '-1' on error
    // 'fd'           - file descriptor of the file to write
    // 'buf'          - buffer to write
    // 'count'        - size of buffer to write
    ssize_t bytes_written = write(file_handle, buffer, buffer_size);
    
    return (bytes_written == buffer_size);
}

static void os_close_handle(HANDLE file_handle){
    close(file_handle);
}

static int os_get_number_of_processors(){
    return sysconf(_SC_NPROCESSORS_ONLN);
}

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
b32 wildcard_cstring_match(char *pattern, char *string){
    
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
    struct os_file_iterator ret = zero_struct;
    
    smm length = cstring_length(search_string);
    smm last_slash = -1;
    for(smm i = length - 1; i >= 0; i--){
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
    struct os_file_iterator ret = zero_struct;
    return ret;
}

b32 os_file_iterator_valid(struct os_file_iterator *iterator){
    return (iterator->directory_handle != null);
}

struct string os_file_iterator_get(struct os_file_iterator *iterator){
    return string_from_cstring(iterator->directory_entry->d_name);
}

// returns the file_name, i.e 'file' for 'C:/path/to/file'
int os_file_iterator_next(struct os_file_iterator *iterator){
    return linux_os_file_iterator_next_directory_entry_wildcard_match(iterator);
}


static f64 os_get_time_in_seconds(void){
    struct timespec ts;
    
    if(clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return -1.0;
    
    return (f64)ts.tv_sec + (f64)ts.tv_nsec * 1e-9;
}

HANDLE os_create_event(void){
    return eventfd(0, 0);
}

void os_wait_for_event(HANDLE event){
    u64 value;
    read(event, &value, sizeof(value));
}


u32 os_get_unix_time(void){
    return time(NULL);
}

