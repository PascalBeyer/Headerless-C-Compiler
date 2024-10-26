// run

#include <stdatomic.h>

atomic_flag a_flag;
atomic_bool a_bool;
atomic_char a_char;
atomic_short a_short;
atomic_int a_int;
atomic_long a_long;
atomic_llong a_llong;

atomic_uchar a_uchar;
atomic_ushort a_ushort;
atomic_uint a_uint;
atomic_ulong a_ulong;
atomic_ullong a_ullong;

// To make the naming more consistent.
static typedef long long llong;
static typedef unsigned char      uchar;
static typedef unsigned short     ushort;
static typedef unsigned int       uint;
static typedef unsigned long      ulong;
static typedef unsigned long long ullong;

int main(){
    
    _Bool n_bool = {0};
    char n_char = {0};
    short n_short = {0};
    int n_int = {0};
    long n_long = {0};
    llong n_llong = {0};
    
    uchar n_uchar = {0};
    ushort n_ushort = {0};
    uint n_uint = {0};
    ulong n_ulong = {0};
    ullong n_ullong = {0};
    
    
    //_____________________________________________________________________________________________________________________
    // Atomic Fences
    
    atomic_thread_fence(memory_order_seq_cst);
    atomic_signal_fence(memory_order_seq_cst);
    
    //_____________________________________________________________________________________________________________________
    // Atomic Flag Functions
    
    n_bool = atomic_flag_test_and_set(&a_flag);
    n_bool = atomic_flag_test_and_set_explicit(&a_flag, memory_order_seq_cst);
    
    atomic_flag_clear(&a_flag);
    atomic_flag_clear_explicit(&a_flag, memory_order_seq_cst);
    
    //_____________________________________________________________________________________________________________________
    // Atomic Exchange Functions
    
    n_bool   = __atomic_exchange_bool(   &a_bool,   n_bool);
    n_char   = __atomic_exchange_char(   &a_char,   n_char);
    n_short  = __atomic_exchange_short(  &a_short,  n_short);
    n_int    = __atomic_exchange_int(    &a_int,    n_int);
    n_long   = __atomic_exchange_long(   &a_long,   n_long);
    n_llong  = __atomic_exchange_llong(  &a_llong,  n_llong);
    n_uchar  = __atomic_exchange_uchar(  &a_uchar,  n_uchar);
    n_ushort = __atomic_exchange_ushort( &a_ushort, n_ushort);
    n_uint   = __atomic_exchange_uint(   &a_uint,   n_uint);
    n_ulong  = __atomic_exchange_ulong(  &a_ulong,  n_ulong);
    n_ullong = __atomic_exchange_ullong( &a_ullong, n_ullong);
    
    //_____________________________________________________________________________________________________________________
    // Atomic Compare Exchange Functions
    
    n_bool = __atomic_compare_exchange_char(&a_char, &n_char, n_char);
    n_bool = __atomic_compare_exchange_short(&a_short, &n_short, n_short);
    n_bool = __atomic_compare_exchange_int(&a_int, &n_int, n_int);
    n_bool = __atomic_compare_exchange_long(&a_long, &n_long, n_long);
    n_bool = __atomic_compare_exchange_llong(&a_llong, &n_llong, n_llong);
    n_bool = __atomic_compare_exchange_uchar(&a_uchar, &n_uchar, n_uchar);
    n_bool = __atomic_compare_exchange_ushort(&a_ushort, &n_ushort, n_ushort);
    n_bool = __atomic_compare_exchange_uint(&a_uint, &n_uint, n_uint);
    n_bool = __atomic_compare_exchange_ulong(&a_ulong, &n_ulong, n_ulong);
    n_bool = __atomic_compare_exchange_ullong(&a_ullong, &n_ullong, n_ullong);
    
    //_____________________________________________________________________________________________________________________
    // Atomic Fetch Add Functions
    
    n_char   = __atomic_fetch_add_char(   &a_char,   n_char);
    n_short  = __atomic_fetch_add_short(  &a_short,  n_short);
    n_int    = __atomic_fetch_add_int(    &a_int,    n_int);
    n_long   = __atomic_fetch_add_long(   &a_long,   n_long);
    n_llong  = __atomic_fetch_add_llong(  &a_llong,  n_llong);
    n_uchar  = __atomic_fetch_add_uchar(  &a_uchar,  n_uchar);
    n_ushort = __atomic_fetch_add_ushort( &a_ushort, n_ushort);
    n_uint   = __atomic_fetch_add_uint(   &a_uint,   n_uint);
    n_ulong  = __atomic_fetch_add_ulong(  &a_ulong,  n_ulong);
    n_ullong = __atomic_fetch_add_ullong( &a_ullong, n_ullong);
    
    //_____________________________________________________________________________________________________________________
    // Atomic Fetch Sub Functions
    
    n_char   = __atomic_fetch_sub_char(   &a_char,   n_char);
    n_short  = __atomic_fetch_sub_short(  &a_short,  n_short);
    n_int    = __atomic_fetch_sub_int(    &a_int,    n_int);
    n_long   = __atomic_fetch_sub_long(   &a_long,   n_long);
    n_llong  = __atomic_fetch_sub_llong(  &a_llong,  n_llong);
    n_uchar  = __atomic_fetch_sub_uchar(  &a_uchar,  n_uchar);
    n_ushort = __atomic_fetch_sub_ushort( &a_ushort, n_ushort);
    n_uint   = __atomic_fetch_sub_uint(   &a_uint,   n_uint);
    n_ulong  = __atomic_fetch_sub_ulong(  &a_ulong,  n_ulong);
    n_ullong = __atomic_fetch_sub_ullong( &a_ullong, n_ullong);
    
    //_____________________________________________________________________________________________________________________
    // Atomic Fetch Or Functions
    
    n_char   = __atomic_fetch_or_char(   &a_char,   n_char);
    n_short  = __atomic_fetch_or_short(  &a_short,  n_short);
    n_int    = __atomic_fetch_or_int(    &a_int,    n_int);
    n_long   = __atomic_fetch_or_long(   &a_long,   n_long);
    n_llong  = __atomic_fetch_or_llong(  &a_llong,  n_llong);
    n_uchar  = __atomic_fetch_or_uchar(  &a_uchar,  n_uchar);
    n_ushort = __atomic_fetch_or_ushort( &a_ushort, n_ushort);
    n_uint   = __atomic_fetch_or_uint(   &a_uint,   n_uint);
    n_ulong  = __atomic_fetch_or_ulong(  &a_ulong,  n_ulong);
    n_ullong = __atomic_fetch_or_ullong( &a_ullong, n_ullong);
    
    //_____________________________________________________________________________________________________________________
    // Atomic Fetch Xor Functions
    
    n_char   = __atomic_fetch_xor_char(   &a_char,   n_char);
    n_short  = __atomic_fetch_xor_short(  &a_short,  n_short);
    n_int    = __atomic_fetch_xor_int(    &a_int,    n_int);
    n_long   = __atomic_fetch_xor_long(   &a_long,   n_long);
    n_llong  = __atomic_fetch_xor_llong(  &a_llong,  n_llong);
    n_uchar  = __atomic_fetch_xor_uchar(  &a_uchar,  n_uchar);
    n_ushort = __atomic_fetch_xor_ushort( &a_ushort, n_ushort);
    n_uint   = __atomic_fetch_xor_uint(   &a_uint,   n_uint);
    n_ulong  = __atomic_fetch_xor_ulong(  &a_ulong,  n_ulong);
    n_ullong = __atomic_fetch_xor_ullong( &a_ullong, n_ullong);
    
    //_____________________________________________________________________________________________________________________
    // Atomic Fetch And Functions
    
    n_char   = __atomic_fetch_and_char(   &a_char,   n_char);
    n_short  = __atomic_fetch_and_short(  &a_short,  n_short);
    n_int    = __atomic_fetch_and_int(    &a_int,    n_int);
    n_long   = __atomic_fetch_and_long(   &a_long,   n_long);
    n_llong  = __atomic_fetch_and_llong(  &a_llong,  n_llong);
    n_uchar  = __atomic_fetch_and_uchar(  &a_uchar,  n_uchar);
    n_ushort = __atomic_fetch_and_ushort( &a_ushort, n_ushort);
    n_uint   = __atomic_fetch_and_uint(   &a_uint,   n_uint);
    n_ulong  = __atomic_fetch_and_ulong(  &a_ulong,  n_ulong);
    n_ullong = __atomic_fetch_and_ullong( &a_ullong, n_ullong);
    
}
