
#define kilo_bytes(a) ((a) * 1024ULL)
#define mega_bytes(a) ((kilo_bytes(a)) * 1024ULL)
#define giga_bytes(a) ((mega_bytes(a)) * 1024ULL)

#include "assert.h"

#pragma compilation_unit("core.c")

__declspec(dllimport) void *VirtualAlloc(void *Address, size_t Size, u32 AllocationType, u32 Protect);

struct memory_arena{
    u8 *base;
    u64 allocated;
    u64 committed;
    u64 reserved;
};

struct memory_arena create_memory_arena(u64 size_to_reserve){
    
    struct memory_arena ret = {0};
    ret.reserved = size_to_reserve;
    ret.base = VirtualAlloc(/*DesiredBase*/0, size_to_reserve, /*MEM_RESERVE*/0x00002000, /*PAGE_READWRITE*/4);
    assert(ret.base);
    
    return ret;
}

static __declspec(noinline) void grow_arena(struct memory_arena *arena, u64 grow_to){
    
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
    assert(grow_to <= arena->reserved);
    
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
    void *Success = VirtualAlloc(arena->base, grow_to, /*MEM_COMMIT*/0x00001000, /*PAGE_READWRITE*/4);
    assert(Success == arena->base);
    
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
    
    return arena->base + allocation_base;
}

u8 *arena_current(struct memory_arena *arena){
    return arena->base + arena->allocated;
}

//_____________________________________________________________________________________________________________________
// Temporary memory.

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


