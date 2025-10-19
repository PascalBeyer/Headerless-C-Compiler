
#define push_struct(arena, type) ((type *)memory_arena_allocate_bytes((arena), sizeof(type), _Alignof(type)))
#define push_array(arena, type, count) ((type *)memory_arena_allocate_bytes((arena), sizeof(type) * (count), _Alignof(type)))

#pragma compilation_unit("memory_arena.c")
