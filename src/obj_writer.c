
// 
// @cleanup: where should this live?
// 
enum CV_basic_type{
    CV_none = 0x0,
    CV_void = 0x3,
    
    CV_Bool = 0x30,
    
    CV_s8  = 0x68,
    CV_u8  = 0x69,
    CV_s16 = 0x72,
    CV_u16 = 0x73,
    CV_s32 = 0x74,
    CV_u32 = 0x75,
    CV_s64 = 0x76,
    CV_u64 = 0x77,
    
    CV_f32 = 0x40,
    CV_f64 = 0x41,
    
    
    //CV_s8_pointer = 0x0168,
    //CV_u8_pointer = 0x0169,
    
    // char *
    CV_s8_pointer = 0x0620,
    
    // unsigned char *
    CV_u8_pointer = 0x0670,
};

void push_f3f2f1_align(struct memory_arena *arena, u32 alignment){
    u8 *align = push_align(arena, alignment);
    u8 *end   = arena_current(arena);
    for(u8 *it = align; it < end; it++){
        *it = 0xf0 + (u8)(end - it);
    }
}

void push_unsigned_number_leaf(struct memory_arena *arena, u64 value){
    if(value < 0x8000){
        *push_struct(arena, u16) = (u16)value;
    }else if(value <= u16_max){
        *push_struct(arena, u16) = /*LF_USHORT*/0x8002;
        *push_struct(arena, u16) = (u16)value;
    }else if(value <= u32_max){
        *push_struct(arena, u16) = /*LF_ULONG*/0x8004;
        *(u32 *)push_data(arena, u8, sizeof(u32)) = (u32)value;
    }else{
        *push_struct(arena, u16) = /*LF_UQUADWORD*/0x800a;
        *(u64 *)push_data(arena, u8, sizeof(u64)) = value;
    }
}

void register_type(u32 *inout_type_index, struct memory_arena *arena, struct memory_arena *scratch, struct ast_type *initial_type){
    
    // @cleanup: This description did not end up being very good, redo it once everything is in place.
    // 
    // In particular explain pdb_type_index != 0 vs pdb_permanent vs pdb_temporary and is_compound_definition.
    // and also make sure all of these are really necessary now.
    // 
    // also maybe rename those.
    //     pdb_permanent = non_predeclaration emitted for structure/unions
    //     pdb_permanent = type_index != 0 for all other types
    //     pdb_temporary = we have seen this struct or union already.
    // 
    // The type index algorithm 
    // 
    // We have to emit types in linear order, meaning types can only reference
    // "earlier" types. This is achived by introducing predeclarations for unions 
    // and structures. 
    // 
    // 1) Because we have to linearize the types, the algorithm has to be recursive.
    // 2) Because we have to emit the types in the stack in "reverse" order from the 
    //    way we visit them, we have to go "back up the stack".
    // 3) Because we have to break struct/union cycles (e.g: struct asd { struct asd *next; };)
    //    we have to detect when we meet a struct or union for the second time.
    // 4) We want to try to emit as little type records as possible, hence we want to detect,
    //    when we have already seen a type.
    // 
    // Because of these constraints, we introduce two type flags: 
    //     'pdb_permanent' - meaning we already have a type index associated with this type.
    //     'pdb_temporary' - meaning its a structure/union type and we have already seen it once.
    // 
    // Now because we have to walk the stack up and down, and we detect whether we have seen
    // structure/union types twice, we have to push all child types to the stack.
    // This means we might push the same type twice:
    //    struct asd{
    //         struct asd2 a;
    //         struct asd2 b;
    //    };
    // in which case, when we get to the second member, it is already pdb_permanent and we have 
    // to skip it.
    // 
    
    if(initial_type->flags & TYPE_FLAG_pdb_permanent){
        // 
        // We have already registered this type.
        // Hence, we don't have to do anything.
        // 
        assert(initial_type->pdb_type_index != 0); // The first type index is 0x1000.
        return;
    }
    
    struct codeview_type_record_header{
        u16 length;
        u16 symbol_kind;
    } *current_type_record = null;
    u32 type_index_at = *inout_type_index;
    
    // Convenience macros to handle type index creation
#define begin_type_record(kind) type_index_at; { current_type_record = push_struct(arena, struct codeview_type_record_header); current_type_record->symbol_kind = kind; }
#define end_type_record() { type_index_at++; push_f3f2f1_align(arena, sizeof(u32)); current_type_record->length = to_u16(arena_current(arena) - (u8 *)&current_type_record->symbol_kind); }

    // 
    // We emit the type record data onto 'arena' and use 'scratch' as an _infinite_ array for the
    // type stack.
    // 
    struct temporary_memory temp = begin_temporary_memory(scratch);
    
    smm type_stack_at = 0;
    smm type_stack_size = 0x40;
    
    struct type_stack_node{
        struct ast_type *type;
        int is_compound_definition;
    } *type_stack = push_uninitialized_data(scratch, struct type_stack_node, type_stack_size);
    
    type_stack[type_stack_at++].type = initial_type;

    // Convenience macro to push stuff to the type stack.
#define push_type_to_stack(type_to_push) {                                         \
    if(type_stack_at == type_stack_size){                                          \
        assert((u8 *)(type_stack + type_stack_size) == arena_current(scratch));    \
        push_uninitialized_data(scratch, struct type_stack_node, type_stack_size); \
        type_stack_size *= 2;                                                      \
    }                                                                              \
    assert(type_to_push);                                                          \
    type_stack[type_stack_at].type = type_to_push;                                 \
    type_stack[type_stack_at].is_compound_definition = 0;                          \
    type_stack_at++;                                                               \
}
    
    while(type_stack_at > 0){
        // 
        // We perform a depth-first "search" over all of the types.
        // This ensures that we register subtypes _before_ their parent types.
        // Because we have to eventually set type-indices for all types, 
        // we have to visit each type "twice", once on the way down
        // and once on the way up.
        // 
        
        struct type_stack_node *current_type_stack_node = &type_stack[type_stack_at-1];
        struct ast_type *current_type = current_type_stack_node->type;
        
        if(current_type->flags & TYPE_FLAG_pdb_permanent){ 
            // 
            // If we already have an associated type index for this type, skip it.
            // As described above, this can happen for
            // struct asd{
            //     struct asd2 a;
            //     struct asd2 b;
            // };
            // 
            
            type_stack_at -= 1;
            continue;
        }
        
        u32 type_index;
        
        switch(current_type->kind){
            case AST_enum:{
                // 
                // The type indices for an enum are 
                //    1) LF_FIELDLIST's containing the enum members (LF_ENUMERATE).
                //    2) An LF_ENUM which is the actual enum referencing its fieldlists.
                // 
                struct ast_compound_type *ast_enum = (struct ast_compound_type *)current_type;
                
                u32 fieldlist_type_index = begin_type_record(/*LF_FIELDLIST*/0x1203);
                
                for(u32 member_index = 0; member_index < ast_enum->amount_of_members; member_index++){
                    struct compound_member *member = &ast_enum->members[member_index];
                    
                    u64 maximum_size_needed = 0;
                    maximum_size_needed += /*sizeof(LF_ENUMERATE)*/2;
                    maximum_size_needed += /*sizeof(attributes)*/2;
                    maximum_size_needed += /*sizeof(LF_LONG)*/2;
                    maximum_size_needed += /*sizeof(int)*/4;
                    maximum_size_needed += member->name->size;
                    maximum_size_needed += /*null-terminator*/1;
                    maximum_size_needed = (maximum_size_needed + 3) & ~3;
                    
                    // 
                    // Check if we have to emit another fieldlist, because we would overflow 
                    // the size of this one. The maximal size is (65536 - 2) as it has to be 
                    // equal to 2 modulo 4 and has to fit in a u16.
                    // 
                    if(maximum_size_needed > 65534 - (u64)(arena_current(arena) - (u8 *)current_type_record)){
                        
                        // 
                        // Just skip entries which are too long.
                        // @cleanup: Emit a warning.
                        // 
                        if(maximum_size_needed > 65534 - /*sizeof(symbol_kind)*/2) continue;
                        
                        end_type_record();
                        
                        // 
                        // Start a new fieldlist.
                        // 
                        u32 new_fieldlist_type_index = begin_type_record(/*LF_FIELDLIST*/0x1203);
                        
                        // 
                        // Link this new fieldlist to the last.
                        // 
                        *push_struct(arena, u16) = /*LF_INDEX*/0x1404;
                        *push_struct(arena, u16) = /*padding*/0;
                        *push_struct(arena, u32) = fieldlist_type_index;
                        
                        fieldlist_type_index = new_fieldlist_type_index;
                    }
                    
                    // 
                    // We should now fit in the fieldlist.
                    // 
                    *push_struct(arena, u16) = /*LF_ENUMERATE*/0x1502;
                    *push_struct(arena, u16) = /*attributes*/3;
                    *push_struct(arena, u16) = /*LF_LONG*/0x8003; // @cleanup: push_signed_number_leaf();
                    *(u32 *)push_data(arena, u8, sizeof(u32)) = (u32)member->enum_value;
                    push_zero_terminated_string_copy(arena, member->name->string);
                    push_f3f2f1_align(arena, sizeof(u32));
                }
                
                end_type_record();
                
                type_index = begin_type_record(/*LF_ENUM*/0x1507);
                
                *push_struct(arena, u16) = (u16)ast_enum->amount_of_members; // @note: This just gets truncated.
                *push_struct(arena, u16) = /*properties*/0;
                *push_struct(arena, u32) = /*underlying type*/CV_s32;
                *push_struct(arena, u32) = fieldlist_type_index;
                push_zero_terminated_string_copy(arena, ast_enum->identifier.string);
                
                end_type_record();
            }break;
            
            case AST_struct: case AST_union:{
                struct ast_compound_type *compound = (struct ast_compound_type *)current_type;
                
                // 
                // If this structure is already marked 'pdb_temporary' we have to emit
                // a predeclaration (or forward reference using pdb terminology). 
                // Because this is marked pdb_temporary, we know the definition if 
                // further up the stack. 
                // 
                if((current_type->flags & TYPE_FLAG_pdb_temporary) && !current_type_stack_node->is_compound_definition){
                    // 
                    // If we have not emitted a predeclaration for this type already, 
                    // do it now.
                    // 
                    if(current_type->pdb_type_index == 0){    
                        u16 lf_kind = current_type->kind == AST_struct ? /*LF_STRUCTURE*/0x1505 : /*LF_UNION*/0x1506;
                        u32 predeclaration_type_index = begin_type_record(lf_kind);
                        
                        *push_struct(arena, u16) = 0; // count (0 for forward ref)
                        *push_struct(arena, u16) = /*forward_ref*/(1 << 7);
                        *push_struct(arena, u32) = 0; // fieldlist (0 for forward ref)
                        
                        if(lf_kind == /*LF_STRUCTURE*/0x1505){
                            *push_struct(arena, u32) = 0; // derived
                            *push_struct(arena, u32) = 0; // vshape
                        }
                        *push_struct(arena, u16) = 0; // size (0 for forward ref)
                        
                        // @cleanup: this could overflow the header length field u16.
                        push_zero_terminated_string_copy(arena, compound->identifier.string);
                        end_type_record();
                        current_type->pdb_type_index = predeclaration_type_index;
                    }
                    
                    type_stack_at -= 1;
                    continue;
                }
                
                current_type_stack_node->is_compound_definition = 1;
                
                // 
                // We can infer, whether this is the way up, or the way down by checking, 
                // if this type has been flagged already. If it has either flag we are on the 
                // way up the type stack (we have seen this type already), otherwise we 
                // are at this type for the first time.
                // 
                if(!(current_type->flags & TYPE_FLAG_pdb_temporary)){
                    current_type->flags |= TYPE_FLAG_pdb_temporary;
                    
                    // 
                    // If we are on the way down, push all subtypes to the type-stack.
                    // 
                    
                    int should_recurse = false;
                    for(u32 member_index = 0; member_index < compound->amount_of_members; member_index++){
                        struct ast_type *type = compound->members[member_index].type;
                        
                        if(type->pdb_type_index != 0){
                            // 
                            // We already know the type-index for this type, 
                            // we don't need to recurse into it.
                            // 
                        }else{
                            push_type_to_stack(type);
                            
                            should_recurse = true;
                        }
                    }
                    
                    if(should_recurse) continue;
                }
                
                // 
                // We are ready. All fields should now have an associated type index.
                // 
                
                u32 fieldlist_type_index = begin_type_record(/*LF_FIELDLIST*/0x1203);
                
                for(u32 member_index = 0; member_index < compound->amount_of_members; member_index++){
                    struct compound_member *member = &compound->members[member_index];
                    if(member->name == globals.invalid_identifier_token) continue;
                    
                    // @note: It does not have to be pdb_permanent, as it could be a predecl.
                    //        This can happen in _weird_ scenarios, where we follow pointers
                    //        and then a struct contains a struct which is lower on the stack.
                    assert(member->type->pdb_type_index); 
                    
                    u64 maximum_size_needed = 0;
                    maximum_size_needed += /*sizeof(LF_MEMBER)*/2;
                    maximum_size_needed += /*sizeof(attributes)*/2;
                    maximum_size_needed += /*sizeof(type_index)*/4;
                    maximum_size_needed += /*sizeof(LF_ULONGLONG)*/2;
                    maximum_size_needed += /*sizeof(u64)*/8;
                    maximum_size_needed += member->name->size;
                    maximum_size_needed = (maximum_size_needed + 3) & ~3;
                    
                    // 
                    // Check if we have to emit another fieldlist, because we would overflow
                    // the length field for this one. Also see the enum case.
                    // 
                    if(maximum_size_needed > 65534 - (u64)(arena_current(arena) - (u8 *)current_type_record)){
                        
                        // 
                        // Just skip entries which are too long.
                        // 
                        if(maximum_size_needed > 65534 - /*sizeof(symbol_kind)*/2) continue;
                        
                        end_type_record();
                        
                        u32 new_fieldlist_type_index = begin_type_record(/*LF_FIELDLIST*/0x1203);
                        
                        // 
                        // Link this new fieldlist to the last.
                        // 
                        *push_struct(arena, u16) = /*LF_INDEX*/0x1404;
                        *push_struct(arena, u16) = /*padding*/0;
                        *push_struct(arena, u32) = fieldlist_type_index;
                        
                        fieldlist_type_index = new_fieldlist_type_index;
                    }
                    
                    *push_struct(arena, u16) = /*LF_MEMBER*/0x150d;
                    *push_struct(arena, u16) = /*attributes*/3;
                    *push_struct(arena, u32) = member->type->pdb_type_index;
                    push_unsigned_number_leaf(arena, member->offset_in_type);
                    push_zero_terminated_string_copy(arena, member->name->string);
                    push_f3f2f1_align(arena, sizeof(u32));
                }
                
                end_type_record();
                
                u16 lf_kind = current_type->kind == AST_struct ? /*LF_STRUCTURE*/0x1505 : /*LF_UNION*/0x1506;
                type_index = begin_type_record(lf_kind);
                
                *push_struct(arena, u16) = (u16)compound->amount_of_members;
                *push_struct(arena, u16) = /*properties*/0;
                *push_struct(arena, u32) = fieldlist_type_index;
                if(lf_kind == /*LF_STRUCTURE*/0x1505){
                    *push_struct(arena, u32) = 0; // derived
                    *push_struct(arena, u32) = 0; // vshape
                }
                push_unsigned_number_leaf(arena, compound->base.size);
                push_zero_terminated_string_copy(arena, compound->identifier.string);
                
                end_type_record();
            }break;
            
            case AST_pointer_type:{
                struct ast_pointer_type *pointer = (struct ast_pointer_type *)current_type;
                
                // 
                // In the case of:
                // 
                //     struct unresolved *pointer;
                //     
                // Where we did not dereference 'pointer', but 'unresolved' gets defined _later_,
                // this pointer is still pointing to an unresolved type.
                // If this is the case we try to patch it here.
                // 
                maybe_resolve_unresolved_type(&pointer->pointer_to);
                
                struct ast_type *pointer_to = pointer->pointer_to;
                
                // 
                // If the type we are pointing to does not yet have an associated type index
                // we need to recurse to emit the typeinfo for it.
                // 
                if(pointer_to->pdb_type_index == 0){
                    push_type_to_stack(pointer_to);
                    continue;
                }
                
                type_index = begin_type_record(/*LF_POINTER*/0x1002);
                
                *push_struct(arena, u32) = pointer_to->pdb_type_index;
                *push_struct(arena, u32) = /*modifiers (size = 8, kind = __ptr64)*/0x1000c;
                
                end_type_record();
            }break;
            
            case AST_array_type:{
                struct ast_array_type *array_type = (struct ast_array_type *)current_type;
                struct ast_type *element_type = array_type->element_type;
                
                // 
                // If the element type to does not yet have an associated type index
                // we need to recurse to emit the typeinfo for it.
                // 
                if(element_type->pdb_type_index == 0){
                    push_type_to_stack(element_type);
                    continue;
                }
                
                type_index = begin_type_record(/*LF_ARRAY*/0x1503);
                
                *push_struct(arena, u32) = element_type->pdb_type_index;
                *push_struct(arena, u32) = CV_s64;
                push_unsigned_number_leaf(arena, array_type->base.size);
                *push_struct(arena, u8) = 0; // Empty "name", not sure why this is here.
                
                end_type_record();
            }break;
            
            case AST_function_type:{
                struct ast_function_type *function_type = (struct ast_function_type *)current_type;
                
                // @note: see comment in the pointer type case.
                maybe_resolve_unresolved_type(&function_type->return_type);
                
                struct ast_type *return_type = function_type->return_type;
                
                // Make sure the 'return_type' has an associated type index.
                if(return_type->pdb_type_index == 0){
                    push_type_to_stack(return_type);
                    continue;
                }
                
                // Make sure all of the arguments have an associated type index.
                b32 should_recurse = false;
                for_ast_list(function_type->argument_list){
                    struct ast_declaration *argument = (struct ast_declaration *)it->value;
                    struct ast_type *argument_type = argument->type;
                    
                    if(argument_type->pdb_type_index == 0){
                        push_type_to_stack(argument_type);
                        should_recurse = true;
                    }
                }
                
                if(should_recurse) continue;
                
                u32 arglist_type_index = begin_type_record(/*LF_ARGLIST*/0x1201);
                *push_struct(arena, u32) = (u32)min_of(function_type->argument_list.count, 16383);
                for_ast_list(function_type->argument_list){
                    struct ast_declaration *argument = (struct ast_declaration *)it->value;
                    struct ast_type *argument_type = argument->type;
                    
                    if((u64)(arena_current(arena) - (u8 *)current_type_record) >= 65536 - 4){
                        // We sadly cannot make sure there is enough room for arguments in the LF_ARGLIST,
                        // as it seems it does not support the LF_INDEX thing. 
                        // When compiling a function with 16384 arguments (amounting to 4 * 16384 = 65536 bytes)
                        // clang-cl crashes and cl gives the following error message:
                        // 
                        //     test3.c(16390): fatal error C1067: compiler limit: 64K limit on size of a type record has been exceeded
                        // 
                        // for now I will just print a warning here and skip emitting debug information
                        // for the rest of the arguments.
                        // 
                        // @cleanup: actually print the warinings for pdb overflows (also for overly long names)
                        break;
                    }
                    
                    *push_struct(arena, u32) = argument_type->pdb_type_index;
                }
                end_type_record();
                
                type_index = begin_type_record(/*LF_PROCEDURE*/0x1008);
                
                *push_struct(arena, u32) = return_type->pdb_type_index;
                *push_struct(arena, u8)  = /*calling convention*/0;  // This is always 0 for some reason.
                *push_struct(arena, u8)  = /*function attributes*/0; // This is just for c++ crazyness.
                *push_struct(arena, u16) = (u16)function_type->argument_list.count; // @cleanup: should we also truncate this to 16383?
                *push_struct(arena, u32) = arglist_type_index;
                
                end_type_record();
            }break;
            
            case AST_bitfield_type:{
                struct ast_bitfield_type *bitfield = (struct ast_bitfield_type *)current_type;
                struct ast_type *base_type = bitfield->base_type;
                assert(base_type->flags & TYPE_FLAG_pdb_permanent); // The base type should be a basic type and thus permanent.
                
                type_index = begin_type_record(/*LF_BITFIELD*/0x1205);
                
                *push_struct(arena, u32) = base_type->pdb_type_index;
                *push_struct(arena,  s8) = (s8)bitfield->width;
                *push_struct(arena,  s8) = (s8)bitfield->bit_index;
                
                end_type_record();
            }break;
            
            case AST_unresolved_type:{
                struct ast_unresolved_type *unresolved = (struct ast_unresolved_type *)current_type;
                
                // 
                // We can only get here from a pointer for a type which is never defined.
                // Therefore, we just need to emit a predeclaration.
                // 
                
                u16 lf_kind = unresolved->kind == AST_struct ? /*LF_STRUCTURE*/0x1505 : /*LF_UNION*/0x1506;
                type_index = begin_type_record(lf_kind);
                
                *push_struct(arena, u16) = 0; // count (0 for forward ref)
                *push_struct(arena, u16) = /*forward_ref*/(1 << 7);
                *push_struct(arena, u32) = 0; // fieldlist (0 for forward ref)
                
                if(lf_kind == /*LF_STRUCTURE*/0x1505){
                    *push_struct(arena, u32) = 0; // derived
                    *push_struct(arena, u32) = 0; // vshape
                }
                *push_struct(arena, u16) = 0; // size (0 for forward ref)
                
                struct string type_name = token_get_string(unresolved->base.token);
                
                // @cleanup: this could overflow the header length field u16.
                push_zero_terminated_string_copy(arena, type_name);
                
                end_type_record();
                
            }break;
            
            default:{
                not_implemented;
            }break;
        }
        
        current_type->flags |= TYPE_FLAG_pdb_permanent;
        current_type->flags &= ~TYPE_FLAG_pdb_temporary;
        current_type->pdb_type_index = type_index;
        
        type_stack_at -= 1;
    }
    
    *inout_type_index = type_index_at;
    
    end_temporary_memory(temp);
}

struct debug_symbols_relocation_info{
    u32 destination_offset;
    struct ast_declaration *source_declaration;
};


void codeview_emit_debug_information_for_function__recursive(struct ast_function *function, struct memory_arena *arena, struct ast *ast, struct memory_arena *relocation_arena, u8 *debug_symbols_base){
    assert(ast->byte_offset_in_function >= 0);
    
    switch(ast->kind){
        case AST_scope:{
            struct ast_scope *scope = (struct ast_scope *)ast;
            
            if(scope->amount_of_declarations){
                
                if(function->scope != ast){
                    struct codeview_block32{
                        u16 length;
                        u16 kind;
                        u32 pointer_to_parent; // filled in by the linker
                        u32 pointer_to_end;    // filled in by the linker
                        u32 scope_size;
                        u32 offset_in_section;
                        u16 section;
                    } *block = push_struct(arena, struct codeview_block32);
                    block->length = sizeof(*block) - 2;
                    block->kind   = /*S_BLOCK32*/0x1103;
                    block->scope_size = scope->scope_end_byte_offset_in_function - scope->base.byte_offset_in_function;
                    
                    block->offset_in_section = scope->base.byte_offset_in_function; // relocated by relocation.
                    block->section = 0; // filled in by relocation.
                    
                    struct debug_symbols_relocation_info *relocation = push_struct(relocation_arena, struct debug_symbols_relocation_info);
                    relocation->destination_offset = (u32)((u8 *)&block->offset_in_section - debug_symbols_base);
                    relocation->source_declaration = &function->as_decl;
                }
                
                for(smm declaration_index = 0; declaration_index < scope->current_max_amount_of_declarations; declaration_index++){
                    struct ast_declaration *decl = scope->declarations[declaration_index];
                    if(!decl) continue;
                    
                    if(decl->base.kind == AST_typedef){
                        // @cleanup: S_UDT
                        continue;
                    }
                    
                    if(decl->base.kind == AST_function){
                        // @cleanup: What should we do here?
                        continue;
                    }
                    
                    if(decl->flags & DECLARATION_FLAGS_is_local_persist){
                        // @cleanup: S_LDATA32
                        continue;
                    }
                    
                    if(decl->flags & DECLARATION_FLAGS_is_enum_member){
                        // @cleanup: S_CONSTANT
                        continue;
                    }
                    
                    // 
                    // It's a "normal" declaration.
                    // 
                    
                    struct codeview_regrel32{
                        u16 length;
                        u16 kind;
                        u32 offset_of_register;
                        u32 type_index;
                        u16 register_index;
                        u8 identifier[];
                    } *regrel = push_struct_(arena, offset_in_type(struct codeview_regrel32, identifier) + decl->identifier->string.size + 1, /*alignment*/4);
                    
                    regrel->kind = /*S_REGREL32*/0x1111;
                    regrel->offset_of_register = (s32)(-decl->offset_on_stack);
                    regrel->type_index = decl->type->pdb_type_index;
                    regrel->register_index = /*CV_AMD64_RBP*/334;
                    memcpy(regrel->identifier, decl->identifier->string.data, decl->identifier->string.size);
                    regrel->identifier[decl->identifier->string.size] = 0;
                    
                    push_f3f2f1_align(arena, sizeof(u32));
                    regrel->length = (u16)(arena_current(arena) - (u8 *)&regrel->kind);
                }
            }
            
            for_ast_list(scope->statement_list){
                codeview_emit_debug_information_for_function__recursive(function, arena, it->value, relocation_arena, debug_symbols_base);
            }
            
            if(scope->amount_of_declarations && function->scope != ast){
                *push_struct(arena, u16) = /*length*/2;
                *push_struct(arena, u16) = /*S_END*/6;
            }
            
        }break;
        
        case AST_if:{
            struct ast_if *ast_if = cast(struct ast_if *)ast;
            codeview_emit_debug_information_for_function__recursive(function, arena, ast_if->statement, relocation_arena, debug_symbols_base);
            if(ast_if->else_statement){
                codeview_emit_debug_information_for_function__recursive(function, arena, ast_if->else_statement, relocation_arena, debug_symbols_base);
            }
        }break;
        
        case AST_do_while: case AST_for:{
            struct ast_for *ast_for = (struct ast_for *)ast;
            
            if(ast_for->scope_for_decl->amount_of_declarations){
                // 
                // @hack: The statement_list of the 'ast_for->scope_for_decl' is actually empty, 
                //        not containing the 'ast_for->body'. Hence, we have introduce the S_BLOCK32
                //        then somehow the 'ast_for->body' and finally end with the 'S_END'.
                //        We do this is the most hacky way, by first emitting the code for the 'scope_for_decl'
                //        this will end for an 'S_END', we "pop" this 'S_END' and emit the code for the 'ast_for->body'.
                //        Finally, we re-emit the 'S_END'.
                // 
                
                codeview_emit_debug_information_for_function__recursive(function, arena, &ast_for->scope_for_decl->base, relocation_arena, debug_symbols_base);
                arena->current -= 4;
                codeview_emit_debug_information_for_function__recursive(function, arena, ast_for->body, relocation_arena, debug_symbols_base);
                *push_struct(arena, u16) = /*length*/2;
                *push_struct(arena, u16) = /*S_END*/6;
                
            }else{
                // 
                // We don't need a scope for the body.
                // 
                codeview_emit_debug_information_for_function__recursive(function, arena, ast_for->body, relocation_arena, debug_symbols_base);
            }
        }break;
        
        case AST_switch:{
            struct ast_switch *ast_switch = cast(struct ast_switch *)ast;
            codeview_emit_debug_information_for_function__recursive(function, arena, ast_switch->statement, relocation_arena, debug_symbols_base);
        }break;
        case AST_label:{
            struct ast_label *ast_label = cast(struct ast_label *)ast;
            if(ast_label->statement) codeview_emit_debug_information_for_function__recursive(function, arena, ast_label->statement, relocation_arena, debug_symbols_base);
        }break;
        case AST_case:{
            struct ast_case *ast_case = cast(struct ast_case *)ast;
            if(ast_case->statement) codeview_emit_debug_information_for_function__recursive(function, arena, ast_case->statement, relocation_arena, debug_symbols_base);
        }break;
        default: break;
    }
    
}

void print_obj(struct memory_arena *arena, struct memory_arena *scratch){
    
    struct memory_arena stack_arena = create_memory_arena(giga_bytes(8), 2.0f, kilo_bytes(10));
    
    // 
    // Gather all symbols. There are a couple of different symbols:
    //     
    //     1) Defined functions (static/external)
    //     2) Undefined functions (external)
    //     3) Dll imports (functions/variables)
    //     4) extern variables
    //     5) static variables
    //     6) auto variables
    //     7) dll-exports (these are like defined variables/functions, 
    //        but we have to put a '.drectve' section.
    //     8) local-persist variables. // @cleanup: test other things at local scope.
    // 
    
    struct ast_list defined_functions  = zero_struct;
    struct ast_list external_functions = zero_struct;
    
    struct ast_list external_variables  = zero_struct;
    struct ast_list automatic_variables = zero_struct;
    struct ast_list zero_initialized_statics = zero_struct;
    struct ast_list defined_variables   = zero_struct;
    
    struct string_list directives = zero_struct;
    string_list_postfix(&directives, scratch, string("/DEFAULTLIB:\"LIBCMT\" /DEFAULTLIB:\"OLDNAMES\" /STACK:0x100000,0x100000 "));
    
    for(struct library_node *library_node = globals.libraries.first; library_node; library_node = library_node->next){
        struct string file_name = strip_file_path(library_node->path);
        
        string_list_postfix(&directives, scratch, string("/DEFAULTLIB:\""));
        string_list_postfix(&directives, scratch, file_name);
        string_list_postfix(&directives, scratch, string("\" "));
    }
    
    // :DeclarationTableLoop
    // 
    // @note: This declaration only exist to facilitate loops that want to iterate all declaration tables.
    //        Both the global one (in this `dummy_global_declaration_unit`) and the local ones in
    //        compilation_unit->static_declaration_table.
    struct compilation_unit dummy_global_compilation_unit = {
        .next = globals.compilation_units.first, 
        .static_declaration_table = globals.global_declarations,
    };
    
    for(struct compilation_unit *compilation_unit = &dummy_global_compilation_unit; compilation_unit; compilation_unit = compilation_unit->next){
        
        struct ast_table *table = &compilation_unit->static_declaration_table;
        
        for(u64 table_index = 0; table_index < table->capacity; table_index++){
            struct ast *ast = table->nodes[table_index].ast;
            if(!ast) continue;
            
            if(ast->kind == AST_declaration){
                struct ast_declaration *decl = (struct ast_declaration *)ast;
                
                if(decl->flags & DECLARATION_FLAGS_is_enum_member) continue;
                
                assert(!(decl->flags & DECLARATION_FLAGS_is_local_persist));
                
                if(decl->flags & DECLARATION_FLAGS_is_static){
                    if(decl->times_referenced == 0) continue;
                    
                    if(decl->assign_expr){
                        ast_list_append(&defined_variables, scratch, &decl->base);
                    }else{
                        ast_list_append(&zero_initialized_statics, scratch, &decl->base);
                    }
                    continue;
                }
                
                if(decl->flags & DECLARATION_FLAGS_is_dllexport){
                    string_list_postfix(&directives, scratch, push_format_string(scratch, "/EXPORT:%.*s,DATA ", decl->identifier->size, decl->identifier->data));
                    
                    ast_list_append(&defined_variables, scratch, &decl->base);
                    continue;
                }
                
                if(decl->assign_expr){
                    if(decl->flags & DECLARATION_FLAGS_is_selectany){
                        print("WARNING: Variable '%.*s' is declarated __declspec(selectany). This is currently unsupported for .obj-files. Ignoring it...\n", decl->identifier->size, decl->identifier->data);
                    }
                    
                    ast_list_append(&defined_variables, scratch, &decl->base);
                    continue;
                }
                
                if(decl->flags & DECLARATION_FLAGS_is_extern){
                    if(decl->times_referenced == 0) continue;
                    
                    ast_list_append(&external_variables, scratch, &decl->base);
                    continue;
                }
                
                if(decl->flags & DECLARATION_FLAGS_is_dllimport){
                    if(decl->times_referenced == 0) continue;
                    
                    ast_list_append(&external_variables, scratch, &decl->base);
                    continue;
                }
                
                ast_list_append(&automatic_variables, scratch, &decl->base);
                
            }else if(ast->kind == AST_function){
                struct ast_function *function = (struct ast_function *)ast;
                
                if(!(function->as_decl.flags & DECLARATION_FLAGS_is_function_that_is_reachable_from_entry)) continue;
                
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_intrinsic)  continue;
                if(function->type->flags & FUNCTION_TYPE_FLAGS_is_inline_asm) continue;
                
                if(function->scope){
                    if(function->as_decl.flags & DECLARATION_FLAGS_is_dllexport){
                        string_list_postfix(&directives, scratch, push_format_string(scratch, "/EXPORT:%.*s ", function->identifier->size, function->identifier->data));
                    }
                    
                    ast_list_append(&defined_functions, scratch, &function->base);
                    
                    for_ast_list(function->static_variables){
                        ast_list_append(&defined_variables, scratch, it->value);
                    }
                    continue;
                }
                
                ast_list_append(&external_functions, scratch, &function->base);
            }else{
                assert(ast->kind == AST_typedef);
            }
        }
    }
    
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        // 
        // Append the declarations for 'global_struct_and_array_literals' to the 'initialized_declarations'.
        // 
        
        for_ast_list(thread_context->global_struct_and_array_literals){
            struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)it->value;
            
            ast_list_append(&defined_variables, arena, &compound_literal->decl->base);
        }
    }
    
    
    // 
    // .OBJ layout:
    // 
    // The layout of an coff-object is very similar to the layout an executable.
    // Its fundamentally the same file format.
    // It starts out with the COFF file header, does not contain an "optional" header
    // and then contains the section headers.
    // 
    push_align(arena, 0x1000);
    u8 *obj_base_address = arena_current(arena);
    
    struct coff_file_header{
        // 
        // The type of machine this object is intended for. In our case 0x8664.
        // 
        u16 machine;
        
        // 
        // The number of sections contained in the object file.
        // 
        u16 amount_of_sections;
        
        // 
        // The number of sections since 00:00 January 1st 1970,
        // when the object file was created.
        // 
        u32 time_date_stamp;
        
        // 
        // The file offset of the COFF symbol table if present.
        // 
        u32 pointer_to_symbol_table;
        
        // 
        // The amount of symbols in the COFF symbol table.
        // 
        u32 amount_of_symbols;
        
        // 
        // Size of the optional header, should be zero for object files.
        // As they don't have an optional header.
        // 
        u16 size_of_optional_header;
        
        // 
        // Characteristics of the file.
        // Seems to be zero for object files.
        // 
        u16 file_characteristics;
    } *coff_file_header = push_struct(arena, struct coff_file_header);
    
    // @note: everything else is zero for now.
    coff_file_header->machine = 0x8664;
    
    struct coff_section_header{
        // 
        // The name of the section.
        // The "$" has a special interpretation in section names in object files.
        // The $ and everything after it get discarded and the charcters after determine 
        // the sorting of the sections.
        // 
        u8 name[8];
        
        // 
        // Total size when the section is loaded into memory.
        // This field should be zero for object files.
        // 
        u32 virtual_size;
        
        // 
        // For executable images, the relative virtual address of the section.
        // For object files, compilers should set this to zero.
        // 
        u32 virtual_address;
        
        // 
        // The size of the section (for object files) or the size of initialized data 
        // on disk (for image files).
        // 
        u32 size_of_raw_data;
        
        // 
        // The offset of the data within the file.
        // For object files, this value should be 4-byte aligned.
        // 
        u32 pointer_to_raw_data;
        
        // 
        // The offset of the relocation entries for the section.
        // Should be zero for executable images.
        // 
        u32 pointer_to_relocations;
        
        // 
        // COFF debugging information is deprecated.
        // 
        u32 pointer_to_line_numbers;
        
        // 
        // The number of relocation entries for the section.
        // 
        u16 number_of_relocations;
        
        // 
        // COFF debugging information is deprecated.
        // 
        u16 number_of_line_numbers;
        
        // 
        // The flags that describe the characteristics for the section.
        // 
        u32 characteristics;
        
    } *section_headers = push_data(arena, struct coff_section_header, 10);
    
    u32 section_header_count = 0;
    
    struct coff_section_header *drectve = null;
    struct coff_section_header *text  = null;
    struct coff_section_header *data  = null;
    struct coff_section_header *rdata = null;
    struct coff_section_header *bss   = null;
    struct coff_section_header *xdata = null;
    struct coff_section_header *pdata = null;
    struct coff_section_header *debug_symbols = null;
    struct coff_section_header *debug_types   = null;
    
    {
        drectve = section_headers + section_header_count++;
        memcpy(drectve->name, ".drectve", sizeof(".drectve") - 1);
        
        u8 *directive_base = arena_current(arena);
        
        struct string string = string_list_flatten(directives, arena);
        string.data[string.size] = ' '; // @note: nuke the null terminator.
        
        drectve->pointer_to_raw_data = (u32)(directive_base - obj_base_address);
        drectve->size_of_raw_data = (u32)(arena_current(arena) - directive_base);
        drectve->characteristics = /*1-Byte align*/0x00100000 | /*Info*/0x00000200 | /*remove*/0x00000800;
    }
    
    if(defined_functions.count){
        text = section_headers + section_header_count++;
        
        push_align(arena, 16); // for convenience in emitting we align it to 16.
        
        u8 *text_base = arena_current(arena);
        
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            
            smm function_size = function->size_of_prolog + function->byte_size_without_prolog;
            
            u8 *memory_for_function = push_uninitialized_data(arena, u8, function_size);
            
            u8 *at = memory_for_function;
            memcpy(at, function->base_of_prolog, function->size_of_prolog);
            at += function->size_of_prolog;
            
            memcpy(at, function->base_of_main_function, function->byte_size_without_prolog);
            function->base_of_main_function = at;
            at += function->byte_size_without_prolog;
            
            push_align_initialized_to_specific_value(arena, 16, 0xcc);
            
            function->offset_in_text_section   = memory_for_function - text_base;
            function->memory_location          = memory_for_function;
            function->relative_virtual_address = memory_for_function - text_base;
        }
        
        memcpy(text->name, ".text", sizeof(".text"));
        text->pointer_to_raw_data = (u32)(text_base - obj_base_address);
        text->size_of_raw_data    = (u32)(arena_current(arena) - text_base);
        text->characteristics     = /*CNT_CODE*/0x00000020 | /*ALIGN_16BYTES*/0x00500000 | /*MEM_READ*/0x40000000 | /*MEM_EXECUTE*/0x20000000;
    }
    
    if(defined_functions.count){
        pdata = section_headers + section_header_count++;
        
        u8 *pdata_base = arena_current(arena);
        
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            
            struct coff_runtime_function{
                u32 begin_address;
                u32 end_address;
                u32 unwind;
            } *runtime_function = push_struct(arena, struct coff_runtime_function);
            
            // These get "fixed-up" by 3 Relocations.
            runtime_function->begin_address = 0; 
            runtime_function->end_address   = (u32)function->byte_size;
            runtime_function->unwind        = 0;
        }
        
        memcpy(pdata->name, ".pdata", sizeof(".pdata"));
        pdata->pointer_to_raw_data = (u32)(pdata_base - obj_base_address);
        pdata->size_of_raw_data    = (u32)(arena_current(arena) - pdata_base);
        pdata->characteristics     = /*INITIALIZED_DATA*/0x00000040 | /*4-byte align*/0x00300000 | /*MEM_READ*/0x40000000;
    }
    
    if(defined_functions.count){
        xdata = section_headers + section_header_count++;
        
        u8 *xdata_base = arena_current(arena);
        
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            
            struct unwind_info{
                u8 version : 3;
                u8 flags   : 5;
                u8 size_of_prolog;
                u8 count_of_codes;
                u8 frame_register : 4;
                u8 frame_offset   : 4;
                
                struct unwind_code{
                    u8 offset_in_prolog;
                    u8 operation_code : 4;
                    u8 operation_info : 4;
                } codes[];
                
            } *unwind_info = push_struct(arena, struct unwind_info);
            
            unwind_info->version = 1;
            unwind_info->flags   = 0;
            unwind_info->size_of_prolog = (u8)(function->size_of_prolog);
            unwind_info->count_of_codes = 0;
            unwind_info->frame_register = REGISTER_BP;
            
            smm stack_space_needed = function->stack_space_needed;
            
            struct unwind_code *stack_allocation_code = push_struct(arena, struct unwind_code);
            stack_allocation_code->offset_in_prolog = (u8)function->rsp_subtract_offset;
            
            if(8 <= stack_space_needed && stack_space_needed <= 128){
                // "Allocate a small-sized area on the stack. The size of the allocation 
                //  is the operation info field times eight plus eight allowing allocations
                //  from 8 to 128 bytes."
                
                stack_allocation_code->operation_code = /*UWOP_ALLOC_SMALL*/2;
                stack_allocation_code->operation_info = (u8)((stack_space_needed - 8)/8);
            }else if(stack_space_needed <= 512 * 1024 - 8){
                // "Allocate a large-sized area on the stack. There are two forms.
                //  If the operation info equals 0, then the size of the allocation 
                //  divided by 8 is recorded in the next slot, allowing allocation 
                //  up to 512k - 8."
                
                stack_allocation_code->operation_code = /*UWOP_ALLOC_LARGE*/1;
                stack_allocation_code->operation_info = 0;
                *push_struct(arena, u16) = (u16)(stack_space_needed/8);
            }else{
                // "If the operation info equals 1, then the unscaled size of the alloation
                //  is recorded in the next two slots in little-endian format, allowing
                //  allocations up to 4GiB - 8."
                assert(stack_space_needed < giga_bytes(4) - 8);
                
                stack_allocation_code->operation_code = /*UWOP_ALLOC_LARGE*/1;
                stack_allocation_code->operation_info = 1;
                *push_struct(arena, u32) = (u32)stack_space_needed;
            }
            
            u8 amount_of_pushed_registers = (u8)__popcnt(function->pushed_register_mask);
            
            struct unwind_code *frame_pointer_code = push_struct(arena, struct unwind_code);
            frame_pointer_code->offset_in_prolog = (u8)(amount_of_pushed_registers + /*mov rbp, rsp*/3);
            frame_pointer_code->operation_code   = /*UWOP_SET_FPREG*/3;
            
            for(u8 register_index = 0, offset_in_prolog = amount_of_pushed_registers; register_index < 16; register_index++){
                if(!(function->pushed_register_mask & (1u << register_index))) continue;
                
                struct unwind_code *pushed_register_code = push_struct(arena, struct unwind_code);
                pushed_register_code->offset_in_prolog = offset_in_prolog--;
                pushed_register_code->operation_code = /*UWOP_PUSH_NONVOL*/0;
                pushed_register_code->operation_info = register_index;
            }
            
            unwind_info->count_of_codes = (u8)((struct unwind_code *)arena_current(arena) - unwind_info->codes);
            
            if(unwind_info->count_of_codes & 1){
                // "The UNWIND_INFO structure must be DWORD aligned in memory."
                push_struct(arena, u16);
            }
        }
        
        memcpy(xdata->name, ".xdata", sizeof(".xdata"));
        xdata->pointer_to_raw_data = (u32)(xdata_base - obj_base_address);
        xdata->size_of_raw_data    = (u32)(arena_current(arena) - xdata_base);
        xdata->characteristics     = /*INITIALIZED_DATA*/0x00000040 | /*4-byte align*/0x00300000 | /*MEM_READ*/0x40000000;
    }
    
    if(zero_initialized_statics.count){
        bss = section_headers + section_header_count++;
        
        smm size = 0;
        for_ast_list(zero_initialized_statics){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            
            smm alignment = get_declaration_alignment(decl);
            smm decl_size = get_declaration_size(decl);
            
            size = align_up(size, alignment);
            decl->relative_virtual_address = size;
            size += decl_size;
        }
        
        memcpy(bss, ".bss", sizeof(".bss"));
        bss->size_of_raw_data = (u32)size;
        bss->characteristics  = /*UNINITIALIZED_DATA*/0x00000080 | /*ALIGN_16BYTES*/0x00500000 | /*MEM_READ*/0x40000000 | /*MEM_WRITE*/0x80000000;
    }
    
    // 
    // Retain information to be able to emit symbols for these things.
    // 
    struct string_symbol{
        struct string_symbol *next;
        u32 string_index;
        u32 offset_in_section;
    };
    
    struct{
        struct string_symbol *first;
        struct string_symbol *last;
    } string_symbols = zero_struct;
    
    u32 float_start_offset = 0;
    u32 float_end_offset = 0;
    
    u32 double_start_offset = 0;
    u32 double_end_offset = 0;
    
    {
        // .rdata 
        rdata = section_headers + section_header_count++;
        
        push_zero_align(arena, 16);
        u8 *rdata_base = arena_current(arena);
        
        struct{
            struct ast_string_literal *literals;
            smm amount_of_literals;
        } string_literals_by_size[5] = zero_struct; // only index 1, 2, 4 are used.
        
        for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
            struct context *thread_context = globals.thread_infos[thread_index].context;
            for(struct ast_string_literal *lit = thread_context->string_literals.first; lit; ){
                struct ast_string_literal *next = lit->next;
                
                // :string_kind_is_element_size
                lit->next = string_literals_by_size[lit->string_kind].literals;
                string_literals_by_size[lit->string_kind].literals = lit;
                string_literals_by_size[lit->string_kind].amount_of_literals += 1;
                
                lit = next;
            }
        }
        
        smm string_index = 0;
        
        for(int element_size = 1; element_size < 5; element_size <<= 1){
            
            struct string_and_index{
                struct string string;
                u32 string_index;
            };
            
            smm amount_of_strings = string_literals_by_size[element_size].amount_of_literals;
            
            smm capacity = u64_round_up_to_next_power_of_two((u64)(1.5 * amount_of_strings));
            struct string_and_index *string_table = push_data(scratch, struct string_and_index, capacity);
            
            push_zero_align(arena, element_size);
            
            for(struct ast_string_literal *lit = string_literals_by_size[element_size].literals; lit; lit = lit->next){
                
#if defined(_Debug)
                struct ast_array_type *array = (struct ast_array_type *)lit->base.resolved_type;
                assert(array->element_type->size == element_size);
#endif
                
                struct string string_literal = lit->value;
                u64 hash = string_djb2_hash(string_literal);
                
                for(smm table_index = 0; table_index < capacity; table_index++){
                    smm index = (hash + table_index) & (capacity - 1);
                    
                    if(string_table[index].string.data == null){
                        
                        u8 *base = push_string_copy(arena, string_literal).data;
                        push_data(arena, u8, element_size);
                        
                        u32 offset_in_section = (u32)(base - rdata_base);
                        u32 string_symbol_index = (u32)string_index++;
                        
                        struct string_symbol *symbol = push_struct(scratch, struct string_symbol);
                        symbol->offset_in_section = offset_in_section;
                        symbol->string_index = string_symbol_index;
                        sll_push_back(string_symbols, symbol);
                        
                        lit->relative_virtual_address = offset_in_section;
                        lit->symbol_table_index = string_symbol_index;
                        
                        string_table[index].string.data  = base;
                        string_table[index].string.size  = string_literal.size;
                        string_table[index].string_index = string_symbol_index;
                        
                        break;
                    }
                    
                    if(!string_match(string_literal, string_table[index].string)) continue;
                    
                    lit->relative_virtual_address = (u32)(string_table[index].string.data - rdata_base);
                    lit->symbol_table_index = string_table[index].string_index;
                    break;
                }
            }
        }
        
        
        struct{
            struct ast_float_literal *literals;
            smm amount_of_literals;
        } float_literals_by_type[2] = zero_struct; // 0 - float, 1 - double
        
        for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
            struct context *thread_context = globals.thread_infos[thread_index].context;
            for(struct ast_float_literal *lit = thread_context->float_literals.first; lit; ){
                struct ast_float_literal *next = lit->next;
                
                int is_double = (lit->base.resolved_type == &globals.typedef_f64);
                
                lit->next = float_literals_by_type[is_double].literals;
                float_literals_by_type[is_double].literals = lit;
                float_literals_by_type[is_double].amount_of_literals += 1;
                
                lit = next;
            }
        }
        
        for(int is_double = 0; is_double < 2; is_double++){
            
            if(is_double){
                double_start_offset = (u32)(arena_current(arena) - rdata_base);
            }else{
                float_start_offset  = (u32)(arena_current(arena) - rdata_base);
            }
            
            smm amount_of_literals = float_literals_by_type[is_double].amount_of_literals;
            
            smm size = is_double ? 8 : 4;
            
            smm capacity = u64_round_up_to_next_power_of_two((u64)(1.5 * amount_of_literals));
            void **table = push_data(scratch, void *, capacity);
            
            push_zero_align(arena, size);
            
            for(struct ast_float_literal *lit = float_literals_by_type[is_double].literals; lit; lit = lit->next){
                
                u64 hash = xor_shift64(*(u64 *)&lit->value);
                
                for(smm table_index = 0; table_index < capacity; table_index++){
                    smm index = (hash + table_index) & (capacity - 1);
                    
                    if(table[index] == null){
                        lit->relative_virtual_address = (u32)(arena_current(arena) - rdata_base);
                        
                        if(is_double){
                            double *out = push_struct(arena, double);
                            *out = lit->value;
                            
                            table[index] = out;
                        }else{
                            float *out = push_struct(arena, float);
                            *out = (float)lit->value;
                            
                            table[index] = out;
                        }
                    }
                    
                    if(is_double){
                        if(*(double *)table[index] != lit->value) continue;
                    }else{
                        if(*(float *)table[index] != (float)lit->value) continue;
                    }
                    
                    lit->relative_virtual_address = (u32)((u8 *)table[index] - rdata_base);
                    break;
                }
            }
            
            if(is_double){
                double_end_offset = (u32)(arena_current(arena) - rdata_base);
            }else{
                float_end_offset  = (u32)(arena_current(arena) - rdata_base);
            }
        }
        
        memcpy(rdata, ".rdata", sizeof(".rdata"));
        rdata->pointer_to_raw_data = (u32)(rdata_base - obj_base_address);
        rdata->size_of_raw_data    = (u32)(arena_current(arena) - rdata_base);
        rdata->characteristics     = /*INITIALIZED_DATA*/0x00000040 | /*ALIGN_16BYTES*/0x00500000 | /*MEM_READ*/0x40000000;
    }
    
    if(defined_variables.count){
        data = section_headers + section_header_count++;
        
        u8 *data_base = arena_current(arena);
        
        for_ast_list(defined_variables){
            struct ast_declaration *decl = cast(struct ast_declaration *)it->value;
            
            smm alignment = get_declaration_alignment(decl);
            smm decl_size = get_declaration_size(decl);
            
            push_zero_align(arena, alignment);
            
            u8 *mem = push_uninitialized_data(arena, u8, decl_size);
            
            if(decl->assign_expr){
                assert(decl->memory_location);
                memcpy(mem, decl->memory_location, decl_size);
            }else{
                memset(mem, 0, decl_size); // @cleanup: I think 'uninitialized' is not a thing anymore.
            }
            
            decl->memory_location = mem;
            decl->relative_virtual_address = (u32)(mem - data_base);
        }
        
        memcpy(data, ".data", sizeof(".data"));
        data->pointer_to_raw_data = (u32)(data_base - obj_base_address);
        data->size_of_raw_data    = (u32)(arena_current(arena) - data_base);
        data->characteristics     = /*INITIALIZED_DATA*/0x00000040 | /*ALIGN_16BYTES*/0x00500000 | /*MEM_READ*/0x40000000 | /*MEM_WRITE*/0x80000000;
    }
    
    
    u32 defined_function_func_id_base = 0;
    {
        debug_types = section_headers + section_header_count++;
        
        push_zero_align(arena, 4);
        
        // 
        // The .debug$T section contains the codeview type data.
        // It starts out with a u32 containing the codeview version
        // and then consists of codeview type records. 
        // 
        u8 *debug_types_base = arena_current(arena);
        *push_struct(arena, u32) = /*CV_SIGNATURE_C13*/4; 
        
        // 
        // Initialize the type indices for basic types.
        // 
#define pdb_init_basic_type_index(type_name)\
{\
    globals.typedef_##type_name.pdb_type_index = CV_##type_name;\
    globals.typedef_##type_name.flags |= TYPE_FLAG_pdb_permanent;\
}
        pdb_init_basic_type_index(void);
        pdb_init_basic_type_index(Bool);
        pdb_init_basic_type_index(s8);
        pdb_init_basic_type_index(s16);
        pdb_init_basic_type_index(s32);
        pdb_init_basic_type_index(s64);
        pdb_init_basic_type_index(u8);
        pdb_init_basic_type_index(u16);
        pdb_init_basic_type_index(u32);
        pdb_init_basic_type_index(u64);
        pdb_init_basic_type_index(f32);
        pdb_init_basic_type_index(f64);
        
#undef pdb_init_basic_type_index
        
        u32 type_index_allocator = 0x1000;
        
        
        for(u64 index = 0; index < globals.compound_types.capacity; index++){
            struct ast_node *node = globals.compound_types.nodes + index;
            if(!node->token) continue;
            
            struct ast_type *initial_type = (struct ast_type *)node->ast;
            register_type(&type_index_allocator, arena, scratch, initial_type);
        }
        
        // 
        // We have to register types for every global variable, 
        // as they might be pointer types or anonymous.
        // 
        for(struct compilation_unit *compilation_unit = &dummy_global_compilation_unit; compilation_unit; compilation_unit = compilation_unit->next){
            struct ast_table *table = &compilation_unit->static_declaration_table; // :DeclarationTableLoop
            
            for(u64 table_index = 0; table_index < table->capacity; table_index++){
                struct ast *ast = table->nodes[table_index].ast;
                if(!ast) continue;
                
                struct ast_declaration *decl = (struct ast_declaration *)ast;
                if(decl->type->pdb_type_index) continue;
                
                if(decl->flags & DECLARATION_FLAGS_is_enum_member){
                    register_type(&type_index_allocator, arena, scratch, decl->type);
                    continue;
                }
                
                if(decl->base.kind == AST_typedef){
                    register_type(&type_index_allocator, arena, scratch, decl->type);
                    continue;
                }
                
                if(ast->kind == AST_declaration){
                    
                    if(decl->flags & DECLARATION_FLAGS_is_dllimport) continue;
                    
                    if(decl->times_referenced == 0){
                        if(decl->flags & DECLARATION_FLAGS_is_static) continue;
                        if(decl->flags & DECLARATION_FLAGS_is_extern) continue;
                    }
                    
                    register_type(&type_index_allocator, arena, scratch, decl->type);
                    continue;
                }
            }
        }
        
        // 
        // Register types for functions.
        // 
        
        for_ast_list(external_functions){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            register_type(&type_index_allocator, arena, scratch, decl->type);
        }
        
        for_ast_list(defined_functions){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            register_type(&type_index_allocator, arena, scratch, decl->type);
        }
        
        for(struct ast_list_node *function_node = defined_functions.first; function_node; function_node = function_node->next){
            struct ast_function *function = (struct ast_function *)function_node->value;
            
            // 
            // Iterate down all statements registering the types of all declarations.
            // @cleanup: This should only really care about scopes and also register types
            //           which are not referenced by declarations.
            // @cleanup: How does this work with compound literals?
            // @note: Because of pointer types, we have to register the types of all declarations.
            // 
            
            smm ast_stack_capacity = 32;
            struct ast_stack_node{
                struct ast *ast;
            } *stack = push_uninitialized_data(&stack_arena, struct ast_stack_node, ast_stack_capacity);
            stack[0].ast = function->scope;
            
#define push_to_stack(ast_to_push) {\
    if(ast_stack_size == ast_stack_capacity){\
        push_uninitialized_data(&stack_arena, struct ast_stack_node, ast_stack_capacity);\
        ast_stack_capacity *= 2;\
    }\
    stack[ast_stack_size++].ast = (ast_to_push);\
}
            
            for(smm ast_stack_size = 1; ast_stack_size > 0; ){
                
                struct ast *ast = stack[--ast_stack_size].ast;
                
                switch(ast->kind){
                    
                    case AST_typedef: case AST_declaration:{
                        struct ast_declaration *decl = (struct ast_declaration *)ast;
                        register_type(&type_index_allocator, arena, scratch, decl->type);
                    }break;
                    
                    case AST_declaration_list:{
                        struct ast_declaration_list *list = cast(struct ast_declaration_list *)ast;
                        for(struct declaration_node *node = list->list.first; node; node = node->next){
                            register_type(&type_index_allocator, arena, scratch, node->decl->type);
                        }
                    }break;
                    
                    case AST_if:{
                        struct ast_if *ast_if = (struct ast_if *)ast;
                        
                        // 
                        // First recurse into the 'condition', then the 'statement', then the 'else_statement'.
                        // 
                        
                        if(ast_if->else_statement) push_to_stack(ast_if->else_statement);
                        push_to_stack(ast_if->statement);
                        push_to_stack(ast_if->condition);
                    }break;
                    
                    case AST_do_while:{
                        struct ast_for *do_while = (struct ast_for *)ast;
                        
                        // 
                        // First recurse into the 'body', and then into the 'condition'.
                        // 
                        
                        if(do_while->condition) push_to_stack(do_while->condition);
                        push_to_stack(do_while->body);
                    }break;
                    
                    case AST_for:{
                        struct ast_for *ast_for = (struct ast_for *)ast;
                        
                        // 
                        // First recurse into the 'decl', then the 'condition', 
                        // then the 'body' and finially the 'increment'.
                        // 
                        if(ast_for->increment) push_to_stack(ast_for->increment);
                        push_to_stack(ast_for->body);
                        if(ast_for->condition) push_to_stack(ast_for->condition);
                        if(ast_for->decl) push_to_stack(ast_for->decl);
                    }break;
                    
                    case AST_scope:{
                        struct ast_scope *scope = (struct ast_scope *)ast;
                        
                        // 
                        // Push all the statements to the ast stack in reverse order.
                        // 
                        
                        smm statement_list_count = scope->statement_list.count;
                        
                        if(ast_stack_size + statement_list_count > ast_stack_capacity){
                            push_uninitialized_data(&stack_arena, struct ast_stack_node, statement_list_count);
                        }
                        
                        ast_stack_size += statement_list_count;
                        
                        smm statement_index = 0;
                        for_ast_list(scope->statement_list){
                            stack[ast_stack_size - (statement_index++ + 1)].ast = it->value;
                        }
                        assert(statement_index == statement_list_count);
                    }break;
                    
                    case AST_switch:{
                        struct ast_switch *ast_switch = cast(struct ast_switch *)ast;
                        
                        // 
                        // First recurse into the thing we 'switch_on' then the 'statement'.
                        // 
                        
                        push_to_stack(ast_switch->statement);
                        push_to_stack(ast_switch->switch_on);
                    }break;
                    
                    case AST_label:{
                        struct ast_label *ast_label = cast(struct ast_label *)ast;
                        if(ast_label->statement) push_to_stack(ast_label->statement);
                    }break;
                    case AST_case:{
                        struct ast_case *ast_case = cast(struct ast_case *)ast;
                        if(ast_case->statement) push_to_stack(ast_case->statement);
                    }break;
                    
                    default: break;
                }
            }
            

        }
        
        
        defined_function_func_id_base = type_index_allocator;
        
        for_ast_list(defined_functions){
            type_index_allocator += 1;
            
            struct ast_function *function = (struct ast_function *)it->value;
            
            u16 *length = push_struct(arena, u16);
            *push_struct(arena, u16) = /*LF_FUNC_ID*/0x1601;
            *push_struct(arena, u32) = /*parent scope*/0; // @incomplete: look at a language with local procedures.
            *push_struct(arena, u32) = function->type->base.pdb_type_index;
            push_zero_terminated_string_copy(arena, function->identifier->string);
            push_f3f2f1_align(arena, sizeof(u32));
            *length = (u16)(arena_current(arena) - (u8 *)(length + 1)); // @cleanup: overflow.
        }
        
        memcpy(debug_types, ".debug$T", sizeof(".debug$T") - 1);
        debug_types->pointer_to_raw_data = (u32)(debug_types_base - obj_base_address);
        debug_types->size_of_raw_data    = (u32)(arena_current(arena) - debug_types_base);
        debug_types->characteristics     = /*INITIALIZED_DATA*/0x00000040 | /*DISCARDABLE*/0x02000000 | /*1-BYTE-ALIGN*/0x00100000 |  /*MEM_READ*/0x40000000;
    } 
    
    // 
    // For the .debug$S section, relocations are used for the addresses of declarations inside the debug information.
    // Usually, each location needs two relocations one being a SECREL relocation and one being a SECTION relocation.
    // 
    struct debug_symbols_relocation_info *debug_symbols_relocations = push_data(scratch, struct debug_symbols_relocation_info, 0);
    
    {
        debug_symbols = section_headers + section_header_count++;
        
        push_zero_align(arena, 4);
        
        // 
        // The .debug$S section contains the codeview symbol data 
        // (opposed to the .debug$T section, which contains type information).
        // It starts out with a u32 containing the codeview version and then
        // consists of 4-byte aligned subsections.
        // 
        
        u8 *debug_symbols_base = arena_current(arena);
        *push_struct(arena, u32) = /*CV_SIGNATURE_C13*/4; 
        
        {
            // 
            // Emit the S_OBJNAME and S_COMPILE3. They always seem to be the first two symbols.
            // 
            
            *push_struct(arena, u32) = /*DEBUG_S_SYMBOLS*/0xf1;
            u32 *subsection_size = push_struct(arena, u32);
            
            {
                u16 *objname_length = push_struct(arena, u16);
                *push_struct(arena, u16) = /*S_OBJNAME*/0x1101;
                
                *push_struct(arena, u32) = 0; // signature @cleanup: what is this?
                push_zero_terminated_string_copy(arena, globals.output_file_path);
                
                push_f3f2f1_align(arena, sizeof(u32));
                *objname_length = (u16)(arena_current(arena) - (u8 *)(objname_length + 1));
            }
            
            {
                u16 *compile3_length = push_struct(arena, u16);
                *push_struct(arena, u16) = /*S_COMPILE3*/0x113c;
                
                struct codeview_compile3{
                    u32 flags;
                    u16 machine;
                    u16 front_end_major_version;
                    u16 front_end_minor_version;
                    u16 front_end_build_version;
                    u16 front_end_QFE_version;
                    u16 back_end_major_version;
                    u16 back_end_minor_version;
                    u16 back_end_build_version;
                    u16 back_end_QFE_version;
                    u8 version_string[1];
                } *compile3 = push_struct(arena, struct codeview_compile3);
                compile3->machine = /*x64*/0xd0;
                compile3->front_end_major_version = 19;
                compile3->front_end_minor_version = 11;
                compile3->front_end_build_version = 25506;
                compile3->back_end_major_version = 19;
                compile3->back_end_minor_version = 11;
                compile3->back_end_build_version = 25506;
                
                compile3->version_string[0] = 0; // @cleanup
                
                push_f3f2f1_align(arena, sizeof(u32));
                *compile3_length = (u16)(arena_current(arena) - (u8 *)(compile3_length + 1));
            }
            
            *subsection_size = (u32)(arena_current(arena) - (u8 *)(subsection_size + 1));
            push_align(arena, 4);
        }
        
        
        // @cleanup: we are using two members of 'struct file' here, 
        //           'offset_in_names' and 'offset_in_f4' both of these are used nowhere else
        //           and we should just allocate arrays here locally.
        
        {
            // 
            // The DEBUG_S_STRINGTABLE is, despite its generic name, only used to hold
            // the file names for the DEBUG_S_FILECHKSUM section. All other strings 
            // (functions names, enum names, etc) are all inline.
            // Hence, we just emit this sections here upfront, so we can use it 
            // to emit the DEBUG_S_FILECHKSUM section immediately after.
            // 
            
            *push_struct(arena, u32) = /*DEBUG_S_STRING_TABLE*/0xf3;
            u32 *subsection_size = push_struct(arena, u32);
            
            u8 *string_table_start = arena_current(arena);
            *push_struct(arena, u8) = 0; // The empty string, always present.
            
            for(smm file_index = 0; file_index < array_count(globals.file_table.data); file_index++){
                struct file *file = globals.file_table.data[file_index];
                if(!file) continue;
                
                file->offset_in_names = (u32)(arena_current(arena) - string_table_start);
                
                push_zero_terminated_string_copy(arena, string_from_cstring(file->absolute_file_path));
            }
            
            *subsection_size = (u32)(arena_current(arena) - (u8 *)(subsection_size + 1));
            push_align(arena, 4);
        }
        
        {
            // 
            // The 'DEBUG_S_FILECHKSUM' subsection contains information about the files
            // used to compile the object file. This file is particularly important for
            // line information later on. We emit this function first, so we have the 
            // offsets ready for the DEBUG_S_LINES sections below.
            // 
            
            *push_struct(arena, u32) = /*DEBUG_S_FILECHKSUM*/0xf4;
            u32 *subsection_size = push_struct(arena, u32);
            
            u8 *section_start = arena_current(arena);
            for(smm file_index = 0; file_index < array_count(globals.file_table.data); file_index++){
                struct file *file = globals.file_table.data[file_index];
                if(!file) continue;
                
                file->offset_in_f4 = (u32)(arena_current(arena) - section_start);
                
                // :padded_file_size
                // 
                // We have padded the file size when allocating for it so we can use 'hash_md5_inplace'
                // here instead of 'hash_md5' which would have to allocate.
                // 
                m128 md5 = hash_md5_inplace(file->file.memory, file->file.size, file->file.size + 128);
                
                struct codeview_file_checksum_header{
                    u32 offset_in_stringtable;
                    u16 hash_function_kind;
                    u8  hash[16];    // variable sized hash (always 16 for md5 and 2 bytes padding for 4-byte alignment)
                    u8  padding[2];
                } *file_checksum_header = push_struct(arena, struct codeview_file_checksum_header);
                file_checksum_header->offset_in_stringtable = file->offset_in_names;
                file_checksum_header->hash_function_kind = /*md5*/0x110;
                memcpy(file_checksum_header->hash, md5._u8, 16);
            }
            
            *subsection_size = (u32)(arena_current(arena) - (u8 *)(subsection_size + 1));
            push_align(arena, 4);
        }
        
        u32 function_id_at = 0; // @note: this indexes the LF_FUNC_ID at 'defined_function_func_id_base'.
        
        for(struct ast_list_node *function_ast_node = defined_functions.first; function_ast_node; function_ast_node = function_ast_node->next){
            
            // 
            // For each defined function, there is a DEBUG_S_SYMBOLS section 
            // containing the debug info for the function.
            // Followed immediately by a DEBUG_S_LINES section,
            // containg the line information for the function.
            // 
            
            struct ast_function *function = (struct ast_function *)function_ast_node->value;
            
            {
                *push_struct(arena, u32) = /*DEBUG_S_SYMBOLS*/0xf1;
                u32 *subsection_size = push_struct(arena, u32);
                
                // @cleanup: Move these into the struct.
                u16 proc32_id = (function->as_decl.flags & DECLARATION_FLAGS_is_static) ? /*S_LPROC32_ID*/0x1146 : /*S_GPROC32_ID*/0x1147;
                // u16 proc32_id = (function->as_decl.flags & DECLARATION_FLAGS_is_static) ? /* LPROC32 */ 0x110f: /* GPROC32 */ 0x1110;
                
                u16 *proc32_length = push_struct(arena, u16);
                *push_struct(arena, u16) = proc32_id;
                
                struct codeview_proc{
                    u32 pointer_to_parent;
                    u32 pointer_to_end;
                    u32 pointer_to_next;
                    u32 procedure_length;
                    u32 debug_start_offset;
                    u32 debug_end_offset;
                    u32 type_index;
                    u32 offset_in_section;
                    u16 section_id;
                    u8 procedure_flags;
                    u8 procedure_name[];
                } *proc_symbol = push_struct_(arena, offset_in_type(struct codeview_proc, procedure_name) + (function->identifier->length + 1), 4);
                proc_symbol->pointer_to_parent  = 0;
                proc_symbol->pointer_to_end     = 0;
                proc_symbol->pointer_to_next    = 0;
                proc_symbol->procedure_length   = (u32)function->byte_size;
                proc_symbol->debug_start_offset = (u32)function->size_of_prolog;
                proc_symbol->debug_end_offset   = (u32)function->byte_size;
                proc_symbol->type_index         = defined_function_func_id_base + function_id_at++; // @note: as this is an S_LPROC32_ID not an S_LPROC32 this is the LF_FUNC_ID not the type.
                proc_symbol->offset_in_section  = 0; // Filled in by a relocation
                proc_symbol->section_id         = 0; // Filled in by a relocation
                proc_symbol->procedure_flags    = 0;
                memcpy(proc_symbol->procedure_name, function->identifier->data, function->identifier->length);
                proc_symbol->procedure_name[function->identifier->length] = 0;
                
                push_f3f2f1_align(arena, sizeof(u32));
                *proc32_length = (u16)(arena_current(arena) - (u8 *)(proc32_length + 1));
                
                struct debug_symbols_relocation_info *relocation = push_struct(scratch, struct debug_symbols_relocation_info);
                relocation->destination_offset = (u32)((u8 *)&proc_symbol->offset_in_section - debug_symbols_base);
                relocation->source_declaration = &function->as_decl;
                
                u16 *frameproc_length = push_struct(arena, u16);
                *push_struct(arena, u16) = /*S_FRAMEPROC*/0x1012; 
                
                struct codeview_frameproc{
                    u32 stack_frame_size;
                    u32 stack_frame_padding_size;
                    u32 offset_of_padding;
                    u32 callee_saved_registers_size;
                    u32 offset_in_section_of_exception_handler;
                    u16 section_id_of_exception_handler;
                    
                    u16 has_alloca     : 1;
                    u16 has_set_jump   : 1;
                    u16 has_long_jump  : 1;
                    u16 has_inline_asm : 1;
                    u16 has_eh_states  : 1; // ?
                    u16 was_declared_inline : 1;
                    u16 has_structured_exception_handling : 1;
                    u16 was_declared_nacked : 1;
                    u16 has_GS_security_checks : 1;
                    u16 has_async_exception_handling : 1;
                    u16 has_GS_but_no_stack_ordering : 1;
                    u16 was_inlined_into_another_function : 1;
                    u16 was_declared_strict_gs_check : 1;
                    u16 was_declared_safe_buffers    : 1;
                    u16 encoded_local_base_pointer     : 2;
                    u16 encoded_parameter_base_pointer : 2;
                    u16 was_compiled_with_pgo_pgu : 1;
                    u16 have_valid_pogo_counts    : 1;
                    u16 was_optimized_for_speed   : 1;
                    u16 contains_cfg_checks_but_no_write_checks : 1;
                    u16 contains_cfg_checks_and_instrumentation : 1;
                    u16 : 9;
                    
                } *frameproc = push_struct(arena, struct codeview_frameproc);
                frameproc->stack_frame_size = (u32)function->stack_space_needed;
                frameproc->encoded_local_base_pointer     = /*rbp*/2;
                frameproc->encoded_parameter_base_pointer = /*rbp*/2;
                frameproc->has_async_exception_handling   = 1;
                frameproc->was_optimized_for_speed        = 1; // always set for some reason
                
                *frameproc_length = (u16)(arena_current(arena) - (u8 *)(frameproc_length + 1));
                
                codeview_emit_debug_information_for_function__recursive(function, arena, function->scope, scratch, debug_symbols_base);
                
                *push_struct(arena, u16) = 2; // length
                *push_struct(arena, u16) = /*S_PROC_ID_END*/0x114f;
                // *push_struct(arena, u16) = /*S_END*/6;
                
                u32 function_debug_info_size = (u32)(arena_current(arena) - (u8 *)(subsection_size + 1));
                
                *subsection_size = function_debug_info_size;
                push_align(arena, 4);
            }
            
            {
                // 
                // The DEBUG_S_LINES contains a mapping of offsets to file and line information. 
                // This subsection feels somewhat overengineered, it has a header and then consists
                // of blocks, each of which covers one file for the function.
                // In practice, this means there are two headers here.
                // 
                // @cleanup: What happens when the function is spread between 
                //           multiple files? What happens when the function is not contigious?
                //           Why are we not using the [section:offset]?
                // 
                *push_struct(arena, u32) = /*DEBUG_S_LINES*/0xf2;
                u32 *subsection_size = push_struct(arena, u32);
                
                struct codeview_lines_header{
                    u32 offset_in_section_contribution;
                    u16 section_id;
                    u16 flags;
                    u32 size_of_the_contribution;
                } *lines_header = push_struct(arena, struct codeview_lines_header);
                lines_header->offset_in_section_contribution = 0; // filled in by a relocation
                lines_header->section_id = 0;                     // filled in by a relocation
                lines_header->flags = 0;
                lines_header->size_of_the_contribution = (u32)function->byte_size;  // everything else is somehow 0.
                
                struct debug_symbols_relocation_info *relocation = push_struct(scratch, struct debug_symbols_relocation_info);
                relocation->destination_offset = (u32)((u8 *)&lines_header->offset_in_section_contribution - debug_symbols_base);
                relocation->source_declaration = &function->as_decl;
                
                u32 file_index = function->scope->token->file_index;
                struct file *file = globals.file_table.data[file_index];
                
                struct codeview_lines_block{
                    u32 offset_in_file_checksums;
                    u32 amount_of_lines;
                    u32 block_size;
                } *lines_block = push_struct(arena, struct codeview_lines_block);
                lines_block->offset_in_file_checksums = file->offset_in_f4;
                
                // The lines for the function are in the format:
                // struct{
                //    u32 offset;
                //    u32 start_line_number     : 24;
                //    u32 optional_delta_to_end : 7;
                //    u32 is_a_statement        : 1;
                // };
                // 
                
                smm line   = function->scope->token->line;
                smm offset = 0;
                
                u8 *lines_start = arena_current(arena);
                
                // 
                // Emit an initial line for the start of the function.
                // 
                *push_struct(arena, u32) = (u32)offset;
                *push_struct(arena, u32) = (u32)(line | /*is_statement*/0x80000000);
                
                smm ast_stack_capacity = 32;
                struct ast_stack_node{
                    struct ast *ast;
                } *stack = push_uninitialized_data(&stack_arena, struct ast_stack_node, ast_stack_capacity);
                stack[0].ast = function->scope;
                
                // 
                // To iterate the nodes in the correct order (orderered by offset), 
                // we have iterate depth first (emit all line-information for grand-children 
                // before any other children).
                // 
                // This means we have to use a stack and push the nodes in reverse order.
                // 
                for(smm ast_stack_size = 1; ast_stack_size > 0; ){
                    
                    struct ast *ast = stack[--ast_stack_size].ast;
                    
                    switch(ast->kind){
                        case AST_typedef: case AST_function:{
                            // These have no code associated with them.
                        }break;
                        
                        case AST_if:{
                            struct ast_if *ast_if = (struct ast_if *)ast;
                            
                            // 
                            // First recurse into the 'condition', then the 'statement', then the 'else_statement'.
                            // 
                            
                            if(ast_if->else_statement) push_to_stack(ast_if->else_statement);
                            push_to_stack(ast_if->statement);
                            push_to_stack(ast_if->condition);
                        }break;
                        
                        case AST_do_while:{
                            struct ast_for *do_while = (struct ast_for *)ast;
                            
                            // 
                            // First recurse into the 'body', and then into the 'condition'.
                            // 
                            
                            push_to_stack(do_while->body);
                            if(do_while->condition) push_to_stack(do_while->condition);
                        }break;
                        
                        case AST_for:{
                            struct ast_for *ast_for = (struct ast_for *)ast;
                            
                            // 
                            // First recurse into the 'decl', then the 'condition', 
                            // then the 'body' and finially the 'increment'.
                            // 
                            if(ast_for->increment) push_to_stack(ast_for->increment);
                            push_to_stack(ast_for->body);
                            if(ast_for->condition) push_to_stack(ast_for->condition);
                            if(ast_for->decl) push_to_stack(ast_for->decl);
                        }break;
                        
                        case AST_scope:{
                            struct ast_scope *scope = (struct ast_scope *)ast;
                            
                            // 
                            // Push all the statements to the ast stack in reverse order.
                            // 
                            
                            smm statement_list_count = scope->statement_list.count;
                            ast_stack_size += statement_list_count;
                            
                            if(ast_stack_size + statement_list_count > ast_stack_capacity){
                                push_uninitialized_data(&stack_arena, struct ast_stack_node, statement_list_count);
                            }
                            
                            smm statement_index = 0;
                            for_ast_list(scope->statement_list){
                                stack[ast_stack_size - (statement_index++ + 1)].ast = it->value;
                            }
                            assert(statement_index == statement_list_count);
                        }break;
                        
                        case AST_switch:{
                            struct ast_switch *ast_switch = cast(struct ast_switch *)ast;
                            
                            // 
                            // First recurse into the thing we 'switch_on' then the 'statement'.
                            // 
                            
                            push_to_stack(ast_switch->statement);
                            push_to_stack(ast_switch->switch_on);
                        }break;
                        
                        case AST_label:{
                            struct ast_label *ast_label = cast(struct ast_label *)ast;
                            if(ast_label->statement) push_to_stack(ast_label->statement);
                        }break;
                        case AST_case:{
                            struct ast_case *ast_case = cast(struct ast_case *)ast;
                            if(ast_case->statement) push_to_stack(ast_case->statement);
                        }break;
                        
                        case AST_declaration:{
                            struct ast_declaration *decl = (struct ast_declaration *)ast;
                            
                            // Dont emit lines for declarations that dont have initializers.
                            if(!decl->assign_expr) break;
                            
                            // Dont emit lines for declarations that are static.
                            if(decl->flags & (DECLARATION_FLAGS_is_static | DECLARATION_FLAGS_is_local_persist)) break;
                            
                            push_to_stack(decl->assign_expr);
                        }break;
                        
                        case AST_compound_literal:{
                            // 
                            // We recurse into compound literals as they might be multi-line.
                            // 
                            
                            struct ast_compound_literal *compound_literal = (struct ast_compound_literal *)ast;
                            
                            // 
                            // Push all the initializers to the ast stack in reverse order.
                            // 
                            
                            smm assignment_list_count = compound_literal->assignment_list.count;
                            ast_stack_size += assignment_list_count;
                            
                            if(ast_stack_size + assignment_list_count > ast_stack_capacity){
                                push_uninitialized_data(&stack_arena, struct ast_stack_node, assignment_list_count);
                            }
                            
                            smm assignment_index = 0;
                            for_ast_list(compound_literal->assignment_list){
                                stack[(ast_stack_size - 1) - assignment_index++].ast = it->value;
                            }
                            assert(assignment_index == assignment_list_count);
                        }break;
                        
                        // 
                        // These two last ones are the ones that actually do emit line records:
                        // 
                        
                        case AST_asm_block:{
                            struct ast_asm_block *asm_block = cast(struct ast_asm_block *)ast;
                            for(struct asm_instruction *inst = asm_block->instructions.first; inst; inst = inst->next){
                                offset = inst->byte_offset_in_function + function->size_of_prolog;
                                line   = inst->token->line;
                                
                                *push_struct(arena, u32) = (u32)offset;
                                *push_struct(arena, u32) = (u32)(line | /*is_statement*/0x80000000);
                            }
                        }break;
                        
                        default:{
                            smm ast_line = ast->token->line;
                            
                            assert(ast->byte_offset_in_function >= 0);
                            
                            smm ast_offset = function->size_of_prolog + ast->byte_offset_in_function;
                            assert(ast_offset >= offset);
                            
                            offset = ast_offset;
                            line   = ast_line;
                            
                            *push_struct(arena, u32) = (u32)offset;
                            *push_struct(arena, u32) = (u32)(line | /*is_statement*/0x80000000);
                        }break;
                    }
                }
                
#undef push_to_stack
                
                smm lines_size = arena_current(arena) - lines_start;
                
                lines_block->amount_of_lines = (u32)(lines_size/8);
                lines_block->block_size = (u32)(sizeof(*lines_block) + lines_size);
                
                *subsection_size = (u32)(arena_current(arena) - (u8 *)(subsection_size + 1));
                push_align(arena, 4);
            }
        }
        
        {
            // 
            // Symbols for data declarations:
            // 
            //     S_GDATA32  - extern or exported variables.
            //     S_LDATA32  - static variables.
            //     S_UDT      - typedefs.
            //     S_CONSTANT - enum members.
            //     
            
            *push_struct(arena, u32) = /*DEBUG_S_SYMBOLS*/0xf1;
            u32 *subsection_size = push_struct(arena, u32);
            
            // :DeclarationTableLoop
            for(struct compilation_unit *compilation_unit = &dummy_global_compilation_unit; compilation_unit; compilation_unit = compilation_unit->next){
                
                struct ast_table *table = &compilation_unit->static_declaration_table; // :DeclarationTableLoop
                
                for(u64 table_index = 0; table_index < table->capacity; table_index++){
                    struct ast *ast = table->nodes[table_index].ast;
                    if(!ast) continue;
                    
                    struct ast_declaration *decl = (struct ast_declaration *)ast;
                    
                    if(decl->flags & DECLARATION_FLAGS_is_enum_member){
                        // 
                        // Emit a 'S_CONSTANT' for all enum members.
                        // 
                        
                        assert(decl->type->kind == AST_enum);
                        u32 enum_type_index = decl->type->pdb_type_index;
                        assert(enum_type_index);
                        
                        s32 value = integer_literal_as_s32(decl->assign_expr);
                        
                        u16 *length = push_struct(arena, u16);
                        *push_struct(arena, u16) = /*S_CONSTANT*/0x1107;
                        *push_struct(arena, u32) = enum_type_index;
                        push_unsigned_number_leaf(arena, value);
                        push_zero_terminated_string_copy(arena, decl->identifier->string);
                        push_f3f2f1_align(arena, sizeof(u32));
                        *length = (u16)(arena_current(arena)- (u8 *)(length + 1));
                        continue;
                    }
                    
                    if(decl->base.kind == AST_typedef){
                        // 
                        // Emit a 'S_UDT' for all typedefs.
                        // 
                        assert(decl->type->pdb_type_index);
                        
                        u16 *length = push_struct(arena, u16);
                        *push_struct(arena, u16) = /*S_UDT*/0x1108;
                        *push_struct(arena, u32) = decl->type->pdb_type_index;
                        push_zero_terminated_string_copy(arena, decl->identifier->string);
                        push_f3f2f1_align(arena, sizeof(u32));
                        *length = (u16)(arena_current(arena)- (u8 *)(length + 1));
                    }
                    
                    if(ast->kind == AST_declaration){
                        assert(!(decl->flags & DECLARATION_FLAGS_is_local_persist));
                        
                        // @note: dllimports do not have debug symbols.
                        if(decl->flags & DECLARATION_FLAGS_is_dllimport) continue;
                        
                        if(decl->times_referenced == 0){
                            if(decl->flags & DECLARATION_FLAGS_is_static) continue;
                            if(decl->flags & DECLARATION_FLAGS_is_extern) continue;
                        }
                        
                        assert(decl->type->pdb_type_index);
                        
                        struct codeview_data32{
                            u16 length;
                            u16 kind;
                            u32 type_index;
                            u32 offset_in_section;
                            u16 section_id;
                            u8  identifier[];
                        } *data_symbol = push_struct_(arena, offset_in_type(struct codeview_data32, identifier) + (decl->identifier->length + 1), 4);
                        data_symbol->kind = (decl->flags & DECLARATION_FLAGS_is_static) ? /* S_LDATA32 */ 0x110c : /* S_GDATA32 */ 0x110d;
                        data_symbol->type_index = decl->type->pdb_type_index;
                        memcpy(data_symbol->identifier, decl->identifier->data, decl->identifier->length);
                        data_symbol->identifier[decl->identifier->length] = 0;
                        
                        struct debug_symbols_relocation_info *relocation = push_struct(scratch, struct debug_symbols_relocation_info);
                        relocation->destination_offset = (u32)((u8 *)&data_symbol->offset_in_section - debug_symbols_base);
                        relocation->source_declaration = decl;
                        
                        push_f3f2f1_align(arena, sizeof(u32));
                        data_symbol->length = (u16)(arena_current(arena) - (u8 *)(&data_symbol->kind));
                    }
                }
            }
            
            *subsection_size = (u32)(arena_current(arena) - (u8 *)(subsection_size + 1));
            push_align(arena, 4);
        }
        
        memcpy(debug_symbols, ".debug$S", sizeof(".debug$S") - 1);
        debug_symbols->pointer_to_raw_data = (u32)(debug_symbols_base - obj_base_address);
        debug_symbols->size_of_raw_data    = (u32)(arena_current(arena) - debug_symbols_base);
        debug_symbols->characteristics     = /*INITIALIZED_DATA*/0x00000040 | /*DISCARDABLE*/0x02000000 | /*1-BYTE-ALIGN*/0x00100000 |  /*MEM_READ*/0x40000000;
    }
    
    smm amount_of_debug_symbols_relocations = push_data(scratch, struct debug_symbols_relocation_info, 0) - debug_symbols_relocations;
    
    struct coff_symbol_table_record{
        // 
        // The name of the symbol is either in there, 
        // if it is at most 8-bytes long. 
        // Otherwise, the name field of the record is four zeroes,
        // followed by the offset into the string table.
        // 
        union{
            u8 short_name[8];
            struct{
                u32 zeroes;
                u32 string_table_offset;
            };
        };
        
        // 
        // The value of the symbol table entry depends on its 
        // section number and its storage class.
        // 
        u32 value;
        
        // 
        // A signed integer specifying a one-based index into 
        // the section table. Special values are:
        //   0 - Undefined (the symbol does not yet have a section)
        //  -1 - Absolute
        //  -2 - Debug
        // 
        s16 section_number;
        
        // 
        // Microsoft tool use this field only to indicate whether the symbol
        // is a function, so that the only two resulting values are 0 and 0x20,
        // for non-function / function respectivly.
        // 
        u16 type;
        
        // 
        // The storage class of the symbol.
        // 
        u8 storage_class;
        
        // 
        // Number of auxiliary entries immediately following this symbol table record.
        // These can be used to communicate more information about the symbol.
        // 
        u8 number_of_auxiliary_entries;
    } *symbol_table_base = push_data(arena, struct coff_symbol_table_record, 0);
    
    // @WARNING: you cannot index 'symbol_table_base' because of alignment.
    
    for(u32 section_index = 0; section_index < section_header_count; section_index++){
        struct coff_section_header *section_header = &section_headers[section_index];
        
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        memcpy(record->short_name, section_header->name, 8);
        record->section_number = (s16)(section_index + 1);
        record->storage_class  = /*IMAGE_SYM_CLASS_STATIC*/3; // "If the Value field is zero, then the symbol represents a section name."
        record->number_of_auxiliary_entries = 1;
        
        struct coff_symbol_table_auxiliary_section_definition_entry{
            // 
            // The same information as in the section header.
            // 
            u32 size_of_raw_data;
            u16 number_of_relocations;
            u16 number_of_line_numbers;
            
            // 
            // COMDAT members.
            // 
            u32 check_sum;
            u16 number;
            u8  selection;
        } *auxiliary_entry = (struct coff_symbol_table_auxiliary_section_definition_entry *)push_data(arena, u8, 18);
        auxiliary_entry->size_of_raw_data = section_header->size_of_raw_data;
        auxiliary_entry->number_of_relocations = 0;
        // @cleanup: check_sum, number, selection are for COMDAT or communal data this implements '__declspec(selectany)'.
    }
    
    struct string_list long_name_strings = zero_struct;
    smm coff_string_table_at = 4;
    
    {
        // 
        // Emit one symbol for each function.
        // 
        
        u32 unwind_info_offset = 0;
        u32 pdata_offset = 0;
        
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            function->symbol_table_index = (arena_current(arena) - (u8 *)symbol_table_base)/18;
            
            struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
            
            struct string name = function->identifier->string;
            
            if(name.size <= 8){
                memcpy(record->short_name, name.data, name.size);
            }else{
                record->string_table_offset = (u32)coff_string_table_at;
                coff_string_table_at += name.size + 1;
                string_list_postfix(&long_name_strings, scratch, name);
            }
            
            record->value = (u32)function->relative_virtual_address;
            record->section_number = (s16)((text - section_headers) + 1); // one based
            record->type = /*function*/0x20;
            record->storage_class = (function->as_decl.flags & DECLARATION_FLAGS_is_static) ? /*IMAGE_SYM_CLASS_STATIC*/3 : /*IMAGE_SYM_CLASS_EXTERNAL*/2;
            
            // 
            // Fill in the symbol for the unwind data for the function.
            // 
            
            struct coff_symbol_table_record *unwind_record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
            
            struct string unwind_string = push_format_string(scratch, "$unwind$%.*s", name.size, name.data);
            
            if(unwind_string.size <= 8){
                memcpy(unwind_record->short_name, unwind_string.data, unwind_string.size);
            }else{
                unwind_record->string_table_offset = (u32)coff_string_table_at;
                coff_string_table_at += unwind_string.size + 1;
                string_list_postfix(&long_name_strings, scratch, unwind_string);
            }
            
            unwind_record->value = unwind_info_offset;
            unwind_record->section_number = (s16)((xdata - section_headers) + 1);
            unwind_record->storage_class = /*IMAGE_SYM_CLASS_STATIC*/3;
            
            // 
            // Advance the offset to the next unwind info.
            // 
            u8 *unwind_data = obj_base_address + xdata->pointer_to_raw_data;
            u8 count_of_codes = unwind_data[2];
            unwind_info_offset += 4 + (count_of_codes + (count_of_codes & 1)) * 2;
            
            struct coff_symbol_table_record *pdata_record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
            
            struct string pdata_string = push_format_string(scratch, "$pdata$%.*s", name.size, name.data);
            
            if(pdata_string.size <= 8){
                memcpy(pdata_record->short_name, pdata_string.data, pdata_string.size);
            }else{
                pdata_record->string_table_offset = (u32)coff_string_table_at;
                coff_string_table_at += pdata_string.size + 1;
                string_list_postfix(&long_name_strings, scratch, pdata_string);
            }
            
            pdata_record->value = pdata_offset;
            pdata_record->section_number = (s16)((pdata - section_headers) + 1);
            pdata_record->storage_class = /*IMAGE_SYM_CLASS_STATIC*/3;
            
            pdata_offset += 12;
        }
    }
    
    for_ast_list(external_functions){
        struct ast_function *function = (struct ast_function *)it->value;
        function->symbol_table_index = (arena_current(arena) - (u8 *)symbol_table_base)/18;
        
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        
        struct string name = function->identifier->string;
        if(function->as_decl.flags & DECLARATION_FLAGS_is_dllimport){
            name = push_format_string(scratch, "__imp_%.*s", name.size, name.data);
        }
        
        if(name.size <= 8){
            memcpy(record->short_name, name.data, name.size);
        }else{
            record->string_table_offset = (u32)coff_string_table_at;
            coff_string_table_at += name.size + 1;
            string_list_postfix(&long_name_strings, scratch, name);
        }
        
        record->value = 0;
        record->section_number = 0; // section is _not yet_ defined.
        record->storage_class = /*IMAGE_SYM_CLASS_EXTERNAL*/2;
        record->type = (function->as_decl.flags & DECLARATION_FLAGS_is_dllimport) ? 0 : /*function*/0x20;
    }
    
    for_ast_list(external_variables){
        struct ast_declaration *decl = (struct ast_declaration *)it->value;
        decl->symbol_table_index = (arena_current(arena) - (u8 *)symbol_table_base)/18;
        
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        
        struct string name = decl->identifier->string;
        if(decl->flags & DECLARATION_FLAGS_is_dllimport){
            name = push_format_string(scratch, "__imp_%.*s", name.size, name.data);
        }
        
        if(name.size <= 8){
            memcpy(record->short_name, name.data, name.size);
        }else{
            record->string_table_offset = (u32)coff_string_table_at;
            coff_string_table_at += name.size + 1;
            string_list_postfix(&long_name_strings, scratch, name);
        }
        
        record->value = 0;
        record->section_number = 0; // section is _not yet_ defined.
        record->storage_class = /*IMAGE_SYM_CLASS_EXTERNAL*/2;
        record->type = 0;
    }
    
    for_ast_list(automatic_variables){
        struct ast_declaration *decl = (struct ast_declaration *)it->value;
        decl->symbol_table_index = (arena_current(arena) - (u8 *)symbol_table_base)/18;
        
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        
        struct string name = decl->identifier->string;
        if(decl->flags & DECLARATION_FLAGS_is_dllimport){
            name = push_format_string(scratch, "__imp_%.*s", name.size, name.data);
        }
        
        if(name.size <= 8){
            memcpy(record->short_name, name.data, name.size);
        }else{
            record->string_table_offset = (u32)coff_string_table_at;
            coff_string_table_at += name.size + 1;
            string_list_postfix(&long_name_strings, scratch, name);
        }
        
        // @cleanup: @incomplete: alignment?
        record->value = (s32)decl->type->size;
        record->section_number = 0; // section is _not yet_ defined.
        record->storage_class = /*IMAGE_SYM_CLASS_EXTERNAL*/2;
        record->type = 0;
    }
    
    for_ast_list(zero_initialized_statics){
        struct ast_declaration *decl = (struct ast_declaration *)it->value;
        decl->symbol_table_index = (arena_current(arena) - (u8 *)symbol_table_base)/18;
        
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        struct string name = decl->identifier->string;
        
        if(name.size <= 8){
            memcpy(record->short_name, name.data, name.size);
        }else{
            record->string_table_offset = (u32)coff_string_table_at;
            coff_string_table_at += name.size + 1;
            string_list_postfix(&long_name_strings, scratch, name);
        }
        
        record->value = (s32)decl->relative_virtual_address;
        record->section_number = (s16)((bss - section_headers) + 1);
        // @note: I'll just do the general thing...
        record->storage_class = (decl->flags & DECLARATION_FLAGS_is_static) ? /*IMAGE_SYM_CLASS_STATIC*/3 : /*IMAGE_SYM_CLASS_EXTERNAL*/2;
        record->type = 0;
    }
    
    for_ast_list(defined_variables){
        struct ast_declaration *decl = (struct ast_declaration *)it->value;
        decl->symbol_table_index = (arena_current(arena) - (u8 *)symbol_table_base)/18;
        
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        struct string name = decl->identifier->string;
        
        if(name.size <= 8){
            memcpy(record->short_name, name.data, name.size);
        }else{
            record->string_table_offset = (u32)coff_string_table_at;
            coff_string_table_at += name.size + 1;
            string_list_postfix(&long_name_strings, scratch, name);
        }
        
        record->value = (s32)decl->relative_virtual_address;
        record->section_number = (s16)((data - section_headers) + 1);
        record->storage_class = (decl->flags & (DECLARATION_FLAGS_is_static | DECLARATION_FLAGS_is_local_persist)) ? /*IMAGE_SYM_CLASS_STATIC*/3 : /*IMAGE_SYM_CLASS_EXTERNAL*/2;
        record->type = 0;
    }
    
    smm string_symbol_base = (arena_current(arena) - (u8 *)symbol_table_base)/18;
    for(struct string_symbol *symbol = string_symbols.first; symbol; symbol = symbol->next){
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        
        struct string name = push_format_string(scratch, "$S%x", symbol->string_index);
        
        if(name.size <= 8){
            memcpy(record->short_name, name.data, name.size);
        }else{
            record->string_table_offset = (u32)coff_string_table_at;
            coff_string_table_at += name.size + 1;
            string_list_postfix(&long_name_strings, scratch, name);
        }
        
        record->value = symbol->offset_in_section;
        record->section_number = (s16)((rdata - section_headers) + 1);
        record->storage_class = /*IMAGE_SYM_CLASS_STATIC*/3;
        record->type = 0;
    }
    
    smm float_symbol_base = (arena_current(arena) - (u8 *)symbol_table_base)/18;
    for(u32 float_offset = float_start_offset; float_offset < float_end_offset; float_offset += 4){
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        
        u32 float_as_u32 = *(u32 *)(obj_base_address + rdata->pointer_to_raw_data + float_offset);
        
        struct string name = push_format_string(scratch, "__real@%.8x", float_as_u32);
        
        record->string_table_offset = (u32)coff_string_table_at;
        coff_string_table_at += name.size + 1;
        string_list_postfix(&long_name_strings, scratch, name);
        
        record->value = float_offset;
        record->section_number = (s16)((rdata - section_headers) + 1);
        record->storage_class = /*IMAGE_SYM_CLASS_STATIC*/3;
        record->type = 0;
    }
    
    smm double_symbol_base = (arena_current(arena) - (u8 *)symbol_table_base)/18;
    for(u32 double_offset = double_start_offset; double_offset < double_end_offset; double_offset += 8){
        struct coff_symbol_table_record *record = (struct coff_symbol_table_record *)push_data(arena, u8, 18);
        
        u64 double_as_u64 = *(u64 *)(obj_base_address + rdata->pointer_to_raw_data + double_offset);
        
        struct string name = push_format_string(scratch, "__real@%.16llx", double_as_u64);
        
        record->string_table_offset = (u32)coff_string_table_at;
        coff_string_table_at += name.size + 1;
        string_list_postfix(&long_name_strings, scratch, name);
        
        record->value = double_offset;
        record->section_number = (s16)((rdata - section_headers) + 1);
        record->storage_class = /*IMAGE_SYM_CLASS_STATIC*/3;
        record->type = 0;
    }
    
    
    smm amount_of_symbols = (arena_current(arena) - (u8 *)symbol_table_base)/18;
    
    // 
    // The string table immediately follows the the symbol table.
    // 
    
    u32 *string_table_size = (u32 *)push_data(arena, u8, 4); // @note: do not align!
    
    for(struct string_list_node *string_node = long_name_strings.list.first; string_node; string_node = string_node->next){
        push_cstring_from_string(arena, string_node->string);
    }
    
    // The string table size includes the leading u32.
    // So the value would be 4 if no strings were present.
    *string_table_size = (u32)(arena_current(arena) - (u8 *)string_table_size);
    
    // 
    // Relocations: 
    // 
    // We now know where everything is located, including all of the symbol table indices.
    // Hence, we can now emit all the relocations.
    // 
    
    struct patch_node *text_patches = null;
    struct patch_node *data_patches = null;
    
    smm amount_of_text_patches = 0;
    smm amount_of_data_patches = 0;
    
    for(smm thread_index = 0; thread_index < globals.thread_count; thread_index++){
        struct context *thread_context = globals.thread_infos[thread_index].context;
        
        for(struct patch_node *patch = thread_context->local_patch_list.first; patch; ){
            struct patch_node *next = patch->next;
            
            struct ast_declaration *dest_declaration = patch->dest_declaration;
            
            if(dest_declaration->base.kind == AST_function){
                patch->next = text_patches;
                text_patches = patch;
                amount_of_text_patches += 1;
            }else{
                assert(dest_declaration->base.kind == AST_declaration);
                patch->next = data_patches;
                data_patches = patch;
                amount_of_data_patches += 1;
            }
            
            patch = next;
        }
    }
    
    // @WARNING: sizeof(struct coff_relocation) is 12 but there are only 10 bytes per relocation.
    struct coff_relocation{
        u32 destination_offset_in_section;
        u32 source_symbol_table_index;
        u16 relocation_type;
    };
    
    if(amount_of_text_patches){ 
        text->pointer_to_relocations = (u32)(arena_current(arena) - obj_base_address);
        
        if(amount_of_text_patches <= 0xffff){
            text->number_of_relocations = (u16)amount_of_text_patches;
        }else{
            assert(amount_of_text_patches < 0xffffffff);
            
            text->characteristics |= /*IMAGE_SCN_LNK_NRELOC_OVFL*/0x01000000;
            text->number_of_relocations = 0xffff;
            
            // If the number of relocations is to big to fit in a u16, 
            // the actual number of relocations is contained in the 
            // VirtualAddress (destination_offset_in_section) field of the first relocation.
            
            struct coff_relocation *relocation = (void *)push_data(arena, u8, 10);
            relocation->relocation_type = 0; // The relocation is ignored.
            relocation->destination_offset_in_section = (u32)(amount_of_text_patches + 1);
        }
        
        u8 *text_relocations_data = push_data(arena, u8, 10 * amount_of_text_patches);
        
        smm index = 0;
        for(struct patch_node *patch = text_patches; patch; patch = patch->next){
            assert(patch->kind == PATCH_rip_relative); // There should only be rip-relative patches to the .text section.
            
            struct ast_function *function = (struct ast_function *)patch->dest_declaration;
            
            smm destination_offset = (function->offset_in_text_section + function->size_of_prolog + patch->location_offset_in_dest_declaration);
            
            struct coff_relocation *relocation = (void *)(text_relocations_data + 10 * index++);
            relocation->relocation_type = /*REL32*/4;
            relocation->destination_offset_in_section = (u32)destination_offset;
            
            // 
            // For instructions like:
            //     c705 05000000 00010000 mov dword ptr [rip+5], 0x100
            // The 'rip_at' is offset from 'destination_offset + 4'.
            // Hence, we have to write a value, which produces the correct value once the patch is applied.
            // 
            s32 value_to_write = (s32)((patch->location_offset_in_dest_declaration  + 4) - patch->rip_at + patch->location_offset_in_source_declaration);
            if(value_to_write){
                u8 *memory_location = function->memory_location + patch->location_offset_in_dest_declaration + function->size_of_prolog;
                *(s32 *)memory_location = value_to_write;
            }
            
            enum ast_kind source_kind = patch->source->kind;
            
            if(source_kind == AST_function || source_kind == AST_declaration){
                struct ast_declaration *source = (struct ast_declaration *)patch->source;
                
                relocation->source_symbol_table_index = (u32)source->symbol_table_index;
            }else if(source_kind == AST_float_literal){
                struct ast_float_literal *source = (struct ast_float_literal *)patch->source;
                
                u32 symbol_table_index;
                if(source->base.resolved_type == &globals.typedef_f64){
                    symbol_table_index = (u32)(double_symbol_base + (source->relative_virtual_address - double_start_offset) / 8);
                }else{
                    symbol_table_index = (u32)(float_symbol_base + (source->relative_virtual_address - float_start_offset) / 4);
                }
                
                relocation->source_symbol_table_index = symbol_table_index;
            }else{
                assert(source_kind == AST_string_literal);
                struct ast_string_literal *source = (struct ast_string_literal *)patch->source;
                
                relocation->source_symbol_table_index = (u32)(string_symbol_base + source->symbol_table_index);
            }
        }
        assert(index == amount_of_text_patches);
    }
    
    if(amount_of_data_patches){
        data->pointer_to_relocations = (u32)(arena_current(arena) - obj_base_address);
        
        if(amount_of_data_patches <= 0xffff){
            data->number_of_relocations = (u16)amount_of_data_patches;
        }else{
            assert(amount_of_data_patches < 0xffffffff);
            
            data->characteristics |= /*IMAGE_SCN_LNK_NRELOC_OVFL*/0x01000000;
            data->number_of_relocations = 0xffff;
            
            // If the number of relocations is to big to fit in a u16, 
            // the actual number of relocations is contained in the 
            // VirtualAddress (destination_offset_in_section) field of the first relocation.
            
            struct coff_relocation *relocation = (void *)push_data(arena, u8, 10);
            relocation->relocation_type = 0; // The relocation is ignored.
            relocation->destination_offset_in_section = (u32)(amount_of_data_patches + 1);
        }
        
        u8 *data_relocations_data = push_data(arena, u8, 10 * amount_of_data_patches);
        
        smm index = 0;
        for(struct patch_node *patch = data_patches; patch; patch = patch->next){
            assert(patch->kind == PATCH_absolute); // There should only be absolute patches to .data
            
            struct ast_declaration *declaration = (struct ast_declaration *)patch->dest_declaration;
            assert(declaration->base.kind == AST_declaration);
            
            struct coff_relocation *relocation = (void *)(data_relocations_data + 10 * index++);
            relocation->relocation_type = /*ADDR64*/1;
            relocation->destination_offset_in_section = (u32)(declaration->relative_virtual_address + patch->location_offset_in_dest_declaration);
            
            // @note: We need the source offset to already be applied to the data.
            *(u64 *)(declaration->memory_location + patch->location_offset_in_dest_declaration) = patch->location_offset_in_source_declaration;
            
            enum ast_kind source_kind = patch->source->kind;
            
            if(source_kind == AST_function || source_kind == AST_declaration){
                struct ast_declaration *source = (struct ast_declaration *)patch->source;
                
                relocation->source_symbol_table_index = (u32)source->symbol_table_index;
            }else{
                assert(source_kind == AST_string_literal);
                struct ast_string_literal *source = (struct ast_string_literal *)patch->source;
                
                relocation->source_symbol_table_index = (u32)(string_symbol_base + source->symbol_table_index);
            }
        }
        assert(index == amount_of_data_patches);
    }
    
    if(defined_functions.count){
        // .pdata reloations:
        // 
        // Each entry in .pdata consists of 3 virtual addresses 
        // and hence needs 3 relocations.
        // The first two are to the function.
        // The third one to the $unwind$function. which has the function->symbol_table__index + 1.
        
        smm amount_of_relocations = 3 * defined_functions.count;
        pdata->pointer_to_relocations = (u32)(arena_current(arena) - obj_base_address);
        
        if(amount_of_relocations <= 0xffff){
            pdata->number_of_relocations = (u16)amount_of_relocations;
        }else{
            assert(amount_of_relocations < 0xffffffff);
            
            pdata->characteristics |= /*IMAGE_SCN_LNK_NRELOC_OVFL*/0x01000000;
            pdata->number_of_relocations = 0xffff;
            
            // If the number of relocations is to big to fit in a u16, 
            // the actual number of relocations is contained in the 
            // VirtualAddress (destination_offset_in_section) field of the first relocation.
            
            struct coff_relocation *relocation = (void *)push_data(arena, u8, 10);
            relocation->relocation_type = 0; // The relocation is ignored.
            relocation->destination_offset_in_section = (u32)(amount_of_relocations + 1);
        }
        
        u8 *pdata_relocations_data = push_data(arena, u8, 10 * amount_of_relocations);
        
        smm relocation_at = 0;
        for_ast_list(defined_functions){
            struct ast_function *function = (struct ast_function *)it->value;
            
            for(u32 index = 0; index < 3; index++){
                struct coff_relocation *relocation = (void *)(pdata_relocations_data + (relocation_at + index) * 10);
                
                relocation->relocation_type = /*ADDR32NB*/3;
                relocation->destination_offset_in_section = (u32)((relocation_at + index) * 4);
                relocation->source_symbol_table_index     = (u32)(function->symbol_table_index + (index == 2));
            }
            
            relocation_at += 3;
        }
    }
    
    if(amount_of_debug_symbols_relocations){
        smm amount_of_relocations = 2 * amount_of_debug_symbols_relocations;
        debug_symbols->pointer_to_relocations = (u32)(arena_current(arena) - obj_base_address);
        
        if(amount_of_relocations <= 0xffff){
            debug_symbols->number_of_relocations = (u16)amount_of_relocations;
        }else{
            assert(amount_of_relocations < 0xffffffff);
            
            debug_symbols->characteristics |= /*IMAGE_SCN_LNK_NRELOC_OVFL*/0x01000000;
            debug_symbols->number_of_relocations = 0xffff;
            
            // If the number of relocations is to big to fit in a u16, 
            // the actual number of relocations is contained in the 
            // VirtualAddress (destination_offset_in_section) field of the first relocation.
            
            struct coff_relocation *relocation = (void *)push_data(arena, u8, 10);
            relocation->relocation_type = 0; // The relocation is ignored.
            relocation->destination_offset_in_section = (u32)(amount_of_relocations + 1);
        }
        
        u8 *debug_symbols_relocations_data = push_data(arena, u8, 10 * amount_of_relocations);
        
        for(smm debug_relocation_at = 0; debug_relocation_at < amount_of_debug_symbols_relocations; debug_relocation_at++){
            
            struct debug_symbols_relocation_info *info = &debug_symbols_relocations[debug_relocation_at];
            
            struct coff_relocation *offset_relocation = (void *)(debug_symbols_relocations_data + (2 * debug_relocation_at) * 10);
            offset_relocation->relocation_type = /*IMAGE_REL_AMD64_SECREL*/0xb;
            offset_relocation->destination_offset_in_section = info->destination_offset;
            offset_relocation->source_symbol_table_index     = (u32)info->source_declaration->symbol_table_index;
            
            struct coff_relocation *section_relocation = (void *)(debug_symbols_relocations_data + (2 * debug_relocation_at + 1) * 10);
            section_relocation->relocation_type = /*IMAGE_REL_AMD64_SECTION*/0xa;
            section_relocation->destination_offset_in_section = info->destination_offset + 4;
            section_relocation->source_symbol_table_index     = (u32)info->source_declaration->symbol_table_index;
        }
    }
    
    // 
    // We are done writing the file. 
    // Patch in some final values, then write the file to disk.
    // 
    
    smm obj_size = arena_current(arena) - obj_base_address;
    
    coff_file_header->amount_of_sections      = (u16)section_header_count;
    coff_file_header->pointer_to_symbol_table = (u32)((u8 *)symbol_table_base - obj_base_address);
    coff_file_header->amount_of_symbols       = (u32)amount_of_symbols;
    
    if(!globals.dont_print_the_files_because_we_are_in_a_test_suite){
        begin_counter(context, write_obj);
        struct string output_file_path = globals.output_file_path;
        struct string obj_full_path = push_format_string(arena, "%.*s.obj", output_file_path.size, output_file_path.data);
        char *obj_name = (char *)obj_full_path.data;
        smm success = os_write_file(obj_name, obj_base_address, obj_size);
        end_counter(context, write_obj);
        
        if(success){
            print("Wrote file: '%s'\n", obj_name);
        }else{
            print("Error: Unable to write file '%s'.\n", obj_name);
            globals.an_error_has_occurred = true;
        }
    }
}



