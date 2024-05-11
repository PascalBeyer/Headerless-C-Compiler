
#if !time_perfomance
#define begin_counter(...)
#define end_counter(...)
#define collate_timing_info(...)
#define report_timing(...)
struct timing_events { u8 unused; };
struct timing_wrapper {struct timing_events timing_events;};
#else

// :timing :performance :counters

enum timing_event_kind{
    EVENT_begin_perf_counter,
    EVENT_end_perf_counter,
};

struct perf_timing_event{
    char *place;
    u64 kind_and_counter;
};

#define perf_event_counter(event) ((event)->kind_and_counter >> 1)
#define perf_event_kind(event)    ((event)->kind_and_counter &  1)

struct timing_bucket{
    struct timing_bucket *next;
    smm at;
    struct perf_timing_event events[0x1000 - 1];
};

static_assert(is_power_of_two(sizeof(struct timing_bucket)));

struct timing_events{
    struct timing_bucket *first;
    struct timing_bucket *last;
};

func void push_timing_event(struct timing_events *events, u64 counter, char *place, enum timing_event_kind kind){
    if(!events->last || events->last->at >= array_count(events->last->events)){
        void *memory = os_allocate_memory(sizeof(struct timing_bucket));
        assert(memory);
        struct timing_bucket *bucket = memory;
        sll_push_back(*events, bucket);
    }
    
    struct perf_timing_event *event = events->last->events + events->last->at++;
    event->kind_and_counter = counter << 1;
    event->kind_and_counter |= kind;
    event->place = place;
}

#define begin_counter(context, _place) \
push_timing_event(&(context)->timing_events, __rdtsc(), #_place, EVENT_begin_perf_counter);


#define end_counter(context, _place) \
push_timing_event(&(context)->timing_events, __rdtsc(), #_place, EVENT_end_perf_counter);

struct timing_wrapper{
    struct timing_events timing_events;
};

struct timing_source_node{
    struct timing_source_node *next;
    struct timing_events events;
};

struct timing_context{
    struct timing_info{
        char *name;
        u64 include_time;
        u64 exclude_time;
        u64 hit_count;
        u32 current_depth;
    } timing_infos[0x1000];
    
    struct{
        struct timing_source_node *first;
        struct timing_source_node *last;
    }source_list;
    
    smm timing_info_count;
    struct memory_arena arena;
};

func struct timing_info *find_or_add_timing_info(struct timing_context *context, char *name){
    for(smm i = 0; i < array_count(context->timing_infos); i++){
        if(!context->timing_infos[i].name){
            context->timing_infos[i].name = name;
            context->timing_info_count += 1;
            return context->timing_infos + i;
        }else if(cstring_match(context->timing_infos[i].name, name)){
            return context->timing_infos + i;
        }
    }
    return null;
}

func void collate_timing_info(struct timing_context *context, struct timing_events events){
    
    {
        struct timing_source_node *source_node = push_struct(&context->arena, struct timing_source_node);
        source_node->events = events;
        sll_push_back(context->source_list, source_node);
    }
    
    struct perf_timing_event *last_event = null;
    
    struct timing_node{
        struct timing_node *next;
        struct perf_timing_event *event;
        struct timing_info *info;
    };
    
    struct{
        struct timing_node *first;
        struct timing_node *last;
    } timing_stack = zero_struct;
    
    for(struct timing_bucket *bucket = events.first; bucket; bucket = bucket->next){
        for(smm i = 0; i < bucket->at; i++){
            struct perf_timing_event *event = bucket->events + i;
            
            struct timing_info *info = find_or_add_timing_info(context, event->place);
            
            //
            // We get a sequence of events like:
            //    event_a_begin
            //      event_b_begin
            //      event_b_end
            //      event_c_start
            //      event_c_end
            //      event_a_begin
            //      event_a_end
            //    event_a_end
            // This Sequence might repeat if in a loop or something.
            
            if(timing_stack.first){
                //
                // 'timing_stack.first' is always the one that is _currently_ executing.
                // So add the time that has passed in the currently executing code path to its exclude time.
                // @note: neither of the two events for this has to be a an event for the currently executing one.
                //        see the sequence above, where 'event_c_start - event_b_end' has to be added to the exclude 
                //        time for event_a.
                //
                timing_stack.first->info->exclude_time += (perf_event_counter(event) - perf_event_counter(last_event));
            }
            
            if(perf_event_kind(event) == EVENT_begin_perf_counter){
                info->hit_count += 1;
                info->current_depth += 1;
                
                struct timing_node *node = push_struct(&context->arena, struct timing_node);
                node->event = event;
                node->info  = info;
                sll_push_front(timing_stack, node);
            }else{
                assert(perf_event_kind(event) == EVENT_end_perf_counter);
                struct timing_node *node = timing_stack.first;
                sll_pop_front(timing_stack);
                
                if(!cstring_match(node->event->place, event->place)){
                    print("Timing data error: expected '%s' got '%s'\n", node->event->place, event->place);
                    os_panic(1);
                }
                assert(info == node->info);
                
                //
                // The include time is the time of the outer most event pair.
                //
                info->current_depth -= 1;
                if(info->current_depth == 0){
                    info->include_time += (perf_event_counter(event) - perf_event_counter(node->event));
                }
            }
            last_event = event;
        }
    }
    assert(sll_is_empty(timing_stack));
}

func void report_timing(struct timing_context *context, u64 time_in_cycles, f64 time_in_seconds){
    
    struct timing_info *place_timing_info = context->timing_infos;
    
    // @cleanup: real sort?
    for(u32 i = 0; i < context->timing_info_count - 1; i++){
        u64 max = place_timing_info[i].include_time;
        u32 index = i;
        for(u32 j = i + 1; j < context->timing_info_count; j++){
            if(place_timing_info[j].include_time > max){
                max = place_timing_info[j].include_time;
                index = j;
            }
        }
        struct timing_info temp = place_timing_info[i];
        place_timing_info[i] = place_timing_info[index];
        place_timing_info[index] = temp;
    }
    
    print("Performance counters: (include)                              cycles        hit       average  percent       time\n");
    for(u32 i = 0; i < context->timing_info_count; i++){
        
        f64 percent = (f64)(s64)place_timing_info[i].include_time/(f64)(s64)time_in_cycles;
        f64 avg = (f64)(s64)place_timing_info[i].include_time/(f64)(s64)place_timing_info[i].hit_count;
        print("   %-50s %13llu %10llu %13.0f %7.2f%% %9.3fs\n", place_timing_info[i].name, place_timing_info[i].include_time, place_timing_info[i].hit_count, avg, percent* 100.0, percent * time_in_seconds);
    }
    
    for(u32 i = 0; i < context->timing_info_count - 1; i++){
        u64 max = place_timing_info[i].exclude_time;
        u32 index = i;
        for(u32 j = i + 1; j < context->timing_info_count; j++){
            if(place_timing_info[j].exclude_time > max){
                max = place_timing_info[j].exclude_time;
                index = j;
            }
        }
        struct timing_info temp = place_timing_info[i];
        place_timing_info[i] = place_timing_info[index];
        place_timing_info[index] = temp;
    }
    
    print("\n\n");
    print("Performance counters: (exclude)                              cycles        hit       average  percent       time\n");
    for(u32 i = 0; i < context->timing_info_count; i++){
        f64 percent = (f64)(s64)place_timing_info[i].exclude_time/(f64)(s64)time_in_cycles;
        f64 avg = (f64)(s64)place_timing_info[i].exclude_time/(f64)(s64)place_timing_info[i].hit_count;
        print("   %-50s %13llu %10llu %13.0f %7.2f%% %9.3fs\n", place_timing_info[i].name, place_timing_info[i].exclude_time, place_timing_info[i].hit_count, avg, percent * 100.0, percent * time_in_seconds);
    }
    print("\n\n");
}

struct serializer{
    smm at;
    smm size;
    u8 *memory;
};

void serializer_append(struct serializer *serial, void *memory, smm size){
    if(serial->at + size > serial->size){
        smm new_size;
        if(serial->size){
            new_size = serial->size * 2;
        }else{
            new_size = mega_bytes(1);
        }
        
        u8 *new_memory = os_allocate_memory(new_size);
        
        if(serial->size){
            memcpy(new_memory, serial->memory, serial->size);
            os_free_memory(serial->memory);
        }
        
        serial->memory = new_memory;
        serial->size = new_size;
    }
    
    assert(serial->at + size <= serial->size);
    memcpy(serial->memory + serial->at, memory, size);
    serial->at += size;
}

void dump_timing_data(struct timing_context *context, char *file_name){
    struct serializer serial = zero_struct;
    smm amount = 0;
    for(smm i = 0; i < array_count(context->timing_infos); i++, amount++){
        if(!context->timing_infos[i].name) break;
        char *string = context->timing_infos[i].name;
        serializer_append(&serial, string, cstring_length(string) + 1);
    }
    
    for(struct timing_source_node *source = context->source_list.first; source; source = source->next){
        for(struct timing_bucket *bucket = source->events.first; bucket; bucket = bucket->next){
            for(smm i = 0; i < bucket->at; i++){
                struct perf_timing_event *event = bucket->events + i;
                struct timing_info *info = find_or_add_timing_info(context, event->place);
                smm index = info - context->timing_infos;
                assert(index < amount);
                serializer_append(&serial, &index, sizeof(index));
                serializer_append(&serial, &event->kind_and_counter, sizeof(event->kind_and_counter));
            }
        }
    }
    
    os_write_file(file_name, serial.memory, serial.at);
}

#endif
