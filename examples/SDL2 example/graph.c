

// for this to compile, you need to paste have 'SDL2.dll' in your path and 
// paste the 'include' sub directory from the sdl source into this directory,
// or use the root folder of 'SDL2' as additional include directory.

// This is is intended to show that linking to 3rd party dll's works just fine.

#include "include/SDL.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// @note: <assert.h> uses wide strings, they are not yet supported.
//        so here is an easy define for assert.
#define assert(a) (!(a) ? (printf("Assert '%s' fired in '%s' at line %d of '%s'\n", #a, __FUNCTION__, __LINE__, __FILE__), 0) : 1)

typedef int8_t    s8;
typedef uint8_t   u8;
typedef int16_t   s16;
typedef uint16_t  u16;
typedef int32_t   s32;
typedef uint32_t  u32;
typedef int64_t   s64;
typedef uint64_t  u64;
typedef intptr_t  smm;
#define true  1
#define false 0

struct render_context{
    u32 window_width;
    u32 window_height;
    SDL_Window *window;
    SDL_Renderer *renderer;
    
    int should_exit;
};

void handle_input(struct render_context *context){
    SDL_Event event;
    
    while(SDL_PollEvent(&event)){
        switch(event.type){
            case SDL_QUIT:{
                context->should_exit = true;
            }break;
            case SDL_KEYDOWN:{
                switch (event.key.keysym.scancode) { 
                    case SDL_SCANCODE_ESCAPE: context->should_exit = true; break;
                }
            }break;
        }
    }
}

void render(struct render_context *context, struct point *points, smm amount){
    SDL_SetRenderDrawColor(context->renderer, 0xff, 0xff, 0xff, 0xff);
    SDL_RenderClear(context->renderer);
    
    // Draw a coordinate cross
    SDL_SetRenderDrawColor(context->renderer, 0, 0, 0, 0xff);
    SDL_RenderDrawLine(context->renderer, 100, 900, 900, 900);
    SDL_RenderDrawLine(context->renderer, 890, 890, 900, 900);
    SDL_RenderDrawLine(context->renderer, 890, 910, 900, 900);
    
    SDL_RenderDrawLine(context->renderer, 100, 900, 100, 100);
    SDL_RenderDrawLine(context->renderer,  90, 110, 100, 100);
    SDL_RenderDrawLine(context->renderer, 110, 110, 100, 100);
    
    // "Desire Red Shade"
    SDL_SetRenderDrawColor(context->renderer, 0xEA, 0x3C, 0x53, 0xff);
    for(smm i = 0; i < amount; i++){
        smm size = 3;
        
        int x = 100 + points[i].x;
        int y = 100 + points[i].y;
        
        // make a nice little cross
        SDL_RenderDrawLine(context->renderer, x - size, y - size, x + size, y + size);
        SDL_RenderDrawLine(context->renderer, x - size, y + size, x + size, y - size);
    }
    
    SDL_RenderPresent(context->renderer);
    SDL_Delay(1000/60);
}

struct render_context init_render_context(u32 window_width, u32 window_height){
    if(SDL_Init(SDL_INIT_EVERYTHING) != 0){
        printf("error initializing SDL: %s\n", SDL_GetError());
        exit(1);
    }
    
    struct render_context render_context = {};
    render_context.window_width  = window_width;
    render_context.window_height = window_height;
    
    SDL_Window* window = SDL_CreateWindow("GAME", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, window_width, window_height, 0); 
    assert(window);
    
    render_context.window = window;
    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    assert(renderer);
    render_context.renderer = renderer;
    
    return render_context;
}


static struct string load_entire_file(char *file_path){
   struct string ret = {};
   FILE *file = fopen(file_path, "rb");
   if(file == NULL){
      printf("Cannot open file '%s'\n", file_path);
      return ret;
   }
   fseek(file, 0, SEEK_END);
   smm size = ftell(file);
   fseek(file, 0, SEEK_SET);
   
   ret.data = (u8 *)malloc(size + 1);
   
   smm bytes_read = fread(ret.data, 1, size, file);
   if(bytes_read != size){
      printf("Warning: loaded truncated file '%s'\n", file_path);
      size = bytes_read;
   }
   ret.size = size; 
   ret.data[size] = 0;
   
   return ret;
}

struct string{
    u8 *data;
    smm size;
};

void eat_leading_whitespace(struct string *string){
    for(smm i = 0; i < string->size; i++){
        if(string->data[i] == ' ' || string->data[i] == '\n' || string->data[i] == '\t'){
            
        }else{
            string->data += i;
            string->size -= i;
            return;
        }
    }
    
    *string = (struct string){};
}

int read_int(struct string *string){
    eat_leading_whitespace(string);
    
    int ret = 0;
    for(smm i = 0; i < string->size; i++){
        if('0' <= string->data[i] && string->data[i] <= '9'){
            ret = 10 * ret + (string->data[i] - '0');
        }else{
            string->data += i;
            string->size -= i;
            return ret;
        }
    }
    
    *string = (struct string){};
    return ret;
}

struct point{
    int x;
    int y;
};

int _start(){
    struct render_context render_context = init_render_context(1000, 1000);
    
    struct string file = load_entire_file("data.txt");
    struct point points[0x100];
    smm point_at = 0;
    while(file.size){
        int x = read_int(&file);
        int y = read_int(&file);
        
        if(point_at > sizeof(points)/sizeof(*points)){
            break;
        }else{
            points[point_at].x = x;
            points[point_at].y = y;
            point_at += 1;
        }
    }
    
    while(!render_context.should_exit){
        handle_input(&render_context);
        render(&render_context, points, point_at);
    }
    
    exit(0);
}

