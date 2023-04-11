#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_surface.h>
#include <SDL2/SDL_stdinc.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h>

// #include "Types/types.h"
#include "Types/idot.h"
#include "Types/iline.h"
#include "Types/floatx.h"
#include "Types/types.h"
#include "DynArr/dynarr.h"
#include "Dict/dict.h"
#include "utils.h"

#define MIN(a,b) ((a) < (b)) ? (a) : (b) 

int TCP_ConnectTo(char *ipv4_addr, char *port);
int SelectSingleton(int fd, int timepout);
void do_nothing(void) {}

enum LineMode {
    Off = 0,
    Start = 1,
    End = 4,
};

typedef struct State {
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Surface *surface;

    void *item_buf;
    DrawableHeader *header_buf;
    void *data_buf;

    DynArr points;
    DynArr lines;

    int socket_fd;
    bool connected;

    enum LineMode line_mode;
    Float2 begin;
    Float2 end;
} State;

State run_app(
    State state,
    void (*on_available_data)(State *), 
    void (*on_event)(State *, SDL_Event),
    void (*render)(State *)) 
{
    SDL_Event event;

    while (state.connected) {
        SDL_PollEvent(&event);

        if (event.type == SDL_QUIT) {
			printf("The user closed the application\n");
            state.connected = false;
            break;
        }

        switch (SelectSingleton(state.socket_fd, 1000)) {
            case -1:
                fprintf(stderr, "select error");
                break;
            case 0:
                // just a timeout, carry on, we don't care
                break;
            default:
                printf("About to receive some bytes\n");
                on_available_data(&state);
        }

        // draw, send and store
        on_event(&state, event);
        render(&state);

        if (state.socket_fd < 0) {
            SDL_ShowSimpleMessageBox(0, "Coonection error", "Connection to the server has been severed", state.window);
            break;
        }

        memset(state.item_buf, 0, MAX_TYPE_SIZE);
    }

    return state;
}

void OnEvent(State *state, SDL_Event event) {

    if (event.type == SDL_KEYDOWN) {
        printf("To start\n");
        state->line_mode = Start;
    } else if (event.type == SDL_MOUSEBUTTONDOWN && state->line_mode == Start) {
        printf("To end (%i, %i)\n", event.button.x, event.button.y);
        state->begin = Float2_New((float) event.motion.x, (float) event.motion.y);
        state->line_mode = End; 
    } else if(event.type == SDL_MOUSEBUTTONUP && state->line_mode == End) {
        state->end = Float2_New((float) event.motion.x, (float) event.motion.y);

        ILine temp_line = ILine_New(
            state->begin.x,
            state->begin.y,
            state->end.x,
            state->end.y
        );

        printf("NEW LINE: (%f, %f, %f, %f)\n", 
            state->begin.x,
            state->begin.y,
            state->end.x,
            state->end.y
        );

        int send_len = TCP_Send(&(state->socket_fd), &temp_line, sizeof(ILine), 0);

        if (send_len == -1)
            return;

        DynArr_Push(&(state->lines), &temp_line.start);

        state->line_mode = Off;
        return;
    } else if (event.type == SDL_MOUSEMOTION && state->line_mode == Off) {
        bool mouse_left_down = SDL_BUTTON_LMASK == SDL_BUTTON(event.motion.state);

        if (mouse_left_down) {
 
            if (state->line_mode == Start) {
                state->begin = Float2_New((float) event.motion.x, (float) event.motion.y);
                state->line_mode = End;
                
                return;
            } else if (state->line_mode == End) {

            }

            IDotData temp_dot = IDotData_New((float) event.motion.x, (float) event.motion.y);
            int send_len = TCP_Send(&(state->socket_fd), &temp_dot, sizeof(IDotData), 0);

            if (send_len == -1)
                return;

            DynArr_Push(&(state->points), &temp_dot.x);
        }
    }

    fflush(stdout);
}

void OnRender(State *state) {
    SDL_SetRenderDrawColor(state->renderer, 
                        0x20, 
                        0x20, 
                        0x20, 
                        0x20);
    SDL_RenderClear(state->renderer);

    SDL_SetRenderDrawColor(state->renderer, 0xFF, 0xFF, 0xFF, 0xFF);
    
    SDL_RenderDrawPointsF(state->renderer, state->points.arr_ptr, state->points.size);
    DynArr_ForEachPtr(state->lines, el, {
        SDL_RenderDrawLinesF(state->renderer, el, 2);
    });

    SDL_RenderPresent(state->renderer);
}

void OnAvailableData(State *state) {
    int recv_bytes = TCP_Recv(
        &(state->socket_fd),
        state->header_buf,
        sizeof(DrawableHeader),
        0);
        
    printf("Header received: ");
    DrawableHeader_Printf(*(state->header_buf));
    printf("\n");

    recv_bytes = TCP_Recv(
        &(state->socket_fd), 
        state->data_buf, 
        MIN(state->header_buf->size, MAX_TYPE_SIZE),
        0
    );

    printf("Got %i bytes\n", recv_bytes);

    if (recv_bytes == -1) {
        // TODO: pop window
        state->connected = false;
        return ;
    }

    switch (state->header_buf->type_id) {
        case ITEM_DOT_TYPE_ID:
            DynArr_Push(&(state->points), state->data_buf);
            break;
        case ITEM_LINE_TYPE_ID:
            DynArr_Push(&(state->lines), state->data_buf); 
            break;
        default:
            printf("Got a malformed message\n");
            break;
    }
}

// TODO: waiting_to_send queue for failed sends
int main(int argc, char *argv[]) {

    if (argc != 3){
        fprintf(stderr, "USAGE: <ip> <port>\n");
        exit(EXIT_FAILURE);
    }

    int server_handle = TCP_ConnectTo(argv[1], argv[2]);

    printf("Succesfull connect\n");

    switch (server_handle) {
        case -1:
            fprintf(stderr, "Specified port \"%s\" is invalid\n", argv[1]);
            exit(EXIT_FAILURE);
        case -2:
            fprintf(stderr, "Cannot create socket.\n");
            exit(EXIT_FAILURE);
        case -3:
            fprintf(stderr, "Invalid remote IP address.\n");
            exit(EXIT_FAILURE);
        case -4:
            fprintf(stderr,"ERROR #4: error in connect().\n");
            exit(EXIT_FAILURE);
    }

    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Surface *surface = NULL;

    // if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS) != 0) {
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        fprintf(stderr, "Could not initialize sdl2: %s\n", SDL_GetError()); 
        return EXIT_FAILURE;
    }

    if (SDL_CreateWindowAndRenderer(500, 500, SDL_WINDOW_RESIZABLE, &window, &renderer) != 0) {
        fprintf(stderr, "Couldn't create window and renderer: %s", SDL_GetError());
        return EXIT_FAILURE;
    }

    State state = {
        .connected = true,
        .socket_fd = server_handle,

        .line_mode = Off,

        .item_buf = calloc(MAX_TYPE_SIZE, 1), 
        .header_buf = (DrawableHeader *) state.item_buf,
        .data_buf = state.header_buf + 1,

        .points = DynArr_WithCapacity(2048, sizeof(Float2)),
        .lines = DynArr_WithCapacity(128, sizeof(ILine)),

        .window = window,
        .renderer = renderer,
        .surface = surface,
    };

    state = run_app(state, OnAvailableData, OnEvent, OnRender);

    // cleaning up
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    DynArr_Free(state.points);
    
    return 0;
}


#define CONNECT_INVALID_PORT -1
#define CONNECT_SOCKET_INIT_ERR -2
#define CONNECT_INVALID_IP_ADDR -3
#define CONNECT_TO_HOST_ERR -4

// 0 > | Success, value is valid 
// -1  | Invalid port
// -2  | Failure to create a socket
// -3  | Invalid IP address
// -4  | Failure to connect
int TCP_ConnectTo(char *ipv4_addr, char *port_str) {
    struct sockaddr_in servaddr; // Serverio address struct

    // port validation
    int port = atoi(port_str);

    if ((port < 1) || (port > 65535)) {
        return -1;
    }

    // socket init
    int socket_handle = socket(AF_INET, SOCK_STREAM,0);

    if ( socket_handle < 0) {
        return -2;
    }

    memset(&servaddr,0,sizeof(servaddr));
    servaddr.sin_family = AF_INET; 
    servaddr.sin_port = htons(port);

    if ( inet_aton(ipv4_addr, &servaddr.sin_addr) <= 0 ) {
        return -3;
    }

    // connecting to server
    if (connect(socket_handle,(struct sockaddr*)&servaddr,sizeof(servaddr))<0){
        return -4;
    }

    return socket_handle;
}