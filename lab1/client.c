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

// TODO: use header files
#include "types.c"

#include "dynarr.h"
#include "dict.h"
#include "utils.h"

int connect_to(char *ipv4_addr, char *port);

Float2 ScreenToCanvasPointTranslation(Float2 windowSize, Float2 positionInWindow) {
    // TODO: Implement translation
    return positionInWindow;
}

char *SDL_MouseMotionEventToString(SDL_MouseMotionEvent e) {
    const size_t STR_LEN = 1024;
    char *str = calloc(STR_LEN, 1);

    sprintf(str, "x: %i, y: %i", e.x, e.y);

    return str;
}

typedef struct ClientState {
    uint64_t identifier;
    DynArr points;
} ClientState;

void do_nothing() {}

// TODO: Translate mouse coordinates to Canvas coordinates
// TODO: waiting_to_send queue for failed sends
int main(int argc, char *argv[]) {
    if (argc != 3){
        fprintf(stderr, "USAGE: <ip> <port>\n");
        exit(EXIT_FAILURE);
    }

    int server_handle = connect_to(argv[1], argv[2]);


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

    ClientState client_state = {
        .identifier = 0,
        .points = DynArr_WithCapacity(1024, sizeof(Float2)),
    };

    // recv(server_handle, &client_state.identifier, sizeof(uint32_t), 0);
    // printf("ident: %u\n", client_state.identifier);

    // recv(server_handle, &client_state.identifier, sizeof(uint32_t), 0);
    // printf("ident: %u\n", client_state.identifier);

    // printf("nothing looks like: %i\n", recv(
    //     server_handle,
    //     &client_state,
    //     sizeof(__uint128_t),
    //     0
    // ));

    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Surface *surface;
    SDL_Event event;

    // if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS) != 0) {
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        fprintf(stderr, "Could not initialize sdl2: %s\n", SDL_GetError()); 
        return EXIT_FAILURE;
    }

    if (SDL_CreateWindowAndRenderer(500, 500, SDL_WINDOW_RESIZABLE, &window, &renderer) != 0) {
        fprintf(stderr, "Couldn't create window and renderer: %s", SDL_GetError());
        return EXIT_FAILURE;
    }

    DynArr points = DynArr_WithCapacity(1024, sizeof(Float2));

    // TODO: retrieve state

    fd_set read_fds;
    FD_ZERO(&read_fds);
    FD_SET(server_handle, &read_fds);

    while (1) {
        SDL_PollEvent(&event);

        if (event.type == SDL_QUIT) {
            exit(0);
            break;
        }

        // TODO: Check if server is alive

        // handle receiving data 
        {
            // this set consists only of one handle, that is the server
            fd_set read_set;
            FD_ZERO(&read_set);
            FD_SET(server_handle, &read_set);
            struct timeval timeout = timeval_FromMicro(10000);

            switch (select(server_handle + 1, &read_set, NULL, NULL, &timeout)) {
                case -1:
                    fprintf(stderr, "select error");
                    break;
                case 0:
                    // just a timeout, just carry on, we don't care
                    break;
                default:
                    do_nothing(); // literally, does nothing, but doesn't compile if I remove it

                    printf("About to receive some bytes\n");

                    int foo = 0;
                    // yay, let's read some data
                    uint32_t type_id = 0;

                    int recv_bytes = recv(
                        server_handle,
                        &type_id,
                        sizeof(uint32_t),
                        0);
                        
                    printf("Bytes received: %i\n", recv_bytes);
                    assert(recv_bytes == sizeof(Header));

                    // I was planning to more types than dots
                    // so just imagine type_id is checked and appropiate type is sellected
                    Float2 dot;
                    recv_bytes = recv(server_handle, &dot, sizeof(Float2), 0);
                    printf("Got %i bytes and a float2: %s\n\n", recv_bytes, Float2_ToString(dot));

                    DynArr_Push(&points, &dot);
            }

        }

        SDL_SetRenderDrawColor(renderer, 
                            0x20, 
                            0x20, 
                            0x20, 
                            0x20);
        SDL_RenderClear(renderer);

        // TODO: if poll, retrive new data

        // TODO: Generalize this:
        // dict: key - (UiMode, event), value - fn(...) -> struct Item

        // draw
        if (event.type == SDL_MOUSEMOTION) {
            bool mouse_left_down = SDL_BUTTON_LMASK == SDL_BUTTON(event.motion.state);

            // event.motion.x
            // event.motion.xrel

            if (mouse_left_down) {
                printf("Drawing dots\n");

                Float2 temp = Float2_New(event.motion.x, event.motion.y);
                IDotData temp_dot = IDot_FromFloat2(temp);

                send(server_handle, &temp_dot, sizeof(IDotData), 0);
                printf("IDotData sent: %s\n", IDot_ToString(temp_dot));
                DynArr_Push(&points, &temp);
            }
        }

        SDL_SetRenderDrawColor(renderer, 0xFF, 0xFF, 0xFF, 0xFF);
        SDL_RenderDrawPointsF(renderer, points.arr_ptr, points.size);

        SDL_RenderPresent(renderer);
    }

    // cleaning up
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    DynArr_Free(points);
    
    close(server_handle);
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
int connect_to(char *ipv4_addr, char *port_str) {
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