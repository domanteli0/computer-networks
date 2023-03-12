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

// TODO: use header files
#include "types.c"
#include "dynarr.c"

#define BUFFLEN 1024

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

// TODO: Translate mouse coordinates to Canvas coordinates
int main(int argc, char *argv[]) {
    char buffer[BUFFLEN];

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

    DynArr points = DynArr_WithCapacityS(1024, sizeof(Float2));

    while (1) {
        SDL_PollEvent(&event);

        if (event.type == SDL_QUIT) {
            exit(0);
            break;
        }

        SDL_SetRenderDrawColor(renderer, 
                            0x20, 
                            0x20, 
                            0x20, 
                            0x20);
        SDL_RenderClear(renderer);

        // draw
        if (event.type == SDL_MOUSEMOTION) {
            bool mouse_left_down = SDL_BUTTON_LMASK == SDL_BUTTON(event.motion.state);

            // event.motion.x
            // event.motion.xrel

            if (mouse_left_down) {
                Float2 temp = Float2_New(event.motion.x, event.motion.y);
                // TODO: send point data to server
                DynArr_Push(&points, &temp);
            }
        }

        SDL_SetRenderDrawColor(renderer, 0xFF, 0xFF, 0xFF, 0xFF);
        SDL_RenderDrawPointsF(renderer, points.arr_ptr, points.size);

        SDL_RenderPresent(renderer);
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    DynArr_Free(points);

    printf("Enter the message: ");
    fgets(buffer, BUFFLEN, stdin);

    // sending stuff to server
    send(server_handle, buffer, strlen(buffer), 0);

    memset(&buffer,0,BUFFLEN);

    // receiving from server
    recv(server_handle, buffer, BUFFLEN, 0);
    printf("Server sent: %s\n", buffer);

    // cleaning up
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