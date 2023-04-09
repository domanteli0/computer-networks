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
#include "DynArr/dynarr.h"
#include "Dict/dict.h"
#include "utils.h"

int TCP_ConnectTo(char *ipv4_addr, char *port);
int SelectSingleton(int fd, int timepout);
void do_nothing(void) {}

Float2 ScreenToCanvasPointTranslation(Float2 windowSize, Float2 positionInWindow) {
    // TODO: Implement translation
    return positionInWindow;
}

void Float2_PrintFromPtr(Float2 *f2) {
    printf("(%f, %f)\n", f2->x, f2->y);
}

char *SDL_MouseMotionEventToString(SDL_MouseMotionEvent e) {
    const size_t STR_LEN = 1024;
    char *str = calloc(STR_LEN, 1);

    sprintf(str, "x: %i, y: %i", e.x, e.y);

    return str;
}

typedef struct State {
    bool connected;
} State;

// TODO: waiting_to_send queue for failed sends
int main(int argc, char *argv[]) {

    assert(sizeof(uint32_t) == sizeof(Float1));

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

    DynArr points = DynArr_WithCapacity(2 * 1024, sizeof(Float2));
    DynArr lines = DynArr_WithCapacity(128, sizeof(ILine));
    State state = {
        .connected = true
    };

    // TODO: retrieve state

    while (state.connected) {
        SDL_PollEvent(&event);

        if (event.type == SDL_QUIT) {
			printf("The user closed the application\n");
            state.connected = false;
            break;
        }

        // TODO: Check if server is alive

        // handle receiving data 
        {
            fd_set read_set = fd_set_Singleton(server_handle);
            struct timeval timeout = timeval_FromMicro(10000);

            switch (select(server_handle + 1, &read_set, NULL, NULL, &timeout)) {
            // switch (SelectSingleton(server_handle, 1000)) {
                case -1:
                    fprintf(stderr, "select error");
                    break;
                case 0:
                    // just a timeout, just carry on, we don't care
                    break;
                default:
                    do_nothing(); // literally, does nothing, but doesn't compile if I remove it

                    printf("About to receive some bytes\n");

                    DrawableHeader header = {
                        .size = 0,
                        .type_id = -1,
                    };

                    int recv_bytes = TCP_Recv(
                        &server_handle,
                        &header,
                        sizeof(DrawableHeader),
                        0);
                        
                    printf("Bytes received: %i\n", recv_bytes);
                    printf("of type_id: 0x%" PRIX16 "\n", header.type_id);

                    // I was planning to more types than dots
                    // so just imagine type_id is checked and appropiate type is sellected
                    Float2 dot;
                    recv_bytes = TCP_Recv(&server_handle, &dot, sizeof(Float2), 0);
                    printf("Got %i bytes and a float2: %s\n\n", recv_bytes, Float2_ToString(dot));

                    if (recv_bytes == -1) {
                        // TODO: pop window
                        state.connected = false;
                        break;
                    }
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

        // draw, send and store
        if (event.type == SDL_MOUSEMOTION) {
            bool mouse_left_down = SDL_BUTTON_LMASK == SDL_BUTTON(event.motion.state);

            // event.motion.x
            // event.motion.xrel

            if (mouse_left_down) {
                printf("Drawing dots\n");

                Float2 temp_f2 = Float2_New((float) event.motion.x, (float) event.motion.y);
                IDotData temp_dot = IDotData_FromFloat2(temp_f2);
                printf("BEFORE SEND: (%f, %f)\n", temp_dot.x, temp_dot.y);
                fflush(stdout);

                uint32_t *t_ptr = (uint32_t *) &temp_dot;

                printf("Values arth:  Size: %" PRIu16 ", Type: 0x%" PRIX16 " (%f %f)\n",
                    *(((uint16_t *) t_ptr) + 1),
                    *(uint16_t *) t_ptr,
                    *((Float1 *) (t_ptr + 1)),
                    *((Float1 *) (t_ptr + 2))
                );
                fflush(stdout);

                int send_len = TCP_Send(&server_handle, &temp_dot, sizeof(IDotData), 0);

                if (send_len == -1) {
                    break;
                }

                DynArr_Push(&points, &temp_f2);
            }

            if (server_handle == -1) {
                break;
            }
        }

        SDL_SetRenderDrawColor(renderer, 0xFF, 0xFF, 0xFF, 0xFF);
        SDL_RenderDrawPointsF(renderer, points.arr_ptr, points.size);

        SDL_RenderPresent(renderer);

        if (server_handle < 0) {
            printf("Connection to the server has been severed.\n");
            break;
        }
    }

	DynArr_ForEach(points, Float2, dot, {
		printf("Ending dot: (%f, %f)\n", dot.x, dot.y);
	});

    // cleaning up
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    DynArr_Free(points);
    
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