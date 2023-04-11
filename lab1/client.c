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
    bool is_drawing_mode;
    Float2 begin;
    Float2 end;
} State;

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

    void *item_buf = calloc(MAX_TYPE_SIZE, 1);
    DrawableHeader *header_buf = (DrawableHeader *) item_buf;
    void *data_buf = header_buf + 1;

    DynArr points = DynArr_WithCapacity(2048, sizeof(Float2));
    DynArr lines = DynArr_WithCapacity(128, sizeof(ILine));
    State state = {
        .connected = true,
        .is_drawing_mode = false,
    };

    Float4 temp_f4 = Float4_New(200, 200, 200, 300);
    DynArr_Push(&lines, &temp_f4);

    // TODO: retrieve state

    while (state.connected) {
        SDL_PollEvent(&event);

        if (event.type == SDL_QUIT) {
			printf("The user closed the application\n");
            state.connected = false;
            break;
        }

        // this handles incoming data 
        {
            switch (SelectSingleton(server_handle, 1000)) {
                case -1:
                    fprintf(stderr, "select error");
                    break;
                case 0:
                    // just a timeout, just carry on, we don't care
                    break;
                default:
                    do_nothing(); // literally, does nothing, but doesn't compile if I remove it

                    printf("About to receive some bytes\n");

                    int recv_bytes = TCP_Recv(
                        &server_handle,
                        header_buf,
                        sizeof(DrawableHeader),
                        0);
                        
                    printf("Header received: ");
                    DrawableHeader_Printf(*header_buf);
                    printf("\n");

                    recv_bytes = TCP_Recv(
                        &server_handle, 
                        data_buf, 
                        MIN(header_buf->size, MAX_TYPE_SIZE),
                        0
                    );

                    printf("Got %" PRIu16 " bytes\n", recv_bytes);

                    if (recv_bytes == -1) {
                        // TODO: pop window
                        state.connected = false;
                        break;
                    }

                    switch (header_buf->type_id)
                    {
                    case ITEM_DOT_TYPE_ID:
                        DynArr_Push(&points, data_buf);
                        break;
                    case ITEM_LINE_TYPE_ID:
                        DynArr_Push(&lines, data_buf); 
                        break;
                    default:
                        printf("Got a malformed message\n");
                        break;
                    }
            }

        }

        SDL_SetRenderDrawColor(renderer, 
                            0x20, 
                            0x20, 
                            0x20, 
                            0x20);
        SDL_RenderClear(renderer);

        // draw, send and store
        if (event.type == SDL_MOUSEMOTION) {
            bool mouse_left_down = SDL_BUTTON_LMASK == SDL_BUTTON(event.motion.state);

            if (mouse_left_down) {
                IDotData temp_dot = IDotData_New((float) event.motion.x, (float) event.motion.y);
                uint32_t *t_ptr = (uint32_t *) &temp_dot;

                int send_len = TCP_Send(&server_handle, &temp_dot, sizeof(IDotData), 0);

                if (send_len == -1) {
                    printf("connection severed. quiting application\n");
                    break;
                }

                DynArr_Push(&points, &temp_dot.x);
            }

            if (server_handle == -1) {
                break;
            }
        }

        SDL_SetRenderDrawColor(renderer, 0xFF, 0xFF, 0xFF, 0xFF);
        SDL_RenderDrawPointsF(renderer, points.arr_ptr, points.size);

        DynArr_ForEachPtr(lines, el, {
            SDL_RenderDrawLinesF(renderer, el, 2);
        });

        SDL_RenderPresent(renderer);

        if (server_handle < 0) {
            printf("Connection to the server has been severed.\n");
            break;
        }

        memset(item_buf, 0, MAX_TYPE_SIZE);
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