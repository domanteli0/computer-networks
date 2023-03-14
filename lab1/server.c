#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/select.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>

#include "dynarr.h"
#include "types.c"

#define MAXCLIENTS 10

int findemptyuser(int c_sockets[]){
    int i;
    for (i = 0; i <  MAXCLIENTS; i++){
        if (c_sockets[i] == -1){
            return i;
        }
    }
    return -1;
}

int main(int argc, char *argv[]){

    if (argc != 2){
        fprintf(stderr, "USAGE: <port>\n");
        return -1;
    }

    unsigned int port = atoi(argv[1]);
    unsigned int clientaddrlen;
    int l_socket;
    int c_sockets[MAXCLIENTS];
    fd_set read_set;

    struct sockaddr_in servaddr;
    struct sockaddr_in clientaddr;

    int maxfd = 0;

    if ((port < 1) || (port > 65535)){
        fprintf(stderr, "ERROR #1: invalid port specified.\n");
        return -1;
    }

    if ((l_socket = socket(AF_INET, SOCK_STREAM,0)) < 0){
        fprintf(stderr, "ERROR #2: cannot create listening socket.\n");
        return -1;
    }

    memset(&servaddr,0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY); 
    servaddr.sin_port = htons(port);

    if (bind (l_socket, (struct sockaddr *)&servaddr,sizeof(servaddr))<0){
        fprintf(stderr,"ERROR #3: bind listening socket.\n");
        return -1;
    }

    if (listen(l_socket, 5) <0){
        fprintf(stderr,"ERROR #4: error in listen().\n");
        return -1;
    }                           

    for (size_t ix = 0; ix < MAXCLIENTS; ix++){
        c_sockets[ix] = -1;
    }

	DynArr dots = DynArr_WithCapacity(1024, sizeof(Float2));

	// this array is pre-filled only for testing purposes
	for (size_t ix = 0; ix < 128; ++ix) {
		Float2 temp = Float2_New((float) ix, (float) ix);
		DynArr_Push(&dots, &temp);
	}

    while(true) {
        FD_ZERO(&read_set);
        for (size_t ix = 0; ix < MAXCLIENTS; ix++){
            if (c_sockets[ix] != -1){
                FD_SET(c_sockets[ix], &read_set);
                if (c_sockets[ix] > maxfd){
                    maxfd = c_sockets[ix];
                }
            }
        }

        FD_SET(l_socket, &read_set);
        if (l_socket > maxfd){
            maxfd = l_socket;
        }
        
        select(maxfd+1, &read_set, NULL , NULL, NULL);

        if (FD_ISSET(l_socket, &read_set)){
            int client_id = findemptyuser(c_sockets);
            if (client_id != -1){
                clientaddrlen = sizeof(clientaddr);
                memset(&clientaddr, 0, clientaddrlen);
                c_sockets[client_id] = accept(l_socket, 
                    (struct sockaddr*)&clientaddr, &clientaddrlen);
                printf("Connected:  %s\n",inet_ntoa(clientaddr.sin_addr));

				DynArr_ForEach(dots, Float2, dot, {
					IDot temp = IDot_FromFloat2(*dot);
					send(client_id, &temp, sizeof(Float2), 0);
				});
            }
        }

        for (size_t ix = 0; ix < MAXCLIENTS; ix++){
            if (c_sockets[ix] != -1){
                if (FD_ISSET(c_sockets[ix], &read_set)){
					IDot dot;
                    int r_len = recv(c_sockets[ix],&dot,sizeof(IDot),0);
					printf("Bytes rececived: %i, sizeof(%zu)\n", r_len, sizeof(IDot));
					printf("%s\n", IDot_ToString(dot));
					assert(r_len == sizeof(IDot));
					DynArr_Push(&dots, &dot);

                    for (size_t jx = 0; jx < MAXCLIENTS; jx++) {
                        if (c_sockets[jx] != -1 && c_sockets[jx] != c_sockets[ix]){
                            int w_len = send(c_sockets[jx], &dot, r_len, 0);
                            if (w_len <= 0){
                                close(c_sockets[jx]);
                                c_sockets[jx] = -1;
                            }
                        }
                    }
                }
            }
        }


    }

    return 0;
}

bool any_alive(int *sockets, size_t num_of_socks) {
	const int NOT_ALIVE = -1;
	bool any_alive = false;

	for (size_t ix = 0; ix < num_of_socks; ++ix) {
		if(sockets[ix] != NOT_ALIVE ) {
			any_alive = true;
			break;
		}
	}	

	return any_alive;
}

