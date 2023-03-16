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
#include <fcntl.h>

#include "DynArr/dynarr.h"
#include "Dict/dict.h"
#include "Types/types.c"

#define MAXCLIENTS 10

int main(int argc, char *argv[]){

    if (argc != 2){
        fprintf(stderr, "USAGE: <port>\n");
        return -1;
    }

    unsigned int port = atoi(argv[1]);
    unsigned int clientaddrlen;
    int l_socket;

	DynArr c_sockets = DynArr_WithCapacity(8, sizeof(int));
    // int c_sockets[MAXCLIENTS];
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

    // for (size_t ix = 0; ix < MAXCLIENTS; ix++){
    //     c_sockets[ix] = -1;
    // }

	DynArr dots = DynArr_WithCapacity(1024, sizeof(Float2));

	// this array is pre-filled only for testing purposes
	for (size_t ix = 0; ix < 128; ++ix) {
		Float2 temp = Float2_New((float) ix, (float) ix);
		DynArr_Push(&dots, &temp);
	}

    while(true) {

        FD_ZERO(&read_set);
		DynArr_ForEach(c_sockets, int, c_sock, {
			FD_SET(c_sock, &read_set);
			if (c_sock > maxfd){
				maxfd = c_sock;
			}
		});

        FD_SET(l_socket, &read_set);
        if (l_socket > maxfd){
            maxfd = l_socket;
        }
        
        select(maxfd+1, &read_set, NULL , NULL, NULL);

		bool new_incoming = FD_ISSET(l_socket, &read_set);
		bool can_accept = c_sockets.size < MAXCLIENTS; 

        if (new_incoming && can_accept) {
			clientaddrlen = sizeof(clientaddr);
			memset(&clientaddr, 0, clientaddrlen);

			int new_con = accept(l_socket, 
				(struct sockaddr*)&clientaddr, &clientaddrlen);

			DynArr_Push(&c_sockets, &new_con);

			printf("Connected:  %s\n",inet_ntoa(clientaddr.sin_addr));

			DynArr_ForEach(dots, Float2, dot, {
				IDotData temp = IDotData_FromFloat2(dot);
				send(new_con, &temp, sizeof(IDotData), 0);
			});
        }

		DynArr_ForEach(c_sockets, int, ci_sock, {
			if (!FD_ISSET(ci_sock, &read_set)) {
				continue;
			}

			int i_sock = ci_sock;

			IDotData dot;
			int r_len = recv(i_sock, &dot, sizeof(IDotData), 0);
			printf("Bytes rececived: %i, sizeof(%zu)\n", r_len, sizeof(IDotData));
			printf("%s\n", IDotData_ToString(dot));
			// assert(r_len == sizeof(IDotData));
			DynArr_Push(&dots, &dot);

			void *temp_ptr = DYNARR_RESERVED_ADDR;
			DynArr_ForEach(c_sockets, int, cj_sock, {
				int j_sock = cj_sock;

				if (j_sock == i_sock) {
					continue;
				}

				int w_len = send(j_sock, &dot, r_len, 0);
				printf("%i bytes sent, IDot: %s\n", w_len, IDotData_ToString(dot));
				if (w_len <= 0) {
					close(j_sock);
					j_sock = -1;
				}	
			});
			DYNARR_RESERVED_ADDR = temp_ptr;

			printf("--\n");
		});

		int filter = -1;
		DynArr_FilterOut(&c_sockets, &filter);
    }

	DynArr_ForEach(c_sockets, int, sock, {
		close(sock);
	});
	close(l_socket);
	DynArr_Free(c_sockets);

	DynArr_Free(dots);

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

