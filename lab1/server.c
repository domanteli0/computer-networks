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
#include <termios.h>
#include <unistd.h>

#include "DynArr/dynarr.h"
#include "Dict/dict.h"
#include "Types/types.h"
#include "Types/floatx.h"
#include "Types/idot.h"
#include "Types/header.h"
#include "BitArr/bitarr.h"
#include "utils.h"

#define MAXCLIENTS 10
#define BACKLOG 5

char GetCharIfAny(int fd, struct timeval timeout);

int TCP_BindAndListen(char *port_str) {
	struct sockaddr_in servaddr;
	memset(&servaddr, 0, sizeof(struct sockaddr_in));

	int port = atoi(port_str);
	int l_socket = 0;

    if ((port < 1) || (port > 65535)) {
		return -1;
    }

	if ((l_socket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
		return -2;
	}

	servaddr.sin_family = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port = htons(port);

	if (bind(l_socket, &servaddr, sizeof(servaddr)) < 0)
	{
		return -3;
	}

	if (listen(l_socket, BACKLOG)) {
		return -4;
	}

	return l_socket;
}

int TCP_Accept(int socket) {
	struct sockaddr_in clientaddr;
	unsigned int clientaddrlen = sizeof(clientaddr);
	memset(&clientaddr, 0, clientaddrlen);

	int new_con = accept(socket,
		(struct sockaddr *) &clientaddr, &clientaddrlen);

	printf("Connected:  %s\n", inet_ntoa(clientaddr.sin_addr));

	return new_con;
}

typedef struct State {
	void *item_buf;
	DynArr c_sockets;
	int l_socket;

	DynArr items;
} State;

/// @brief 
/// @param state 
/// @param on_accept 			a function which deals with new connections
/// @param on_available_data 	a function which deals with existing connection,
/// i.e. handles data received from clients
/// @return the end state at the time the server was stoped
State run_server(State state, int (*on_accept)(State *), int (*on_available_data)(State *, int)) {
	while(true) {
		// shutdown gracefully if instructed
		if (GetCharIfAny(STDIN_FILENO, timeval_FromMicro(100)) == 'q')
			break;

    	fd_set read_set;
		int maxfd = fd_set_FromArrWithServerFD(
			&read_set,
			state.c_sockets.arr_ptr,
			state.c_sockets.size,
			state.l_socket
		);

		struct timeval network_timeout = timeval_FromMicro(100);
		select(maxfd + 1, &read_set, NULL, NULL, &network_timeout);

		bool new_incoming = FD_ISSET(state.l_socket, &read_set);
		bool can_accept = state.c_sockets.size < MAXCLIENTS;

		// accepts a new connection
		if (new_incoming && can_accept)
		{
			int new_con = on_accept(&state);
			if (new_con != -1)
				DynArr_Push(&(state.c_sockets), &new_con);
		}

		// handles incoming data
		DynArr_ForEachPtr(state.c_sockets, ci_sock, {
			int *sock = ci_sock;
			if (!FD_ISSET(*sock, &read_set))
				continue;

			printf("Data available, about to read from a socket\n");
			*sock = on_available_data(&state, *sock);
		});

		int filter = -1;
		DynArr_FilterOut(&state.c_sockets, &filter);

		// printf("One event loop finished\n");
	}

	return state;
}

int OnAccept(State *state) {
	printf("ATTEMPTING TO ACCEPT NEW CON\n");

	int new_con = TCP_Accept(state->l_socket);
	if (new_con == -1)
		return -1;

	DynArr_ForEachPtr(state->items, item_ptr, {
		printf(
			"Sending a thing, header [TYPE: 0x%" PRIx16 " | SIZE: %" PRIu16 " ]\n",
			((DrawableHeader *) item_ptr)->type_id,	
			((DrawableHeader *) item_ptr)->size	
		);
		
		TCP_Send(&new_con, item_ptr, ((DrawableHeader *) item_ptr)->size + sizeof(DrawableHeader), 0);
	});

	return new_con;
}

int OnAvailableData(State *state, int sender_fd) {

	int r1_len = TCP_Recv(&sender_fd, state->item_buf, sizeof(DrawableHeader), 0);
	int r2_len = TCP_Recv(&sender_fd, ((DrawableHeader *) state->item_buf) + 1, ((DrawableHeader *) state->item_buf)->size, 0);
	int r_len = r1_len + r2_len;
	printf("Bytes rececived: %i + %i = %i\n", r1_len, r2_len, r_len);

	if (r1_len < 0 || r2_len < 0)
		return sender_fd;

	assert(r1_len > 0);
	assert(r2_len > 0);
	assert(r_len > 0);

	DynArr_Push(&(state->items), state->item_buf);

	DynArr_ForEach(state->c_sockets, int, i_sock, {
		if (i_sock == sender_fd)
			continue;

		printf("Sending to fd %i\n", i_sock);
		TCP_Send(&i_sock, state->item_buf, r_len, 0);
	});

	return sender_fd;
}

int main(int argc, char *argv[])
{
	if (argc != 2)
	{
		fprintf(stderr, "USAGE: <port>\n");
		return -1;
	}

	State state;

	switch (state.l_socket = TCP_BindAndListen(argv[1]))
	{
		case -1:
			fprintf(stderr, "ERROR #1: invalid port specified.\n");
			exit(1);
		case -2:
			fprintf(stderr, "ERROR #2: cannot create listening socket.\n");
			exit(1);
		case -3:
			fprintf(stderr, "ERROR #3: bind listening socket.\n");
			exit(1);	
		case -4:
			fprintf(stderr, "ERROR #4: failed listen()\n");
			exit(1);
		default:
			break;
	}

	state.c_sockets = DynArr_WithCapacity(8, sizeof(int));
	state.item_buf = calloc(MAX_TYPE_SIZE, 1);
	state.items = DynArr_WithCapacity(1024, MAX_TYPE_SIZE);

	// this array is pre-filled only for testing purposes
	for (size_t ix = 0; ix < 128; ++ix)
	{
		IDotData temp = IDotData_New((float)ix, (float)ix);
		DynArr_Push(&state.items, &temp);
	}

	for (size_t ix = 10; ix < 100; ix += 10) {
		ILine temp = ILine_New(ix, ix, ix, ix + 10);
		DynArr_Push(&state.items, &temp);
	}

	run_server(state, OnAccept, OnAvailableData);

	DynArr_ForEach(state.c_sockets, int, sock, {
		if (sock != -1)
			close(sock);
	});

	free(state.item_buf);

	close(state.l_socket);
	DynArr_Free(state.c_sockets);

	DynArr_Free(state.items);

	return 0;
}

// returns a character from stdin
// if where isn't any 0xFF is returned (non valid UTF-8 byte)
//
// Side effects: temporarily disables stdin buffering
char GetCharIfAny(int fd, struct timeval timeout)
{
	char ret = 0xFF;

	fd_set read_set = fd_set_Singleton(fd);
	if (select(fd + 1, &read_set, NULL, NULL, &timeout) == 1)
	{
		ret = getchar();
	}

	return ret;
}
