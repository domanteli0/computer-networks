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
#include "Types/types.c"
#include "utils.h"

#define MAXCLIENTS 10
#define BACKLOG 5

char GetCharIfAny(int fd, struct timeval timeout);

int TCP_BindAndListen(char *port_str, struct sockaddr_in *servaddr) {
	int port = atoi(port_str);
	int l_socket = 0;

    if ((port < 1) || (port > 65535)) {
		return -1;
    }

	if ((l_socket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
		return -2;
	}

	memset(servaddr, 0, sizeof(*servaddr));
	servaddr->sin_family = AF_INET;
	servaddr->sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr->sin_port = htons(port);

	if (bind(l_socket, (struct sockaddr *)servaddr, sizeof(*servaddr)) < 0)
	{
		return -3;
	}

	if (listen(l_socket, BACKLOG)) {
		return -4;
	}

	return l_socket;
}

int main(int argc, char *argv[])
{
	if (argc != 2)
	{
		fprintf(stderr, "USAGE: <port>\n");
		return -1;
	}

	DynArr c_sockets = DynArr_WithCapacity(8, sizeof(int));
    fd_set read_set;

	int l_socket;
	struct sockaddr_in servaddr;
	struct sockaddr_in clientaddr;
	int maxfd = 0;

	switch (l_socket = TCP_BindAndListen(argv[1], &servaddr))
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

	printf("l_socket: %i\n", l_socket);

	DynArr dots = DynArr_WithCapacity(1024, sizeof(Float2));

	// this array is pre-filled only for testing purposes
	for (size_t ix = 0; ix < 128; ++ix)
	{
		Float2 temp = Float2_New((float)ix, (float)ix);
		DynArr_Push(&dots, &temp);
	}

	printf("dots.size %zu\n", dots.size);

	while (true)
	{
		// shutdown gracefully if instructed
		if (GetCharIfAny(STDIN_FILENO, timeval_FromMicro(100)) == 'q')
		{
			break;
		}

		FD_ZERO(&read_set);
		DynArr_ForEach(c_sockets, int, c_sock, {
			FD_SET(c_sock, &read_set);
			if (c_sock > maxfd)
			{
				maxfd = c_sock;
			}
		});

		FD_SET(l_socket, &read_set);
		if (l_socket > maxfd)
		{
			maxfd = l_socket;
		}

		struct timeval network_timeout = timeval_FromMicro(100);

		select(maxfd + 1, &read_set, NULL, NULL, &network_timeout);

		bool new_incoming = FD_ISSET(l_socket, &read_set);
		bool can_accept = c_sockets.size < MAXCLIENTS;

		if (new_incoming && can_accept)
		{
			printf("ATTEMPTING TO ACCEPT NEW CON\n");
			unsigned int clientaddrlen = sizeof(clientaddr);
			memset(&clientaddr, 0, clientaddrlen);

			int new_con = accept(l_socket,
								 (struct sockaddr *)&clientaddr, &clientaddrlen);

			DynArr_Push(&c_sockets, &new_con);

			printf("Connected:  %s\n", inet_ntoa(clientaddr.sin_addr));

			for (size_t DYNARR_RESERVED_INDEX = 0; DYNARR_RESERVED_INDEX < (dots).size; ++DYNARR_RESERVED_INDEX)
			{
				void *DYNARR_RESERVED_ADDR = (dots).arr_ptr + (DYNARR_RESERVED_INDEX * (dots).size_of_elem);
				Float2 dot = *((Float2 *)DYNARR_RESERVED_ADDR);
				{
					printf("Sending init bytes\n");
					fflush(stdout);
					IDotData temp = IDotData_FromFloat2(dot);
					send(new_con, &temp, sizeof(IDotData), 0);
				}
			}
		}

		DynArr_ForEach(c_sockets, int, ci_sock, {
			if (!FD_ISSET(ci_sock, &read_set))
			{
				continue;
			}

			printf("Doing some network things\n");

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

				if (j_sock == i_sock)
				{
					continue;
				}

				int w_len = send(j_sock, &dot, r_len, 0);
				printf("%i bytes sent, IDot: %s\n", w_len, IDotData_ToString(dot));
				if (w_len <= 0)
				{
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
