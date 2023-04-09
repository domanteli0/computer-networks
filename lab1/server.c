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

	void *item_buf = calloc(MAX_TYPE_SIZE, 1);
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

	DynArr dots = DynArr_WithCapacity(1024, sizeof(IDotData));

	// this array is pre-filled only for testing purposes
	for (size_t ix = 0; ix < 128; ++ix)
	{
		IDotData temp = IDotData_New((float)ix, (float)ix);
		DynArr_Push(&dots, &temp);
	}

	while (true)
	{
		// shutdown gracefully if instructed
		if (GetCharIfAny(STDIN_FILENO, timeval_FromMicro(100)) == 'q')
			break;

		FD_ZERO(&read_set);
		DynArr_ForEach(c_sockets, int, c_sock, {
			FD_SET(c_sock, &read_set);
			// maxfd = c_sock > maxfd ? c_sock : maxfd;
			if (c_sock > maxfd)
				maxfd = c_sock;
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
								 (struct sockaddr *) &clientaddr, &clientaddrlen);

			DynArr_Push(&c_sockets, &new_con);

			printf("Connected:  %s\n", inet_ntoa(clientaddr.sin_addr));

			DynArr_ForEach(dots, IDotData, dot, {
				printf("Sending init bytes: (%f, %f)\n", dot.x, dot.y);
				TCP_Send(&new_con, &dot, sizeof(IDotData), 0);
				fflush(stdout);	
			});
		}

		DynArr_ForEach(c_sockets, int, ci_sock, {
			if (!FD_ISSET(ci_sock, &read_set))
				continue;

			printf("About to read from the netwrok socket\n");

			int r1_len = TCP_Recv(&ci_sock, item_buf, sizeof(DrawableHeader), 0);
			int r2_len = TCP_Recv(&ci_sock, ((DrawableHeader *) item_buf) + 1, ((DrawableHeader *)item_buf)->size, 0);
			int r_len = r1_len + r2_len;
			printf("Bytes rececived: %i + %i = %i\n", r1_len, r2_len, r_len);
			printf("%s\n", IDotData_ToString( *((IDotData *) item_buf) ));

			if (r1_len < 0 || r2_len < 0)
				continue; 

			DynArr_Push(&dots, item_buf);

			void *temp_ptr = DYNARR_RESERVED_ADDR;
			DynArr_ForEach(c_sockets, int, cj_sock, {
				if (cj_sock == ci_sock)
					continue;

				int w_len = TCP_Send(&cj_sock, item_buf, r_len, 0);
				printf("%i bytes sent, IDot: %s\n", w_len, IDotData_ToString(*((IDotData *) item_buf) ));
			});
			DYNARR_RESERVED_ADDR = temp_ptr;

			printf("--\n");
		});

		int filter = -1;
		DynArr_FilterOut(&c_sockets, &filter);
	}

	DynArr_ForEach(dots, IDotData, dot, {
		printf("Ending dot: (%f, %f)\n", dot.x, dot.y);
	});

	DynArr_ForEach(c_sockets, int, sock, {
		close(sock);
	});

	free(item_buf);

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
