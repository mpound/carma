#ifndef tcpip_h
#define tcpip_h

/* Server socket initialization */

int tcp_server_sock(int port, int qlen);
int udp_server_sock(int port, int qlen);

int tcp_connect(char *host, int port, int dowait);
int udp_connect(char *host, int port, int dowait);

/* Selection of blocking versus non-blocking I/O */

int tcp_set_blocking(int sock, int doblock);

#endif
