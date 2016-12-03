
#CFLAGS=-Wall -I/usr/local/include -I$(EILOC) -Ideps/erlang_portutil -Ideps/pihwm/lib
CFLAGS= -I/usr/local/include -I$(EILOC) -fpic -shared -I/usr/lib/erlang/usr/include/ -I../deps/rpi_ws281x -L../deps/rpi_ws281x
CC=gcc

ws281x_nif: ws281x_nif.o
	$(CC) $(CFLAGS) -o ../priv/ws281x_nif.so ws281x_nif.c -Wl,--whole-archive -lws2811 -Wl,--no-whole-archive