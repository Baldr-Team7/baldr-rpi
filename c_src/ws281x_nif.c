#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <signal.h>

#include "erl_nif.h"

#include "mailbox.h"
#include "clk.h"
#include "gpio.h"
#include "dma.h"
#include "pwm.h"
#include "rpihw.h"

#include "ws2811.h"

#define ARRAY_SIZE(stuff)                        (sizeof(stuff) / sizeof(stuff[0]))

ws2811_t ledstring =
{
    .freq = 800000,
    .dmanum = 5,
    .channel =
    {
        [0] =
        {
            .gpionum = 18,
            .count = 1,
            .invert = 0,
            .brightness = 255,
            .strip_type = WS2811_STRIP_GRB,
        },
        [1] =
        {
            .gpionum = 0,
            .count = 0,
            .invert = 0,
            .brightness = 0,
        },
    },
};

// There are four functions that may be called during the lifetime
// of a NIF. load, reload, upgrade, and unload. Any of these functions
// can be left unspecified by passing NULL to the ERL_NIF_INIT macro.
//
// NIFs are awesome.

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
	ws2811_return_t ret;
	
    if ((ret = ws2811_init(&ledstring)) != WS2811_SUCCESS)
    {
        printf("%s\n", ws2811_get_return_t_str(ret));

        return -1;
    }		

    return 0;
}

static ERL_NIF_TERM
ws281x_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ws2811_return_t ret;

    uint32_t color;
    int led;

    enif_get_int( env, argv[0], &led   );
    enif_get_int( env, argv[1], &color );

	ledstring.channel[0].leds[led] = color;

    if ((ret = ws2811_render(&ledstring)) != WS2811_SUCCESS)
    {
        return enif_make_tuple(env, 
            enif_make_atom(env, "error"),
            enif_make_string(env, ws2811_get_return_t_str(ret), ERL_NIF_LATIN1)
        ); 
    }

    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"ws281x_nif", 2, ws281x_nif}
};

ERL_NIF_INIT(ws281x_nif, nif_funcs, &load, NULL, NULL, NULL);