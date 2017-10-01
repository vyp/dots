#include <stdio.h>
#include <stdlib.h>

#include <linux/input.h>

#include "event.h"

const struct input_event
compose_up     = {.type = EV_KEY, .code = KEY_COMPOSE,  .value = 0},
meta_up        = {.type = EV_KEY, .code = KEY_LEFTMETA, .value = 0},
compose_down   = {.type = EV_KEY, .code = KEY_COMPOSE,  .value = 1},
meta_down      = {.type = EV_KEY, .code = KEY_LEFTMETA, .value = 1},
compose_repeat = {.type = EV_KEY, .code = KEY_COMPOSE,  .value = 2},
meta_repeat    = {.type = EV_KEY, .code = KEY_LEFTMETA, .value = 2};

int main(void) {
    int meta_is_down = 0, compose_give_up = 0;
    struct input_event input;

    setbuf(stdin, NULL), setbuf(stdout, NULL);

    while (read_event(&input)) {
        if (input.type != EV_KEY) {
            write_event(&input);
            continue;
        }

        if (meta_is_down) {
            if (equal(&input, &meta_down) ||
                equal(&input, &meta_repeat)) {
                continue;
            }

            if (equal(&input, &meta_up)) {
                meta_is_down = 0;

                if (compose_give_up) {
                    compose_give_up = 0;
                    write_event(&meta_up);
                    continue;
                }

                write_event(&compose_down);
                write_event(&compose_up);
                continue;
            }

            if (!compose_give_up && input.value) {
                compose_give_up = 1;
                write_event(&meta_down);
            }
        } else {
            if (equal(&input, &meta_down)) {
                meta_is_down = 1;
                continue;
            }
        }

        write_event(&input);
    }
}
