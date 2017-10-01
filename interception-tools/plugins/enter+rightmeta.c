#include <stdio.h>
#include <stdlib.h>

#include <linux/input.h>

#include "event.h"

const struct input_event
enter_up        = {.type = EV_KEY, .code = KEY_ENTER,     .value = 0},
meta_up         = {.type = EV_KEY, .code = KEY_RIGHTMETA, .value = 0},
enter_down      = {.type = EV_KEY, .code = KEY_ENTER,     .value = 1},
meta_down       = {.type = EV_KEY, .code = KEY_RIGHTMETA, .value = 1},
enter_repeat    = {.type = EV_KEY, .code = KEY_ENTER,     .value = 2},
meta_repeat     = {.type = EV_KEY, .code = KEY_RIGHTMETA, .value = 2};

int main(void) {
    int enter_is_down = 0, meta_give_up = 0;
    struct input_event input;

    setbuf(stdin, NULL), setbuf(stdout, NULL);

    while (read_event(&input)) {
        if (input.type != EV_KEY) {
            write_event(&input);
            continue;
        }

        if (enter_is_down) {
            if (equal(&input, &enter_down) ||
                equal(&input, &enter_repeat)) {
                continue;
            }

            if (equal(&input, &enter_up)) {
                enter_is_down = 0;

                if (meta_give_up) {
                    meta_give_up = 0;
                    write_event(&meta_up);
                    continue;
                }

                write_event(&enter_down);
                write_event(&enter_up);
                continue;
            }

            if (!meta_give_up && input.value) {
                meta_give_up = 1;
                write_event(&meta_down);
            }
        } else {
            if (equal(&input, &enter_down)) {
                enter_is_down = 1;
                continue;
            }
        }

        write_event(&input);
    }
}
