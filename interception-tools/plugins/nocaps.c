#include <stdio.h>
#include <stdlib.h>

#include <linux/input.h>

#include "event.h"

const struct input_event
ctrl_up         = {.type = EV_KEY, .code = KEY_LEFTCTRL, .value = 0},
capslock_up     = {.type = EV_KEY, .code = KEY_CAPSLOCK, .value = 0},
ctrl_down       = {.type = EV_KEY, .code = KEY_LEFTCTRL, .value = 1},
capslock_down   = {.type = EV_KEY, .code = KEY_CAPSLOCK, .value = 1},
ctrl_repeat     = {.type = EV_KEY, .code = KEY_LEFTCTRL, .value = 2},
capslock_repeat = {.type = EV_KEY, .code = KEY_CAPSLOCK, .value = 2};

int main(void) {
    int capslock_is_down = 0;
    struct input_event input;

    setbuf(stdin, NULL), setbuf(stdout, NULL);

    while (read_event(&input)) {
        if (input.type != EV_KEY) {
            write_event(&input);
            continue;
        }

        if (capslock_is_down) {
            if (equal(&input, &capslock_down) ||
                equal(&input, &capslock_repeat)) {
                continue;
            }

            if (equal(&input, &capslock_up)) {
                capslock_is_down = 0;
                write_event(&ctrl_up);
                continue;
            }

            if (input.value) {
                write_event(&ctrl_down);
            }
        } else {
            if (equal(&input, &capslock_down)) {
                capslock_is_down = 1;
                continue;
            }
        }

        write_event(&input);
    }
}
