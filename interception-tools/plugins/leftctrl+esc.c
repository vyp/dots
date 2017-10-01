#include <stdio.h>
#include <stdlib.h>

#include <linux/input.h>

#include "event.h"

const struct input_event
esc_up      = {.type = EV_KEY, .code = KEY_ESC,      .value = 0},
ctrl_up     = {.type = EV_KEY, .code = KEY_LEFTCTRL, .value = 0},
esc_down    = {.type = EV_KEY, .code = KEY_ESC,      .value = 1},
ctrl_down   = {.type = EV_KEY, .code = KEY_LEFTCTRL, .value = 1},
esc_repeat  = {.type = EV_KEY, .code = KEY_ESC,      .value = 2},
ctrl_repeat = {.type = EV_KEY, .code = KEY_LEFTCTRL, .value = 2};

int main(void) {
    int ctrl_is_down = 0, esc_give_up = 0;
    struct input_event input;

    setbuf(stdin, NULL), setbuf(stdout, NULL);

    while (read_event(&input)) {
        if (input.type != EV_KEY) {
            write_event(&input);
            continue;
        }

        if (ctrl_is_down) {
            if (equal(&input, &ctrl_down) ||
                equal(&input, &ctrl_repeat)) {
                continue;
            }

            if (equal(&input, &ctrl_up)) {
                ctrl_is_down = 0;

                if (esc_give_up) {
                    esc_give_up = 0;
                    write_event(&ctrl_up);
                    continue;
                }

                write_event(&esc_down);
                write_event(&esc_up);
                continue;
            }

            if (!esc_give_up && input.value) {
                esc_give_up = 1;
                write_event(&ctrl_down);
            }
        } else {
            if (equal(&input, &ctrl_down)) {
                ctrl_is_down = 1;
                continue;
            }
        }

        write_event(&input);
    }
}
