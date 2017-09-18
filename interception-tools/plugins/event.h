#ifndef EVENT_HEADER
#define EVENT_HEADER

int equal(const struct input_event *first, const struct input_event *second);
int read_event(struct input_event *event);
void write_event(const struct input_event *event);

#endif
