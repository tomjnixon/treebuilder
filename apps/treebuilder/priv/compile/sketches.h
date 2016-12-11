#ifndef SKETCHED_H
#define SKETCHED_H

struct Sketch {
    void (*setup)(void);
    void (*loop)(void);
};

void get_sketches(Sketch **sketches, int *n);

#endif
