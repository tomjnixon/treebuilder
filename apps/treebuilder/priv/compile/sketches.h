#ifndef SKETCHES_H
#define SKETCHES_H

#include <stddef.h>

struct Sketch {
  void (*setup)(void);
  void (*loop)(void);
};

void get_sketches(Sketch **sketches, size_t *n);

#endif
