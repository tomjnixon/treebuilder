#ifndef LEDS_H
#define LEDS_H
#include <stdint.h>

typedef union {
    struct {
        int8_t x, y, z, rz, ry, rx;
    };
    int8_t axes[3];
} LED;

extern const LED *const leds;
const int num_leds = 400;

extern const int8_t axis_min[3];
extern const int8_t axis_max[3];

#endif
