#ifndef REAL_PATTERN_H
#define REAL_PATTERN_H

#include <stdint.h>
#include "leds.h"

const int brightness = 100;

void setPixel(uint32_t num, int color);
void setPixel(uint32_t num, uint8_t red, uint8_t green, uint8_t blue);

void show(void);

#endif
