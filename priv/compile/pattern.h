#ifndef PATTERN_H
#define PATTERN_H

#include <stdint.h>
#include "leds.h"

const int brightness = 100;

void setPixel(uint32_t num, int color);
void setPixel(uint32_t num, uint8_t red, uint8_t green, uint8_t blue);

void show(void);

#ifdef JS_MODE
uint32_t millis(void);
uint32_t micros(void);

void setup();
void loop();
#endif

#endif
