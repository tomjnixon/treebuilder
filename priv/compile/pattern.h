#ifndef PATTERN_H
#define PATTERN_H

#include <stdint.h>
#include "leds.h"

const int brightness = 100;

void setPixel(uint32_t num, uint32_t color);
void setPixel(uint32_t num, uint8_t red, uint8_t green, uint8_t blue);

void show(void);

// Convert HSL (Hue, Saturation, Lightness) to RGB (Red, Green, Blue)
//
//   hue:        0 to 359 - position on the color wheel, 0=red, 60=orange,
//                            120=yellow, 180=green, 240=blue, 300=violet
//
//   saturation: 0 to 100 - how bright or dull the color, 100=full, 0=gray
//
//   lightness:  0 to 100 - how light the color is, 100=white, 50=color, 0=black
int makeColor(unsigned int hue, unsigned int saturation, unsigned int lightness);

#ifdef JS_MODE
uint32_t millis(void);
uint32_t micros(void);

void setup();
void loop();
#endif

#endif
