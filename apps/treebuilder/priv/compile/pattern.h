#ifndef PATTERN_H
#define PATTERN_H

#include <stdint.h>
#include <string.h>
#include "leds.h"

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
int makeColor(unsigned int hue, unsigned int saturation,
              unsigned int lightness);

struct message_t {
  char *topic;
  char *payload;
  size_t payload_len;
};
void message_free(message_t msg);

size_t message_available();
message_t message_read();

void publish(const char *topic, size_t payload_len, const char *payload);

void subscribe(const char *topic);

void priv_set_timeout(uint32_t timeout);

#ifdef JS_MODE
#include <stdio.h>
uint32_t millis(void);
uint32_t micros(void);

void setup();
void loop();
#else
#define printf
#endif

#endif
