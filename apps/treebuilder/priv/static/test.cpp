// YOUR NAME HERE

#include "pattern.h"

const uint32_t speed = 20;
uint32_t last_offset;

void setup() {
    last_offset = 0;
}

void loop() {
    uint32_t offset = (millis() / speed);
    
    if (offset == last_offset) return;
    last_offset = offset;
    
    for (int i = 0; i < num_leds; i++) {
        int offset_idx = (offset + i) % num_leds;
        int hue = (offset_idx * 359) / num_leds;
        setPixel(i, makeColor(hue, 100, 50));
    }
    
    show();
}
