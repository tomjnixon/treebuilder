#include "pattern.h"

uint32_t next_time;
uint32_t offset;


void setup() {
    next_time = millis();
    offset = 0;
}

void loop() {
    if (millis() > next_time) {
        next_time += 60;
        
        for (int i = 0; i < num_leds; i++)
            setPixel(i, makeColor(((offset + i) % num_leds * 359) / num_leds, 100, 20));
        show();
        
        offset++;
    }
}
