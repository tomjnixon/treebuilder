#include "pattern.h"
#include <setjmp.h>
#include "emscripten.h"

uint32_t ledColors[num_leds];
uint32_t ledColors_out[num_leds];
volatile bool leds_updated;

void setPixel(uint32_t num, uint32_t color) {
    ledColors[num] = color;
}
void setPixel(uint32_t num, uint8_t red, uint8_t green, uint8_t blue) {
    ledColors[num] = (red << 16) | (green << 8) | blue;
}

uint32_t millis(void) {
    return EM_ASM_INT_V({
        return (new Date()).getTime();
    });
}

uint32_t micros(void) {
    return EM_ASM_INT_V({
        return (new Date()).getTime() * 1000;
    });
}


void show(void) {
    for (int i = 0; i < num_leds; i++)
        ledColors_out[i] = ledColors[i];
    leds_updated = true;
}

extern "C" {
    uint32_t *get_ledColors_out() {
        return ledColors_out;
    }
    
    int get_num_leds() {
        return num_leds;
    }
    
    uint8_t *get_led_positions() {
        return (uint8_t *)leds;
    }
    
    size_t get_led_positions_stride() {
        return sizeof(*leds);
    }
    
    volatile bool *get_leds_updated() {
        return &leds_updated;
    }
    
    void c_setup() {
        setup();
    }
    
    void c_loop() {
        loop();
    }
}
