#include "pattern.h"
#include "pattern_impl.h"
#include "OctoWS2811.h"

namespace pattern_impl {
    const int ledsPerStrip = 50;
    const int numStrips = 5;
    const int numLeds = ledsPerStrip * numStrips;
    
    DMAMEM int displayMemory[ledsPerStrip*6];
    int drawingMemory[ledsPerStrip*6];
    const int config = WS2811_RGB | WS2811_800kHz;
    OctoWS2811 leds(ledsPerStrip, displayMemory, drawingMemory, config);
    
    int brightness = 30;
    
    void setup() {
        leds.begin();
    }
}


void setPixel(uint32_t i, uint32_t color) {
    if (i >= 50*3) {
        i += 50;
    }
    pattern_impl::leds.setPixel(i, color);
}
void setPixel(uint32_t i, uint8_t red, uint8_t green, uint8_t blue) {
    if (i >= 50*3) {
        i += 50;
    }
    pattern_impl::leds.setPixel(i, red, green, blue);
}

void show(void) {
    pattern_impl::leds.show();
}
