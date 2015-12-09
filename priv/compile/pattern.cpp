#include "OctoWS2811.h"
#include "pattern.h"
#include "pattern_impl.h"

namespace pattern_impl {
    const int ledsPerStrip = 50;
    const int numStrips = 5;
    const int numLeds = ledsPerStrip * numStrips;
    
    DMAMEM int displayMemory[ledsPerStrip*6];
    int drawingMemory[ledsPerStrip*6];
    const int config = WS2811_RGB | WS2811_800kHz;
    OctoWS2811 leds(ledsPerStrip, displayMemory, drawingMemory, config);
    
    unsigned int brightness = 85;
    
    void setup() {
        leds.begin();
    }
    
    void clear() {
        for (int i = 0; i < num_leds; i++)
            setPixel(i, 0);
    }
    
    uint8_t scaleChannel(uint8_t channel) {
        return (((unsigned int)channel) * brightness) >> 8;
    }
    
    uint32_t scaleColor(uint32_t color) {
        return ( ((((color & 0xff00ff) * brightness) >> 8) & 0xff00ff)
               | ((((color & 0x00ff00) * brightness) >> 8) & 0x00ff00) );
    }
}


void setPixel(uint32_t i, uint32_t color) {
    if (i >= 50*3) {
        i += 50;
    }
    pattern_impl::leds.setPixel(i, pattern_impl::scaleColor(color));
}

void setPixel(uint32_t i, uint8_t red, uint8_t green, uint8_t blue) {
    if (i >= 50*3) {
        i += 50;
    }
    pattern_impl::leds.setPixel(i,
            pattern_impl::scaleChannel(red),
            pattern_impl::scaleChannel(green),
            pattern_impl::scaleChannel(blue));
}

void show(void) {
    pattern_impl::leds.show();
}
