#include "WProgram.h"
#include "sketches.h"
#include "leds.h"
#include "pattern_impl.h"

namespace sketches {
    Sketch *sketches;
    int num_sketches;

    const uint32_t sketch_run_time = 1000 * 60;
    uint32_t next_time;
    int current_sketch;

    void setup(void) {
        get_sketches(&sketches, &num_sketches);
        
        pattern_impl::setup();
        
        current_sketch = 0;
        next_time = millis() + sketch_run_time;
        sketches[current_sketch].setup();
    }
    
    void loop(void) {
        if (next_time <= millis()) {
            next_time += sketch_run_time;
            current_sketch = (current_sketch + 1) % num_sketches;
            sketches[current_sketch].setup();
            Serial.write(current_sketch);
        }
        
        sketches[current_sketch].loop();
    }
    
    void switch_to(int num) {
        if (num < num_sketches) {
            current_sketch = num;
            sketches[current_sketch].setup();
            next_time = millis() + sketch_run_time;
        }
    }
};

extern "C" int main(void)
{
    Serial.begin(9600);
    
    sketches::setup();
    while (1) {
        sketches::loop();
        while (Serial.available()) {
            sketches::switch_to(Serial.read());
        }
    }
}

