#include <setjmp.h>
#include "emscripten.h"
#include "pattern.h"

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
  return EM_ASM_INT_V({ return (new Date()).getTime(); });
}

uint32_t micros(void) {
  return EM_ASM_INT_V({ return (new Date()).getTime() * 1000; });
}

void show(void) {
  for (int i = 0; i < num_leds; i++) ledColors_out[i] = ledColors[i];
  leds_updated = true;
}

extern "C" {
  EMSCRIPTEN_KEEPALIVE
  uint32_t *get_ledColors_out() {
    return ledColors_out;
  }

  EMSCRIPTEN_KEEPALIVE
  int get_num_leds() {
    return num_leds;
  }

  EMSCRIPTEN_KEEPALIVE
  uint8_t *get_led_positions() {
    return (uint8_t *)leds;
  }

  EMSCRIPTEN_KEEPALIVE
  size_t get_led_positions_stride() {
    return sizeof(*leds);
  }

  EMSCRIPTEN_KEEPALIVE
  volatile bool *get_leds_updated() {
    return &leds_updated;
  }

  EMSCRIPTEN_KEEPALIVE
  void c_setup() {
    setup();
  }

  EMSCRIPTEN_KEEPALIVE
  void c_loop() {
    loop();
  }

  void c_publish(const char *topic, size_t payload_len, const char *payload);
  void c_subscribe(const char *topic);
  size_t c_message_available();
  message_t c_message_read();

  EMSCRIPTEN_KEEPALIVE
  void c_fill_message(message_t *message, char *topic, size_t payload_len,
                      char *payload) {
    message->topic = topic;
    message->payload_len = payload_len;
    message->payload = payload;
  }
}

void publish(const char *topic, size_t payload_len, const char *payload) {
  c_publish(topic, payload_len, payload);
}

void subscribe(const char *topic) {
  c_subscribe(topic);
}

size_t message_available() {
  return c_message_available();
}

message_t message_read() {
  return c_message_read();
}

void priv_set_timeout(uint32_t timeout) {
}
