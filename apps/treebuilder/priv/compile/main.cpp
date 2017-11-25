#include "Arduino.h"
#include "leds.h"
#include "pattern.h"
#include "pattern_impl.h"
#include "sketches.h"

namespace comms {
  void send_switched(size_t sketch);
  void clear_messages();
  void send_unsub_all();
};  // namespace comms

namespace sketches {
  Sketch *sketches;
  size_t num_sketches;

  const uint32_t sketch_run_time = 1000 * 30;
  uint32_t next_time;
  size_t current_sketch;

  void switch_common(size_t num) {
    if (num < num_sketches) {
      current_sketch = num;
      pattern_impl::clear();
      comms::send_unsub_all();
      comms::clear_messages();
      comms::send_switched(current_sketch);
      sketches[current_sketch].setup();
    }
  }

  void setup(void) {
    get_sketches(&sketches, &num_sketches);

    pattern_impl::setup();

    switch_common(0);
    next_time = millis() + sketch_run_time;
  }

  void loop(void) {
    if (next_time <= millis()) {
      switch_common((current_sketch + 1) % num_sketches);
      next_time += sketch_run_time;
    }

    sketches[current_sketch].loop();
  }

  void switch_to(size_t num) {
    switch_common(num);
    next_time = millis() + sketch_run_time;
  }

};  // namespace sketches

namespace comms {

  void wait_frame_boundary() {
    while (true) {
      while (!Serial.available())
        ;
      if (Serial.read() == 0x7E) return;
    }
  }

  char read_char() {
    while (!Serial.available())
      ;
    char c = Serial.read();

    if (c == 0x7D) {
      while (!Serial.available())
        ;
      return Serial.read() ^ 0x20;

    } else {
      return c;
    }
  }

  void write_frame_start() {
    Serial.write(0x7e);
  }

  void write_frame_end() {
    Serial.write(0x7f);
  }

  void write_char(char c) {
    if (c == 0x7d || c == 0x7e || c == 0x7f) {
      Serial.write(0x7d);
      Serial.write(c ^ 0x20);
    } else {
      Serial.write(c);
    }
  }

  enum recv_cmd {
    RECV_SWITCH = 0,
    RECV_MESSAGE = 1,
  };

  enum send_cmd {
    SEND_SWITCHED = 0,
    SEND_PUBLISH = 1,
    SEND_UNSUB_ALL = 2,
    SEND_SUBSCRIBE = 3,
  };

  const size_t MSG_Q_LEN = 3;

  message_t message_queue[MSG_Q_LEN];
  size_t message_queue_len = 0;

  message_t pop_message() {
    message_t res = message_queue[0];

    for (size_t i = 0; i < message_queue_len - 1; i++)
      message_queue[i] = message_queue[i + 1];

    message_queue_len--;

    return res;
  }

  void push_message(message_t message) {
    if (message_queue_len == MSG_Q_LEN) {
      message_free(pop_message());
    }

    message_queue[message_queue_len] = message;
    message_queue_len++;
  }

  void clear_messages() {
    for (size_t i = 0; i < message_queue_len; i++)
      message_free(message_queue[i]);
    message_queue_len = 0;
  }

  int16_t next_sketch = -1;

  void loop() {
    while (Serial.available()) {
      wait_frame_boundary();

      switch (read_char()) {
        case RECV_SWITCH:
          next_sketch = ((uint16_t)read_char()) << 8;
          next_sketch |= (uint16_t)read_char();
          break;
        case RECV_MESSAGE:
          message_t message;

          uint8_t topic_len = read_char();

          message.topic = (char *)malloc(topic_len + 1);
          message.topic[topic_len] = 0;

          for (size_t i = 0; i < topic_len; i++) {
            message.topic[i] = read_char();
          }

          message.payload_len = ((uint16_t)read_char()) << 8;
          message.payload_len |= (uint16_t)read_char();

          message.payload = (char *)malloc(message.payload_len + 1);
          message.payload[message.payload_len] = 0;

          for (size_t i = 0; i < message.payload_len; i++) {
            message.payload[i] = read_char();
          }

          push_message(message);
          break;
      }

      // consume frame end
      while (!Serial.available())
        ;
      Serial.read();
    }
  }

  void send_switched(size_t sketch) {
    write_frame_start();
    write_char(SEND_SWITCHED);
    write_char(sketch >> 8);
    write_char(sketch);
    write_frame_end();
  }

  void send_publish(const char *topic, size_t payload_len,
                    const char *payload) {
    size_t topic_len = strlen(topic);
    if (topic_len > 255) topic_len = 255;

    write_frame_start();
    write_char(SEND_PUBLISH);

    write_char(topic_len);
    for (size_t i = 0; i < topic_len; i++) write_char(topic[i]);

    write_char(payload_len >> 8);
    write_char(payload_len);
    for (size_t i = 0; i < payload_len; i++) write_char(payload[i]);

    write_frame_end();
  }

  void send_unsub_all() {
    write_frame_start();
    write_char(SEND_UNSUB_ALL);
    write_frame_end();
  }

  void send_subscribe(const char *topic) {
    size_t topic_len = strlen(topic);
    if (topic_len > 255) topic_len = 255;

    write_frame_start();

    write_char(SEND_SUBSCRIBE);
    write_char(topic_len);
    for (size_t i = 0; i < topic_len; i++) write_char(topic[i]);

    write_frame_end();
  }
};  // namespace comms

size_t message_available() {
  if (comms::message_queue_len == 0) comms::loop();

  return comms::message_queue_len;
}

message_t message_read() {
  return comms::pop_message();
}

void publish(const char *topic, size_t payload_len, const char *payload) {
  comms::send_publish(topic, payload_len, payload);
}

void subscribe(const char *topic) {
  comms::send_subscribe(topic);
}

void setup() {
  Serial.begin(9600);

  sketches::setup();
}

void loop() {
  sketches::loop();
  comms::loop();

  if (comms::next_sketch != -1) {
    sketches::switch_to(comms::next_sketch);
    comms::next_sketch = -1;
  }
}
