'use strict';

// function connect_ws(url) {
//     if (url === undefined)
//         url = "ws://" + window.location.host + "/ws";

//     return {
//         to_send_reconnect: [],
//         to_send: [],

//         send_all_queued: function() {
//             while (this.to_send.length && this.ws.readyState == this.ws.OPEN)
//                 this.ws.send(this.to_send.shift());
//         }

//         connect: function(reconnect) {
//             this.ws = new WebSocket(ws_url);

//             this.ws.addEventListener("open", (function() {
//                 this.to_send = _.concat(this.to_send, this.to_send_reconnect)
//                 this.send_all_queued();
//             }).bind(this));

//             this.ws.addEventListener("close", (function() {
//                 setTimeout(this.connect.bind(this), 1000);
//             }).bind(this));
//         },

//         subscribe: function(topic) {
//             var message = JSON.stringify({type: "subscribe", topic: topic});
//             this.to_send.push(message);
//             this.to_send.push(message);
//         }
//     };
// }

class WSConnection {
    constructor(url) {
        if (url === undefined)
            url = "ws://" + window.location.host + "/ws";

        this.url = url;

        this.to_send_reconnect = [];
        this.to_send = [];
        this.closed = false;

        this.connect();
    }

    send_all_queued() {
        while (this.to_send.length && this.ws.readyState == this.ws.OPEN)
            this.ws.send(this.to_send.shift());
    }

    connect() {
        this.to_send = this.to_send.concat(this.to_send_reconnect);

        this.ws = new WebSocket(this.url);

        this.ws.addEventListener("open", (function() {
            this.send_all_queued();
        }).bind(this));

        this.ws.addEventListener("close", (function(e) {
            if (!this.closed)
                setTimeout(this.connect.bind(this), 1000);
        }).bind(this));

        this.ws.addEventListener("message", (function(ws_message) {
            var message = JSON.parse(ws_message.data);
            if (message.type == "message") {
                if (this.on_message !== undefined)
                    this.on_message(message);
            }
        }).bind(this));
    }

    subscribe(topic) {
        var message = JSON.stringify({type: "subscribe", topic: topic});
        this.to_send.push(message);
        this.send_all_queued();
    }

    publish_string(topic, payload_string) {
        return this.publish_base64(topic, btoa(payload_string));
    }

    publish_byte_array(topic, payload_bytes) {
        return this.publish_base64(topic, base64js.fromByteArray(payload_bytes));
    }

    publish_base64(topic, payload64) {
        var message = JSON.stringify({
            type: "publish",
            topic: topic,
            payload64: payload64});
        this.to_send.push(message);
        this.send_all_queued();
    }

    close() {
        this.closed = true;
        this.ws.close();
    }
}
