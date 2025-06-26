export function createMediaSource() {
    return new MediaSource();
}

export function isTypeSupported(mime) {
    return MediaSource.isTypeSupported(mime);
}

export function addSourceBuffer(mediaSource) {
    return function(mime) {
        return mediaSource.addSourceBuffer(mime);
    }
}

export function urlCreateObjectURL(mediaSource) {
    return function() {
        URL.createObjectURL(mediaSource);
    }
}

export function appendBuffer(sourceBuffer) {
    return function(arrayBuffer) {
        return sourceBuffer.appendBuffer(arrayBuffer);
    }
}

export function playVideo(videoElement) {
    return function(websocket) {
        return function() {
            console.log("CALLED")
            const mimeCodec = 'video/mp4; codecs="avc1.640028, mp4a.40.2"';

            if (!window.MediaSource || !MediaSource.isTypeSupported(mimeCodec)) {
                console.error("MSE not supported or codec mismatch");
                return;
            }

            const mediaSource = new MediaSource();
            videoElement.src = URL.createObjectURL(mediaSource);

            websocket.binaryType = 'arraybuffer';

            mediaSource.addEventListener('sourceopen', () => {
                const sourceBuffer = mediaSource.addSourceBuffer(mimeCodec);
                const queue = [];

                sourceBuffer.addEventListener('updateend', () => {
                    if (queue.length > 0 && !sourceBuffer.updating) {
                        sourceBuffer.appendBuffer(queue.shift());
                    }
                });

                websocket.onmessage = (event) => {
                    const chunk = new Uint8Array(event.data);

                    if (sourceBuffer.updating) {
                        queue.push(chunk);
                    } else {
                        sourceBuffer.appendBuffer(chunk);
                    }
                };

                websocket.onclose = () => {
                    console.log("WebSocket closed");
                };

                websocket.onerror = (err) => {
                    console.error("WebSocket error:", err);
                };
            });
        }
    }
}
