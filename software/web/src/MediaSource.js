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
            websocket.binaryType = 'arraybuffer';
            const mimeCodec = 'video/mp4; codecs="avc1.640028"';

            if (!window.MediaSource) {
                console.error("MSE not supported");
                window.alert("MSE not supported")
                return;
            }

            if (!MediaSource.isTypeSupported(mimeCodec)) {
                console.error("Codec not supported");
                window.alert("Codec not supported")
            }

            const mediaSource = new MediaSource();
            const segmentQueue = [];
            let isBuffering = true;

            videoElement.src = URL.createObjectURL(mediaSource);

            mediaSource.addEventListener('sourceopen', () => {
                const sourceBuffer = mediaSource.addSourceBuffer(mimeCodec);

                sourceBuffer.addEventListener('updateend', () => {
                    feedBuffer(sourceBuffer);
                    if (isBuffering && getBufferedSeconds() >= 1) {
                        videoElement.play()
                        isBuffering = false;
                    }
                });

                websocket.onmessage = (event) => {
                    console.log("ON MESSAGE")
                    const chunk = new Uint8Array(event.data);
                    segmentQueue.push(chunk);
                    feedBuffer(sourceBuffer);
                };
            });

            function feedBuffer(sourceBuffer) {
                console.log("FEED BUFFER segment queue has size", segmentQueue.length)
                if (!sourceBuffer || sourceBuffer.updating || segmentQueue.length <= 0) {
                    return;
                }

                const nextSegment = segmentQueue.shift();
                console.log("segment queue has size", segmentQueue.length)
                sourceBuffer.appendBuffer(nextSegment);
            }

            function getBufferedSeconds() {
                const buffered = videoElement.buffered;
                const currentTime = videoElement.currentTime;
                console.log("GETBUFFEREDSECONDS, currentTime", currentTime)
                for (let i = 0; i < buffered.length; i++) {
                    console.log("BUFFERED I start, end", i, buffered.start(i), buffered.end(i))
                    if (buffered.start(i) <= currentTime && buffered.end(i) >= currentTime) {
                        console.log("buffered seconds: ", buffered.end(i) - currentTime)
                        return buffered.end(i) - currentTime;
                    }
                }
                return 0;
            }

            websocket.onclose = () => {
                console.log("WebSocket closed");
            };

            websocket.onerror = (err) => {
                console.error("WebSocket error:", err);
            };
        }
    }
}
