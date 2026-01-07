import aiortc

pc = aiortc.RTCPeerConnection()


def on_message(msg):
    counter = int.from_bytes(msg, "big")
    print("received:", counter)


channel = pc.createDataChannel("bits")
channel.on("message")(on_message)
