# NOTE to self: this was not ran/tested in Lab

import asyncio
import aiortc

pcs = set()


async def broadcast_bits():
    counter = 0

    while True:
        data = counter.to_bytes(4, "big")

        for pc in pcs:
            for ch in pc.getTransceivers():
                if isinstance(ch.sender, aiortc.RTCDataChannel):
                    try:
                        ch.sender.send(data)
                    except Exception:
                        pass
        counter += 1
        await asyncio.sleep(0.01)  # 100 updates per sec


async def main():
    # simulate just one PC
    pc = aiortc.RTCPeerConnection()
    pcs.add(pc)

    channel = pc.createDataChannel("bits")
    channel.on("message")(lambda msg: print("peer said:", msg))

    # in real setup, exchange SDP offer/answer via websocket
    asyncio.create_task(broadcast_bits())
    await asyncio.Future()


asyncio.run(main())
