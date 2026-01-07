import asyncio
import websockets


async def client():
    async with websockets.connect("ws://localhost:8765") as ws:
        await ws.send("Hello everyone")
        print(await ws.recv())


asyncio.run(client())
