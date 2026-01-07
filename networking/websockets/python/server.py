import asyncio
import websockets

clients = set()


async def handler(ws):
    clients.add(ws)

    try:
        async for message in ws:
            for c in clients:
                if c != ws:
                    await c.send(message)
    finally:
        clients.remove(ws)


async def main():
    async with websockets.serve(handler, "0.0.0.0", 8765):
        await asyncio.Future()


asyncio.run(main())
