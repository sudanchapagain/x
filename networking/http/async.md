async allows for concurrency

an example of simple async HTTP server

```python
import asyncio


async def handle(reader, wrtier):
    data = await reader.read(1024)  # assume data read for some functionality ig
    await asyncio.sleep(1)  # assume I/O work or something blocking

    response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nOK"

    writer.write(response.encode())
    await writer.drain()
    writer.close()


async def main():
    server = await asyncio.start_server(handle, "127.0.0.1", 8000)
    async with server:
        await server.server_forever()


asyncio.run(main())
```
