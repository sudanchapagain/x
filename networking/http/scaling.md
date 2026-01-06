scaling within the same machine "usually" means combining async with multiple
processes.

```python
import multiprocessing
import asyncio


def run_worker(port):
    async def handle(reader, writer):
        data = await reader.read(1024)
        response = b"HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nOK"
        writer.write(response)
        await writer.drain()
        writer.close()

    async def main():
        server = await asyncio.start_server(handle, "127.0.0.1", port)
        async with server:
            await server.serve_forever()

    asyncio.run(main())


if __name__ == "__main__":
    for i in range(multiprocessing.cpu_count()):
        p = multiprocessing.Process(target=run_worker, args=(8000 + i,))
        p.start()
```

