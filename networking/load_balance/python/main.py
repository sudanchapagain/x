import asyncio
import itertools

# active backends
backends = [
    ("127.0.0.1", 8001),
    ("127.0.0.1", 8002),
]
# loop through active backends
cycle = itertools.cycle(backends)


# one socket to another
async def pipe(reader, writer):
    try:
        # read upto 4kb bytes
        while data := await reader.read(4096):
            # send to dest
            writer.write(data)
            # remember drain applies backpressure i.e. pause instead of
            # buffering forever on slow connection
            await writer.drain()
    except Exception:
        pass
    finally:
        writer.close()


# per incoming client conn
async def handle(client_r, client_w):
    host, port = next(cycle)  # choose
    backend_r, backend_w = await asyncio.open_connection(host, port)  # establish conn

    # wire them up
    await asyncio.gather(
        pipe(client_r, backend_w),
        pipe(backend_r, client_w),
    )


asyncio.run(asyncio.start_server(handle, "0.0.0.0", 8080))
