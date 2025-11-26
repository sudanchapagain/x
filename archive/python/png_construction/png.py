import struct
import zlib


def create_chunk(chunk_type, data):
    length = struct.pack("!I", len(data))
    crc = struct.pack("!I", zlib.crc32(chunk_type + data) & 0xFFFFFFFF)
    return length + chunk_type + data + crc


output_path = "img.png"
png_sig = b"\x89PNG\r\n\x1a\n"

width = 4
height = 4
bit_depth = 8
color_type = 3
compression = 0
filter_method = 0
interlace = 0

ihdr_data = struct.pack(
    "!IIBBBBB",
    width,
    height,
    bit_depth,
    color_type,
    compression,
    filter_method,
    interlace,
)

ihdr_chunk = create_chunk(b"IHDR", ihdr_data)

palette = [
    255, 0, 0,  # red
    0, 255, 0,  # green
    0, 0, 255,  # blue
]

palette += [0, 0, 0] * (256 - len(palette) // 3)
plte_chunk = create_chunk(b"PLTE", bytes(palette))

trns_data = bytes([255, 255, 255])
trns_chunk = create_chunk(b"tRNS", trns_data)

pixel_rows = [
    [0, 1, 2, 0],
    [0, 1, 2, 0],
    [0, 1, 2, 0],
    [0, 1, 2, 0]
]

raw_image_data = bytearray()
for row in pixel_rows:
    raw_image_data.append(0)
    raw_image_data.extend(row)

compressed_data = zlib.compress(bytes(raw_image_data))
idat_chunk = create_chunk(b"IDAT", compressed_data)

iend_chunk = create_chunk(b"IEND", b"")

with open(output_path, "wb") as f:
    f.write(png_sig)
    f.write(ihdr_chunk)
    f.write(plte_chunk)
    f.write(trns_chunk)
    f.write(idat_chunk)
    f.write(iend_chunk)
