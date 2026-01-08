from django.http import StreamingHttpResponse
import time


def bit_counter(request):
    def generate():
        counter = 0

        while True:
            yield counter.to_bytes(4, "big")
            counter += 1
            time.sleep(0.1)  # 10 per sec

    return StreamingHttpResponse(generate(), content_type="application/octet-stream")
