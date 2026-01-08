import requests

r = requests.get("http://127.0.0.1:8000/counter/", stream=True)
for chunk in r.iter_content(4):
    print(int.from_bytes(chunk, "big"))
