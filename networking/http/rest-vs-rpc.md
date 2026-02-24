REST
====

resources over HTTP using standard verbs

server
------

```python
from flask import Flask, jsonify

app = Flask(__name__)

@app.route("/users/<int:users_id>", methods=["GET"])
def get_user(user_id):
    return jsonify({"id": users_id, "name": "sudan chapagain"})
```

client
------

```python
import requests

r = requests.get("http://localhost:8000/users/1")
print(r.json())
```

a resource is being requested which is identified with a URL

RPC
===

server
------

```python
from xmlrpc.server import SimpleXMLRPCServer

def add(a, b):
    return a + b

server = SimpleXMLRPCServer(("localhost", 8000))
server.register_function(add, "add")
server.serve_forever()
```

client
------

```python
from xmlrpc.client import ServerProxy

proxy = ServerProxy("http://localhost:8000")
print(proxy.add(2, 3))
```

a literal function is being called at server from the client.
