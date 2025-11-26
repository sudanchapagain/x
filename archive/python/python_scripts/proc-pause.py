# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "psutil",
#     "flask",
# ]
# ///


import socket
import threading
import time
import webbrowser

import psutil
from flask import Flask, jsonify

data_lock = threading.Lock()
latest_snapshot = []


def poll_processes(interval=1):
    global latest_snapshot
    while True:
        snapshot = []
        for p in psutil.process_iter(["pid", "name", "cpu_percent", "memory_percent"]):
            try:
                info = p.info
                snapshot.append(
                    {
                        "pid": info["pid"],
                        "name": info["name"] or "unknown",
                        "cpu": float(info["cpu_percent"] or 0),
                        "mem": float(info["memory_percent"] or 0),
                    }
                )
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                continue
        snapshot.sort(key=lambda x: x["cpu"], reverse=True)
        with data_lock:
            latest_snapshot = snapshot[:50]
        time.sleep(interval)


app = Flask(__name__)


@app.route("/api/processes")
def get_processes():
    with data_lock:
        return jsonify(latest_snapshot)


@app.route("/")
def index():
    return HTML_PAGE


HTML_PAGE = """
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="utf-8">
    <style>
        body {
            font-family: sans-serif;
            background: #121212;
            color: #999;
            margin: 0;
            padding: 0;
            max-width: 800px;
            margin: 0 auto;
        }

        h2 {
            text-align: center;
            margin: 10px;
        }

        table {
            border-collapse: collapse;
            margin: 0 auto;
            overflow-y: scroll;
            height: 90vh;
            display: block;
        }

        thead {
            position: sticky;
            top: 0;
            background: #222;
        }

        th,
        td {
            padding: 4px 8px;
            border-bottom: 1px solid #333;
            white-space: nowrap;
        }

        .paused {
            color: #ffcc00;
        }
    </style>
</head>

<body>
    <h2><span id="status" class="paused">(Running)</span></h2>
    <table id="procTable">
        <thead>
            <tr>
                <th>PID</th>
                <th>Name</th>
                <th>CPU %</th>
                <th>MEM %</th>
            </tr>
        </thead>
        <tbody></tbody>
    </table>
    <script>
        let paused = false;
        let backlog = [];
        const MAX_BACKLOG = 100;

        function updateTable(snapshot) {
            if (!Array.isArray(snapshot)) return;
            const tbody = document.querySelector("#procTable tbody");
            let html = '';
            snapshot.forEach(p => {
                html += `<tr>
            <td>${p.pid}</td>
            <td>${p.name}</td>
            <td>${p.cpu.toFixed(1)}</td>
            <td>${p.mem.toFixed(1)}</td>
        </tr>`;
            });
            tbody.innerHTML = html;
        }

        async function fetchData() {
            if (paused) return;
            try {
                const res = await fetch('/api/processes');
                const data = await res.json();
                if (!Array.isArray(data)) return;
                backlog.push(data);
                if (backlog.length > MAX_BACKLOG) backlog.shift();
                updateTable(data);
            } catch (e) {
                console.error('fetch failed', e);
            }
        }

        document.addEventListener('keydown', e => {
            if (e.key === ' ') {
                paused = !paused;
                const st = document.getElementById('status');
                if (paused) {
                    st.textContent = '(Paused)';
                    st.classList.add('paused');
                } else {
                    st.textContent = '(Running)';
                    st.classList.remove('paused');
                    backlog.forEach(snap => updateTable(snap));
                }
            }
        });

        setInterval(fetchData, 1000);
    </script>
</body>

</html>
"""


def get_free_port():
    s = socket.socket()
    s.bind(("", 0))
    port = s.getsockname()[1]
    s.close()
    return port


if __name__ == "__main__":
    port = get_free_port()
    url = f"http://127.0.0.1:{port}/"
    print(f"serving on {url}")
    threading.Thread(target=lambda: webbrowser.open(url), daemon=True).start()
    threading.Thread(target=poll_processes, daemon=True).start()
    app.run(port=port)
