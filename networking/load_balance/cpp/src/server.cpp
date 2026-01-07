#include <httplib.h>
#include <iostream>
#include <thread>
#include <chrono>
#include <cstdlib>
#include <cstring>

std::string get_hostname() {
    char hostname[256];
    if (gethostname(hostname, sizeof(hostname)) == 0) {
        return std::string(hostname);
    } else {
        return "unknown";
    }
}

int main(int argc, char* argv[]) {
    int port = 8081;

    for (int i = 1; i < argc; ++i) {
        if (std::strcmp(argv[i], "--port") == 0 && i + 1 < argc) {
            port = std::atoi(argv[i + 1]);
        }
    }

    httplib::Server svr;

    svr.Get("/", [port](const httplib::Request& req, httplib::Response& res) {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));

        std::cout << "backend " << port << " received request: " << req.method << " " << req.path << std::endl;

        std::string body;
        body += "backend server on port " + std::to_string(port) + "\n";
        body += "host: " + get_hostname() + "\n";
        body += "req path: " + req.path + "\n";
        body += "req method: " + req.method + "\n";
        body += "req headers:\n";

        for (const auto& header : req.headers) {
            body += "  " + header.first + ": " + header.second + "\n";
        }

        res.set_content(body, "text/plain");
    });

    svr.Get("/health", [](const httplib::Request&, httplib::Response& res) {
        res.set_content("healthy", "text/plain");
        res.status = 200;
    });

    std::cout << "server started at " << port << std::endl;
    svr.listen("0.0.0.0", port);

    return 0;
}

