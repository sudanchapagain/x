#include <iostream>
#include <string>
#include <vector>
#include <atomic>
#include <thread>
#include <mutex>
#include <shared_mutex>
#include <chrono>
#include <csignal>
#include <memory>
#include <httplib.h>
#include <curl/curl.h>
#include <netdb.h>
#include <unistd.h>

struct Backend {
    std::string url;
    bool alive;
    mutable std::shared_mutex mutex;

    explicit Backend(std::string u) : url(std::move(u)), alive(true) {}

    void set_alive(bool status) {
        std::unique_lock lock(mutex);
        alive = status;
    }

    bool is_alive() const {
        std::shared_lock lock(mutex);
        return alive;
    }
};

struct LoadBalancer {
    std::vector<std::unique_ptr<Backend>> backends;
    std::atomic<size_t> current{0};

    Backend* next_backend() {
        const size_t n = backends.size();
        if (n == 0) return nullptr;
        
        for (size_t i = 0; i < n; ++i) {
            size_t idx = (current.fetch_add(1) + i) % n;
            if (backends[idx]->is_alive()) {
                return backends[idx].get();
            }
        }
        return nullptr;
    }

    void health_check() {
        for (auto& backend : backends) {
            bool alive = is_backend_alive(backend->url);
            backend->set_alive(alive);
            std::cout << "Backend " << backend->url << " is " << (alive ? "alive" : "dead") << std::endl;
        }
    }

    void health_check_periodically(std::chrono::seconds interval, std::atomic<bool>& running) {
        while (running.load()) {
            health_check();
            std::this_thread::sleep_for(interval);
        }
    }

    static bool is_backend_alive(const std::string& backend_url) {
        std::string host = backend_url;
        if (host.substr(0, 7) == "http://") {
            host = host.substr(7);
        }
        if (auto pos = host.find('/'); pos != std::string::npos) {
            host = host.substr(0, pos);
        }

        struct addrinfo hints{}, *res;
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;

        if (getaddrinfo(host.c_str(), "80", &hints, &res) != 0) {
            return false;
        }

        int sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
        if (sockfd == -1) {
            freeaddrinfo(res);
            return false;
        }

        struct timeval timeout {2, 0};
        setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));

        int result = connect(sockfd, res->ai_addr, res->ai_addrlen);
        close(sockfd);
        freeaddrinfo(res);

        return result == 0;
    }
};

void proxy_request(const httplib::Request& req, httplib::Response& res, const std::string& backend_url) {
    CURL* curl = curl_easy_init();
    if (!curl) {
        res.status = 503;
        res.set_content("Service Unavailable", "text/plain");
        return;
    }

    std::string target_url = backend_url + req.path;
    curl_easy_setopt(curl, CURLOPT_URL, target_url.c_str());
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, req.method.c_str());
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    struct curl_slist* headers = nullptr;
    for (const auto& [key, value] : req.headers) {
        headers = curl_slist_append(headers, (key + ": " + value).c_str());
    }
    headers = curl_slist_append(headers, "X-Proxy: Simple-Load-Balancer");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

    if (!req.body.empty()) {
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, req.body.c_str());
        curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, req.body.size());
    }

    std::string response_body;
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION,
        +[](void* contents, size_t size, size_t nmemb, void* userp) -> size_t {
            auto* body = static_cast<std::string*>(userp);
            size_t total_size = size * nmemb;
            body->append(static_cast<char*>(contents), total_size);
            return total_size;
        });
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_body);

    CURLcode code = curl_easy_perform(curl);
    if (code != CURLE_OK) {
        res.status = 503;
        res.set_content("Service Unavailable", "text/plain");
    } else {
        long http_code = 0;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);
        res.status = static_cast<int>(http_code);
        res.set_content(response_body, "application/json");
    }

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
}

std::atomic<bool> running{true};

void signal_handler(int signal) {
    if (signal == SIGINT) {
        running = false;
    }
}

int main() {
    curl_global_init(CURL_GLOBAL_ALL);
    std::signal(SIGINT, signal_handler);

    int port = 8080;
    std::chrono::seconds check_interval(60);

    LoadBalancer lb;
    std::vector<std::string> servers = {
        "http://localhost:8081",
        "http://localhost:8082",
        "http://localhost:8083"
    };

    for (const auto& server : servers) {
        lb.backends.emplace_back(std::make_unique<Backend>(server));
        std::cout << "configured backend: " << server << '\n';
    }

    lb.health_check();

    std::thread health_thread([&]() {
        lb.health_check_periodically(check_interval, running);
    });

    httplib::Server server;
    server.set_keep_alive_max_count(10);
    server.set_read_timeout(5, 0);
    server.set_write_timeout(10, 0);

    server.Get(".*", [&](const httplib::Request& req, httplib::Response& res) {
        auto backend = lb.next_backend();
        if (!backend) {
            res.status = 503;
            res.set_content("Service Unavailable", "text/plain");
            return;
        }
        proxy_request(req, res, backend->url);
    });

    server.Post(".*", [&](const httplib::Request& req, httplib::Response& res) {
        auto backend = lb.next_backend();
        if (!backend) {
            res.status = 503;
            res.set_content("Service Unavailable", "text/plain");
            return;
        }
        proxy_request(req, res, backend->url);
    });

    std::cout << "Load Balancer started at port " << port << std::endl;
    server.listen("0.0.0.0", port);

    running = false;
    if (health_thread.joinable()) health_thread.join();

    curl_global_cleanup();
}
