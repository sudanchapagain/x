package np.com.sudanchapagain

import io.github.flaxoos.ktor.server.plugins.ratelimiter.RateLimiting
import io.github.flaxoos.ktor.server.plugins.ratelimiter.implementations.TokenBucket
import kotlin.time.Duration.Companion.seconds
import io.ktor.server.application.Application
import io.ktor.server.application.install
import io.ktor.server.plugins.defaultheaders.DefaultHeaders
import io.ktor.server.routing.route
import io.ktor.server.routing.routing
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.time.Duration
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.CopyOnWriteArrayList

fun main() {
    val backendPorts = listOf(9001, 9002, 9003)
    val backends = CopyOnWriteArrayList<Backend>()
    val servers = mutableListOf<BackendServer>()

    backendPorts.forEach { port ->
        val backend = Backend("localhost", port)
        backends.add(backend)
        val server = BackendServer(port)
        server.start()
        servers.add(server)
    }

    val strategy = RoundRobinStrategy()
    val lb = LoadBalancer(8080, backends, strategy)
    lb.start()

    val healthChecker = HealthChecker(backends)
    healthChecker.start()

    // servers to start and health check to run initially
    Thread.sleep(3000)

    println("\n---starting concurrent HTTP clients---\n")

    val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(5)).build()

    val clientExecutor = Executors.newFixedThreadPool(10)
    val totalRequests = 10

    repeat(totalRequests) { i ->
        clientExecutor.execute {
            try {
                val request = HttpRequest.newBuilder().uri(URI.create("http://localhost:8080/request/$i")).GET().build()

                val response = client.send(request, HttpResponse.BodyHandlers.ofString())
                println("[Client] Request #$i -> Status: ${response.statusCode()}, Body: ${response.body().trim()}")
            } catch (e: Exception) {
                println("[Client] Error: ${e.message}")
            }
        }
    }

    clientExecutor.shutdown()
    clientExecutor.awaitTermination(30, TimeUnit.SECONDS)

    println("\n---Shutting down one server to test health check---\n")
    servers[0].stop()

    Thread.sleep(7000)

    println("\n---Starting more requests with one server down---\n")
    val clientExecutor2 = Executors.newFixedThreadPool(5)
    repeat(6) { i ->
        clientExecutor2.execute {
            try {
                val request = HttpRequest.newBuilder().uri(URI.create("http://localhost:8080/batch2/$i")).GET().build()

                val response = client.send(request, HttpResponse.BodyHandlers.ofString())
                println("[Client] Batch 2 #$i -> Status: ${response.statusCode()}, Body: ${response.body().trim()}")
            } catch (e: Exception) {
                println("[Client] Error: ${e.message}")
            }
        }
    }

    clientExecutor2.shutdown()
    clientExecutor2.awaitTermination(30, TimeUnit.SECONDS)

    println("\n--- Cleaning up ---\n")
    lb.stop()
    healthChecker.stop()
    servers.forEach { it.stop() }
    System.exit(0)
}


fun Application.configureHttp() {
    install(DefaultHeaders) {
        header("X-Engine", "Ktor")
    }
}

fun Application.configureRateLimiting() {
    routing {
        route("/") {
            install(RateLimiting) {
                rateLimiter {
                    type = TokenBucket::class
                    capacity = 100
                    rate = 10.seconds
                }
            }
        }
    }
}
