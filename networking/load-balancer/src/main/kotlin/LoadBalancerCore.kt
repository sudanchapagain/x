package np.com.sudanchapagain

import java.net.Socket
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.TimeUnit
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.client.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.client.engine.cio.*
import io.ktor.http.*
import io.ktor.utils.io.jvm.javaio.*

data class Backend(
    val host: String,
    val port: Int,
    @Volatile var isHealthy: Boolean = true
) {
    override fun toString(): String = "$host:$port (healthy=$isHealthy)"
}

class BackendServer(val port: Int) {
    private var server: EmbeddedServer<NettyApplicationEngine, NettyApplicationEngine.Configuration>? = null

    fun start() {
        server = embeddedServer(Netty, port = port) {
            routing {
                get("/{...}") {
                    println("[Backend $port] Received: ${call.request.httpMethod.value} ${call.request.uri}")
                    call.respondText("handled by :$port\n")
                }

                post("/{...}") {
                    val body = call.receiveText()
                    println("[Backend $port] Received POST: $body")
                    call.respondText("handled by :$port (POST received)\n")
                }
            }
        }.start(wait = false)
        println("[Backend $port] ktor started on port $port")
    }

    fun stop() {
        server?.stop(1000, 5000)
    }
}

interface SelectionStrategy {
    fun select(backends: List<Backend>): Backend?
}

class RoundRobinStrategy : SelectionStrategy {
    private val counter = AtomicInteger(0)

    override fun select(backends: List<Backend>): Backend? {
        if (backends.isEmpty()) return null
        val count = counter.getAndIncrement()
        val index = (count and Int.MAX_VALUE) % backends.size
        return backends[index]
    }
}

class HealthChecker(
    private val backends: List<Backend>, private val intervalMs: Long = 5000
) {
    private val scheduler = Executors.newSingleThreadScheduledExecutor()

    fun start() {
        scheduler.scheduleAtFixedRate({
            backends.forEach { backend ->
                val wasHealthy = backend.isHealthy
                backend.isHealthy = try {
                    Socket(backend.host, backend.port).use { true }
                } catch (e: Exception) {
                    false
                }
                if (wasHealthy != backend.isHealthy) {
                    println("[HealthChecker] Backend ${backend.host}:${backend.port} is now ${if (backend.isHealthy) "HEALTHY" else "UNHEALTHY"}")
                }
            }
        }, 0, intervalMs, TimeUnit.MILLISECONDS)
    }

    fun stop() {
        scheduler.shutdown()
    }
}

class LoadBalancer(
    private val publicPort: Int, private val backends: List<Backend>, private val strategy: SelectionStrategy
) {
    private var server: EmbeddedServer<NettyApplicationEngine, NettyApplicationEngine.Configuration>? = null
    private val client = HttpClient(CIO)

    fun start() {
        server = embeddedServer(Netty, port = publicPort) {
            configureHttp()
            configureRateLimiting()

            routing {
                route("/{...}") {
                    handle {
                        handleRequest(call)
                    }
                }
            }
        }.start(wait = false)
        println("[LoadBalancer] Ktor started on port $publicPort")
    }

    private suspend fun handleRequest(call: ApplicationCall) {
        val healthyBackends = backends.filter { it.isHealthy }
        val backend = strategy.select(healthyBackends)

        if (backend == null) {
            call.respond(HttpStatusCode.ServiceUnavailable, "Error: No healthy backends available")
            return
        }

        println("[LoadBalancer] Forwarding ${call.request.httpMethod.value} ${call.request.uri} to: ${backend.host}:${backend.port}")

        try {
            val response = client.request("http://${backend.host}:${backend.port}${call.request.uri}") {
                method = call.request.httpMethod
                headers {
                    call.request.headers.forEach { name, values ->
                        if (name.equals(HttpHeaders.Host, ignoreCase = true)) return@forEach
                        if (name.equals(HttpHeaders.ContentLength, ignoreCase = true)) return@forEach
                        if (name.equals(HttpHeaders.TransferEncoding, ignoreCase = true)) return@forEach
                        if (name.equals(HttpHeaders.Upgrade, ignoreCase = true)) return@forEach
                        if (name.equals(HttpHeaders.Connection, ignoreCase = true)) return@forEach
                        appendAll(name, values)
                    }
                    append(HttpHeaders.XForwardedFor, call.request.local.remoteHost)
                }
                setBody(call.receiveChannel())
            }

            call.respondOutputStream(status = response.status) {
                response.bodyAsChannel().copyTo(this)
            }
        } catch (e: Exception) {
            println("[LoadBalancer] Proxy error: ${e.message}")
            call.respond(HttpStatusCode.InternalServerError, "Error: Failed to reach backend")
        }
    }

    fun stop() {
        server?.stop(1000, 5000)
        client.close()
    }
}
