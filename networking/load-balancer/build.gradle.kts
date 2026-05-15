plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(ktorLibs.plugins.ktor)
}

group = "np.com.sudanchapagain"
version = "1.0.0-SNAPSHOT"

application {
    mainClass = "np.com.sudanchapagain.MainKt"
}

kotlin {
    jvmToolchain(17)
}

dependencies {
    implementation(ktorLibs.network.tls)
    implementation(ktorLibs.server.core)
    implementation(ktorLibs.server.defaultHeaders)
    implementation(ktorLibs.server.httpRedirect)
    implementation(ktorLibs.server.netty)
    implementation(ktorLibs.client.core)
    implementation(ktorLibs.client.cio)
    implementation(libs.flaxoos.ktorServerRateLimiting)
    implementation(libs.logback.classic)

    testImplementation(kotlin("test"))
    testImplementation(ktorLibs.server.testHost)
}
