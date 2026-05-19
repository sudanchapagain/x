plugins {
    id("java")
    id("application")
}

group = "np.com.sudanchapagain.gol"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(platform("org.junit:junit-bom:5.10.0"))
    testImplementation("org.junit.jupiter:junit-jupiter")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

tasks.test {
    useJUnitPlatform()
}

application {
    mainClass = "np.com.sudanchapagain.gol.Main"
}

tasks.jar {
    manifest {
        attributes["Main-Class"] = "np.com.sudanchapagain.gol.Main"
    }
}
