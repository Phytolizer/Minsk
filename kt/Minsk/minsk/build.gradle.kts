import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.6.10"
    application
}

group = "dev.phytolizer"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-reflect:1.6.10")
    testImplementation("io.kotest:kotest-runner-junit5:5.2.1")
    testImplementation("io.kotest:kotest-assertions-core:5.2.1")
    testImplementation("io.kotest:kotest-framework-datatest:5.2.1")
}

tasks.test {
    useJUnitPlatform()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "11"
}
