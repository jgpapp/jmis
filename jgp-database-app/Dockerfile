# Use a more specific JRE image (smaller footprint)
FROM eclipse-temurin:25-jre-alpine AS runtime

# Create non-root user for security
RUN addgroup -S spring && adduser -S spring -G spring

# Build stage
FROM eclipse-temurin:25-alpine AS build
WORKDIR /workspace/app

# Copy gradle wrapper and dependencies first (better caching)
COPY gradlew .
COPY gradle gradle
COPY build.gradle settings.gradle ./

# Download dependencies (cached layer)
RUN chmod +x ./gradlew && ./gradlew dependencies --no-daemon

# Copy source code
COPY src src

# Build application
RUN ./gradlew clean bootJar -x test --no-daemon && \
    mkdir -p build/dependency && \
    cd build/dependency && \
    jar -xf ../libs/*.jar

# Runtime stage
FROM runtime
VOLUME /tmp

# Copy from build stage
ARG DEPENDENCY=/workspace/app/build/dependency
COPY --from=build ${DEPENDENCY}/BOOT-INF/lib /app/lib
COPY --from=build ${DEPENDENCY}/META-INF /app/META-INF
COPY --from=build ${DEPENDENCY}/BOOT-INF/classes /app

# Switch to non-root user
USER spring:spring

# Optimized Java options for containerized Spring Boot
ENTRYPOINT ["java", \
    "-XX:+UseContainerSupport", \
    "-XX:MaxRAMPercentage=75.0", \
    "-XX:+ExitOnOutOfMemoryError", \
    "-Djava.security.egd=file:/dev/./urandom", \
    "-Dspring.profiles.active=production", \
    "-cp", "app:app/lib/*", \
    "org.springframework.boot.loader.launch.JarLauncher"]

EXPOSE 8082 8443
