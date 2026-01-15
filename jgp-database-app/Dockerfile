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
RUN ./gradlew clean bootJar -x test --no-daemon

# Runtime stage
FROM eclipse-temurin:25-jre-alpine

# Create non-root user for security
RUN addgroup -S spring && adduser -S spring -G spring

VOLUME /tmp

# Copy the built JAR file
COPY --from=build /workspace/app/build/libs/*.jar /app/app.jar

# Switch to non-root user
USER spring:spring

WORKDIR /app

HEALTHCHECK --interval=30s --timeout=3s --start-period=60s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:8082/jgp-app/api/v1/actuator/health || exit 1

# Optimized Java options for containerized Spring Boot
ENTRYPOINT ["java", \
    "-XX:+UseContainerSupport", \
    "-XX:MaxRAMPercentage=75.0", \
    "-XX:+ExitOnOutOfMemoryError", \
    "-Djava.security.egd=file:/dev/./urandom", \
    "-Dspring.profiles.active=production", \
    "-jar", \
    "app.jar"]

EXPOSE 8082 8443
