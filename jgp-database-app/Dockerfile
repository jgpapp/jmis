FROM eclipse-temurin:22-alpine
COPY . /home/jgp
WORKDIR /home/jgp
ENV HOME=/home
RUN ./gradlew clean -x test bootJar && mv /home/jgp/build/libs/*.jar /home/jgp-1.0.0.jar && rm -rf /home/jgp
ENTRYPOINT ["java"]
CMD ["-jar", "-Dspring.profiles.active=production", "/home/jgp-1.0.0.jar"]
EXPOSE 8082 8443