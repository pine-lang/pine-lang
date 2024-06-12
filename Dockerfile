# Stage 1: Build the Uberjar (amd64 and arm64)
FROM clojure:lein-2.8.3 AS build
WORKDIR /app
COPY project.clj .
COPY ./src src
RUN lein deps
RUN rm src/pine/config.*
COPY src/pine/config.default src/pine/config.clj
RUN lein uberjar

# Stage 2: Create a minimal runtime image (amd64 and arm64)
FROM openjdk:8-jre-slim
WORKDIR /app
COPY --from=build /app/target/pine-0.1.0-SNAPSHOT-standalone.jar /app/pine.jar
COPY src/pine/config.clj /app/src/pine/config.clj
COPY src/pine/pine.bnf /app/src/pine/pine.bnf
CMD ["java", "-jar", "pine.jar"]
