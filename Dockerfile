FROM clojure:lein-2.8.3 AS build
WORKDIR /app
COPY project.clj .
COPY ./src src
RUN lein deps
RUN rm src/pine/config.*
COPY src/pine/config.default src/pine/config.clj
CMD ["lein", "trampoline", "ring", "server-headless", "33333"]
