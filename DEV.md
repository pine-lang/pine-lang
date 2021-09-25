# Dev

## Clojure container

```
docker run --rm -it \
  -w `pwd` \
  -v `pwd`:`pwd` \
  -v ~/.m2/repository:/root/.m2/repository \
  --name lein \
  -p 33333:33333 \
  --add-host host.docker.internal:host-gateway \
  clojure:lein-2.8.1 \
  bash
```

## Install dependencies

```
lein deps
```

## Run repl

```
lein repl :start :host 0.0.0.0 :port 33333
```
