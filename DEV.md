# Dev

## Clojure container

```
docker run --rm -it \
  -u `id -u $USER`:`id -g $USER` \
  -w `pwd` \
  -v ~/:/home/${USER} \
  -v `pwd`:`pwd` \
  -v /etc/passwd:/etc/passwd \
  -v ~/.m2/repository:/root/.m2/repository \
  --name lein \
  -p 33333:33333 \
  --add-host host.docker.internal:host-gateway \
  clojure:lein-2.8.3 \
  lein repl
```
