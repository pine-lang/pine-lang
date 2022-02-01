# Dev setup

Instead of running the server, run the repl:

```
./repl.sh
```

From emacs (using cider), connect to the repl:

```
M-x cider-connect-clj
```

Cider will detect the repl running on localhost. Select it and you are good to go.

## FAQ

### How to run the http server while the repl is running?

I could make it work by explicitly running a jetty server but as soon as I did
that, **cider stopped showing me evaluated results in emacs**.

Anyway, here are the steps:

In project.clj:

```
   :repl {
          :plugins [[cider/cider-nrepl "0.26.0"]
                    [refactor-nrepl "2.5.1"] ]
          :dependencies [[nrepl/nrepl "0.8.3"]
                         ;; [ring/ring-jetty-adapter "1.2.1"] ;; <-- add this dependency
                         ]
          }
```

Now, the server can be run explicitly once the `repl` is running (using `./repl.sh`):

```
(ns pine.repl
  (:require
   [compojure.handler :as handler]
   [pine.handler :as h]
   [ring.adapter.jetty :as ring]))

(ring/run-jetty (handler/site h/app) {:port 33333})
```
