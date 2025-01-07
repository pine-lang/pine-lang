;; Taken and modified from https://clojure.org/guides/tools_build

(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as io]))

(def class-dir "target/classes")
(def uber-file "target/pine-standalone.jar")

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis @basis
                  :ns-compile '[pine.core]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main 'pine.core}))

(defn info [_]
  (prn {:file uber-file}))
