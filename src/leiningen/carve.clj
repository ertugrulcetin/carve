(ns leiningen.carve
  (:require [leiningen.core.eval :as lein]
            [leiningen.core.project :as project]))


(defn- plugin-dep-vector
  [{:keys [plugins]}]
  (some
   (fn [[plugin-symb :as dep-vector]]
     (when (= plugin-symb 'ertu/lein-carve)
       dep-vector))
   plugins))


(defn- check-namespace-decls-profile
  [project]
  {:dependencies [(plugin-dep-vector project)]})


(defn carve
  "Carve will search through your code for unused vars."
  [project]
  (let [project (project/merge-profiles project [(check-namespace-decls-profile project)])]
    (lein/eval-in-project
     project
     `(carve.main/carve '~(:carve project))
     '(require 'carve.main))))
