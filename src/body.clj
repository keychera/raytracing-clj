(ns body
  (:require
   [clojure.spec.alpha :as s]
   [ray :as ray]
   [vec3a :as vec3a]))

;; ray-hittable body

;; hit-fn is (fn [ray t-min t-max] hit-record)
(s/def ::hit-fn fn?)

(defn hit-record [{::ray/keys [^doubles direction]} ^doubles point ^doubles outward-normal ^double t]
  (let [front-face? (< (vec3a/dot direction outward-normal) 0)
        normal      (if front-face? outward-normal (vec3a/negative outward-normal))]
    {::point point ::normal normal ::t t}))
