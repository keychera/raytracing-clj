(ns body
  (:require
   [clojure.spec.alpha :as s]
   [ray :as ray]
   [vec3 :as vec3]))

;; ray-hittable body

;; hit-fn is (fn [ray t-min t-max] hit-record)
(s/def ::hit-fn fn?)

(defn hit-record [{::ray/keys [direction]} point outward-normal t]
  (let [front-face? (< (vec3/dot direction outward-normal) 0)
        normal      (if front-face? outward-normal (vec3/negative outward-normal))]
    {::point point ::normal normal ::t t}))
