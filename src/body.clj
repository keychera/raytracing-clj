(ns body
  (:require
   [clojure.spec.alpha :as s]))

;; ray-hittable body

;; hit-fn is (fn [ray t-min t-max] hit-record)
(s/def ::hit-fn fn?)
(s/def ::mat any?)
