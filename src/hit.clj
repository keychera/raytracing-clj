(ns hit 
  (:require
   [ray :as ray]
   [vec3a :as vec3a]
   [clojure.spec.alpha :as s]))

;; hit-record
(s/def ::what any?)
(s/def ::point any?)
(s/def ::normal any?)
(s/def ::t any?)

(defn calc-normal ^doubles [{::ray/keys [^doubles direction]} ^doubles outward-normal]
  (let [front-face? (< (vec3a/dot direction outward-normal) 0)]
    (if front-face? outward-normal (vec3a/negative outward-normal))))