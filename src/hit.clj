(ns hit
  (:require
   [clojure.spec.alpha :as s]
   [ray :as ray]
   [vec3a :as vec3a]))

;; hit-record
(s/def ::what any?)
(s/def ::point any?)
(s/def ::normal any?)
(s/def ::t any?)
(s/def ::front-face? any?)

(defn front-face? [{::ray/keys [^doubles direction]} ^doubles outward-normal]
  (< (vec3a/dot direction outward-normal) 0))
