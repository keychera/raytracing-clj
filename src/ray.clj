(ns ray 
  (:require
   [vec3]))

;; ray is {::origin vec3 ::direction vec3}

(defn at [{::keys [origin direction]} t]
  (vec3/add origin (vec3/multiply direction t)))

(defn color [ray] [0 0 0])