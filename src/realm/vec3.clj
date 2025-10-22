(ns realm.vec3)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; to the typed land
(definterface Vec3Ops
  (vec3 [^long target ^double u0 ^double u1 ^double u2])
  ;; member
  (^double x [^long i])
  (^double y [^long i])
  (^double z [^long i])
  ;; scalar
  (^double lengthSquared [^long v])
  (^double length [^long v])
  (^double dot [^long u ^long v])
  ;; vec3 ops, mutate target on realm
  (copy [^long target ^long u])
  (add [^long target ^long u ^long v])
  (subt [^long target ^long u ^long v])
  (multVec3 [^long target ^long u ^long v])
  (mult [^long target ^long u ^double scalar])
  (divi [^long target ^long u ^double scalar])
  (cross [^long target ^long u ^long v])
  (unitVec3 [^long target ^long v])
  ;; take
  (read [^long i]))

(defmacro operator [op this target u v]
  `(let [u0# (.x ~this ~u) u1# (.y ~this ~u) u2# (.z ~this ~u)
         v0# (.x ~this ~v) v1# (.y ~this ~v) v2# (.z ~this ~v)]
     (.vec3 ~this ~target (~op u0# v0#) (~op u1# v1#) (~op u2# v2#))))

(defmacro operator-scalar [op this target v scalar]
  `(let [x# (.x ~this ~v) y# (.y ~this ~v) z# (.z ~this ~v)]
     (.vec3 ~this ~target (~op x# ~scalar) (~op y# ~scalar) (~op z# ~scalar))))

(deftype Realm [^doubles realm]
  Vec3Ops
  (vec3 [_ target u0 u1 u2]
    (doto realm
      (aset target u0)
      (aset (+ 1 target) u1)
      (aset (+ 2 target) u2)))

  (x [_ i] (aget realm i))
  (y [_ i] (aget realm (+ 1 i)))
  (z [_ i] (aget realm (+ 2 i)))

  (lengthSquared [this v]
    (let [x (.x this v) y (.y this v) z (.z this v)]
      (+ (* x x) (* y y) (* z z))))

  (length [this v]
    (Math/sqrt (.lengthSquared this v)))

  (dot [this u v]
    (+ (* (.x this u) (.x this v))
       (* (.y this u) (.y this v))
       (* (.z this u) (.z this v))))

  (copy [this target u]
    (.vec3 this target (.x this u) (.y this u) (.z this u)))

  (add [this target u v]
    (operator + this target u v))

  (subt [this target u v]
    (operator - this target u v))

  (multVec3 [this target u v]
    (operator * this target u v))

  (mult [this target v scalar]
    (operator-scalar * this target v scalar))

  (divi [this target v scalar]
    (operator-scalar / this target v scalar))

  (cross [this target u v]
    (let [u0 (.x this u) u1 (.y this u) u2 (.z this u)
          v0 (.x this v) v1 (.y this v) v2 (.z this v)]
      (.vec3 this target
               (- (* u1 v2) (* u2 v1))
               (- (* u2 v0) (* u0 v2))
               (- (* u0 v1) (* u1 v0)))))

  (unitVec3 [this target v]
    (.divi this target v (.length this v)))

  (read [this i] [(.x this i) (.y this i) (.z this i)]))


(comment
  (-> (doto (Realm. (make-array Double/TYPE 9))
        (.vec3 0 0.0 1.0 1.0)
        (.vec3 3 0.0 1.0 1.0)
        (.add 6 0 3))
      (.lengthSquared 6)))