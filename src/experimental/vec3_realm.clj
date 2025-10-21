(ns experimental.vec3-realm)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; to the typed land
(definterface Vec3Ops
  ;; member
  (^double x [^long i])
  (^double y [^long i])
  (^double z [^long i])
  ;; scalar
  (^double lengthSquared [v])
  (^double length [v])
  (^double dot [u v])
  ;; vec3 ops, mutate target on realm
  (create [^long target ^double u0 ^double u1 ^double u2])
  (copy [target u])
  (add [target u v])
  (subtract [target u v])
  (multVec3 [target u v])
  (multScalar [target u ^double scalar])
  (divideScalar [target u ^double scalar])
  (cross [target u v])
  (unitVec3 [target v])
  ;; take
  (read [i]))

(defmacro operator [op this target u v]
  `(let [u0# (.x ~this ~u) u1# (.y ~this ~u) u2# (.z ~this ~u)
         v0# (.x ~this ~v) v1# (.y ~this ~v) v2# (.z ~this ~v)]
     (.create ~this ~target (~op u0# v0#) (~op u1# v1#) (~op u2# v2#))))

(defmacro operator-scalar [op this target v scalar]
  `(let [x# (.x ~this ~v) y# (.y ~this ~v) z# (.z ~this ~v)]
     (.create ~this ~target (~op x# ~scalar) (~op y# ~scalar) (~op z# ~scalar))))

(deftype Realm [^doubles realm]
  Vec3Ops
  (create [_ ^long target ^double u0 ^double u1 ^double u2]
    (doto realm
      (aset target u0)
      (aset (+ 1 target) u1)
      (aset (+ 2 target) u2)))

  (^double x [_ ^long i] (aget realm i))
  (^double y [_ ^long i] (aget realm (+ 1 i)))
  (^double z [_ ^long i] (aget realm (+ 2 i)))

  (^double lengthSquared [this v]
    (let [x (.x this v) y (.y this v) z (.z this v)]
      (+ (* x x) (* y y) (* z z))))

  (^double length [this v]
    (Math/sqrt (.lengthSquared this v)))

  (^double dot [this u v]
    (+ (* (.x this u) (.x this v))
       (* (.y this u) (.y this v))
       (* (.z this u) (.z this v))))

  (copy [this target u]
    (.create this target (.x this u) (.y this u) (.z this u)))

  (add [this target u v]
    (operator + this target u v))

  (subtract [this target u v]
    (operator - this target u v))

  (multVec3 [this target u v]
    (operator * this target u v))

  (multScalar [this target v ^double scalar]
    (operator-scalar * this target v scalar))

  (divideScalar [this target v ^double scalar]
    (operator-scalar / this target v scalar))

  (cross [this target u v]
    (let [u0 (.x this u) u1 (.y this u) u2 (.z this u)
          v0 (.x this v) v1 (.y this v) v2 (.z this v)]
      (.create this target
               (- (* u1 v2) (* u2 v1))
               (- (* u2 v0) (* u0 v2))
               (- (* u0 v1) (* u1 v0)))))

  (unitVec3 [this target v]
    (.divideScalar this target v (.length this v)))

  (read [this i] [(.x this i) (.y this i) (.z this i)]))


(comment
  (-> (doto (Realm. (make-array Double/TYPE 9))
        (.create 0 0.0 1.0 1.0)
        (.create 3 0.0 1.0 1.0)
        (.add 6 0 3))
      (.lengthSquared 6)))