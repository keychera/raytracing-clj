(ns experimental.vec3i)

(defmacro x ^double [^doubles realm i] `(aget ~realm (* ~i 3)))
(defmacro y ^double [^doubles realm i] `(aget ~realm (+ 1 (* ~i 3))))
(defmacro z ^double [^doubles realm i] `(aget ~realm (+ 2 (* ~i 3))))

(defmacro create! [^doubles realm target x y z]
  `(doto ~realm
     (aset (* ~target 3) (double ~x))
     (aset (+ 1 (* ~target 3)) (double ~y))
     (aset (+ 2 (* ~target 3)) (double ~z))))

(defmacro operator [op ^doubles realm target u v]
  `(let [u0# (x ~realm ~u) u1# (y ~realm ~u) u2# (z ~realm ~u)
         v0# (x ~realm ~v) v1# (y ~realm ~v) v2# (z ~realm ~v)]
     (create! ~realm ~target (~op u0# v0#) (~op u1# v1#) (~op u2# v2#))))

(defmacro operator-scalar [op ^doubles realm target v scalar]
  `(let [x# (x ~realm ~v) y# (y ~realm ~v) z# (z ~realm ~v)]
     (create! ~realm ~target (~op x# ~scalar) (~op y# ~scalar) (~op z# ~scalar))))

(defn closure-1 [^doubles realm u1 closure-fn]
  (closure-fn realm (x realm u1) (y realm u1) (z realm u1)))

;; return scalar
(defn length-squared [^doubles realm v]
  (let [x (x realm v) y (y realm v) z (z realm v)]
    (+ (* x x) (* y y) (* z z))))

(defn length [^doubles realm v]
  (Math/sqrt (length-squared realm v)))

(defn dot [^doubles realm u v]
  (+ (* (x realm u) (x realm v))
     (* (y realm u) (y realm v))
     (* (z realm u) (z realm v))))

;; mutate value on the realm, return mutated realm
(defn copy! [^doubles realm target u]
  (create! realm target (x realm u) (y realm u) (z realm u)))

(defn add! [^doubles realm target u v]
  (operator + realm target u v))

(defn subtract! [^doubles realm target u v]
  (operator - realm target u v))

(defn mult-vec3! [^doubles realm target u v]
  (operator * realm target u v))

(defn mult-scalar! [^doubles realm target v scalar]
  (operator-scalar * realm target v scalar))

(defn divide! ^doubles [^doubles realm target v scalar]
  (operator-scalar / realm target v scalar))

(defn cross! [^doubles realm target u v]
  (let [u0 (x realm u) u1 (y realm u) u2 (z realm u)
        v0 (x realm v) v1 (y realm v) v2 (z realm v)]
    (create! realm target
             (- (* u1 v2) (* u2 v1))
             (- (* u2 v0) (* u0 v2))
             (- (* u0 v1) (* u1 v0)))))

(defn unit-vec3! [^doubles realm target v]
  (-> realm
      (divide! target v (length realm v))))


;; take a vec3 from the realm
(defn read! [^doubles realm i]
  [(x realm i) (y realm i) (z realm i)])


(comment
  (def realm (make-array Double/TYPE 9))

  (-> realm
      (create! 1 3.0 -2.0 0.5)
      (unit-vec3! 1 1)
      (read! 1)))


