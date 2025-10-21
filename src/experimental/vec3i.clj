(ns experimental.vec3i)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn x ^double [^doubles realm ^long i] (aget realm i))
(defn y ^double [^doubles realm ^long i] (aget realm (+ 1 i)))
(defn z ^double [^doubles realm ^long i] (aget realm (+ 2 i)))

(defmacro create! [^doubles realm target x y z]
  `(let [i# (long ~target)]
     (doto ^doubles ~realm
       (aset i# ~x)
       (aset (+ 1 i#) ~y)
       (aset (+ 2 i#) ~z))))

(defmacro operator [op ^doubles realm target u v]
  `(let [u0# (x ~realm ~u) u1# (y ~realm ~u) u2# (z ~realm ~u)
         v0# (x ~realm ~v) v1# (y ~realm ~v) v2# (z ~realm ~v)]
     (create! ~realm ~target (~op u0# v0#) (~op u1# v1#) (~op u2# v2#))))

(defmacro operator-xyz [op ^doubles realm target u x y z]
  `(let [u0# (x ~realm ~u) u1# (y ~realm ~u) u2# (z ~realm ~u)]
     (create! ~realm ~target (~op u0# ~x) (~op u1# ~y) (~op u2# ~z))))

(defmacro operator-scalar [op ^doubles realm target v scalar]
  `(let [x# (x ~realm ~v) y# (y ~realm ~v) z# (z ~realm ~v)]
     (create! ~realm ~target (~op x# ~scalar) (~op y# ~scalar) (~op z# ~scalar))))

;; return scalar
(defn length-squared ^double [^doubles realm v]
  (let [x (x realm v) y (y realm v) z (z realm v)]
    (+ (* x x) (* y y) (* z z))))

(defn length ^double [^doubles realm v]
  (Math/sqrt (length-squared realm v)))

(defn dot ^double [^doubles realm ^doubles u ^doubles v]
  (+ (* (x realm u) (x realm v))
     (* (y realm u) (y realm v))
     (* (z realm u) (z realm v))))

;; mutate value on the realm, return mutated realm
;; the typehint of the return is the type of `realm`, not the vector
;; these mutating fns are storing vector operation back to the realm via `target`
;; though by using doto the return becomes not important
(defn copy! ^doubles [^doubles realm target u]
  (create! realm target (x realm u) (y realm u) (z realm u)))

(defn add! ^doubles [^doubles realm target u v]
  (operator + realm target u v))

(defn subtract! ^doubles [^doubles realm target u v]
  (operator - realm target u v))

(defn mult-vec3! ^doubles [^doubles realm target u v]
  (operator * realm target u v))

(defn mult-scalar! ^doubles [^doubles realm target v ^double scalar]
  (operator-scalar * realm target v scalar))

(defn divide! ^doubles [^doubles realm target v ^double scalar]
  (operator-scalar / realm target v scalar))

(defn cross! ^doubles [^doubles realm target u v]
  (let [u0 (x realm u) u1 (y realm u) u2 (z realm u)
        v0 (x realm v) v1 (y realm v) v2 (z realm v)]
    (create! realm target
             (- (* u1 v2) (* u2 v1))
             (- (* u2 v0) (* u0 v2))
             (- (* u0 v1) (* u1 v0)))))

(defn unit-vec3! ^doubles [^doubles realm ^doubles target v]
  (divide! realm target v (length realm v)))

;; take a vec3 from the realm
(defn read! [^doubles realm i]
  [(x realm i) (y realm i) (z realm i)])


(comment
  (def realm (make-array Double/TYPE 9))

  (-> realm
      (create! 1 3.0 -2.0 0.5)
      (unit-vec3! 1 1)
      (read! 1)))


