(ns experimental.vec3i)

(defmacro x ^double [^doubles realm i] `(aget ~realm (* ~i 3)))
(defmacro y ^double [^doubles realm i] `(aget ~realm (+ 1 (* ~i 3))))
(defmacro z ^double [^doubles realm i] `(aget ~realm (+ 2 (* ~i 3))))

(defmacro create! [^doubles realm i x y z]
  `(doto ~realm
     (aset (* ~i 3) ~x)
     (aset (+ 1 (* ~i 3)) ~y)
     (aset (+ 2 (* ~i 3)) ~z)))

(defmacro operator [op ^doubles realm u v target]
  `(let [u0# (x ~realm ~u) u1# (y ~realm ~u) u2# (z ~realm ~u)
         v0# (x ~realm ~v) v1# (y ~realm ~v) v2# (z ~realm ~v)]
     (create! ~realm ~target (~op u0# v0#) (~op u1# v1#) (~op u2# v2#))))

(defmacro operator-scalar [op ^doubles realm v scalar target]
  `(let [x# (x ~realm ~v) y# (y ~realm ~v) z# (z ~realm ~v)]
     (create! ~realm ~target (~op x# ~scalar) (~op y# ~scalar) (~op z# ~scalar))))

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

;; mutate value on the realm, return realm
(defn add! [^doubles realm u v target]
  (operator + realm u v target))

(defn subtract! [^doubles realm u v target]
  (operator - realm u v target))

(defn mult-vec3! [^doubles realm u v target]
  (operator * realm u v target))

(defn mult-scalar! [^doubles realm v scalar target]
  (operator-scalar * realm v scalar target))

(defn divide! [^doubles realm v scalar target]
  (operator-scalar / realm v scalar target))

(defn cross! [^doubles realm u v target]
  (let [u0 (x realm u) u1 (y realm u) u2 (z realm u)
        v0 (x realm v) v1 (y realm v) v2 (z realm v)]
    (create! realm target
             (- (* u1 v2) (* u2 v1))
             (- (* u2 v0) (* u0 v2))
             (- (* u0 v1) (* u1 v0)))))

(defn unit-vec3! [^doubles realm v target]
  (-> realm
      (divide! v (length realm v) target)))


;; take a vec3 from the realm
(defn read! [^doubles realm i]
  [(x realm i) (y realm i) (z realm i)])


(comment
  (def realm (make-array Double/TYPE 9))

  (-> realm
      (create! 1 3.0 -2.0 0.5)
      (unit-vec3! 1 1)
      (read! 1)))


