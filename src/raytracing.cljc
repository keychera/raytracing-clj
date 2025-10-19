(ns raytracing
  (:require
   #?@(:bb  [[clojure.java.io :as io]]
       :clj [[clj-async-profiler.core :as prof]
             [clojure.java.io :as io]
             [ppm2png :refer [ppm->png]]])
   [hit :as hit]
   [material :as material]
   [hittable :as hittable]
   [ray :as ray]
   [vec3a :as vec3a]))

;; following https://raytracing.github.io/books/RayTracingInOneWeekend.html

#?(:clj (set! *warn-on-reflection* true))

(defn clamp [x min-v max-v] (min max-v (max x min-v)))

(defn linear->gamma [^double component]
  (if (> component 0) (Math/sqrt component) 0))

(defn write-color! [^java.io.BufferedWriter out color]
  (let [[r g b] (mapv #(int (* 256 (clamp (linear->gamma %) 0.0 0.999))) color)]
    (.write out (str r " " g " " b "\n"))))

;; sugar, spice, everything nice
;; why macro? the color looks nicer on my editor
(defmacro BLACK [] `(vec3a/make))
(defmacro RGB [r g b] `(vec3a/make ~r ~g ~b))

(defn hit-anything [ray ^"[Lclojure.lang.PersistentArrayMap;" bodies-arr t-min t-max]
  (let [length (count bodies-arr)]
    (loop [i 0 closest-so-far t-max hit-record nil]
      (if (< i length)
        (let [body    (aget bodies-arr i)
              hit-fn  (::hittable/hit-fn body)
              got-hit (hit-fn body ray t-min closest-so-far)]
          (if (some? got-hit)
            (recur (inc i) (::hit/t got-hit) got-hit)
            (recur (inc i) closest-so-far    hit-record)))
        hit-record))))

(defn ray-color [{::ray/keys [^doubles direction] :as ray} depth ^"[Lclojure.lang.PersistentArrayMap;" world-arr]
  (if (<= depth 0)
    (BLACK)
    (if-let [hit-record (hit-anything ray world-arr 1e-3 ##Inf)]
      (let [scatter-fn (some-> hit-record ::hit/what ::material/scatter-fn)
            scattered  (when scatter-fn (scatter-fn ray hit-record))]
        (if scattered
          (vec3a/mult-vec3 (ray-color (::material/scattered-ray scattered) (dec depth) world-arr)
                           (::material/attenuation scattered))
          (BLACK)))
      (let [y (vec3a/y (vec3a/unit direction))
            a (* 0.5 (+ y 1.0))]
        (vec3a/add (vec3a/mult-scalar (vec3a/make 1.0 1.0 1.0) (- 1.0 a))
                   (vec3a/mult-scalar (vec3a/make 0.5 0.7 1.0) a))))))

(defn deg->rad [^double d]
  (-> d (* Math/PI) (/ 180.0)))

(def hittables
  [;; ground
   (merge (hittable/sphere (vec3a/make 0.0 -100.5 -1.0) 100.0)
          (material/lambertian (RGB 0.8 0.8 0.0)))
   ;; center
   (merge (hittable/sphere (vec3a/make 0.0 0.0 -1.2) 0.5)
          (material/lambertian (RGB 0.1 0.2 0.5)))
   ;; left
   (merge (hittable/sphere (vec3a/make -1.0 0.0 -1.0) 0.5)
          (material/dielectric 1.5))
   ;; bubble
   (merge (hittable/sphere (vec3a/make -1.0  0.0 -1.0) 0.4)
          (material/dielectric (/ 1.00 1.5)))
   ;; right
   (merge (hittable/sphere (vec3a/make 1.0 0.0 -1.0) 0.5)
          (material/metal (RGB 0.8 0.6 0.2) 1.0))])

(def R (Math/cos (/ Math/PI 4)))

(def hittables2
  [(merge (hittable/sphere (vec3a/make (- R) 0.0 -1.0) R)
          (material/lambertian (RGB 0.0 0.0 1.0)))
   ;; right
   (merge (hittable/sphere (vec3a/make R 0.0 -1.0) R)
          (material/lambertian (RGB 1.0 0.0 0.0)))])

;; unsure if this is a good idea
;; what it does: check if local binding have sym, if not, nil instead of error
;; purpose: to make the inner part of main is still eval-able
(defmacro local [sym]
  (first (filter #(= (str sym) (str "(quote " % ")")) (keys &env))))

(defmacro vars->map [& vars]
  (zipmap (map (comp keyword name) vars) vars))

(defn- defocus-disk-sample [center defocus-disk-u defocus-disk-v]
  (let [p (vec3a/random-in-unit-disk)]
    (-> center
        (vec3a/add (vec3a/mult-scalar defocus-disk-u (vec3a/x p)))
        (vec3a/add (vec3a/mult-scalar defocus-disk-v (vec3a/y p))))))

(defn -main [& args]
  (let [samples-per-px #?(:clj (or (some-> (first args) Integer/parseInt) 100))
        max-depth      #?(:clj (or (some-> (second args) Integer/parseInt) 50))]
    (println "config:" (vars->map samples-per-px max-depth))
    (time
     (#?@(:clj     [prof/profile {:event :alloc}]
          :default [do])
      (let [;; image
            to-render       (to-array hittables)

            ;; camera
            aspect-ratio    16/9
            image-width     400
            image-height    (int (/ image-width aspect-ratio))
            samples-per-px  (or (local 'samples-per-px) 100)
            max-depth       (or (local 'max-depth) 50)
            vfov            20.0
            look-from       (vec3a/make -2.0 2.0 1.0)
            look-at         (vec3a/make 0.0 0.0 -1.0)
            vup             (vec3a/make 0.0 1.0 0.0)
            defocus-angle   10.0
            focus-dist      3.4

            theta           (deg->rad vfov)
            h               (Math/tan (/ theta 2))
            viewport-height (* 2.0 h focus-dist)
            viewport-width  (* viewport-height (/ image-width image-height)) ;; not using aspect-ratio is deliberate here

            w               (vec3a/unit (vec3a/subtract look-from look-at))
            u               (vec3a/unit (vec3a/cross vup w))
            v               (vec3a/cross w u)

            camera-center   look-from
            viewport-u      (vec3a/mult-scalar u viewport-width)
            viewport-v      (vec3a/mult-scalar (vec3a/negative v) viewport-height) ;; negative because we want upper-left to be zero and increases at we scan down
            pixel-du        (vec3a/divide viewport-u image-width)
            pixel-dv        (vec3a/divide viewport-v image-height)
            upper-left      (-> camera-center
                                (vec3a/subtract (vec3a/mult-scalar w focus-dist))
                                (vec3a/subtract (vec3a/divide viewport-u 2))
                                (vec3a/subtract (vec3a/divide viewport-v 2)))
            pixel-00-loc    (vec3a/add upper-left (vec3a/mult-scalar (vec3a/add pixel-du pixel-dv) 0.5))

            defocus-radius  (* focus-dist (Math/tan (deg->rad (/ defocus-angle 2.0))))
            defocus-disk-u  (vec3a/mult-scalar u defocus-radius)
            defocus-disk-v  (vec3a/mult-scalar v defocus-radius)

            colors          (for [j (range image-height)
                                  i (range image-width)]
                              (loop [k 0 accum nil]
                                (if (< k samples-per-px)
                                  (let [pixel-sample  (-> pixel-00-loc
                                                          (vec3a/add (vec3a/mult-scalar pixel-du (+ i (- (rand) 0.5))))
                                                          (vec3a/add (vec3a/mult-scalar pixel-dv (+ j (- (rand) 0.5)))))
                                        ray-origin    (if (<= defocus-angle 0)
                                                        camera-center
                                                        (defocus-disk-sample camera-center defocus-disk-u defocus-disk-v))
                                        ray-direction (vec3a/subtract pixel-sample ray-origin)
                                        a-ray         #::ray{:origin ray-origin :direction ray-direction}
                                        color         (ray-color a-ray max-depth to-render)
                                        accum         (if accum
                                                        (vec3a/add accum color)
                                                        color)]
                                    (recur (inc k) accum))
                                  (vec3a/divide accum samples-per-px))))]
        #?(:clj
           (with-open [out (io/writer "scene.ppm")]
             (.write out (str "P3\n" image-width " " image-height "\n255\n"))
             (doseq [color colors]
               (write-color! out color))))
        #?(:bb  :noop
           :clj (ppm->png "scene.ppm" "scene.png")))))))
