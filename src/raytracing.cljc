(ns raytracing
  (:require
   #?@(:bb  [[clojure.java.io :as io]]
       :clj [[clojure.java.io :as io]
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

(def hittables
  [(merge (hittable/sphere (vec3a/make  0.0 -100.5 -1.0) 100.0)
          (material/lambertian (RGB 0.8 0.8 0.0)))
   (merge (hittable/sphere (vec3a/make  0.0  0.0 -1.0) 0.5)
          (material/lambertian (RGB 0.1 0.2 0.5)))
   (merge (hittable/sphere (vec3a/make -1.0  0.0 -1.0) 0.5)
          (material/dielectric 1.5))
   (merge (hittable/sphere (vec3a/make  1.0  0.0 -1.0) 0.5)
          (material/metal (RGB 0.8 0.6 0.2) 1.0))])

(defn hit-anything [ray bodies t-min t-max]
  (loop [[body & remaining] bodies
         closest-so-far     t-max
         hit-record         nil]
    (if (some? body)
      (let [hit-fn  (::hittable/hit-fn body)
            got-hit (hit-fn body ray t-min closest-so-far)]
        (if (some? got-hit)
          (recur remaining (::hit/t got-hit) got-hit)
          (recur remaining closest-so-far     hit-record)))
      hit-record)))


(defn ray-color [{::ray/keys [^doubles direction] :as ray} depth world]
  (if (<= depth 0)
    (BLACK)
    (if-let [hit-record (hit-anything ray world 1e-3 ##Inf)]
      (let [scatter-fn (some-> hit-record ::hit/what ::material/scatter-fn)
            scattered  (when scatter-fn (scatter-fn ray hit-record))]
        (if scattered
          (vec3a/mult-vec3 (ray-color (::material/scattered-ray scattered) (dec depth) world)
                           (::material/attenuation scattered))
          (BLACK)))
      (let [y (vec3a/y (vec3a/unit direction))
            a (* 0.5 (+ y 1.0))]
        (vec3a/add (vec3a/mult-scalar (vec3a/make 1.0 1.0 1.0) (- 1.0 a))
                   (vec3a/mult-scalar (vec3a/make 0.5 0.7 1.0) a))))))

(defn -main []
  (time
   (let [;; image
         aspect-ratio    16/9
         image-width     400
         image-height    (int (/ image-width aspect-ratio))
         samples-per-px  100
         max-depth       50

         ;; camera
         focal-length    1.0
         viewport-height 2.0
         ;; not using aspect-ratio is deliberate here
         viewport-width  (* viewport-height (/ image-width image-height))
         camera-center   (vec3a/make)
         viewport-u      (vec3a/make viewport-width 0.0 0.0)
         viewport-v      (vec3a/make 0.0 (- viewport-height) 0.0) ;; negative because we want upper-left to be zero and increases at we scan down
         pixel-du        (vec3a/divide viewport-u image-width)
         pixel-dv        (vec3a/divide viewport-v image-height)
         upper-left      (-> camera-center
                             (vec3a/subtract (vec3a/make 0.0 0.0 focal-length))
                             (vec3a/subtract (vec3a/divide viewport-u 2))
                             (vec3a/subtract (vec3a/divide viewport-v 2)))
         pixel-00-loc    (vec3a/add upper-left (vec3a/mult-scalar (vec3a/add pixel-du pixel-dv) 0.5))

         colors          (for [j (range image-height)
                               i (range image-width)]
                           (->> #(let [pixel-sample  (-> pixel-00-loc
                                                         (vec3a/add (vec3a/mult-scalar pixel-du (+ i (- (rand) 0.5))))
                                                         (vec3a/add (vec3a/mult-scalar pixel-dv (+ j (- (rand) 0.5)))))
                                       ray-direction (vec3a/subtract pixel-sample camera-center)
                                       a-ray         #::ray{:origin camera-center :direction ray-direction}]
                                   (ray-color a-ray max-depth hittables))
                                (repeatedly samples-per-px)
                                (reduce vec3a/add)
                                ((fn [color] (vec3a/divide color samples-per-px)))))]
     #?(:clj
        (with-open [out (io/writer "scene.ppm")]
          (.write out (str "P3\n" image-width " " image-height "\n255\n"))
          (doseq [color colors]
            (write-color! out color))))
     #?(:bb  :noop
        :clj (ppm->png "scene.ppm" "scene.png")))))
