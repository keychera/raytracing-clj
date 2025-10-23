(ns realm.rng
  (:import
   [java.util.random RandomGenerator]
   [java.util.random RandomGeneratorFactory]))

(def rng (.create (RandomGeneratorFactory/of "Xoshiro256PlusPlus")))

(defmacro rand-double
  ([] `(.nextDouble ^RandomGenerator rng))
  ([origin bound] `(.nextDouble ^RandomGenerator rng ~origin ~bound)))