(ns utils.map
  (:require
   #?(:clj [clojure.spec.alpha :as s] :cljs [cljs.spec.alpha :as s])
   [clojure.walk :as walk]))

(s/fdef transform-key
  :args (s/cat :f fn? :m1 map? :m2 map? :k keyword?)
  :ret map?)

(defn transform-key
  "Returns a new map derived from m2, where the value for keyword k in m1 
   is associated with the keyword result of applying function f to keyword k."
  [f m1 m2 k]
  (assoc m2 (f k) (k m1)))

(s/fdef transform-key-ns
  :args (s/cat :ns string? :m1 map? :m2 map? :k keyword?)
  :ret map?)

(defn transform-key-ns
  "Returns a new map derived from m2, where the value for keyword k in m1
   is associated with a keyword that has the same name as k, but in a different namespace ns."
  [ns m1 m2 k]
  (assoc m2 (keyword ns (name k)) (k m1)))

(s/fdef transform
  :args (s/cat :m map? :f fn? :e map?)
  :ret map?)

(defn transform
  "Returns a new map where the values for the keys are the result of reducing the keys of m by f,
   where f is a function that takes the old map, the new map and each key of m.
   Additionally, if the map of exceptions e has a value for a key in m, and the value is a function, 
   this function is used in place of function f. Alternatively, if the value is a keyword, 
   the value for the key in m is associated with this keyword in m."
  [m f e]
  (reduce #(if-let [f-e (%2 e)]
             (if-let [k-e (keyword f-e)]
               (assoc %1 k-e (%2 m))
               (f-e m %1 %2))
             (f m %1 %2)) {} (keys m)))

(s/fdef transform-keys
  :args (s/cat :m map? :f fn?)
  :ret map?)

; https://stackoverflow.com/questions/21483044/transforming-keys-of-a-map-in-clojure/21483588#21483588
(defn transform-keys
  "Returns a new map with all keys transformed, 
   where f is a function that will take a keyword and return a new keyword."
  [m f]
  (into {} (map #(update-in % [0] f) m)))

(s/fdef remove-nil
  :args (s/cat :m map? :ks (s/* keyword?))
  :ret map?)

; https://stackoverflow.com/questions/29362150/remove-nil-values-from-deeply-nested-maps
(defn remove-nil
  "Remove pairs of key-value where value is nil from a (possibly nested) map,
   except for values whose key is in ks."
  [m & ks]
  (walk/postwalk
   #(if (and (not (record? %)) (map? %))
      (into {} (remove (fn [[k v]] (and
                                    (or (empty? ks) (contains? (set ks) k))
                                    (nil? v))) %))
      %)
   m))

(s/fdef remove-empty
  :args (s/cat :m map? :ks (s/* keyword?))
  :ret map?)

(defn remove-empty
  "Remove pairs of key-value where value is an empty coll from a (possibly nested) map,
   except for values whose key is in ks."
  [m & ks]
  (walk/postwalk
   #(if (and (not (record? %)) (map? %))
      (into {} (remove (fn [[k v]] (and
                                    (or (empty? ks) (contains? (set ks) k))
                                    (coll? v)
                                    (empty? v))) %))
      %)
   m))

(s/fdef reverse-all-seq
  :args (s/cat :m map? :ks (s/* keyword?))
  :ret map?)

; https://stackoverflow.com/questions/19150172/deep-reverse-clojure
(defn reverse-all-seq
  "Reverse values for pairs of key-value where value is a seq in a (possibly nested) map, 
   except for values whose key is in ks."
  [m & ks]
  (clojure.walk/postwalk
   #(if (and (not (record? %)) (map? %))
      (into {} (map (fn [[k v]]
                      (if (and (or (empty? ks) (contains? (set ks) k)) (seq? v))
                        [k (clojure.core/reverse v)]
                        [k v])) %))
      %)
   m))