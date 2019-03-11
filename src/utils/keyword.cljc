(ns utils.keyword
  (:require 
   #?(:clj [clojure.spec.alpha :as s] :cljs [cljs.spec.alpha :as s])))

(s/fdef add-prefix
  :args (s/cat :prefix string? :k keyword?)
  :ret keyword?)

(defn add-prefix
  [prefix k]
  (->> k
       name
       (str prefix)
       keyword))