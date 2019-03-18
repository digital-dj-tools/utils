(ns utils.json
  (:require 
   [clojure.string :as str]
   #?(:clj [clojure.data.json :as j])
   #?(:cljs [clojure.walk :as walk])))

(defn parse-str
  ([json]
   (parse-str json {:keywordize? true}))
  ([json {:keys [keywordize? default] :or {keywordize? true default {}}}]
   #?(:clj
      (if-not (str/blank? json)
        (j/read-str json :key-fn (if keywordize? keyword identity))
        default)
      :cljs
      (if-not (str/blank? json)
        (let [parsed (js->clj (js/JSON.parse json))]
          (if keywordize?
            (walk/keywordize-keys parsed)
            parsed))
        default))))

#?(:clj
   (defn parse
     ([reader]
      (parse reader {:keywordize? true}))
     ([reader {:keys [keywordize?] :or {keywordize? true}}]
      (j/read reader :key-fn (if keywordize? keyword identity)))))

#?(:clj
   (defn emit-str
     [data]
     (j/write-str data)))

#?(:clj
   (defn emit
     [data writer]
     (j/write data writer)))