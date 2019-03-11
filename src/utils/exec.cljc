(ns utils.exec
  (:require
   #?(:clj [clojure.core.async :as async] :cljs [cljs.core.async :as async])
   #?(:clj [clojure.java.io :as io])
   #?(:clj [clojure.spec.alpha :as s] :cljs [cljs.spec.alpha :as s])))

; cljs implementation based on https://gist.github.com/frankhenderson/d60471e64faec9e2158c

; clj implementation based on https://stackoverflow.com/questions/45292625/how-to-perform-non-blocking-reading-stdout-from-a-subprocess-in-clojure

#?(:cljs (def spawn (.-spawn (js/require "child_process"))))

#?(:cljs
   (defn exec-chan
     "spawns a child process for cmd with args. routes stdout, stderr, and
      the exit code to a channel. returns the channel immediately."
     [cmd args]
     (let [chan (async/chan)
           proc (spawn cmd (if args (clj->js args) (clj->js [])))]
       (.on (.-stdout proc) "data"  #(async/put! chan [:out (str %)]))
       (.on (.-stderr proc) "data"  #(async/put! chan [:err (str %)]))
       (.on proc "close" #(async/put! chan [:exit (str %)]))
       chan)))

#?(:clj
   (defn read-lines
     [chan eos-chan stream-kw stream]
     (async/go
       (with-open [reader (io/reader stream)]
         (loop []
           (if-let [line (.readLine reader)]
             (do
               (async/>! chan [stream-kw line])
               (recur))
             (async/>! eos-chan stream-kw)))))))

#?(:clj
   (defn exec-chan
     "spawns a child process for cmd with args. routes stdout, stderr, and
      the exit code to a channel. returns the channel immediately."
     [cmd args]
     (let [chan (async/chan)
           eos-chan (async/chan)
           builder (ProcessBuilder. (into-array String (cons cmd (map str args))))
           process (.start builder)]
       (read-lines chan eos-chan :out (.getInputStream process))
       (read-lines chan eos-chan :err (.getErrorStream process))
       (async/go
         (async/<! eos-chan)
         (async/<! eos-chan)
         (.waitFor process)
         (async/>! chan [:exit (.exitValue process)]))
       chan)))

(s/fdef exec
  :args (s/cat :cmd string? :args (s/* any?))
  :ret map?)

(defn exec
  "executes cmd with args. returns a channel immediately which
   will eventually receive a result map of 
   {:out [stdout-lines] :err [stderr-lines] :exit [exit-code]}"
  [cmd & args]
  (let [chan (exec-chan cmd args)]
    ; TODO use async/go-loop (or even async/reduce) instead of loop/recur?
    (async/go (loop [output (async/<! chan) result {}]
                (if (= :exit (first output))
                  (assoc result :exit (second output))
                  (recur (async/<! chan) (update result (first output) #(conj (or % []) (second output)))))))))
