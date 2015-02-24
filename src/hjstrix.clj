(ns hjstrix
  (:require [clojure.core.async :refer [chan buffer go-loop go <! >! >!! timeout alt!! close!]]))

(def commands (atom {}))
(def metrics (atom {}))

(defn- circuit-breaker [cmdname]
  (> (count (get-in @metrics [cmdname :calls])) 10)
  )

(defn- metric [cmdname type]
  (swap! metrics update-in [cmdname type] conj (java.util.Date.)))

(defn- fallback [cmd err args]
  (metric (str (:run cmd)) err)
  (if-let [fallback (:fallback cmd)]
    (apply fallback err args)
    (println "ERROR! no fallback for" err)))

(defn command
  "create a command with the run function (should not return nil), a fallback function (takes err and args), a timeout (in ms), with pool size (max concurrency)"
  [run fallback timeout poolsize]
  (when-not (contains? @commands (str run))
    (let [buf (buffer poolsize)
          ch (chan buf)
          cmdname (str run)]
      (swap! commands assoc cmdname {:run run :fallback fallback :timeout timeout :chan ch :buffer buf :poolsize poolsize})
      (swap! metrics assoc cmdname {:calls [] :success [] :failed [] :timeouts [] :full [] :circuit []})
      (go-loop []
               (let [[outchan args] (<! ch)]
                    (try
                      (>! outchan (or (apply run args) ""))
                      (metric cmdname :success)
                      (catch Exception e
                        (metric cmdname :failed)
                        (close! outchan)
                        (if fallback (apply fallback e args) (throw e))))
               (recur))))))

(defn run
  "run the function as a command"
  [func & args]
  (let [cmdname (str func)]
    (if-let [cmd (get @commands cmdname)]
      (do
        (metric cmdname :calls)
        (when (circuit-breaker cmdname)
            (fallback cmd :circuit args))
        (if (.full? (:buffer cmd))
          (do
            (fallback cmd :full args))
          (let [ch (chan)]
            (when (>!! (:chan cmd) [ch args])
              (alt!!
                (timeout (:timeout cmd))
                  (fallback cmd :timeouts args)
                ch
                  ([result] result))))))
    (println "ERROR! NO COMMAND" cmdname))))

(comment
    ;;boot -d org.clojure/core.async:0.1.346.0-17112a-alpha -s src repl
    (defn myfun [a] (println "myfun" a) (Thread/sleep 5000) (println "myfun done") true)
    (defn myfall [err & args] (println "myfall" err args) false)
    (command myfun myfall 3000 3)
    (run myfun "bla")
    (run myfun "moo")
  )
