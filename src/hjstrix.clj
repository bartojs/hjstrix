(ns hjstrix
  (:require [clojure.core.async :refer [chan go-loop go <! >! timeout]]))

(def commands (atom {}))

(defn command 
  "crate a command called name with the run function, a fallback function and a timeout in ms, with pool size"
  [run fallback timeout poolsize] 
  (when-not (contains? @commands (str run))
    (let [ch (chan poolsize)]
      (swap! commands assoc (str run) {:run run :fallback fallback :timeout timeout :chan ch :poolsize poolsize})
      (go-loop [] 
               (let [args (<! ch)] 
                    ;; this is where we would run
                    (println "go-loop" (str run) args)) 
               (recur)))))

(defn run 
  [func & args]
  (if-let [cmd (get @commands (str func))]
    (do (go 
          (>! (:chan cmd) args)
          ))
    (println "ERROR! NO COMMAND" (str func)))) 

(comment
    (defn myfun [a] (println "myfun" a))    
    (command myfun nil 0 3)
    (run myfun "bla") 
    (run myfun "moo") 
  ) 

