(ns dke.contrib.tasks
  (:use [dke.contrib.dkeutils :only (run-as-script? form-to-string)]
        [clojure.contrib.json :only (json-str read-json)]))

(defmacro task [arglist & body]
  (.toString
    `(fn [& args#]
       (apply (fn ~arglist ~@body) args#))))

; returns a queued task representation of a task and corresponding arguments
; i.e. returns a pair of the form:
;      [task-string json-arguments-string]
(defn task-pair [t-str args]
  [t-str (json-str args)])

; returns a pair of the form [function-f json-arguments-for-function-f]
(defn load-task [[t-str json-args]]
  (let [task-fn (load-string t-str)
        task-args (read-json json-args)]
    (fn [] (apply task-fn task-args))))

(defn main []
  (let [t (task [a b]
            (println a)
            (println b))
        marshalled-task (task-pair t ["David" "Ellis"])
        unmarshalled-task-fn (load-task marshalled-task)]
    (println marshalled-task)
    (println "------------------------")
    (println (unmarshalled-task-fn))))

(defn main2 []
  (let [cp (task []
             (map #(java.io.File. %)
                  (.split (System/getProperty "java.class.path")
                          (System/getProperty "path.separator"))))
        marshalled-task (task-pair cp [])
        unmarshalled-task-fn (load-task marshalled-task)]
    (println marshalled-task)
    (println "------------------------")
    (println (unmarshalled-task-fn))))

; Only run the application automatically if run as a script, not if loaded in a REPL with load-file.
(if (run-as-script?) (main))
