(ns app.core
  (:require
   [clj-async-profiler.core :as prof]
   [taoensso.timbre :as timbre
    :refer [log  trace  debug  info  warn  error  fatal  report
            logf tracef debugf infof warnf errorf fatalf reportf
            spy get-env]]))

(defn -main []
  (debugf "MEM %dG" (/ (-> (Runtime/getRuntime) .maxMemory) 1024 1024))
  nil)
