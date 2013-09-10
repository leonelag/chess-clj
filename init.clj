;;
;; Paste this in your repl to run the server
;;

(require 'chess.core)
(require 'chess.web)
(require 'ring.adapter.jetty)

(defn run
  ;; http://blog.darevay.com/2010/11/compojure-the-repl-and-vars/
  [options]
  (let [options (merge {:port 8080
                        :join? true}
                       options)]
    (ring.adapter.jetty/run-jetty (var chess.web/handler) options)))
(def server (run {:join? false}))
