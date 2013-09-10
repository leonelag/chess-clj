(ns chess.web
  (:use compojure.core)
  (:require [compojure.handler  :as handler]
            [compojure.route    :as route]
            [ring.util.response :as resp]
            [ring.middleware.params :as rmw-p]))

(defroutes app-routes
  (POST "/login" {params       :params
                  form-params  :form-params
                  bd           :body}
        {:status 200
         :content-type "text/plain"
         :body (str "p: [" params "]"
                    "f: [" form-params "]"
                    "bd: [" bd "]")})
  (GET "/hello" [] "Hello World")
  (GET "/" [] (resp/redirect "/index.html"))
  (route/resources "/")
  (route/not-found "Not Found"))

(def handler
  (handler/site app-routes))


;;
;; To start a web server from the repl.
;;
;; (defn run
;;   ;; http://blog.darevay.com/2010/11/compojure-the-repl-and-vars/
;;   [options]
;;   (let [options (merge {:port 8080
;;                         :join? true}
;;                        options)]
;;     (ring.adapter.jetty/run-jetty (var chess.web/app-routes) options)))
;; (def server (run {:join? false}))
;; (defn stop (.stop server))
;;; To stop the server
;; (.stop server)
