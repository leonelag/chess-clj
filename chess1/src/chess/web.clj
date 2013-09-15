(ns chess.web
  (:use compojure.core
        sandbar.stateful-session)
  (:require [compojure.handler  :as handler]
            [compojure.route    :as route]
            [ring.util.response :as resp]
            [ring.middleware.params :as rmw-p]
            [clojure.data.json :as json]

            [ring.middleware.stacktrace]))

(defn log [& args]
  (let [print   #(. System/out print %)
        println #(. System/out println)]
    (when args
      (print (first args)))
    (doseq [s (rest args)]
      (print " ")
      (print s))
    (println)))

(def users
  "Available user names and passwords for login."
  [["kasparov"      "kasparov"]
   ["deepblue"      "deepblue"]
   ["bobbyfischer"  "bobbyfischer"]
   ["freddymercury" "freddymercury"]
   ["obelix"        "obelix"]
   ["leonel"        "leonel"]])

(def logged-in
  "Users currently logged in."
  (atom #{}))

(defn login
  "Tries to match a username and password against the database.
   If it succeeds, logs the user in.
   Returns nil otherwise."
  [username password]
  (if (some (fn [[u p]]
              (and (= u username)
                   (= p password)))
            users)
    (swap! logged-in conj username)))
(defn logout
  "Logs the user out"
  [username]
  (session-put! :username nil)
  (swap! logged-in disj username))

;; helper functions for JSON responses
(defn json-response
  [body]
  {:status       200
   :headers      {"Content-Type" "application/json; charset=UTF-8"}
   :body         (json/write-str body)})
(defn json-ok-msg
  [msg]
  (json-response {:status "ok"
                  :msg msg}))
(defn json-err-msg
  [msg]
  (json-response {:status "nok"
                  :msg    msg}))

(defroutes app-routes
  (POST "/app/login" {form-params :form-params}
        ;; if user is already logged in, do nothing.
        (when-not (session-get :username)
          (let [username (get form-params "username")
                password (get form-params "password")]
            (if (and username
                     password
                     (login username password))
              (do (session-put! :username username)
                  (json-ok-msg  "Login successful."))
              (json-err-msg "Invalid login.")))))
  (POST "/app/logout" {}
        (when-let [username (session-get :username)]
          (do (logout username)
              (json-ok-msg "Logout successful."))))
  (GET "/app/users.json" {}
       (json-response {:users @logged-in}))
  (GET "/app/login.json" {}
       (let [username (session-get :username)]
         (json-response {:logged (if username
                                   username
                                   false)}))) ;; needs boolean false here.
  
  (GET "/hello" [] "Hello World")
  (GET "/" [] (resp/redirect "/index.html"))
  (route/resources "/")
  (route/not-found "Not Found"))

(def handler
  (-> app-routes
      (handler/site)

      (ring.middleware.stacktrace/wrap-stacktrace)
      
      (wrap-stateful-session)))

;;
;; To start a web server from the repl.
;;
;; (defn run
;;   ;; http://blog.darevay.com/2010/11/compojure-the-repl-and-vars/
;;   [options]
;;   (let [options (merge {:port 8080
;;                         :join? true}
;;                        options)]
;;     (ring.adapter.jetty/run-jetty (var chess.web/handler) options)))
;; (def server (run {:join? false}))
;; (defn stop (.stop server))
;;; To stop the server
;; (.stop server)
