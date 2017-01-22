(ns sikaliiga.ui.store)

(defmulti reducer (fn [state action] (first action)))

(defmethod reducer :default [state action]
  #?(:cljs (js/console.warn "Default reducer called with" (clj->js state) (clj->js action))
     :clj (println "Default reducer called with" state action))
  state)

(defn make-dispatcher [state reducer middleware-chain]
  (let [dispatch (fn dispatch [action]
                   (if-not (or (nil? action) (nil? (first action)))
                     (swap! state reducer action)))
        dispatch-proxy (atom dispatch)
        next (reduce (fn [next mw]
                        (let [f (mw state dispatch-proxy)]
                           (f next)))
                     dispatch (reverse middleware-chain))]
        (reset! dispatch-proxy next)))

(defn multi-middleware [state dispatch]
  (fn [next]
    (fn [action]
      (cond
        ;; Support dispatch action by just the type
        ;; @example `(dispatch :foo-bar)`
        (not (coll? action)) (@dispatch [action])
        ;; Check if action is a collection of actions and dispatch each separately
        ;; @example `(dispatch [[:foo-bar] [:bar-foo]])`
        (coll? (first action)) (vec (map @dispatch action))
        :else (next action)))))

(defn make-logger-middleware [tag]
  (fn [state dispatch]
    (fn [next]
      (fn [action]
        #?(:cljs (js/console.log tag action)
           :clj (println tag action))
        (let [r (next action)]
          #?(:cljs (js/console.log tag state)
             :clj (println tag state))
          r)))))

(defn make-dispatch [state]
  (make-dispatcher state reducer [multi-middleware (make-logger-middleware "dispatch")]))
