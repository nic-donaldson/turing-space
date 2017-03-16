(ns turing-space.core
  (:gen-class))

;;;; Turing machine (https://en.wikipedia.org/wiki/Turing_machine)
;;; A turing machine is a 7-tuple where
;;; Q is a finite, non-empty set of states.
;;; Gamma is a finite, non-empty set of tape alphabet symbols.
;;; b in Gamma is the blank symbol.
;;; Sigma is the set of input symbols (not including b).
;;; delta is a partial function called the transition function that takes
;;;  the current state (not including any states in F), and a symbol.
;;;  It returns a new state, a symbol to write on the tape, and a left or right
;;;  movement.
;;; q0 in Q is the initial state
;;; F in Q is the set of final states

;;; We're going to add tape to this to keep it all
;;; in one spot. Assuming undefined spots to contain the
;;; blank symbol.
;;; Here's the three state busy beaver from Wikipedia:
(def three-state-busy-beaver
  {:Q     #{:A :B :C :HALT}
   :G     #{0 1}
   :b     0
   :S     #{1}
   :state :A
   :F     #{:HALT}
   :d     {0 {:A [1 :R :B]
              :B [1 :L :A]
              :C [1 :L :B]}
           1 {:A [1 :L :C]
              :B [1 :R :B]
              :C [1 :R :HALT]}}
   :tape  {:left    []
           :current 0
           :right   []}})

(defn generate-delta []
  (let [:Q #{1 2}
        :F #{2}
        :G #{0 1}]
    (println "zzz")))

(defn lookup-action
  [{delta :d state :state :as machine} sym]
  (get-in delta [sym state]))

(defn apply-action-to-tape
  [sym move {:keys [left current right] :as tape} blank]
  (let [left  (if (empty? left) [blank] left)
        right (if (empty? right) [blank] right)]
    (condp = move
      :L {:left    (pop left)
          :current (last left)
          :right   (conj right sym)}
      :R {:left    (conj left sym)
          :current (last right)
          :right   (pop right)}
      (throw (ex-info "Not a valid move"
                      {:move move})))))

;;; Transition function
(defn transition
  "Performs turing machine transition on provided machine with
  tape, returning new machine."
  [{:keys [Q G b S state F d tape] :as machine}]
  (if (state F)
    ;; if we're in a final state, nothing happens
    machine
    ;; let's get the action, compute the new tape,
    ;; set the new state,
    ;; and return the new machine.
    (let [sym                      (:current tape)
          [new-sym move new-state] (lookup-action machine sym)
          new-tape                (apply-action-to-tape new-sym move tape b)]
      (-> machine
          (assoc-in [:tape] new-tape)
          (assoc-in [:state] new-state)))))

(defn xtransition
  "Extended transition function. Runs machine until it halts
  or n-steps are performed. Returns resulting machine."
  [machine n-steps]
  (loop [{:keys [state F] :as machine} machine
         rem-steps                     n-steps]
    (if (or (= 0 rem-steps) (state F))
      [machine rem-steps]
      (recur (transition machine) (dec rem-steps)))))

