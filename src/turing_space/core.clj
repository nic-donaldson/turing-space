(ns turing-space.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [com.hypirion.clj-xchart :as c]))

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
   ;:S     #{1}
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

(def no-transition-machine
  {:Q #{0 1}
   :G #{0 1}
   :b 0
   ;:S #{1}
   :state 0
   :F #{1}
   :d {}
   :tape {:left []
          :current 0
          :right []}})

(defn generate-actions [states final-states gamma]
  (lazy-seq
   (cons nil (for [next-state   (clojure.set/difference states final-states)
                   out-sym gamma
                   move    [:R :L]]
               [out-sym move next-state]))))

(defn generate-state->actions [cur-state states final-states gamma]
  (for [action (generate-actions states final-states gamma)]
    (hash-map cur-state action)))

(defn generate-state-action-maps [states final-states gamma]
  ;; take list of {state action} maps to {state1 action1 state2 action2 ...} maps
  (map #(into {} %)
       ;; take the cartesian product of all {state action} pairs
       (apply combo/cartesian-product
              ;; generate {state action} for each state
              (map #(generate-state->actions % states final-states gamma) states))))

(defn generate-delta [states final-states gamma]
  (let [state-action-maps (generate-state-action-maps states final-states gamma)]
    (map #(into {} %) (apply combo/cartesian-product
                             (for [in-sym gamma]
                               (map #(hash-map in-sym %) state-action-maps))))))

(defn generate-machines [states gamma blank start-state
                        final-states tape-sym]
  (for [delta (generate-delta states final-states gamma)]
    {:Q     states
     :G     gamma
     :b     blank
     :state start-state
     :F     final-states
     :d     delta
     :tape  {:left [] :current tape-sym :right []}}))

(defn lookup-action
  [delta sym state]
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
  tape, returning new machine. Returns nil if the machine has
  halted."
  [{:keys [Q G b S state F d tape] :as machine}]
  (let [sym                                 (:current tape)
        [new-sym move new-state :as action] (lookup-action d sym state)]
    (if (or (= nil action) (state F))
      ;; if we're in a final state or there is no transition, nothing happens
      nil
      (let [new-tape (apply-action-to-tape new-sym move tape b)]
        (-> machine
            (assoc-in [:tape] new-tape)
            (assoc-in [:state] new-state))))))

(defn xtransition
  "Extended transition function. Runs machine until it halts
  or n-steps are performed. Returns resulting machine."
  [machine n-steps]
  (loop [machine   machine
         rem-steps n-steps]
    (if (= 0 rem-steps)
      [machine rem-steps]
      (if-let [next-machine (transition machine)]
        (recur next-machine (dec rem-steps))
        [machine rem-steps]))))

(defn uhhh []
  (let [machines (generate-machines #{:A :B :C} #{0 1} 0 :A #{:C} 0)
        results (pmap #(xtransition % 100) machines)
        remaining-steps (map second results)]
    (c/view
     c/xy-chart
     {"remaining steps" {:x (range (count remaining-steps))
                         :y remaining-steps}}
     {:title "test"
      :theme :matlab})))
