(ns ga.example
  (:use ga.ga))

; Given a list of elements, construct a new list using the elements in that set.
; Example: (rand-from-list '(1 2 3 4 5) 2) => (3 5)
(defn rand-from-list [lst num]
  (let [total-el (count lst)]
    (map (fn [x] (nth lst (rand-int total-el))) (range 0 num))))

; Creates a population using rand-from list. Helpful for creating init-fn.
(defn rand-pop [lst num num-pop]
  (map (fn [x] (rand-from-list lst num)) (range 0 num-pop)))

(defn list-crossover [parents]
  (let [s1 (first parents)
        s2 (second parents)
        point (rand-int (min (count s1)
                             (count s2)))]
    [(concat (take point s1) (drop point s2))]))


(def dna (map str ['q 'w 'e 'r 't 'y 'u 'i 'o 'p 'a 's 'd 'f 'g 'h 'j 'k 'l 'z 'x 'c 'v 'b 'n 'm]))

(defn hello-random-individual []
  (rand-from-list dna (rand-int 100)))

(defn hello-fitness [lst]
  (reduce + (map #(if (= %1 %2) 1 0)
                  lst
                  '("h" "e" "l" "l" "o" "w" "o" "r" "l" "d"))))

(defn run-example []
  (let [generations 100
        func-map {:fit-fn hello-fitness
                  :init-fn (partial rand-pop dna 10)
                  ;:init-fn (fn [n] (take n (repeatedly hello-random-individual)))
                  :mut-fn identity
                  :sel-fn roulette-select
                  :cross-fn list-crossover
                  :terminate? #(= %3 generations)}
        set-map {:pop-size 100
                 :crossover-rate 75
                 :mutation-rate 0}]
    (println (first (sort-by #(hello-fitness %) > (run-ga func-map set-map))))))

(run-example)