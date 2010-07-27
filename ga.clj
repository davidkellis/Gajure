(ns ga.ga
  (:use [clojure.contrib.seq-utils :only (find-first)]
        [dke.contrib.dkeutils :only (tupleize)]))

; Creates a population of num-pop individuals using the generator function build-rand-individual.
;
; Arguments:
; build-rand-individual - a function of zero arguments that returns a randomly generated individual
; num-pop - the number of individuals in the population
(defn build-rand-pop [build-rand-individual num-pop]
  (map (fn [x] (build-rand-individual)) (range 0 num-pop)))

; Selects/returns a single individual from the population s.t. the liklihood of returning a particular
; individual is porportional to its fitness value.
; I used the following to check the behavior of roulette-select:
; (map (fn [[k v]] (println k "->" v ";" (/ (float v) 100000))) (frequencies (map (fn [x] (roulette-select [10 20 30 40 50] [5 4 3 2 1] 15)) (range 100000))))
; The values produced seem to fall in line with the frequencies given by: (map #(/ (float %) 15) [1 2 3 4 5])
(defn roulette-select [pop-seq fitness-seq fitness-total]
  (let [target (* (rand) fitness-total)]
    (first (find-first #(>= (second %) target)
                       (tupleize pop-seq
                                 (reductions + fitness-seq))))))

; Apply cross-fn to parent-population to generate a new population.
; cross-fn: takes a lazy sequence of parents with which to cross in order to produce children (or a single child).
;           returns a sequence containing at least one child.
; select-parent is a function that when called returns a single parent from the parent population.
; pop-size is the size of the new baby population that we are producing
(defn do-crossover [cross-fn select-parent pop-size]
  (loop [new-population []]
    (if (>= (count new-population) pop-size)
      (take pop-size new-population)
      (recur (concat new-population
                     (cross-fn (repeatedly select-parent)))))))

(defn do-mutation [population mutate-if-needed]
  (map mutate-if-needed population))

; Pass two maps, one for functions, the other for settings.
; 
; For func-map:
; init-fn: takes one numeric argument (the population count) and initializes population with that number of individuals.
;          returns a population of individuals
; fit-fn: takes a population individual
;         returns it's fitness value.
; sel-fn: takes a list of individuals, a list of corresponding fitness values, and a total fitness value (sum of fitness values over entire collection).
;         returns an individual according to its fitness.
; cross-fn: takes a lazy sequence of parents with which to cross in order to produce children (or a single child).
;           returns a sequence containing at least one child.
; mut-fn: takes an individual
;         returns a mutated individual.
; terminate?: takes a list of individuals, a list of corresponding fitness values, and the current generation iteration;
;             returns a boolean
; 
; For config-map:
; pop-size is size of population
; crossover-rate is the rate of crossover (0-100)
; mutation-rate is the rate of mutation (0-100)
;
; Returns the youngest/newest population.
(defn run-ga [{:keys [init-fn fit-fn sel-fn cross-fn mut-fn terminate?] :as fns}
              {:keys [pop-size crossover-rate mutation-rate] :as config}]
  (let [mutate-if-needed #(if (> mutation-rate (rand-int 100))
                            (mut-fn %1)
                            %1)
        cross-if-needed #(if (> crossover-rate (rand-int 100))
                            (cross-fn %1)
                            [(first %1)])]
    (loop [pop (init-fn pop-size)
           gen 1]
      (let [fitness-values (map fit-fn pop)
            total-fitness (reduce + fitness-values)]
        (if (terminate? pop fitness-values gen)
          pop
          (recur (do-mutation (do-crossover cross-if-needed #(sel-fn pop fitness-values total-fitness) pop-size)
                              mutate-if-needed)
                 (inc gen)))))))