(ns aoc-2020.core
  (:gen-class))


(defn filename-to-integers [fname]
  (map read-string (clojure.string/split-lines (slurp fname))))

(defn sums-to-with [sum firstterm list] 
  (cond
    (empty? list) nil
    (= (+ firstterm (first list)) sum) (first list) 
    :else (sums-to-with sum firstterm (rest list))))

(defn sums-to [sum list]
  (if
    (empty? list) nil
    (let
      [firstterm (first list)
      newlist (rest list)
      secondterm (sums-to-with sum firstterm newlist)]
      (if secondterm
        [firstterm secondterm]
        (sums-to sum newlist)))))

(defn day1part1 [filename & args] 
	(let [expenses (filename-to-integers filename)
              terms (sums-to 2020 expenses)]
		(println (* (get terms 1) (get terms 0)))))
	
(defn exists [value values]
	(cond
		(empty? values) nil
		:else (if
			(= value (first values) value)
			(exists value (rest values)))))

(defn as-sum-of-n [n value values]
  (cond
    (= 0 n) (if
      (= 0 value) '()
      nil)
    (empty? values) nil
    :else (let
      [ n1 (- n 1)
        attemptfirst (first values)
        value1 (- value attemptfirst)
        values1 (rest values)
        p (as-sum-of-n n1 value1 values1)]
        (if p
          (cons attemptfirst p)
          (as-sum-of-n n value values1)))))

(defn day1part2 [filename & args]
  (let [
    expenses (filename-to-integers filename)
    threeones (as-sum-of-n 3 2020 expenses)]
      (println (apply * threeones))
        
	(println expenses)))
  
(def days-parts-functions {
	1, {1, day1part1, 2, day1part2}})

(defn day-part [day part & args]
	(let [part-functions (days-parts-functions (read-string day))]
		(if part-functions
        		(let [func (part-functions (read-string part))]
                		(if func
		                        (apply func args)
                		        (println "Don't know how to handle this day and part yet!")))
			(println "Don't know about this day"))))

(defn -main
  [day part & args]
  (println "Day is " day ", part is" part)
  (apply day-part day part args)
)

