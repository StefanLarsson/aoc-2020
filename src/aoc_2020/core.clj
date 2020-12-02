(ns aoc-2020.core
  (:gen-class))

(defn filename-to-integers [fname]
  (map read-string (clojure.string/split-lines (slurp fname))))

(def passwordpattern (re-pattern "(\\d+)-(\\d+) (.): (.*)"))

(defn as-sum-of-n [n value values]
  (cond
    (< value 0) nil
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
      (println (apply * threeones))))

	
(defn day1part1 [filename & args] 
  (let [
    expenses (filename-to-integers filename)
    terms (as-sum-of-n 2 2020 expenses)]
    (println (apply * terms))))


(defn line2list [line]
  (let
    [groups (re-find passwordpattern line)
    from (read-string (get groups 1))
    to (read-string (get groups 2))
    letter (get groups 3)
    password (get groups 4)]
    {:from from :to to :letter letter :password password}))  

(defn iscorrect [line]
  (let
    [parsed (line2list line)
    lettermatch (re-pattern (parsed :letter))
    ntimes (count (re-seq (re-pattern (parsed :letter)) (parsed :password)))]
    (and (<= (parsed :from) ntimes) (<= ntimes (parsed :to))) 
))
    
    

(defn day2part1 [filename & args]
  (let [lines (clojure.string/split-lines (slurp filename))]
    (println (count (filter iscorrect lines)))
))

(def days-parts-functions {
	1, {1, day1part1, 2, day1part2},
	2, {1, day2part1}})

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

