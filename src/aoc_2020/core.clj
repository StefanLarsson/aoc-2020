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

(defn parsepasswordline [line]
  (let [[_ & refs] (re-find passwordpattern line)]
    refs))

(defn iscorrectlist [alist]
  (let [
    [_from _to letter password] alist
    from (read-string _from)
    to (read-string _to)
    lettermatch (re-pattern letter)
    ntimes (count (re-seq lettermatch password))]
    (and (<= from ntimes) (<= ntimes to))))

(defn iscorrectlist2 [alist]
  (let [
    [_from _to letter password] alist
    from (read-string _from)
    to (read-string _to)
    lettermatch (re-pattern letter)
    num1 (if
      (= (nth password (- from 1)) (first letter)) 1 0)
    num2 (if
      (= (nth password (- to 1)) (first letter)) 1 0)]
    (= 1 (+ num1 num2))))

(defn count-lines-satisfying [fname condition]
  (let [lines (clojure.string/split-lines (slurp fname))]
    (count (filter condition lines))))

(defn day2part1 [filename & args]
  (println (count-lines-satisfying filename (comp iscorrectlist parsepasswordline))))

(defn day2part2 [filename & args]
  (println (count-lines-satisfying filename (comp iscorrectlist2 parsepasswordline))))

(defn tree-sum [pos line]
  (if (= (nth line pos) \.)
    0
    1))

(defn tree-summer-reducer [width [dx dy]]
  (fn [ret v]
    (if (= 0 (mod (:posy ret) dy))
      {
        :acc (+ (:acc ret) (tree-sum (:posx ret) v))
        :posx (mod (+ (:posx ret) dx) width)
        :posy (inc (:posy ret))
      }
      {
        :acc (:acc ret)
        :posx (:posx ret)
        :posy (inc (:posy ret))
      }
    )
  )
)

(defn sum-trees-over [lines]
  (let
    [width (count (first lines))]
    (fn [[dx dy]]
      (:acc (reduce (tree-summer-reducer width [dx dy]) {:acc 0 :posx 0 :posy 0} lines)))))
    

(defn day3part1 [& args]
  (let [
    lines (clojure.string/split-lines (slurp "resources/day3/input.txt"))]
    (do
      (println ((sum-trees-over lines) '(3 1)))
)))

(defn day3part2 [& args]
  (let
    [grads '( (1 1) (3 1) (5 1) (7 1) (1 2))
    lines (clojure.string/split-lines (slurp "resources/day3/input.txt"))]
    (do
      (println (reduce * (map (sum-trees-over lines) grads)))
    )
  )
)

(def days-parts-functions {
	1, {1, day1part1, 2, day1part2},
	2, {1, day2part1, 2, day2part2}
	3, {1, day3part1  2 day3part2}
})

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

