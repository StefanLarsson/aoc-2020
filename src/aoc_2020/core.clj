(ns aoc-2020.core
  (:require clojure.set)
  (:gen-class))

;; Read a file and split it into lines
(def filename-to-lines #(clojure.string/split-lines (slurp %)))

;; Read a file and one integer from each line
(defn filename-to-integers  [fname]
  (map read-string (filename-to-lines fname))
)

;; Day 1

(defn as-sum-of-n [n value values]
  "Finds (if it exists) an n item subsequence (not necessarily contiguous) of values whose sum is value"
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

;; Day 2

(def passwordpattern #"(\\d+)-(\\d+) (.): (.*)")

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

;; Day 3
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
      (str "The toboggan encountered " ((sum-trees-over lines) '(3 1)) " trees.")
)))

(defn day3part2 [& args]
  (let
    [grads '( (1 1) (3 1) (5 1) (7 1) (1 2))
    lines (clojure.string/split-lines (slurp "resources/day3/input.txt"))]
    (do
      (str
        "The product of the numbers of trees the toboggan encountered over the slopes is "
        (reduce * (map (sum-trees-over lines) grads))
      )
    )
  )
)

;; Day 4
(defn keys-in-passport [p]
  (set (map second (re-seq #"(\S*):\S*" p)))
)

(defn valid-passport [p]
  (clojure.set/subset? #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" } p)
)

(defn keys-vals-in-passport [p]
  (apply sorted-map (flatten (map #(subvec % 1) (re-seq #"(\S+):(\S+)" p)))))

(defn has-valid? [pmap key validator]
  (let [value (pmap key)]
    (if value
      (validator value)
    )
  )
)

(def is-valid-ecl? #(re-matches #"amb|blu|brn|gry|grn|hzl|oth" %))

(def has-valid-ecl? #(has-valid? % "ecl" is-valid-ecl?))

(def is-valid-pid? #(re-matches #"\d{9}" %))

(def has-valid-pid? #(has-valid? % "pid" is-valid-pid?))

(def is-valid-hcl? #(re-matches #"#[0-9a-f]{6}" %))

(def has-valid-hcl? #(has-valid? % "hcl" is-valid-hcl?))

(def is-four-digits? #(re-matches #"\d{4}" %))

(defn is-valid-year [s min max]
  (if (is-four-digits? s)
    (let [year (read-string s)]
      (and (<= min year) (<= year max))
    )
  )
)

(def is-valid-byr? #(is-valid-year % 1920 2002))
(def is-valid-iyr? #(is-valid-year % 2010 2020))
(def is-valid-eyr? #(is-valid-year % 2020 2030))

(defn has-valid? [pmap key validator]
  (let [value (pmap key)]
    (if value
      (validator value)
    )
  )
)

(def has-valid-byr? #(has-valid? % "byr" is-valid-byr?))
(def has-valid-iyr? #(has-valid? % "iyr" is-valid-iyr?))
(def has-valid-eyr? #(has-valid? % "eyr" is-valid-eyr?))

(defn is-valid-hgt? [s]
  (let
    [ [_ svalue unit] (re-matches #"(\d+)(cm|in)" s)]
    (cond
      (= "cm" unit)
      (let
        [value (read-string svalue)]
        (and (<= 150 value) (<= value 193))
      )
      (= "in" unit)
      (let
        [value (read-string svalue)]
          (and (<= 59 value) (<= value 76))
      )
    )
  )
)

(def has-valid-hgt? #(has-valid? % "hgt" is-valid-hgt?))

(defn day4part1 [& args]
  (let
    [passporttexts (clojure.string/split (slurp "resources/day4/input.txt") #"\n\n")]
    (println (count (filter valid-passport (map keys-in-passport passporttexts))))
  )
)

(defn really-valid-passport [pmap]
  (and
    (has-valid-byr? pmap)
    (has-valid-iyr? pmap)
    (has-valid-eyr? pmap)
    (has-valid-hgt? pmap)
    (has-valid-hcl? pmap)
    (has-valid-ecl? pmap)
    (has-valid-pid? pmap)
  )
)

(defn day4part2 [& args]
  (let
    [passporttexts (clojure.string/split (slurp "resources/day4/input.txt") #"\n\n")]
    (do
      (println (count (filter really-valid-passport (map keys-vals-in-passport passporttexts))))
    )
  )
)

;; Day 5

(defn silly-binary-reducer [zerochar ret a]
  (do
    (+ (* 2 ret) (if (= zerochar a) 0 1))
  )
)

(def row-binary-reducer #(silly-binary-reducer \F %1 %2))

(defn row-silly-binary [s]
  (reduce row-binary-reducer 0 s))

(def col-binary-reducer #(silly-binary-reducer \L %1 %2))

(defn col-silly-binary [s]
  (reduce col-binary-reducer 0 s))

(defn string-to-seat-id [s]
  (let
    [
      row (row-silly-binary (subs s 0 7))
      col (col-silly-binary (subs s 7))
    ]
    (+ col (* 8 row))
  )
)

(defn day5part1 [& args]
  (do
    (let
      [rowids (map string-to-seat-id (clojure.string/split-lines (slurp "resources/day5/input.txt")))]
      (do
        (println (apply max rowids))
      )
    )
  )
)

(defn day5part2 [& args]
  (do
    (let
      [rowids (map string-to-seat-id (clojure.string/split-lines (slurp "resources/day5/input.txt")))
      minid (apply min rowids)
      maxid (apply max rowids)
      sorted (sort rowids)]
      (do
        (println (loop [curr (first sorted) rem (rest sorted)]
          (let [next (first rem)]
            (if (= next (inc curr)) (recur next (rest rem)) (inc curr)))))
      )
    )
  )
)

;; Day 6
;; Note: Representation is bad. How to represent a person that did not answer yes to any question?
;;       Is a line not considered blank if it contains whitespace only?

(defn read-questionnaire-groups [fname]
  (let [input (slurp fname)]
    (clojure.string/split input #"\n\n")
  )
)

(defn unique-letters-in-string [s]
  (into #{} (clojure.string/replace s #"[^a-z]" ""))
)

(def q-group-to-qs #(clojure.string/split-lines %))

(def count-letters-in-all  #(count (apply clojure.set/intersection (map unique-letters-in-string %))))

(defn day6part1 []
  (let [groups (read-questionnaire-groups "resources/day6/input.txt")]
    (println (reduce + (map #(count (unique-letters-in-string %)) groups)))
  )
)

(defn day6part2 []
  (let [groups (read-questionnaire-groups "resources/day6/input.txt")]
    (println (reduce + (map count-letters-in-all (map q-group-to-qs groups))))
  )
)
;; Generic day handling
(def days-parts-functions {
	1 {1 day1part1 2 day1part2}
	2 {1 day2part1 2 day2part2}
	3 {1 day3part1 2 day3part2}
	4 {1 day4part1 2 day4part2}
	5 {1 day5part1 2 day5part2}
	6 {1 day6part1 2 day6part2}
})

(defn day-part [day part & args]
	(let [part-functions (days-parts-functions (read-string day))]
		(if part-functions
        		(let [func (part-functions (read-string part))]
                		(if func
		                        (println (apply func args))
                		        (println "Don't know how to handle this day and part yet!")))
			(println "Don't know about this day"))))

(defn -main
  [day part & args]
  (println "Day is " day ", part is" part)
  (apply day-part day part args)
)

