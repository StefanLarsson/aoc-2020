(ns aoc-2020.core
  (:require clojure.set)
  (:gen-class))

;; Read a file and split it into lines
(def filename-to-lines #(clojure.string/split-lines (slurp %)))

;; Read a file and one integer from each line
(defn filename-to-integers  [fname]
  (map read-string (filename-to-lines fname))
)

;; Read a file and split it into groups of lines
;; on empty lines.
(defn filename-to-line-groups [fname]
  (clojure.string/split (slurp fname) #"\n\n")
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

(defn day1part2 [& args]
  (let
    [filename "resources/day1/input.txt"
    expenses (filename-to-integers filename)
    threeones (as-sum-of-n 3 2020 expenses)]
    (str "The product of three numbers whose sum is 2020 is " (apply * threeones))
  )
)

(defn day1part1 [& args]
  (let
    [filename "resources/day1/input.txt"
    expenses (filename-to-integers filename)
    terms (as-sum-of-n 2 2020 expenses)]
    (str "The product of two numbers whose sum is 2020 is " (apply * terms))
  )
)

;; Day 2

(def passwordpattern #"(\d+)-(\d+) (.): (.*)")

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

(defn day2part1 [& args]
  (let [filename "resources/day2/input.txt"]
    (str "The number of lines with compliant passwords is " (count-lines-satisfying filename (comp iscorrectlist parsepasswordline)))
  )
)

(defn day2part2 [& args]
  (let [filename "resources/day2/input.txt"]
    (str "The number of lines with compliant passwords is " (count-lines-satisfying filename (comp iscorrectlist2 parsepasswordline)))
  )
)

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
    [passporttexts (filename-to-line-groups "resources/day4/input.txt")]
    (str "The number of valid passports is " (count (filter valid-passport (map keys-in-passport passporttexts))))
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
    [passporttexts (filename-to-line-groups "resources/day4/input.txt")]
    (str "The number of really valid passports is " (count (filter really-valid-passport (map keys-vals-in-passport passporttexts))))
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
    [row (row-silly-binary (subs s 0 7))
    col (col-silly-binary (subs s 7))]
    (+ col (* 8 row))
  )
)

(defn day5part1 [& args]
  (do
    (let
      [rowids (map string-to-seat-id (clojure.string/split-lines (slurp "resources/day5/input.txt")))]
      (str "The maximum seat id is " (apply max rowids))
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
      (let
        [myseat (loop
                     [curr (first sorted) rem (rest sorted)]
                     (let
                       [next (first rem)]
                       (if (= next (inc curr)) (recur next (rest rem)) (inc curr))
                     )
                   )]
        (str "My seat id is " myseat)
      )
    )
  )
)

;; Day 6
;; Note: Representation is bad. How to represent a person that did not answer yes to any question?
;;       Is a line not considered blank if it contains whitespace only?

(defn unique-letters [s]
  (set (re-seq #"[a-z]" s))
)

(defn day6part1 []
  (let [groups (filename-to-line-groups "resources/day6/input.txt")]
    (str "The sum is " (reduce + (map (comp count unique-letters) groups)))
  )
)

(def count-letters-in-all-lines
  (comp count (partial apply clojure.set/intersection) (partial map unique-letters) clojure.string/split-lines))

(defn day6part2 []
  (let [groups (filename-to-line-groups "resources/day6/input.txt")]
    (str "The sum is " (reduce + (map count-letters-in-all-lines groups)))
  )
)

;; Day 7 Bags
(defn line-to-edge-seq [s]
  "Parses a line into a sequence of vectors [container containee n]
   where a bag of color container contains n bags of color containee"
  (let
    [[_ containing rhs] (re-matches #"(.*) bags contain (.*)" s)
     ;; The lines with "no other bags" will be dropped but this is ok for now
     rhsmatches (map rest (re-seq #"(\d+) ([ a-z]+) bags?" rhs))
    ]
    (for [[number color] rhsmatches] [containing color (Integer. number)])
  )
)

(defn edge-to-contained-edge [[container containee n]]
  { containee #{container}}
)

(defn edge-to-weighted-contains-edge [[container containee n]]
  { container #{[containee n]}}
)

(defn depth-first [start g]
  "Traverses graph g (adjacency list representation)
   outputting all elements that are reachable from node start"
  (loop
    [needed (list start)
    found #{}]
    (if
      (empty? needed) found ;; A list will work like a stack
      (let
        [curr (first needed)
        children (g curr)
        needed-children (filter (complement #(contains? found %)) children)
        new-needed (into (rest needed) needed-children)]
        (if (contains? found curr)
          (recur new-needed found)
          (recur new-needed (conj found curr))
        )
      )
    )
  )
)

(defn day7part1 []
  (let
    [ lines (filename-to-lines "resources/day7/input.txt")
      edges (map edge-to-contained-edge (apply concat (map line-to-edge-seq lines)))
      mygraph (apply merge-with into edges)]
    (str "There are " (dec (count (depth-first "shiny gold" mygraph))) " bag colors that can contain a shiny gold bag")
  )
)

(defn count-total-bags-contained-in [graph bag]
  (let [containees (graph bag)]
    (if (empty? containees) 0
         (reduce + (map (fn [[bag1 weight]] (* weight (+ 1 (count-total-bags-contained-in graph bag1)))) containees))
    )
  )
)

(defn day7part2 []
  (let
    [ lines (filename-to-lines "resources/day7/input.txt")
      edges (map edge-to-weighted-contains-edge (apply concat (map line-to-edge-seq lines)))
      mygraph (apply merge-with into edges)]
    (str "A shiny gold bag will contain " (count-total-bags-contained-in mygraph "shiny gold") " bags.")
  )
)

;;Day 8 Handheld Halting
(defn op [_op]
  (cond (= "acc" _op) :acc
        (= "jmp" _op) :jmp
        (= "nop" _op) :nop))

(defn line-to-instruction [s]
  (let
    [[_ _op arg] (re-matches #"(nop|acc|jmp) ([+\-]\d+)" s)]
    { :op (op _op) :arg (Integer. arg) }
  )
)

(def init-state { :addr 0 :acc 0})

(defn execute [state [op arg]]
   (cond
     (= :nop op)
       { :addr (inc (state :addr)) :acc (state :acc)}
     (= :acc op)
       { :addr (inc (state :addr)) :acc (+ (state :acc) arg)}
     (= :jmp op)
       { :addr (+ (state :addr) arg) :acc (state :acc)}
   )
)

(defn run-until-halt-or-done [instructions]
  (let [length (count instructions)]
	  (loop
	    [ visited #{}
	      state init-state]
	    (let [addr (state :addr)
		  op ((get instructions addr) :op)
		  arg ((get instructions addr) :arg)
		  new-state (execute state [op arg])]
		 (cond
                   (contains? visited (new-state :addr))
		     new-state
                   (= (new-state :addr) length)
                     new-state
                   :else 
		     (recur (conj visited addr) new-state)
		 )
	    )
	  )
  )
)

(defn fix-instructions [instructions]
   "Fix the program by changing either a nop to a jmp or the other way around"
   (loop [fix 0]
     (do
       (cond
         (= ((get instructions fix)  :op) :jmp)
           (do
            (println (run-until-halt-or-done (assoc instructions fix {:op :nop :arg 0})))
           )
         (= ((get instructions fix)  :op) :nop)
           (println (run-until-halt-or-done (assoc instructions fix {:op :jmp :arg ((get instructions fix) :arg) })))
         :else (println "Nothing o change")
       )
       (recur (inc fix))
     )
   )
)
     
  
(defn day8part1 []
  (let [lines (filename-to-lines "resources/day8/input.txt")
        instructions (into [] (map line-to-instruction lines))
        final-state (run-until-halt-or-done instructions)]
     (str "The accumulator holds the value " (final-state :acc) " before it loops")
  )
)

(defn day8part2 []
  (let [lines (filename-to-lines "resources/day8/input.txt")
        instructions (into [] (map line-to-instruction lines))
        final-state (fix-instructions instructions)]
     (str "The accumulator holds the value " (final-state :acc) " before it loops")
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
	7 {1 day7part1 2 day7part2}
	8 {1 day8part1 }
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
  "Advent of Code 2020"
  ([] (doseq [[day parts] days-parts-functions]
        (do
          (println "Day:" day)
          (doseq [[part fn] parts]
            (do
             (println "Part: " part)
             (println (fn))
            )
          )
        )
      )
   )

  ([day part & args]
    (println "Day is " day ", part is" part)
    (apply day-part day part args)
  )
)

