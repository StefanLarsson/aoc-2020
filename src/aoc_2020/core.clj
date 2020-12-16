(ns aoc-2020.core
  (:require clojure.set)
  (:gen-class)
  (:use clojure.math.combinatorics))

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

(defn standard-day-filename [day]
  (str "resources/day" day "/input.txt"))

(defn days-input-as-lines [day]
  (-> day
      standard-day-filename
      filename-to-lines))

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
      [ n1 (dec n)
        attemptfirst (first values)
        value1 (- value attemptfirst)
        values1 (rest values)
        p (as-sum-of-n n1 value1 values1)]
        (if p
          (cons attemptfirst p)
          (recur n value values1)))))

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

(def passport-validity-map
  { "byr" is-valid-byr?
   "iyr" is-valid-iyr?
   "eyr" is-valid-eyr?
   "hgt" is-valid-hgt?
   "hcl" is-valid-hcl?
   "ecl" is-valid-ecl?
   "pis" is-valid-pid?
   }
)


(defn validate-map [m checkers]
  "m a map to validate
  checkers a map from fields to checkers
  checks that all the values for all the fields exist and fulfill the corresponding checker"
  (and))

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
    (for [[number color] rhsmatches] [containing color  (Integer/parseInt number)])
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
    { :op (op _op) :arg (Integer/parseInt arg) }
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

(defn switch-fix-attempt [instruction]
  "Returns the attempted fix for the instruction, if possible"
  (case (instruction :op)
    :jmp (assoc instruction :op :nop)
    :nop (assoc instruction :op :jmp)
    nil
  )
)

(defn fix-instructions [instructions]
   "Fix the program by changing either a nop to a jmp or the other way around"
   (loop [fix 0]
     (let
       [instruction (get instructions fix)
       fix-instruction (switch-fix-attempt instruction)]
	       (if fix-instruction
		 (let [fixed-result (run-until-halt-or-done (assoc instructions fix fix-instruction))]
                   (if (= (count instructions) (fixed-result :addr)) fixed-result
                       (recur (inc fix))
                   )
		 )
	         (recur (inc fix))
	       )
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
     (str "The accumulator holds the value " (final-state :acc) " when it stops, after fix")
  )
)

;; Day 9 Encoding Error

(def is-sum-of-two (partial as-sum-of-n 2))

(defn first-invalid [n numbers]
   (if (is-sum-of-two (get numbers n) (take n numbers))
     (recur n (vec (rest numbers)))
     (get numbers n)
   )
)


(defn day9part1 []
  (let [numbers (filename-to-integers "resources/day9/input.txt")]
    (str "The first number that is not the sum of two of the preceding 25 numbers is " (first-invalid 25 (vec numbers)))
  )
)

(defn initial-summing-to [target terms]
  "Finds an initial subsequence summing to target"
  (cond
    (< target 0) nil
    (= target 0) '()
    (empty? terms) nil
    :else (let [first-term (first terms)
               end (initial-summing-to (- target first-term) (rest terms))]
               (if end (conj end first-term ))
          )
  )
)

(defn consecutive-summing-to [target terms]
   "Finds a subsequence summing to target"
   (let [initial (initial-summing-to target terms)]
        (if initial initial
            (recur target (rest terms))
        )
   )
)

(defn day9part2 []
  (let
    [numbers (filename-to-integers "resources/day9/input.txt")
    target (->> "resources/day9/input.txt"
                 filename-to-integers
                 vec
                 (first-invalid 25))
    mysubs (consecutive-summing-to target numbers)]
    (str "The sum of the max and min of the first subsequence summing to " target " is " (+ (apply max mysubs) (apply min mysubs)))
  )
)

;; Day 10 Adapter Array

(defn joltage-reducer [[one-diffs three-diffs joltage ] nextjoltage]
  (let [diff (- nextjoltage joltage)]
    (cond
      (= diff 1) [(inc one-diffs) three-diffs nextjoltage]
      (= diff 3) [one-diffs (inc three-diffs) nextjoltage]
      :else [one-diffs three-diffs nextjoltage]
    )
  )
)

(defn day10part1 []
  (let [[one-diffs three-diffs _]
    (->> "resources/day10/input.txt"
        filename-to-integers
        sort
        (reduce joltage-reducer [0 0 0])
        )]
    (str "The product of the number of one jolt differences and the number of three jolt differences is " (* one-diffs (inc three-diffs)))
  )
)

(defn adj-list-from-sorted [sorted-joltages]
  (loop [from (first sorted-joltages)
         potential-tos (rest sorted-joltages)
         adj-list {}]
    (if (empty? potential-tos)
      adj-list
      (recur
        (first potential-tos)
        (rest potential-tos)
        (assoc adj-list from (take-while (partial >= (+ 3 from)) potential-tos))
      )
    )
  )
)

(defn count-paths-to-terminal [start g]
  "Traverses graph g (adjacency list representation)
   counting the number of paths to terminal element"
  (loop
    [needed (list start)
    found 0]
    (if
      (empty? needed) found ;; A list will work like a stack
      (let
        [curr (first needed)
        children (g curr)
        needed-children children
        new-needed (into (rest needed) needed-children)]
        (if (empty? children)
          (recur new-needed (inc found))
          (recur new-needed found)
        )
      )
    )
  )
)

(defn countit [g]
  (let [[key val] (first g)]
    (count-paths-to-terminal key g)))

(defn split-list-on-greater-than-2 [coll]
  (loop [found-finished '()  ;; list of lists
         building '()        ;; list
         to-be-examined coll] ;; list
    (let [next (first to-be-examined)
          previous (first building)]
    (cond
      (empty? to-be-examined) (if (empty? building) found-finished (conj found-finished building))
      (empty? building) (recur found-finished (conj building next) (rest to-be-examined))
      :else (if (> next (+ 2 previous))
        (recur (conj found-finished (reverse building)) (list next) (rest to-be-examined)) ;; Start building a new
        (recur found-finished (conj building next) (rest to-be-examined))
      )
    )
    )
  )
)


(defn day10part2first []
  "Divides the things into sections, multiplying them together"
  (let [pathcounts (-> "resources/day10/input.txt"
                       filename-to-integers
                       sort
                       (conj 0)
                       split-list-on-greater-than-2
                       reverse
                       (->> (map adj-list-from-sorted)
                            (map countit))
                       )]
    (str "The number of possible combinations is " (apply * pathcounts))
  )
)
;; Continuation: Note that we already have a topological sorting!
(defn countpaths [joltages]
  (loop [done {(first joltages) 1}
         remaining (rest joltages)]
    (if (empty? remaining) done
      (let [nextkey (first remaining)
            next1 (inc nextkey)
            next2 (inc next1)
            next3 (inc next2)
            getzerodefault (fn [key] (or (done key) 0))
            nextvalue (+ (getzerodefault next1) (getzerodefault next2) (getzerodefault next3))]
        (recur (assoc done nextkey nextvalue) (rest remaining))
      )
    )
  )
)

(defn day10part2 []
  (let [joltages (-> "resources/day10/input.txt"
               filename-to-integers
               sort
               (conj 0)
               reverse)]
  (str "The number of possible combinations is " ((countpaths joltages) 0))))

;; Day 11 Seating System

(def test-seat-initial (clojure.string/split-lines
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
  ))

(def test-seat-final (clojure.string/split-lines "#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##"))
(defn count-occupied-oneline [line]
  (count (filter (partial = \#) line))
  )

(defn state [c]
  "Converts a character to a state"
  ({\. :floor \# :occupied-seat \L :empty-seat} c))

(defn build-board [lines]
  "Create a board from a sequence of line"
  (loop [boardlines []
         textlines lines]
    (if (empty? textlines)
      boardlines
      (recur (conj boardlines (into [] (map state (first textlines))))
             (rest textlines)
             )
      )
    )
  )

(defn board-height [board] (count board))

(defn board-width [board] (count (first board)))

(defn state-to-char [state]
  (case state
    :floor \.
    :occupied-seat \#
    :empty-seat \L))

(defn statevector-to-string [states]
  (apply str (map state-to-char states)))

(defn board-to-string [board]
  (clojure.string/join "\n" (map statevector-to-string board)))


(defn get-state [board [x y]]
  (nth (nth board y) x))

(defn neighbours-1 [ w h x y]
  (for
    [i (range (dec x) (+ 2 x))
    j (range (dec y) (+ 2 y))
    :when (and (>= i 0) (>= j 0) (< i w) (< j h) (not (and (= i x) (= j y))) )]
    [i j]))

(def neighbours (memoize neighbours-1))

(defn count-occupied-adjacent [board x y]
  (let
    [w (board-width board)
     h (board-height board)
     neighbours (neighbours w h x y)
     neighbour-states (map (partial get-state board) neighbours)
     neighbour-occupied (filter #(= :occupied-seat %) neighbour-states)]
    (count neighbour-occupied)
  )
)

(defn new-state [state n]
  "Calculates the new state of a position if the old state was state and
  n adjacent seats are occupied"
  (case state
    :floor :floor
    :empty-seat (if (= n 0) :occupied-seat :empty-seat)
    :occupied-seat (if (>= n 4) :empty-seat :occupied-seat)))

;;(defn step-position [board x y]
;;  (new-state (get-state board [x y]) (count-occupied-adjacent board x y)))

(defn step-position [board x y]
  (case (get-state board [x y])
    :floor :floor
    :empty-seat (if (= 0 (count-occupied-adjacent board x y)) :occupied-seat :empty-seat)
    :occupied-seat (if (>= (count-occupied-adjacent board x y) 4) :empty-seat :occupied-seat)))

(defn step-board [board position-stepper]
  (let [w (board-width board)
        h (board-height board)
        ]
    (loop [i 0
           new-board '()]
      (if
        (= i h)
        (into [] (reverse new-board))
        (recur (inc i) (conj new-board
      (loop [j 0
             new-row '()]
        (if (= j w)
          (into [] (reverse new-row))
          (let
            ;;[state (new-state (get-state board [j i]) (count-occupied-adjacent board j i))]
            [state (position-stepper board j i)]
             (recur (inc j) (conj new-row state))
          )
        )
      )
      ))))))

(def directions (list [-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]))

(defn count-visible-occupied [board x y]
  (let
    [w (board-width board)
     h (board-height board)]

    (reduce +
	(for [direction directions]
	  (loop
	    [x (+ x (first direction))
	     y (+ y (second direction))]
	    (cond
	      (or (< x 0) (>= x w) (< y 0) (>= y h) (= :empty-seat (get-state board [x y]))) 0
	      (= :occupied-seat (get-state board [x y]) ) 1
	      :else (recur (+ x (first direction)) (+ y (second direction)))
	    )
	  )
	)
    )
  )
)


(defn step-position-2 [board x y]
  (case (get-state board [x y])
    :floor :floor
    :empty-seat (if (= 0 (count-visible-occupied board x y)) :occupied-seat :empty-seat)
    :occupied-seat (if (>= (count-visible-occupied board x y) 5) :empty-seat :occupied-seat)))


(defn count-occupied [lines]
  (reduce + (map count-occupied-oneline lines)))

(defn day11part1int []
  (let [initial-board (-> 11
                  standard-day-filename
                  filename-to-lines
                  build-board
                  )]
    (loop [oldboard initial-board
           newboard (step-board initial-board step-position)]
      (if (= (board-to-string oldboard) (board-to-string newboard))
        (count-occupied (clojure.string/split-lines (board-to-string newboard)))
        (recur newboard (step-board newboard step-position))))))

(defn day11part1[]
  (format "The final board has %d occupied positions" (day11part1int)))

(defn day11part2int []
  (let
    [initial-board (-> 11
                  standard-day-filename
                  filename-to-lines
                  build-board
                  )]
    (loop [oldboard initial-board
           newboard (step-board initial-board step-position-2)]
      (if (= (board-to-string oldboard) (board-to-string newboard))
        (count-occupied (clojure.string/split-lines (board-to-string newboard)))
        (recur newboard (step-board newboard step-position-2))))))

(defn day11part2 []
  (format "The final board has %d occupied positions" (day11part2int)))

;; Day 12 Rain Risk

(def initial-state [0 0 0])
;; A state is a vector of W/E position, S/N position and facing

(defn direction [^long facing]
  (case (long (mod facing 360))
    0   [ 1  0]
    90  [ 0  1]
    180 [-1  0]
    270 [ 0 -1]
  )
)
(defn navigate-ship [[x y facing] [action value]]
  (case action
    :north [x (+ y value) facing]
    :south [x (- y value) facing]
    :east  [(+ x value) y facing]
    :west  [(- x value) y facing]
    :left  [x y (+ facing value)]
    :right [x y (- facing value)]
    :forward (let
               [[dx dy] (direction facing)]
               [(+ x (* value dx)) (+ y (* value dy)) facing])
  )
)

(defn rotate-left [value [dx dy]]
  (case (long (mod value 360))
    0   [dx dy]
     90 [(- dy) dx]
    180 [(- dx) (- dy)]
    270 [dy (- dx)]
  )
)

(defn navigate-ship-and-waypoint [[[x y] [dx dy]] [action value]]
  (case action
    :north [[x y ] [dx (+ dy value)]]
    :south [[x y ] [dx (- dy value)]]
    :east [[x y ] [(+ dx value)  dy ]]
    :west [[x y ] [(- dx value) dy]]
    :left [[x y] (rotate-left value [dx dy])]
    :right [[x y] (rotate-left (- 360 value) [dx dy])]
    :forward [[(+ x (* value dx)) (+ y (* value dy))] [dx dy]]
  )
)

(defn line-to-nav-instruction [line]
  (let
    [[_ action-string val-string] (re-matches #"(.)(.*)" line)
     value (read-string val-string)
     action (case action-string
               "E" :east
               "W" :west
               "N" :north
               "S" :south
               "L" :left
               "R" :right
               "F" :forward) ]
    [action value]
))
(defn navigate [instructions]
  (loop
    [state initial-state
     instructions instructions]
    (if
      (empty? instructions) state
      (recur (navigate-ship state (first instructions)) (rest instructions))
    )
  )
)

(defn navigate-with-waypoint [instructions]
  (loop
    [state [[0 0] [ 10 1]]
     instructions instructions]
    (if
      (empty? instructions) state
      (recur (navigate-ship-and-waypoint state (first instructions)) (rest instructions))
    )
  )
)


(defn day12part1 []
  (let
    [lines (-> 12
             standard-day-filename
             filename-to-lines)
    instructions (map line-to-nav-instruction lines)
    [^long x ^long y facing] (navigate instructions)]
    (format "The Manhattan distance from the ships starting point to the final point is %d" (+ (Math/abs x) (Math/abs y)))
  )
)

(defn day12part2 []
  (let
    [lines (-> 12
             standard-day-filename
             filename-to-lines)
    instructions (map line-to-nav-instruction lines)
    [[^long x ^long y] [dx dy]] (navigate-with-waypoint instructions)]
    (format "The Manhattan distance from the ships starting point to the final point is %d" (+ (Math/abs x) (Math/abs y)))
  )
)

;; Day 13 Shuttle Search
(defn busids-from-line [line]
  (let
    [id-strings (filter (complement (partial = "x")) (re-seq #"[^,]+" line))
     ids (map read-string id-strings)]
    ids))

(defn minutes-to-wait [earliest bus-id]
  (- bus-id (mod earliest bus-id))
  )



(defn day13part1 []
  (let [lines (-> 13
                  standard-day-filename
                  filename-to-lines)
        earliest-time (read-string (first lines))
        ids (busids-from-line (second lines))
        ids-sorted (sort #(- (minutes-to-wait earliest-time %1) (minutes-to-wait earliest-time %2)) ids)]
    (format "The earliest possible departure time is %d "
    (* (first ids-sorted) (minutes-to-wait earliest-time (first ids-sorted))))
  )
)

;; Part 2
;; Find t such that:
;; Bus line id0 departs at t (i.e. id0 divides t)
;; and for all k
;; Bus line id[k] departs at t + k ( i.e. id1 divides (t + k) i.e. t = -k (mod id1)
;; So we have a set of congruences to solve
;; Bring out the Chinese!
;; n  = q1*m   + r1          common divisors of m and n are same as of m and r1
;; m  = q2*r1  + r2          common divisors om m and r1 same as r1 and r2
;; r1 = q3*r2  + r3          common divisors om r1 and r2 same as r2 and r3

;; Expressing remainders in terms of m and n?
;; Very first step: m = 0*n + 1*m
;; Second: r1 = 1*n - q1*m
;; Third:  r2 = 1*m - q2 *r1
;; Assume r_i = a_i*n + b_i*m
;; We have r_(i+2) = r_i -  q_(i+2) * r_(i+1)
;;         r_(i+2) = a_i*n + b_i * n - q_i+2 * (a_(i+1)*n + b_(i+1) * m)
;;         r_(i+2) = (a_i - q_(i+2) * a_(i+1)) * n + (b_i - q_(i+2)*b_(i+1)) * m
;; So a_(i+2) = (a_i - q_(i+2) * a_(i+1))
;;    b_(i+2) = (b_i - q_(i+2) * b_(i+1))

(defn gcd [n0 m0]
  "Returns the gcd of n0 and m0, and a vector of the coefficients from the Euclidean algorithm"
  (loop [n n0
         m m0
         a1 1
         b1 0
         a2 0
         b2 1]
    (let [^long q (quot n m)
          ^long r (mod n m)]
;;      (println m n a1 b1  a2 b2 q r " and " m "=" a2 "*" n0 "+" b2 "*" m0)
      (if
        (= 0 r) [m [a2 b2]]
        (recur m r a2 b2 (- a1 (* q a2)) (- b1 (* q b2)))
      )
    )
  )
)

(defn solve-pair-congruences [[a1 m1] [a2 m2]]
  "Finds a solution to x = a1 (mod m1) and x = a2 (mod m2)"
  (let
    [[_ [u v]] (gcd m1 m2)]
;    (println m1 m2 u v a1 a2)
     (+ (* a1 m2 v) (* a2 m1 u))
  )
)

(defn solve-congruences [congruences]
  "Congruences is a sequence of pairs [a m]. Returns the smallest non-negative x which is
  congruent to a mod m for all the pairs. The sequence must not be empty. The moduli must be pairwise relatively prime"
  (loop [ x 0 prod 1 congruences congruences]
    (if
      (empty? congruences) (mod x prod)
      (let
        [[a m] (first congruences)
         y (solve-pair-congruences [x prod] [a m])
         nextm (* m prod)]
    ;    (println (type y) (type m))
    ;     (println "We believe " x " is congruent to all and to " prod " and " y "is congruent to that and " a " mod " m".")
        (recur (long (mod y nextm)) (long nextm) (rest congruences))
      )
    )
  )
)


(defn day13part2 []
  (let [indexed-strings (->> 13
                 standard-day-filename
                 filename-to-lines
                 second
                 (re-seq #"[^,]+")
                 (map-indexed #(vector %1 %2)))
        indexed-interesting-strings (filter (fn [[index bus-id]] (not= "x" bus-id)) indexed-strings)
        congruences (map (fn [[i string]] [(- i) (bigint string)]) indexed-interesting-strings)
        solution (solve-congruences congruences)
        ]

    (str "The earliest time where all buses will depart at the right offset is " solution)
    )
  )

;; Day 14 Docking Data

(defn parse-mask [line]
  (let [[_ mask] (re-matches #"mask = (.*)" line)]
    (if mask
      [:mask mask])))

(defn parse-mem [line]
  (let [[_ address value] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
    (if value
      [:mem (Integer/parseInt address) (Integer/parseInt value)]
      )
    )
  )

(defn parse-docking-line [line]
                    (or (parse-mem line) (parse-mask line)))

;; How to define a state for this machine?
;; initial-state: mask undefined (at least the first instruction is a mask!)
;; 0 in memory at all positions
(defn initial-docking-state []
  [(sorted-map) nil]
  )

(defn execute-mask [[memory _] mask]
  [memory mask]
  )

(defn put-with-mask [^String mask value]
  (let [constant-mask (Long/parseLong (.replace mask \X \0) 2)
        user-mask (Long/parseLong (.replace mask \X \1) 2)]
    (bit-or (bit-and value user-mask) constant-mask)))

(defn execute-mem [[memory mask] address value]
  (let [old (or (get memory address) 0)
        new (put-with-mask mask value)]
    [(assoc memory address new) mask]
  )
)

(def docking-machine
  {
   :mem execute-mem
   :mask execute-mask
  }
)

(defn execute-instr [machine-description state instr]
  (let [op (first instr)
        args (rest instr)]
    (apply (machine-description op) state args)
  )
)

(def execute-docking-instr (partial execute-instr docking-machine))

(defn mask-address-bit [mask-input address-input]
  (case mask-input
    \0 address-input
     mask-input
    )
  )

(defn mask-bit-possibilities [c]
  (case c
    \X [ 0 1]
    \0 [ 0]
    \1 [ 1]
    )
  )


(defn expand-addresses [mask address]
  (let
    [non-padded (Long/toBinaryString address)
     length (. non-padded length)
     all-seqs (->> non-padded
            (concat (repeat (- 36 length) \0))
            (map vector mask)
            (map #(apply mask-address-bit %))
            (map mask-bit-possibilities)
            (apply cartesian-product))
           ]
    (map (partial reduce #(+ ( * 2 %1) %2)) all-seqs  )
  )
)

(defn execute-mem-2 [[memory mask] address value]
  (let [addresses (expand-addresses mask address)]
    (loop [memory memory
           addresses addresses]
      (if (empty? addresses) [memory mask]
        (recur (assoc memory (first addresses) value) (rest addresses))))))

(def docking2-machine
  {
   :mem execute-mem-2
   :mask execute-mask
  }
)

(def execute-docking2-instr (partial execute-instr docking2-machine))

(defn sum-state [[memory _]]
  (reduce + (for [[key val] memory] val))
)

(defn day14part1 []
  (let [instructions (->> 14
                      standard-day-filename
                      filename-to-lines
                      (map parse-docking-line))
                      final-state (reduce execute-docking-instr (initial-docking-state) instructions)
        ]
    (format "The sum of all memory locations is %d" (sum-state final-state))
        ))

(defn day14part2 []
  (let [instructions (->> 14
                      standard-day-filename
                      filename-to-lines
                      (map parse-docking-line))
                      final-state (reduce execute-docking2-instr (initial-docking-state) instructions)
        ]
    (format "The sum of all memory locations is %d" (sum-state final-state))
        ))

;; Day 15 Rambunctious Recitation

;Representation of a game
; a map of all the numbers spoken, and when they were last spoken
; and keeping track of which turn it is (about to be played)
;  [map turn]
; the map shall be from number to: something keeping track of when it was last spoken
; AND if it exists a diff telling how many days apart it was last spoken
; The state shall also contain the last number spoken

(defn play-initial [initial-numbers]
  "Sets up the state according to initial-numbers. Assumes no repetition."
  (loop [last-spoken nil
         spoken  {}
         turn 0
         numbers initial-numbers]
    (if (empty? numbers) [last-spoken spoken turn]
      (recur (first numbers) (assoc spoken  last-spoken turn) (inc turn) (rest numbers)))))

(defn play-state [last-spoken spoken turn]
;  (println "spoke " last-spoken "at turn " turn)
  (let [last-spoken-previous (spoken last-spoken)]
    (if last-spoken-previous
      [ (- turn last-spoken-previous) (assoc spoken last-spoken turn) (inc turn)]
      [0 (assoc spoken last-spoken turn) (inc turn)])))

(defn play-to-turn [[last-spoken spoken ^long turn1] ^long turn]
  (if (= turn1 turn) last-spoken
    (recur (play-state last-spoken spoken turn1) turn)
    )
  )

(defn day15part1 []
  (format "The number spoken at turn 2020 is %d"
  (play-to-turn (play-initial '(18,11,9,0,5,1)) 2020))
  )

(defn day15part2 []
  (format "The number spoken at turn 30000000 is %d"
  (play-to-turn (play-initial '(18,11,9,0,5,1)) 30000000)
  ))

;; Day 16 Ticket Translation

(defn parse-field-rule [line]
  "Parses a line withi a rule for a field. return value [field min1 max1 min2 max2]"
  (let
    [[_ field min1 max1 min2 max2] (re-matches #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" line)]
    [field (Integer/parseInt min1) (Integer/parseInt max1) (Integer/parseInt min2) (Integer/parseInt max2)]))

(defn parse-ticket [line]
  "Parses a ticket. Returns a sequence of the values"
  (map #(Integer/parseInt %) (re-seq #"\d+" line)))

(defn valid-for-rule? [value [rule-name min1 max1 min2 max2]]
  "Verifies whether the value is valid for the rule"
  (or (and (<= min1 value) (<= value max1)) (and (<= min2 value) (<= value max2))))

(defn completely-invalid-value? [rules value]
  "Checks if a value is not valid for any of the rules"
    (not-any? (partial valid-for-rule? value) rules))

(defn completely-invalid? [rules ticket]
  "Checks if a ticket is completely invalid, i.e. if it has at least one completely invalid value"
  (not-empty (filter (partial completely-invalid-value? rules) ticket)))

(defn ticket-scanning-error-rate [rules tickets]
  "Calculates the ticket scanning error rate for the tickets given the rules"
  (reduce + (filter (partial completely-invalid-value? rules) (flatten tickets))))

(defn rules-and-tickets [lines]
  (let
    [[rule-lines more] (split-with (partial not= "") lines)
     my-ticket-line (first (drop 2 more))  ;; drop "" and "your ticket"
     nearby-ticket-lines (drop 5 more)]    ;; drop "", "your ticket", my actual ticket, empty line, "nearby tickets"]

    [(map parse-field-rule rule-lines) (parse-ticket my-ticket-line) (map parse-ticket nearby-ticket-lines)]))


(defn day16part1 []
  (let
    [lines (days-input-as-lines 16)
     [rules _ nearby-tickets] (rules-and-tickets lines)]

    (format "The ticket scanning error rate is %d" (ticket-scanning-error-rate rules nearby-tickets))))

(defn rule-ok-for-all-values? [values rule]
  (every?  #(valid-for-rule? % rule) values))

(defn filter-ok-rules-for-values [rules values]
  (filter (partial rule-ok-for-all-values? values) rules))

(defn values-ok-for-rule? [rule values]
  "If all the values are ok for the rule"
  (every? #(valid-for-rule? % rule) values ))

(defn filter-ok-values-for-rule [columns rule]
  "Find all sets of values that are ok for this rule"
  (filter  #(values-ok-for-rule? rule %) columns))

(defn eliminate [vector-of-seqs]
  (loop [i 0]
    (let [theseq (vector-of-seqs i)]
      (println theseq)
    (cond
      (= 1 (count theseq)) (let [single (first theseq)]
                             (into [] (map #(remove (partial = single) %) vector-of-seqs)))
      (= (count vector-of-seqs) i) nil
      :else (recur (inc i)) )
    )))

(defn remove-value [value [colnum field-options]]
  [colnum (remove (partial = value) field-options)])

(defn single-first-removed [[finished-map column-field-options]]
  (let [[colnum single-list] (first column-field-options)
        field (first single-list)]
    ;;[ (into finished-map colnum field) (map (partial remove-value field) (rest column-field-options))]))
    [(into finished-map [[colnum field]])  (map (partial remove-value field) (rest column-field-options))]))

(defn day16part2 []
  (let
    [[rules my-ticket nearby-tickets] (rules-and-tickets (-> 16 standard-day-filename filename-to-lines))
     ok-tickets (filter (complement (partial completely-invalid? rules)) nearby-tickets)
     columns (apply map vector ok-tickets)
     possible-fields-for-columns (map-indexed  #(vector %1 (map first %2)) (map (partial filter-ok-rules-for-values rules) columns))
     possible-columns-for-fields (map (partial filter-ok-values-for-rule columns) rules)
     possible-fields-sorted (sort (fn [[_ list1] [_ list2]] (- (count list1) (count list2))) possible-fields-for-columns)]
;    (println possible-columns-for-fields)
    possible-fields-sorted))

;;
;; Generic day handling
(def days-parts-functions
  (sorted-map
	1 {1 day1part1   2 day1part2}
	2 {1 day2part1   2 day2part2}
	3 {1 day3part1   2 day3part2}
	4 {1 day4part1   2 day4part2}
	5 {1 day5part1   2 day5part2}
	6 {1 day6part1   2 day6part2}
	7 {1 day7part1   2 day7part2}
	8 {1 day8part1   2 day8part2}
	9 {1 day9part1   2 day9part2}
	10 {1 day10part1 2 day10part2 }
	11 {1 day11part1  2 day11part2}
	12 {1 day12part1  2 day12part2}
	13 {1 day13part1  2 day13part2}
	14 {1 day14part1  2 day14part2}
	15 {1 day15part1  2 day15part2}
	16 {1 day16part1  2 day16part2}
  )
)

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
          (doseq [[part fn] parts]
            (do
             (println (format "Day %2d, Part %2d: %s" day  part  (fn)))
            )
          )
        )
      )
   )
  ([day] (doseq [[part fn] (days-parts-functions (read-string day))]
                 (println (format "Day% 2d, Part %2d: %s" (read-string day)  part (fn)))))
  ([day part & args]
    (println "Day is " day ", part is" part)
    (apply day-part day part args)
  )
)

