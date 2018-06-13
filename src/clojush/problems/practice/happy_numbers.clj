(ns clojush.problems.practice.happy-numbers
  (:use [clojush interpreter random pushstate]))

(def test-cases (concat (take 100 (rest (range)))
                        '(103, 109, 129, 130, 133, 139, 167, 176, 188, 190, 192, 193, 203, 208,
                               219, 226, 230, 236, 239, 262, 263, 280, 291, 293, 301, 302, 310, 313,
                               319, 320, 326)))

(def bad-nums '(4 16 37 58 89 145 42 20))

(defn get-digits
  "Takes a number and returns a list containing its digits"
  [num]
  ; Convert the number to a str, look at each digit, then convert to integer
  (map #(- (int %) 48) (seq (str num))))
; ^ This is kind of hacky - subtracts 48 from char value bc 0's char val is 48

(defn operate
  "Takes a list of digits, squares each, and adds up the results"
  [lst]
  (apply + (map #(* % %) lst)))

(defn is-happy?
  "Takes a number and returns true if it is a popl number"
  [number]
  (if (zero? number)
    false
    (let [result (operate (get-digits number))]
      (cond
        (= 1 result)  true
        (some #{result} bad-nums) false
        :else (recur result)))))

(defn happy-numbers-error-function
  [ind]
  (assoc ind :errors
         (for [input test-cases]
           (let [answer (top-item :boolean
                                  (run-push (:program ind)
                                            (push-item input :input (make-push-state))))]
             (if (= (is-happy? input) answer)
               0
               1)))))

(def happy-numbers-atom-generators
  (concat (registered-for-stacks [:integer :boolean :code :exec])
          ('in1
           (fn [] (lrand-int 10)))))

(def argmap
  {:error-function happy-numbers-error-function
   :atom-generators happy-numbers-atom-generators})
