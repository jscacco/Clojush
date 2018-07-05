(ns clojush.instructions.vectors
  (:use [clojush pushstate globals]
        clojush.instructions.common))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stack instructions for vectors

(define-registered vector_integer_pop (with-meta (popper :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_pop (with-meta (popper :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_pop (with-meta (popper :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_pop (with-meta (popper :vector_string) {:stack-types [:vector_string]}))

(define-registered vector_integer_dup (with-meta (duper :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_dup (with-meta (duper :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_dup (with-meta (duper :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_dup (with-meta (duper :vector_string) {:stack-types [:vector_string]}))

(define-registered vector_integer_dup_times (with-meta (dup-timeser :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_dup_times (with-meta (dup-timeser :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_dup_times (with-meta (dup-timeser :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_dup_times (with-meta (dup-timeser :vector_string) {:stack-types [:vector_string :integer]}))

(define-registered vector_integer_dup_items (with-meta (dup-itemser :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_dup_items (with-meta (dup-itemser :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_dup_items (with-meta (dup-itemser :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_dup_items (with-meta (dup-itemser :vector_string) {:stack-types [:vector_string :integer]}))

(define-registered vector_integer_swap (with-meta (swapper :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_swap (with-meta (swapper :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_swap (with-meta (swapper :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_swap (with-meta (swapper :vector_string) {:stack-types [:vector_string]}))

(define-registered vector_integer_rot (with-meta (rotter :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_rot (with-meta (rotter :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_rot (with-meta (rotter :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_rot (with-meta (rotter :vector_string) {:stack-types [:vector_string]}))

(define-registered vector_integer_flush (with-meta (flusher :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_flush (with-meta (flusher :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_flush (with-meta (flusher :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_flush (with-meta (flusher :vector_string) {:stack-types [:vector_string]}))

(define-registered vector_integer_eq (with-meta (eqer :vector_integer) {:stack-types [:vector_integer :boolean]}))
(define-registered vector_float_eq (with-meta (eqer :vector_float) {:stack-types [:vector_float :boolean]}))
(define-registered vector_boolean_eq (with-meta (eqer :vector_boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_eq (with-meta (eqer :vector_string) {:stack-types [:vector_string :boolean]}))

(define-registered vector_integer_stackdepth (with-meta (stackdepther :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_stackdepth (with-meta (stackdepther :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_stackdepth (with-meta (stackdepther :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_stackdepth (with-meta (stackdepther :vector_string) {:stack-types [:vector_string :integer]}))

(define-registered vector_integer_yank (with-meta (yanker :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_yank (with-meta (yanker :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_yank (with-meta (yanker :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_yank (with-meta (yanker :vector_string) {:stack-types [:vector_string :integer]}))

(define-registered vector_integer_yankdup (with-meta (yankduper :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_yankdup (with-meta (yankduper :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_yankdup (with-meta (yankduper :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_yankdup (with-meta (yankduper :vector_string) {:stack-types [:vector_string :integer]}))

(define-registered vector_integer_shove (with-meta (shover :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_shove (with-meta (shover :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_shove (with-meta (shover :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_shove (with-meta (shover :vector_string) {:stack-types [:vector_string :integer]}))

(define-registered vector_integer_empty (with-meta (emptyer :vector_integer) {:stack-types [:vector_integer :boolean]}))
(define-registered vector_float_empty (with-meta (emptyer :vector_float) {:stack-types [:vector_float :boolean]}))
(define-registered vector_boolean_empty (with-meta (emptyer :vector_boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_empty (with-meta (emptyer :vector_string) {:stack-types [:vector_string :boolean]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common instructions for vectors

(defn concater
  "Returns a function that takes a state and concats two vectors on the type stack."
  [type]
  (fn [state]
    (if (not (empty? (rest (type state))))
      (let [first-item (stack-ref type 0 state)
            second-item (stack-ref type 1 state)]
        (if (>= max-vector-length (+ (count first-item)
                                     (count second-item)))
          (->> (pop-item type state) 
            (pop-item type)
            (push-item (vec (concat second-item first-item)) type))
          state))
      state)))

(define-registered vector_integer_concat (with-meta (concater :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_concat (with-meta (concater :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_concat (with-meta (concater :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_concat (with-meta (concater :vector_string) {:stack-types [:vector_string]}))

(defn conjer
  "Returns a function that takes a state and conj's an item onto the type stack."
  [vec-type lit-type]
  (fn [state]
    (if (and (not (empty? (vec-type state)))
             (not (empty? (lit-type state))))
      (let [result (conj (vec (top-item vec-type state)) (top-item lit-type state))]
        (if (>= max-vector-length (count result))
          (push-item result
                     vec-type
                     (pop-item lit-type (pop-item vec-type state)))
          state))
      state)))

(define-registered vector_integer_conj (with-meta (conjer :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_conj (with-meta (conjer :vector_float :float) {:stack-types [:vector_float :float]}))
(define-registered vector_boolean_conj (with-meta (conjer :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_conj (with-meta (conjer :vector_string :string) {:stack-types [:vector_string :string]}))

(defn taker
  "Returns a function that takes a state and takes the first N items from the type
   stack, where N is from the integer stack."
  [type]
  (fn [state]
    (if (and (not (empty? (type state)))
             (not (empty? (:integer state))))
      (push-item (vec (take (top-item :integer state)
                            (top-item type state)))
                 type
                 (pop-item type (pop-item :integer state)))
      state)))

(define-registered vector_integer_take (with-meta (taker :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_take (with-meta (taker :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_take (with-meta (taker :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_take (with-meta (taker :vector_string) {:stack-types [:vector_string :integer]}))

(defn subvecer
  "Returns a function that takes a state and takes the subvec of the top item
   on the type stack."
  [type]
  (fn [state]
    (if (and (not (empty? (type state)))
             (not (empty? (rest (:integer state)))))
      (let [vect (top-item type state)
            first-index (min (count vect) (max 0 (stack-ref :integer 1 state)))
            second-index (min (count vect) (max first-index (stack-ref :integer 0 state)))]
        (push-item (subvec vect first-index second-index)
                   type
                   (pop-item type (pop-item :integer (pop-item :integer state)))))
      state)))

(define-registered vector_integer_subvec (with-meta (subvecer :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_subvec (with-meta (subvecer :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_subvec (with-meta (subvecer :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_subvec (with-meta (subvecer :vector_string) {:stack-types [:vector_string :integer]}))

(defn firster
  "Returns a function that takes a state and gets the first item from the type stack."
  [type lit-type]
  (fn [state]
    (if (and (not (empty? (type state)))
             (not (empty? (first (type state))))) ;Make sure the top vec isn't empty
      (push-item (first (top-item type state))
                 lit-type
                 (pop-item type state))
      state)))

(define-registered vector_integer_first (with-meta (firster :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_first (with-meta (firster :vector_float :float) {:stack-types [:vector_float :float]}))
(define-registered vector_boolean_first (with-meta (firster :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_first (with-meta (firster :vector_string :string) {:stack-types [:vector_string :string]}))

(defn laster
  "Returns a function that takes a state and gets the last item from the type stack."
  [type lit-type]
  (fn [state]
    (if (and (not (empty? (type state)))
             (not (empty? (first (type state))))) ;Make sure the top vec isn't empty
      (push-item (last (top-item type state))
                 lit-type
                 (pop-item type state))
      state)))

(define-registered vector_integer_last (with-meta (laster :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_last (with-meta (laster :vector_float :float) {:stack-types [:vector_float :float]}))
(define-registered vector_boolean_last (with-meta (laster :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_last (with-meta (laster :vector_string :string) {:stack-types [:vector_string :string]}))

(defn nther
  "Returns a function that takes a state and gets the nth item from the type stack."
  [type lit-type]
  (fn [state]
    (if (and (not (empty? (type state)))
             (not (empty? (:integer state)))
             (not (empty? (first (type state))))) ;Make sure the top vec isn't empty
      (let [vect (stack-ref type 0 state)
            index (mod (stack-ref :integer 0 state) (count vect))]
        (push-item (nth vect index)
                   lit-type
                   (pop-item :integer (pop-item type state))))
      state)))

(define-registered vector_integer_nth (with-meta (nther :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_nth (with-meta (nther :vector_float :float) {:stack-types [:vector_float :float :integer]}))
(define-registered vector_boolean_nth (with-meta (nther :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean :integer]}))
(define-registered vector_string_nth (with-meta (nther :vector_string :string) {:stack-types [:vector_string :string :integer]}))

(defn rester
  "Returns a function that takes a state and takes the rest of the top item
   on the type stack."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (push-item (vec (rest (top-item type state)))
                 type
                 (pop-item type state))
      state)))

(define-registered vector_integer_rest (with-meta (rester :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_rest (with-meta (rester :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_rest (with-meta (rester :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_rest (with-meta (rester :vector_string) {:stack-types [:vector_string]}))

(defn butlaster
  "Returns a function that takes a state and takes the butlast of the top item
   on the type stack."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (push-item (vec (butlast (top-item type state)))
                 type
                 (pop-item type state))
      state)))

(define-registered vector_integer_butlast (with-meta (butlaster :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_butlast (with-meta (butlaster :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_butlast (with-meta (butlaster :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_butlast (with-meta (butlaster :vector_string) {:stack-types [:vector_string]}))

(defn lengther
  "Returns a function that takes a state and takes the length of the top item
   on the type stack."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (push-item (count (top-item type state))
                 :integer
                 (pop-item type state))
      state)))

(define-registered vector_integer_length (with-meta (lengther :vector_integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_length (with-meta (lengther :vector_float) {:stack-types [:vector_float :integer]}))
(define-registered vector_boolean_length (with-meta (lengther :vector_boolean) {:stack-types [:vector_boolean :integer]}))
(define-registered vector_string_length (with-meta (lengther :vector_string) {:stack-types [:vector_string :integer]}))

(defn reverser
  "Returns a function that takes a state and takes the reverse of the top item
   on the type stack."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (push-item (vec (reverse (top-item type state)))
                 type
                 (pop-item type state))
      state)))

(define-registered vector_integer_reverse (with-meta (reverser :vector_integer) {:stack-types [:vector_integer]}))
(define-registered vector_float_reverse (with-meta (reverser :vector_float) {:stack-types [:vector_float]}))
(define-registered vector_boolean_reverse (with-meta (reverser :vector_boolean) {:stack-types [:vector_boolean]}))
(define-registered vector_string_reverse (with-meta (reverser :vector_string) {:stack-types [:vector_string]}))

(defn pushaller
  "Returns a function that takes a state and pushes every item from the first
   vector onto the appropriate stack."
  [type lit-type]
  (fn [state]
    (if (empty? (type state))
      state
      (loop [lit-list (reverse (top-item type state))
             loop-state (pop-item type state)]
        (if (empty? lit-list)
          loop-state
          (recur (rest lit-list)
                 (push-item (first lit-list) lit-type loop-state)))))))

(define-registered vector_integer_pushall (with-meta (pushaller :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_pushall (with-meta (pushaller :vector_float :float) {:stack-types [:vector_float :float]}))
(define-registered vector_boolean_pushall (with-meta (pushaller :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_pushall (with-meta (pushaller :vector_string :string) {:stack-types [:vector_string :string]}))

(defn emptyvectorer
  "Returns a function that takes a state and pushes a boolean of whether the top
   vector is empty."
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (push-item (empty? (top-item type state))
                 :boolean
                 (pop-item type state))
      state)))

(define-registered vector_integer_emptyvector (with-meta (emptyvectorer :vector_integer) {:stack-types [:vector_integer :boolean]}))
(define-registered vector_float_emptyvector (with-meta (emptyvectorer :vector_float) {:stack-types [:vector_float :boolean]}))
(define-registered vector_boolean_emptyvector (with-meta (emptyvectorer :vector_boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_emptyvector (with-meta (emptyvectorer :vector_string) {:stack-types [:vector_string :boolean]}))

(defn containser
  "Returns a function that takes a state and tells whether the top lit-type item
   is in the top type vector."
  [type lit-type]
  (fn [state]
    (if (or (empty? (type state))
            (empty? (lit-type state)))
      state
      (let [item (top-item lit-type state)
            vect (top-item type state)
            result (<= 0 (.indexOf vect item))]
        (push-item result
                   :boolean
                   (pop-item lit-type (pop-item type state)))))))

(define-registered vector_integer_contains (with-meta (containser :vector_integer :integer) {:stack-types [:vector_integer :integer :boolean]}))
(define-registered vector_float_contains (with-meta (containser :vector_float :float) {:stack-types [:vector_float :float :boolean]}))
(define-registered vector_boolean_contains (with-meta (containser :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_contains (with-meta (containser :vector_string :string) {:stack-types [:vector_string :string :boolean]}))

(defn indexofer
  "Returns a function that takes a state and finds the index of the top lit-type
   item in the top type vector."
  [type lit-type]
  (fn [state]
    (if (or (empty? (type state))
            (empty? (lit-type state)))
      state
      (let [item (top-item lit-type state)
            vect (top-item type state)
            result (.indexOf vect item)]
        (push-item result
                   :integer
                   (pop-item lit-type (pop-item type state)))))))

(define-registered vector_integer_indexof (with-meta (indexofer :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_indexof (with-meta (indexofer :vector_float :float) {:stack-types [:vector_float :float :integer]}))
(define-registered vector_boolean_indexof (with-meta (indexofer :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean :integer]}))
(define-registered vector_string_indexof (with-meta (indexofer :vector_string :string) {:stack-types [:vector_string :string :integer]}))

(defn occurrencesofer
  "Returns a function that takes a state and counts the occurrences of the top lit-type
   item in the top type vector."
  [type lit-type]
  (fn [state]
    (if (or (empty? (type state))
            (empty? (lit-type state)))
      state
      (let [item (top-item lit-type state)
            vect (top-item type state)
            result (count (filter #(= % item) vect))]
        (push-item result
                   :integer
                   (pop-item lit-type (pop-item type state)))))))

(define-registered vector_integer_occurrencesof (with-meta (occurrencesofer :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_occurrencesof (with-meta (occurrencesofer :vector_float :float) {:stack-types [:vector_float :float :integer]}))
(define-registered vector_boolean_occurrencesof (with-meta (occurrencesofer :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean :integer]}))
(define-registered vector_string_occurrencesof (with-meta (occurrencesofer :vector_string :string) {:stack-types [:vector_string :string :integer]}))

(defn seter
  "Returns a function that takes a state and replaces, in the top type vector,
   item at index (from integer stack) with the first lit-type item."
  [type lit-type]
  (fn [state]
    (if (or (empty? (type state))
            (empty? (lit-type state))
            (empty? (:integer state))
            (and (= lit-type :integer) (empty? (rest (:integer state)))))
      state
      (let [vect (top-item type state)
            item (if (= lit-type :integer)
                   (stack-ref :integer 1 state)
                   (top-item lit-type state))
            index (if (empty? vect)
                    0
                    (mod (top-item :integer state) (count vect)))
            result (if (empty? vect)
                     vect
                     (assoc vect
                            index
                            item))]
        (push-item result
                   type
                   (pop-item lit-type (pop-item :integer (pop-item type state))))))))

(define-registered vector_integer_set (with-meta (seter :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_set (with-meta (seter :vector_float :float) {:stack-types [:vector_float :float :integer]}))
(define-registered vector_boolean_set (with-meta (seter :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean :integer]}))
(define-registered vector_string_set (with-meta (seter :vector_string :string) {:stack-types [:vector_string :string :integer]}))

(defn replaceer
  "Returns a function that takes a state and replaces all occurrences of the second lit-type item
   with the first lit-type item in the top type vector."
  [type lit-type]
  (fn [state]
    (if (or (empty? (type state))
            (empty? (rest (lit-type state))))
      state
      (let [result (replace {(stack-ref lit-type 1 state) (stack-ref lit-type 0 state)}
                            (top-item type state))]
        (push-item result
                   type
                   (pop-item lit-type (pop-item lit-type (pop-item type state))))))))

(define-registered vector_integer_replace (with-meta (replaceer :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_replace (with-meta (replaceer :vector_float :float) {:stack-types [:vector_float :float]}))
(define-registered vector_boolean_replace (with-meta (replaceer :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_replace (with-meta (replaceer :vector_string :string) {:stack-types [:vector_string :string]}))

(defn replacefirster
  "Returns a function that takes a state and replaces the first occurrence of the second lit-type item
   with the first lit-type item in the top type vector."
  [type lit-type]
  (fn [state]
    (if (or (empty? (type state))
            (empty? (rest (lit-type state))))
      state
      (let [index (.indexOf (top-item type state) (stack-ref lit-type 1 state))
            result (if (< index 0)
                     (top-item type state)
                     (assoc (top-item type state) index (stack-ref lit-type 0 state)))]
        (push-item result
                   type
                   (pop-item lit-type (pop-item lit-type (pop-item type state))))))))

(define-registered vector_integer_replacefirst (with-meta (replacefirster :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_replacefirst (with-meta (replacefirster :vector_float :float) {:stack-types [:vector_float :float]}))
(define-registered vector_boolean_replacefirst (with-meta (replacefirster :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_replacefirst (with-meta (replacefirster :vector_string :string) {:stack-types [:vector_string :string]}))

(defn removeer
  "Returns a function that takes a state and removes all occurrences of the first lit-type item
   in the top type vector."
  [type lit-type]
  (fn [state]
    (if (or (empty? (type state))
            (empty? (lit-type state)))
      state
      (let [result (vec (remove #(= % (top-item lit-type state))
                                (top-item type state)))]
        (push-item result
                   type
                   (pop-item lit-type (pop-item type state)))))))

(define-registered vector_integer_remove (with-meta (removeer :vector_integer :integer) {:stack-types [:vector_integer :integer]}))
(define-registered vector_float_remove (with-meta (removeer :vector_float :float) {:stack-types [:vector_float :float]}))
(define-registered vector_boolean_remove (with-meta (removeer :vector_boolean :boolean) {:stack-types [:vector_boolean :boolean]}))
(define-registered vector_string_remove (with-meta (removeer :vector_string :string) {:stack-types [:vector_string :string]}))

(defn iterateer
  "Returns a function that takes a state and iterates over the type vector using
   the code on the exec stack. If the vector isn't empty, expands to:
      ((first vector) (top-item :exec state) (rest vector) exec_do*vector_type (top-item :exec state) rest_of_program)"
  [type lit-type instr]
  (fn [state]
    (if (or (empty? (type state))
            (empty? (:exec state)))
      state
      (let [vect (top-item type state)]
      (cond
        (empty? vect) (->> state
                           (pop-item type)
                           (pop-item :exec))
        (empty? (rest vect)) (->> state ;If the rest of the vector is empty, we're done iterating.
                               (pop-item type)
                               (push-item (first vect) lit-type))
        :else (->> state
                (pop-item type)
                (push-item instr :exec)
                (push-item (vec (rest vect)) :exec)
                (push-item (top-item :exec state) :exec)
                (push-item (first vect) lit-type)))))))

(define-registered exec_do*vector_integer (with-meta (iterateer :vector_integer :integer 'exec_do*vector_integer) {:stack-types [:vector_integer :integer :exec] :parentheses 1}))
(define-registered exec_do*vector_float (with-meta (iterateer :vector_float :float 'exec_do*vector_float) {:stack-types [:vector_float :float :exec] :parentheses 1}))
(define-registered exec_do*vector_boolean (with-meta (iterateer :vector_boolean :boolean 'exec_do*vector_boolean) {:stack-types [:vector_boolean :boolean :exec] :parentheses 1}))
(define-registered exec_do*vector_string (with-meta (iterateer :vector_string :string 'exec_do*vector_string) {:stack-types [:vector_string :string :exec] :parentheses 1}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions for mapping and the hof_result stack
(defn map_driver
  "Does the bulk of the work for map. In-type and out-type are strings"
  [in-type out-type]
  (fn [state]
    (let [in-stack (if (= in-type "char") :string (keyword (str "vector_" in-type)))
          out-stack (if (= out-type "char") :string (keyword (str "vector_" out-type)))]
      ;; Check if the top vector for in-stack is empty. If so, stop.
      ;; Move top of :hof_result to top of out-stack.
      (if (empty? (top-item in-stack state))
        (let [result (top-item :hof_result state)]
          (->> state
               (pop-item in-stack)
               (pop-item :exec)
               (push-item result out-stack)
               (pop-item :hof_result)))
        ;; If it isn't empty, map the next value and store the result
        ;; in the top of :hof-result.
        (let [value (first (top-item in-stack state))
              block (top-item :exec state)
              rest-of-values (vec (rest (top-item in-stack state)))]
          (->> state
               (pop-item in-stack)
               (push-item rest-of-values in-stack)
               (pop-item :exec)
               (push-item
                (list 'environment_new (list (symbol (str "return_hof_" out-type))
                                  value block) 
                      (symbol (str "hof_result_conj_" out-type)) 
                      (symbol (str "exec_map_helper_" in-type "_to_" out-type)) 
                      block)
                :exec)))))))

(defn mapper
  "Places a new vector on top of :hof_result, then does map_driver."
  [in-type out-type]
  (fn [state]
    (push-item (symbol (str "exec_map_helper_" in-type "_to_" out-type)) :exec
               (push-item [] :hof_result state))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper instructions for map

(define-registered exec_map_helper_integer_to_integer (with-meta (map_driver "integer" "integer") {:stack-types [:exec :hof_result :vector_integer]}))
(define-registered exec_map_helper_integer_to_float (with-meta (map_driver "integer" "float") {:stack-types [:exec :hof_result :vector_integer :vector_float]}))
(define-registered exec_map_helper_integer_to_boolean (with-meta (map_driver "integer" "boolean") {:stack-types [:exec :hof_result :vector_integer :vector_boolean]}))
(define-registered exec_map_helper_integer_to_string (with-meta (map_driver "integer" "string") {:stack-types [:exec :hof_result :vector_integer :vector_string]}))
(define-registered exec_map_helper_integer_to_char (with-meta (map_driver "integer" "char") {:stack-types [:exec :hof_result :vector_integer :string]}))

(define-registered exec_map_helper_float_to_integer (with-meta (map_driver "float" "integer") {:stack-types [:exec :hof_result :vector_float :vector_integer]}))
(define-registered exec_map_helper_float_to_float (with-meta (map_driver  "float" "float") {:stack-types [:exec :hof_result :vector_float]}))
(define-registered exec_map_helper_float_to_boolean (with-meta (map_driver "float" "boolean") {:stack-types [:exec :hof_result :vector_float :vector_boolean]}))
(define-registered exec_map_helper_float_to_string (with-meta (map_driver "float" "string") {:stack-types [:exec :hof_result :vector_float :vector_string]}))
(define-registered exec_map_helper_float_to_char (with-meta (map_driver "float" "char") {:stack-types [:exec :hof_result :vector_float :string]}))

(define-registered exec_map_helper_boolean_to_integer (with-meta (map_driver "boolean" "integer") {:stack-types [:exec :hof_result :vector_boolean :vector_integer]}))
(define-registered exec_map_helper_boolean_to_float (with-meta (map_driver  "boolean" "float") {:stack-types [:exec :hof_result :vector_boolean :vector_float]}))
(define-registered exec_map_helper_boolean_to_boolean (with-meta (map_driver "boolean" "boolean") {:stack-types [:exec :hof_result :vector_boolean]}))
(define-registered exec_map_helper_boolean_to_string (with-meta (map_driver "boolean" "string") {:stack-types [:exec :hof_result :vector_boolean :vector_string]}))
(define-registered exec_map_helper_boolean_to_char (with-meta (map_driver "boolean" "char") {:stack-types [:exec :hof_result :vector_boolean :string]}))

(define-registered exec_map_helper_string_to_integer (with-meta (map_driver "string" "integer") {:stack-types [:exec :hof_result :vector_string :vector_integer]}))
(define-registered exec_map_helper_string_to_float (with-meta (map_driver  "string" "float") {:stack-types [:exec :hof_result :vector_string :vector_float]}))
(define-registered exec_map_helper_string_to_boolean (with-meta (map_driver "string" "boolean") {:stack-types [:exec :hof_result :vector_string :vector_boolean]}))
(define-registered exec_map_helper_string_to_string (with-meta (map_driver "string" "string") {:stack-types [:exec :hof_result :vector_string]}))
(define-registered exec_map_helper_string_to_char (with-meta (map_driver "string" "char") {:stack-types [:exec :hof_result :vector_string :string]}))

(define-registered exec_map_helper_char_to_integer (with-meta (map_driver "char" "integer") {:stack-types [:exec :hof_result :string :vector_integer]}))
(define-registered exec_map_helper_char_to_float (with-meta (map_driver  "char" "float") {:stack-types [:exec :hof_result :string :vector_float]}))
(define-registered exec_map_helper_char_to_boolean (with-meta (map_driver "char" "boolean") {:stack-types [:exec :hof_result :string :vector_boolean]}))
(define-registered exec_map_helper_char_to_string (with-meta (map_driver "char" "string") {:stack-types [:exec :hof_result :string :vector_string]}))
(define-registered exec_map_helper_char_to_char (with-meta (map_driver "char" "char") {:stack-types [:exec :hof_result :string]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; end helper, begin normal

(define-registered exec_map_integer_to_integer (with-meta (mapper "integer" "integer") {:stack-types [:exec :vector_integer]}))
(define-registered exec_map_integer_to_float (with-meta (mapper "integer" "float") {:stack-types [:exec :vector_integer :vector_float]}))
(define-registered exec_map_integer_to_boolean (with-meta (mapper "integer" "boolean") {:stack-types [:exec  :vector_integer :vector_boolean]}))
(define-registered exec_map_integer_to_string (with-meta (mapper "integer" "string") {:stack-types [:exec :vector_integer :vector_string]}))
(define-registered exec_map_integer_to_char (with-meta (mapper "integer" "char") {:stack-types [:exec :vector_integer :string]}))

(define-registered exec_map_float_to_integer (with-meta (mapper "float" "integer") {:stack-types [:exec :vector_float :vector_integer]}))
(define-registered exec_map_float_to_float (with-meta (mapper  "float" "float") {:stack-types [:exec :vector_float]}))
(define-registered exec_map_float_to_boolean (with-meta (mapper "float" "boolean") {:stack-types [:exec :vector_float :vector_boolean]}))
(define-registered exec_map_float_to_string (with-meta (mapper "float" "string") {:stack-types [:exec :vector_float :vector_string]}))
(define-registered exec_map_float_to_char (with-meta (mapper "float" "char") {:stack-types [:exec :vector_float :string]}))

(define-registered exec_map_boolean_to_integer (with-meta (mapper "boolean" "integer") {:stack-types [:exec :vector_boolean :vector_integer]}))
(define-registered exec_map_boolean_to_float (with-meta (mapper  "boolean" "float") {:stack-types [:exec :vector_boolean :vector_float]}))
(define-registered exec_map_boolean_to_boolean (with-meta (mapper "boolean" "boolean") {:stack-types [:exec :vector_boolean]}))
(define-registered exec_map_boolean_to_string (with-meta (mapper "boolean" "string") {:stack-types [:exec :vector_boolean :vector_string]}))
(define-registered exec_map_boolean_to_char (with-meta (mapper "boolean" "char") {:stack-types [:exec :vector_boolean :string]}))

(define-registered exec_map_string_to_integer (with-meta (mapper "string" "integer") {:stack-types [:exec :vector_string :vector_integer]}))
(define-registered exec_map_string_to_float (with-meta (mapper  "string" "float") {:stack-types [:exec :vector_string :vector_float]}))
(define-registered exec_map_string_to_boolean (with-meta (mapper "string" "boolean") {:stack-types [:exec :vector_string :vector_boolean]}))
(define-registered exec_map_string_to_string (with-meta (mapper "string" "string") {:stack-types [:exec :vector_string]}))
(define-registered exec_map_string_to_char (with-meta (mapper "string" "string") {:stack-types [:exec :vector_string :string]}))

(define-registered exec_mapper_char_to_integer (with-meta (map_driver "char" "integer") {:stack-types [:exec :string :vector_integer]}))
(define-registered exec_mapper_char_to_float (with-meta (map_driver  "char" "float") {:stack-types [:exec :string :vector_float]}))
(define-registered exec_mapper_char_to_boolean (with-meta (map_driver "char" "boolean") {:stack-types [:exec :string :vector_boolean]}))
(define-registered exec_mapper_char_to_string (with-meta (map_driver "char" "string") {:stack-types [:exec :string :vector_string]}))
(define-registered exec_mapper_char_to_char (with-meta (map_driver "char" "char") {:stack-types [:exec :string]}))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hof_return_[TYPE] instructions which push to the return stack HOF maps.

(defn return-hofer
  "Pushes an HOF map to the return stack so that all other returns no-op."
  [type]
  (fn [state]
    (push-item {:hof true :type type} :return state)))

(define-registered return_hof_integer (with-meta (return-hofer :integer) {:stack-types [:return :hof_result]}))
(define-registered return_hof_float (with-meta (return-hofer :float) {:stack-types [:return :hof_result]}))
(define-registered return_hof_string (with-meta (return-hofer :string) {:stack-types [:return :hof_result]}))
(define-registered return_hof_char (with-meta (return-hofer :char) {:stack-types [:return :hof_result]}))
(define-registered return_hof_boolean (with-meta (return-hofer :boolean) {:stack-types [:return :hof_result]}))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hof_result_conj_[TYPE] instructions which push the top of specified stack to result vector.

(defn hof-result-conjer
  "Pop from passed type and conj that to the front of the top vector of :hof_result."
  [type]
  (fn [state]
    (let [values (top-item :hof_result state)
          item (top-item type state)]
      (push-item (conj values item) :hof_result
                 (pop-item :hof_result
                           (pop-item type state))))))

(define-registered hof_result_conj_integer (with-meta (hof-result-conjer :integer) {:stack-types [:hof_result :integer]}))
(define-registered hof_result_conj_float (with-meta (hof-result-conjer :float) {:stack-types [:hof_result :float]}))
(define-registered hof_result_conj_string (with-meta (hof-result-conjer :string) {:stack-types [:hof_result :string]}))
(define-registered hof_result_conj_char (with-meta (hof-result-conjer :char) {:stack-types [:hof_result :char]}))
(define-registered hof_result_conj_boolean (with-meta (hof-result-conjer :boolean) {:stack-types [:hof_result :boolean]}))

         
