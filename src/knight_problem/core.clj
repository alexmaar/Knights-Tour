(ns knight-problem.core
  (:gen-class))

(def size 5)

(def valid-moves "possible moves from field" [[-2 1] [-2 -1] [2 1] [2 -1] [1 2] [1 -2] [-1 2] [-1 -2]])

(defn empty-array "creates empty array 2d"[]
  (vec (repeat size (vec (repeat size :e)))))

;; (defn print-array "prints array" [array]
;;   (map println array))

(defn in-array [[x y]]
  (and 
   (<= 0 x (- size 1))
   (<= 0 y (- size 1))))

(defn is-free "checks if given has already been visited" [array [x y]]
  ((complement =) :X (nth (nth array x) y)))

(defn is-valid [array [x y]]
  (and
   (in-array [x y])
   (is-free array [x y])))

(defn make-move "makes filed visited" [array [x y]]
  (assoc array x (assoc (nth array x) y :X)))

(defn next-moves "returns all possible next moves for given field" [array [x y]]
  (filter (complement nil?) (map (fn [[x-idx y-idx]] 
                                   (let [next-x (+ x-idx x)
                                         next-y (+ y-idx y)]
                                     (if (and
                                          true
                                          (is-valid array [next-x next-y]))
                                       [next-x next-y]
                                       nil))) valid-moves)))

(defn filled-array "checks if every field has been visited"[array]
  (empty? (filter #(some #{:e} %) array)))

(defn make-tour "recursive funcion for searching knight's path" [array [x y] result]
  (if (filled-array array)
    result
    (some
     (fn [[next-x next-y]]
       (make-tour (make-move array [next-x next-y]) [next-x next-y] (conj result [next-x next-y]))) (next-moves array [x y]))))
    
    ;;  (first (filter (fn [[next-x next-y]]
    ;;                    (make-tour (make-move array [next-x next-y]) [next-x next-y] (conj result [next-x next-y]))) (next-moves array [x y])))))
    ;;  
                
          
  (defn start-tour "start function" [x y]
    (let [start-array (make-move (empty-array) [x y])]
    (make-tour start-array [x y] [[x y]])))

 
