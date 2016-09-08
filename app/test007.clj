(in-ns 'bts.core)

(use 'clongra.core)

(def M 8)
(def K 8)

(defn int-adjs
  [i]
  (take M (iterate inc (* i 10))))

(defn int-show
  [i]
  (str i))


(defn test1
  []
  (with-out-str (print-tree 1 int-adjs int-show K)) nil)
