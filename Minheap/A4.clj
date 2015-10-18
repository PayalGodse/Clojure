(defn make-tree
[left value right]
{:left left :val value :right right})

(defn height-compare [tree]
   (if (> (height (:left tree)) (height (:right tree)))
     true
     false))

(defn height [tree]
  (if-not (nil? tree)
    (inc (max (height (:left tree)) (height (:right tree))))
    0))


(defn insert [tree value]

  (cond
   (nil? tree) (make-tree nil value nil)
   (<= (compare value (:val tree)) 0)
    (let [temp (:val tree)]
      (insert (make-tree (:left tree) value (:right tree)) temp))

   (> (compare value (:val tree)) 0) (if (height-compare tree)
                             (make-tree (:left tree) (:val tree) (insert (:right tree) value))
                            (make-tree (insert (:left tree) value) (:val tree) (:right tree)))))


(insert (insert (insert (insert (insert (insert nil 10) 3) 2) 1) 4) 5)

(insert (insert (insert (insert (insert (insert nil "hello") "3") "2") "1") "4") "5")

(def tree {:left {:left {:left nil, :val 10, :right nil}, :val 2, :right {:left nil, :val 5, :right nil}}, :val 1, :right {:left {:left nil, :val 4, :right nil}, :val 3, :right nil}})

(defn preorder
  [tree]
  (if-not (nil? tree)
    (concat (vector (:val tree)) (preorder (:left tree)) (preorder (:right tree)))))

(concat (vector 5) nil nil)
  (apply println (preorder {:left nil :val 10 :right nil}))


(defn print-preorder
  [tree]
  (apply str (preorder sample-tree)))


  (insert (insert (insert (insert (insert (insert (insert nil "mango") "eating") "juice") "drinking") "studying") "having") "fun")