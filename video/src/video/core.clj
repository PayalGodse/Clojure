(ns video.core
  (:require [seesaw.core :as seesaw]
            [seesaw.dev :as dev])

  )

(declare get-due-date)

(defn insert-initial-data []
    ( spit "movies.txt" (zipmap [1] [(zipmap [:name :quant :price :rent] ["A" 1 10 1])]))
   ( spit "rent.txt" (zipmap ["aa"] [(zipmap [:due-date :name] [(get-due-date) "A"])])))

(def due-days 14)

(defn get-due-date []
  (.format (java.text.SimpleDateFormat. "MM/dd/yyyy") (java.util.Date. (+ (* due-days 86400 1000) (.getTime (java.util.Date.)))))
  )

(defn- make-rent-map [n]
  (zipmap [:due-date :name] [(get-due-date) n]))

(defn- make-movie-map [n q p r]
  (zipmap [:name :quant :price :rent] [n q p r]))

(defn read-file [file]
  (atom (read-string (slurp file)))
  )

  (defn add-movie [id n q p r]
    (let [m (read-file "movies.txt")]
  (spit "movies.txt" (swap! m  assoc-in [id] (make-movie-map n p q r)))))

    (defn total-quant [id]
   (let [m (read-string (slurp "movies.txt"))]
(+ (:quant (get m id)) (:rent (get m id)))))

   (defn available-quant [id]
   (let [m (read-string (slurp "movies.txt"))]
(:quant (get m id))))

     (defn rented-quant [id]
   (let [m (read-string (slurp "movies.txt"))]
(:rent (get m id))))

   (defn remove-movie [id]
    (let [m (read-file "movies.txt")]
  (spit "movies.txt" (swap! m dissoc id))))

  (defn get-movie-by-id [id]
    (let [m (read-string (slurp "movies.txt"))]
    (get m id)))

  (defn get-name-by-id [id]
    (let [m (read-string (slurp "movies.txt"))]
    (:name (get m id))))


  (defn get-movie-by-name [n]
    (let [m (read-string (slurp "movies.txt"))]
    (doseq[[k v] m]
      (if (= (:name v) n)
        (make-movie-map (:name v) (:quant v) (:price v) (:rent v))))))

    (defn get-id-by-mname [n]
      (let [key (atom -1)]
    (let [m (read-string (slurp "movies.txt"))]
    (doseq[[k v] m]
      (if (= (:name v) n)
        (reset! key k))))
        @key))

  (defn find-price-by-name [n]
   (:price (get-movie-by-name n)))

    (defn find-quantity-by-name [n]
   (:quant (get-movie-by-name n)))


     (defn find-price-by-id [id]
     (let [m (read-string (slurp "movies.txt"))]
      (:price (get m id))))


  (defn change-price [id n]
     (let [m (read-file "movies.txt")]
(spit "movies.txt" (reset! m (update-in @m [id :price] + (- n (find-price-by-id id))))
)))

  (defn add-copies [id n]
   (let [m (read-file "movies.txt")]
(spit "movies.txt" (reset! m (update-in @m [id :quant] + n)))))

   (defn remove-copies [id]
   (let [m (read-file "movies.txt")]
     (if (and (= (available-quant id )   1) (= (rented-quant id) 0))
       (remove-movie id)
       (if (> (available-quant id )   1)
      (spit "movies.txt" (reset! m (update-in @m [id :quant] dec)))))))


    (defn rent-movie [id renter]
   (let [m (read-file "movies.txt")]
     (reset! m (update-in @m [id :quant] dec))
(reset! m (update-in @m [id :rent] inc))
    (spit "movies.txt" @m))
 (let [m (read-file "rent.txt")]
  (spit "rent.txt" (swap! m  assoc-in [renter] (make-rent-map (get-name-by-id id))))))

(defn return-movie [id renter]
   (let [m (read-file "movies.txt")]
     (reset! m (update-in @m [id :quant] inc))
(reset! m (update-in @m [id :rent] dec))
    (spit "movies.txt" @m))
    (let [m (read-file "rent.txt")]
  (spit "rent.txt" (swap! m dissoc renter))))
