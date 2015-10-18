(ns video.gui
  (:use [seesaw.util :only [illegal-argument]])
  (:require [seesaw.core :as seesaw]
            [video.core :refer :all]
            [seesaw.swingx :as swingx])
  (:gen-class))

(declare rent-screen)
(declare main-screen)
(declare input-rent)
(declare input-remove)
(declare available-screen)

(defn -main
  [& args]
  (insert-initial-data)
  (main-screen)
  )

(defn- display
  [content width height]
  (let [window (seesaw/frame :id :mainframe
                             :title "Video Store"
                             :content content
                             :width width
                             :height height)]
    (seesaw/show! window)))

(defn- avail-table []
  (let [table (swingx/table-x
               :id :table-available
               :horizontal-scroll-enabled? true
              :model [
                      :columns [:name :price :quant]
                      :rows (vec (vals (sort (read-string (slurp "movies.txt")))))]
                :listen [:selection (fn [event](seesaw/alert "Click button to select and rent/remove movie" ))])]

    table))


(defn- make-rent-vector []

 (let [v (atom '()) m (read-string (slurp "rent.txt"))]
   (doseq [x (vec (keys m)) y (vec (vals m))]
    (swap! v conj (assoc y :renter x)))
   (vec (distinct @v))))


(defn- rent-table []
  (let [table (swingx/table-x
               :id :table-rent
               :horizontal-scroll-enabled? true
              :model [
                      :columns [:name :renter :due-date]
                      :rows (distinct (make-rent-vector))]
               :listen [:selection (fn [event](seesaw/alert "Click button to select and return movie" ))] )]

  table))

(defn- input-rent [movie renter event]
  (rent-movie (get-id-by-mname  movie) renter)
   (let [root (seesaw/to-root event)]
     (seesaw/dispose! root))
  (available-screen))

(defn- input-remove [movie event]
  (remove-copies (get-id-by-mname movie))
   (let [root (seesaw/to-root event)]
     (seesaw/dispose! root))
  (available-screen)
  )

(defn- input-return [movie event]
  (return-movie (get-id-by-mname (:name movie)) (:renter movie))
  (let [root (seesaw/to-root event)]
     (seesaw/dispose! root))
  (rent-screen)
  )

(defn- available-screen []
  (let [window (seesaw/border-panel
                :id :avail-movies-border
                :vgap 15 :hgap 15 :border 15
                :north (seesaw/label "Available Movies")
                :center (seesaw/scrollable (avail-table))
                :south (seesaw/horizontal-panel

                 :items [(seesaw/button
                          :text "Rent"
                           :listen [:action (fn [event](input-rent (seesaw/input "Select a movie"
                                                                      :choices (vec (map #(:name %) (vals (sort (read-string (slurp "movies.txt"))))))
                                                                      :title "Select movie to rent")
                                                          (seesaw/input "Enter renter name" :title "Renter name") event))] )
                         (seesaw/button
                          :text "Remove"
                          :listen  [:action (fn [event]
                                              (input-remove (seesaw/input "Select a movie"
                                                                      :choices (vec (map #(:name %) (vals (sort (read-string (slurp "movies.txt"))))))
                                                                      :title "Select movie to remove") event))])]
                  :border 55)
                )]
    (display window 500 500)))

(defn- rent-screen []
  (let [window (seesaw/border-panel
                :vgap 15 :hgap 15 :border 15
                :north (seesaw/label "Rented Movies")
                :center (seesaw/scrollable (rent-table))
                :south (seesaw/horizontal-panel
                 :items [(seesaw/button
                          :text "Return movie"
                           :listen [:action (fn [event] (input-return (seesaw/input "Select a renter"
                                                                      :choices (make-rent-vector)
                                                                      :to-string :renter
                                                                      :title "Select renter") event) )])]
                  :border 55)
                )]
   (display window 500 500) ))


(defn- main-screen []

  (let [window (seesaw/border-panel
                :vgap 15 :hgap 15 :border 15
        :center (seesaw/vertical-panel
                 :items [(seesaw/button
                          :text "Insert Movies"
                           :listen [:action (fn [event] (add-movie (read-string (seesaw/input "Movie Id" :title "Insert movie details"))
                                                                  (seesaw/input "Movie Name" :title "Insert movie details")
                                                                  (read-string (seesaw/input "Movie Price" :title "Insert movie details"))
                                                                  (read-string (seesaw/input "Movie Quantity" :title "Insert movie details"))
                                                                  0 ) (seesaw/alert "Movie added successfully") )])

                         (seesaw/button
                          :text "Rented Movies"
                          :listen [:action (fn [event](rent-screen))])
                         (seesaw/button
                          :text "Available Movies"
                         :listen [:action (fn [event](available-screen))])
                          ]
    :border 55))]

    (display window 300 300)))
