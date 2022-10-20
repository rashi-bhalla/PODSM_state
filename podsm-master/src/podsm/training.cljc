;;(compile 'podsm.training.inspectablearf)
(ns podsm.training
  (:require
   [podsm.dataset.base
    :refer [pop-record-and-rest get-schema]]
   [podsm.evaluation
    :refer [results-summary get-communication-to-sites get-confusion-frequencies get-class-performance-measures print-confusion-matrix get-accuracy get-proportions-transmitted-to-t-sites pretty-site-communication]]
   [podsm.classifier.distributed.base
    :refer [get-all-sites get-site-changelog get-sites]]
   [podsm.classifier.distributed.dynamic-distributed
    :refer [dynamic-distributed-classifier]]
   [podsm.classifier.distributed.dynamic-monitors
    :refer [make-creation-significant-agreement-monitor-factory
            make-removal-inverted-agreement-monitor-factory
            make-removal-accuracy-usage-monitor-factory
            make-blacklist-permanent-monitor-factory
            make-blacklist-source-accuracy-monitor-factory]]
 ;; [podsm.classifier.distributed.distributed :refer [describe-model]]
   [podsm.classifier.base

    :refer [process-record describe-model]]
    [podsm.classifier.distributed.distributed :refer [prob-cond-check]]
   [podsm.utils
    :refer [map-vals percent debug find-all-nested mean populated-vector? std-dev positions updated-mean updated-std-dev]]
   [podsm.utils.random
    :refer [seeded-shuffle]]
   [podsm.classifier.distributed.sites
    :refer [make-site-structure p-site t-site get-active-sites]]
   [podsm.trees.base :refer :all]
   ;; [podsm.trees.parse-model :refer :all]
    [podsm.classifier.moa-classifier :refer :all]
    [podsm.trees.graph-tree :refer [save-graph-dot]]
)

)

;;(def output-dir (str "H:/data/nasa"))
;;(def prob-cond-check (atom {}))
(defn con [feature-options fvalues feature-set x] 
;;(println "option" feature-options  ((nth feature-set x) :name))
(case feature-options
:numeric
(do 
    
;;(println "numeric block")

      (let [mean-value (mean fvalues) 
           
           std-dev (cond 
                     (= (count fvalues) 1)
                     0
                     :else
                     (std-dev fvalues)
)
           total-count (count fvalues)
               ;; func-values (cal-funct mean-value std-dev fvalues)
           ;; index ((nth feature-set x) :name)   
            ] 
   ;;  (if (= ((nth feature-set x) :name) "T7")
;; (println "check" ((nth feature-set x) :name) "  " (count fvalues)  "   " mean-value "   " std-dev)
;; )
 [mean-value std-dev total-count]
)
        )
:nominal
 (do   
;;(println "reached nominal block")      
      (let [
           counters-freq (frequencies fvalues)
           counters-nominal

 (for [x (vals counters-freq) ]

  (/ x (count fvalues))

) 
cardinalities (for [x counters-nominal
                    y (keys counters-freq)
] [y x]

)
]
[counters-freq (count fvalues)]
           )
)

)

  
)
(defn cal-prob [dataset classifier recordset-for-site site-id]
;;(println "calculations")
;;(spit (str output-dir "/" "recordset-for-site.txt") (with-out-str (pr recordset-for-site)))
;;(println site-id)
(let [
;;testing

;;getting values of troubled records
    values1 (->> recordset-for-site
                  (map :values))
;;getting model
   dfeature-set (describe-model classifier)
  every-site (get-all-sites classifier)

;;getting schema of all sites 
   all-feature-set (find-all-nested dfeature-set :schema)
;;getting schema of trouble site 
;;dropping the class column
    feature-set (->> every-site 
                     (filter #(= (get % 0) (first site-id)))
                    (map #(get % 1)) 
                     (map :site-schema) 
                      (first)
                      (drop-last)
)
;; (drop-last (nth all-feature-set (first site-id)) )


       ]
;; (println "schema" all-feature-set)
;;   (println "id" (first site-id) "set" feature-set)
     
;; (spit (str output-dir "/" "dataset.txt") (with-out-str (pr dataset)))
 ;;(spit (str output-dir "/" "full-model.txt") (with-out-str (pr dfeature-set)))
;;(spit (str output-dir "/" "model.txt") (with-out-str (pr all-feature-set)))

                  
  (loop [x 0
        getting-result []
 ]
  (if (< x (count feature-set))
(do
  ;;(println str "results for" x "---is")
  (let [feature-options ((nth feature-set x) :options)
        all-dataset-feature (->> dataset
                                 :schema)
       feature-name (nth feature-set x)
feature-index (first (positions #{feature-name} all-dataset-feature) ) 
 
                               
       fvalues (->> values1 
                  (map #(nth % feature-index))         
                 (sort) 
                  (into [])
                  
)
                     

        ]
;;(println feature-index)
;;(println "site-id" site-id "feature-index" feature-options "   " feature-name)
;;(spit (str output-dir "/" "errcoll.txt") (with-out-str (pr fvalues)))

;; (println (con feature-options fvalues feature-set x))  

  (recur (inc x) (conj getting-result [(con feature-options fvalues feature-set x)  ((nth feature-set x) :name)] 
                       ))
  )
 )
  getting-result
)
  )
  ))


(defn update-prob [prob-cond-check1 record result dataset classifier]
;;(println "update")
(let [prob-keys (->>  prob-cond-check1
                   (map keys)
)]
;;(println prob-keys)
;;(println record)
(doseq [prob-key prob-keys]

;;(println prob-key)
(let [ 
tsite (get (:site-communication result) (first prob-key))
dfeature-set (describe-model classifier)
;;getting schema of all sites 
   all-feature-set (find-all-nested dfeature-set :schema)
 every-site (get-all-sites classifier)
all-dataset-feature (->> dataset
                                 :schema
 )

tcount (cond
;;when no site is unconfident @prob-cond-check is returned unchanged
(= (count tsite) 0)
(do
;;(println "no tsite")
(loop [x 0]
(when (< x 2)
(let [


tdata1 (->> prob-cond-check1
           (map #(get % (first prob-key)))
           (filter #(not= % nil)) 
            (first)
(into (sorted-map))
        (map key)
     
           )
ftry (->> @prob-cond-check
          
        (map #(select-keys % [(first prob-key)]))
          (filter #(not= % {}))
         
         
           )  
 feature-try  (first (positions #{(first ftry)} @prob-cond-check))
t (nth tdata1 x)
ftry1 (->> @prob-cond-check
           
          (map #(select-keys % [(first prob-key)]))
            (filter #(not= % {}))
          (first)
          (map val)
             ;; (nth 1)
            (first)
            ;;(first)
           ;;   (map #(select-keys % [t]))
           ;;    (filter #(not= % {}))
          ;;      (first)
          ;; (map val)
         
           )  
ftry11 (->> ftry1
          ;; (first)
          ;;  (first)
             (map #(select-keys % [t]))
              (filter #(not= % {}))
               (first)
)

 feature-try1 (first (positions #{ftry11} ftry1))


;;  tdata1 (->> @prob-cond-check
;;         (map #(get % x))
;;           first
        
;; )

;;t (first (map key tdata1)) 

]
;;(println  feature-try "   "   feature-try1)
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :other-site-conf] inc))
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :total] inc))
)
(recur (inc x))
)
)
@prob-cond-check
)
;;@prob-cond-check ;;issue may arise

;;when only site is unconfident and @prob-cond-check is changed for that site
(= (count tsite) 1)

(do
;;(println "this" tsite)
(let [
t (first tsite)
;;feature-set (drop-last (nth all-feature-set t))
 feature-set (->> every-site 
                     (filter #(= (get % 0) t))
                    (map #(get % 1)) 
                     (map :site-schema) 
                      (first)
                      (drop-last)
)
tdata (->> prob-cond-check1
           (map #(get % (first prob-key)))
           (filter #(not= % nil))   
            (first)
            (map #(get % t))
(filter #(not= % nil))
          ;; (map #(select-keys % [(first prob-key)]))
          ;;   (filter #(not= % {}))
          ;;  (first)
          ;; (map val)
          ;;    ;; (nth 1)
          ;;  (first)
          ;;  (first)
          ;;    (map #(select-keys % [t]))
          ;;     (filter #(not= % {}))
          ;;      (first)

          ;; (map val)
         
           ) 
tdata-other-key (->> prob-cond-check1
           (map #(get % (first prob-key)))
           (filter #(not= % nil)) 
            (first)
(into (sorted-map))
        (map key)
         (filter #(not= % t))
           (first)
     
           )


  
 
 tdata1 (->> tdata
               (map :own-unconf|other-conf)
        )
]
;;(spit (str output-dir "/" "fs.txt") (with-out-str (pr dfeature-set)))
;;(println "check" tdata-other)
;;(println "feature" feature-set)
(loop [x 0
     
]

(if (< x (count feature-set))

(do
 (let [

feature-options ((nth feature-set x) :options)

updated-value (cond
      (= feature-options :numeric)
       (do
(let [ 
 feature-name (nth feature-set x)
feature-index (first (positions #{feature-name} all-dataset-feature) )
value-in-record (nth (:values record) feature-index)
tdata111 (->> tdata1
             (first)
)
 y (first (get tdata111 x))
update-mean (updated-mean (get y 0) value-in-record (get y 2))

update-std-dev (updated-std-dev (get y 1) value-in-record (get y 2) (get y 0) update-mean)
update-count (+ (get y 2) 1)
ftry (->> @prob-cond-check
          
        (map #(select-keys % [(first prob-key)]))
          (filter #(not= % {}))
         
         
           )  
 feature-try  (first (positions #{(first ftry)} @prob-cond-check))


ftry1 (->> @prob-cond-check
           
          (map #(select-keys % [(first prob-key)]))
            (filter #(not= % {}))
          (first)
          (map val)
             ;; (nth 1)
            (first)
            ;;(first)
           ;;   (map #(select-keys % [t]))
           ;;    (filter #(not= % {}))
          ;;      (first)
          ;; (map val)
         
           )  
ftry11 (->> ftry1
          ;; (first)
          ;;  (first)
             (map #(select-keys % [t]))
              (filter #(not= % {}))
               (first)
)

 feature-try1 (first (positions #{ftry11} ftry1))

ftry11-other (->> ftry1
          ;; (first)
          ;;  (first)
             (map #(select-keys % [tdata-other-key]))
              (filter #(not= % {}))
               (first)
)

 feature-try1-other (first (positions #{ftry11-other} ftry1))




]
;;  (if (= ((nth feature-set x) :name) "T7")
;; (println "update" ((nth feature-set x) :name) "  "  "   "update-mean "   " update-std-dev "  " update-count " " value-in-record)
;; )
;;(println "x is"  x "   " ftry11 "  " )
;;(println "   " feature-try "    " feature-try1 "    "  feature-try1-other)
;;(println "x is" @prob-cond-check)
;;(println "feature-try" "x is" x " "  feature-name " " feature-try)
;;  (swap! prob-cond-check #(update-in % [2 0] assoc 1 22))
 (swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :own-unconf|other-conf x 0 0] (constantly update-mean)))
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :own-unconf|other-conf x 0 1] (constantly update-std-dev)))
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :own-unconf|other-conf x 0 2] (constantly update-count)))
(when (= x 0)

(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :other-site-conf] inc))
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :total] inc))
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1-other tdata-other-key :total] inc))
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1-other tdata-other-key :other-site-unconf] inc))
)
;; (swap! prob-cond-check #(update-in % [2 11 0 2 :other-site-unconf] (constantly 22)))
 ;; (swap! prob-cond-check #(update-in % [2 2 3 :other-site-unconf] (constantly 22))) ;;this worked first on changed
;;(println "x is" @prob-cond-check)


)) 
(= feature-options :nominal)
(println "nominal att")
;;code here
  )

]

)
(recur (inc x))
)
))
)
@prob-cond-check
)
 ;; when both the sites are unconfident and @prob-cond-check is changed for both of them
(= (count tsite) 2)
(do 
;;(println tsite)
(doseq [t tsite]

(let [    
;;feature-set (drop-last (nth all-feature-set t) )
 feature-set (->> every-site 
                     (filter #(= (get % 0) t))
                    (map #(get % 1)) 
                     (map :site-schema) 
                      (first)
                      (drop-last)
)

tdata1 (->> prob-cond-check1
           (map #(get % (first prob-key)))
           (filter #(not= % nil))   
            (first)
            (map #(get % t))
(filter #(not= % nil))
          ;; (map #(select-keys % [(first prob-key)]))
          ;;   (filter #(not= % {}))
          ;;  (first)
          ;; (map val)
          ;;    ;; (nth 1)
          ;;  (first)
          ;;  (first)
          ;;    (map #(select-keys % [t]))
          ;;     (filter #(not= % {}))
          ;;      (first)

          ;; (map val)
         
           ) 

 ;; tdata1 (->> @prob-cond-check
 ;;                  ;;  (first)
 ;;              (filter #(= (:site-id %) t))
 ;;       )
 tdata11 (->> tdata1
            (map :both-unconf)) 
 ] 


(loop [x 0
     
]

(if (< x (count feature-set))

(do

 (let [

feature-options ((nth feature-set x) :options)

         updated-value (cond
      (= feature-options :numeric)
       (do
(let [ 
 feature-name (nth feature-set x)
feature-index (first (positions #{feature-name} all-dataset-feature) )
value-in-record (nth (:values record) feature-index)
;; tdata111 (nth tdata11 x)
;; y (first tdata111)
tdata111 (->> tdata11
             (first)
)
 y (first (get tdata111 x))

update-mean (updated-mean (get y 0) value-in-record (get y 2))

update-std-dev (updated-std-dev (get y 1) value-in-record (get y 2) (get y 0) update-mean)
update-count (+ (get y 2) 1)

ftry (->> @prob-cond-check
          
        (map #(select-keys % [(first prob-key)]))
          (filter #(not= % {}))
         
           )  

 feature-try  (first (positions #{(first ftry)} @prob-cond-check))

ftry1 (->> @prob-cond-check
           
          (map #(select-keys % [(first prob-key)]))
            (filter #(not= % {}))
          (first)
          (map val)
             ;; (nth 1)
            (first)
            ;;(first)
           ;;   (map #(select-keys % [t]))
           ;;    (filter #(not= % {}))
          ;;      (first)
          ;; (map val)
         
           )  
ftry11 (->> ftry1
          ;; (first)
          ;;  (first)
             (map #(select-keys % [t]))
              (filter #(not= % {}))
               (first)
)

 feature-try1 (first (positions #{ftry11} ftry1))


]
;;  (if (= ((nth feature-set x) :name) "T7")
;; (println "update" ((nth feature-set x) :name) "  "   "   " update-mean "   " update-std-dev "   " update-count "  " value-in-record)
;; )
;;(println feature-try (first prob-key) feature-try1 t)
;;(println x)


 (swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :both-unconf x 0 0] (constantly update-mean)))
  (swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t  :both-unconf x 0 1] (constantly update-std-dev)))
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t  :both-unconf x 0 2] (constantly update-count)))
(when (= x 0)
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t :other-site-unconf] inc))
(swap! prob-cond-check #(update-in % [feature-try (first prob-key) feature-try1 t  :total] inc))
 )  
))   )

]
)
(recur (inc x))
)
))
)      
)
@prob-cond-check
)
) ;;condition brackets   ;;main cond division
;;high level rackets
    ]
)
)
@prob-cond-check
 ;;(spit (str output-dir "/" "cond1.txt") (with-out-str (pr result)))
)
 )
(defn final-prob [results tsites record-sets dataset classifier i batch1]

;;(def getting-result [:site-id "" :both-unconf ""])


(let [
sites1   tsites
;; (->> results
;;             (map #(vector (:site-communication %) 
;;                    (:id %)))
;;              (filter #(= (second %) i))
;;               (map first)
;;               last
;;               keys
;;            (filter #(>= % 9))
            
;; )
sites (get-all-sites classifier)


 ]
;;(println "sites" sites1)

 ;;(spit (str output-dir "/" "cond1.txt") (with-out-str (pr sites1)))

(loop [x 0
      getting-results []
    ;;  gettings []
       ]
             
(if (< x (count sites1))
(do
;(println " is" (nth sites1 x))
;;(println "x is" x)
(let  [         
sites11 (nth sites1 x)
  all-records (->> results
             (map #(vector (get (:site-communication %)  (nth sites1 x)
                                ) 
                   (:id %)))
)
total-non-nil-records (->> all-records
                                            (map #(get % 0))
                                            (filter #(not= % nil))
                                            (count)               
                                            )


 ;; all-records10 (->> results
;;              (map #(vector (get (:site-communication %) 18
;;                                 ) 
;;                    (:id %)))
;; )

;; [sites11 getting-result1 getting-result2]
  historic
       (cond
;; (< total-non-nil-records 5000)
;; (do
;; (let [

;;  one-trouble-records (->> all-records
;;                                           (filter #(= (count (first %)) 1))) 
                 
;;  troubled-source-sites (->> one-trouble-records
;;                                             (map #(first %))
;;                                             (distinct) 
;;                                             )
;;  first-ssite (ffirst troubled-source-sites)
;;  getting-result1 {first-ssite nil}
;;  second-ssite (first (second troubled-source-sites))
;;  getting-result2 {second-ssite nil}
;;                                         ; historic {sites11 getting-result1 getting-result2}}
;;                  historic {sites11 [getting-result1 getting-result2]}
;; ]
;; historic
;; )

;; )

         (>  total-non-nil-records (/ batch1 2))
         (do
       ;; (println "got" sites11)
           (let [   
                 ;;calculation of probability where both sites are unconf

                 ;;list of records by both troubled source-site
                 ;; list of records where both sites are troubled
                 ;;prob common in both sites calculation
                 two-trouble-records (->> all-records
                                          (filter #(= (count (first %)) 2)))  
                 ;;record-ids of records where both sites are troubled
                 record-ids-for-two  (->> two-trouble-records
                                          (map #(get % 1))
                                          )  

                 recordsets-for-two  (for [ x record-sets
                                           y record-ids-for-two :when (= y (:id x))] x)

                 ;;calculation of probability where first site is unconf

                 ;;list of records with only one troubled source site
                 one-trouble-records (->> all-records
                                          (filter #(= (count (first %)) 1))) 


                        one-trouble-site (->> one-trouble-records
                                              (map #(first %))
                                            (distinct) 
                                                )
                 
                 tt (->> two-trouble-records
                                            (map #(first %))
                                            (distinct)
 (first)
  
                                            )



tt1 (list (first tt))
tt2 (list (second tt))
ttfi (list tt1 tt2)


                 ;;index of troubled source site 
                 ;;list of troubled source site  
                 ;;one trouble records are not there
                 troubled-source-sites 
(cond 
(or (=  one-trouble-records ()) (= (count one-trouble-site) 1))
ttfi
:else 
(->> one-trouble-records
                                            (map #(first %))
                                            (distinct) 
                                            )

)





                 ;; troubled-source-sites (->> two-trouble-records
                 
                 ;;                               (map #(first %))
                 ;;                                 (first) 
                 ;;                                 )

                 ;; second-site-quant (->> sites
                 
                 ;;  (filter #(= (get % 0) (first (second troubled-source-sites))))
                 ;;                   (map #(get % 1)) 
                 ;;                   (map :confidence-moving-average)                        ;; (first)
                 ;;                    ;;(map #(find-all-nested % :current-ma) ) 
                 ;;                      (map :current-ma)
                 ;;                      (first)
                 ;; )
                 ;; first-site-quant (->> sites
                 
                 ;;  (filter #(= (get % 0) (ffirst troubled-source-sites)))
                 ;;                   (map #(get % 1)) 
                 ;;                   (map :confidence-moving-average)                        ;; (first)
                 ;;                    ;;(map #(find-all-nested % :current-ma) ) 
                 ;;                      (map :current-ma)
                 ;;                      (first)
                 ;; )


                 ;;calculation of formuleas for 1st site

                 only-other-site-unconf-records (->> all-records
                                                     (filter #(= (first %) (second troubled-source-sites)))                             (count)
                                                     )
                 other-site-unconf-records (+ only-other-site-unconf-records (count two-trouble-records ))

                 total-records-reaching-ts (->> all-records
                                                (map #(get % 0))
                                                (filter #(not= % nil))
                                                (count)               
                                                )

                 prob-other-site-unconf other-site-unconf-records
                 prob-other-site-conf (- total-records-reaching-ts other-site-unconf-records)

                 second-site-quant (/ prob-other-site-conf total-records-reaching-ts)
                 ;;list of records for 1st troubled source site where site 1 is unconf and site2 is conf
                 ;;list of records for 1st troubled source site where site 1 is unconf and site2 is conf
                 record-for-one (->> one-trouble-records
                                     (filter #(= (first %) (first troubled-source-sites)))
                                     
                                     )
                 record-id-for-one (->> record-for-one
                                        (map #(get % 1))
                                        )  


                 ;;record id +values of records where site1 is unconf and site2 is conf
                 recordset-for-one
                 (for [ x record-sets
                       y record-id-for-one :when (= y (:id x))] x)

                 ;;getting prob result
                 both-unconf (cal-prob dataset classifier recordsets-for-two (first troubled-source-sites))

                 own-site-unconf-other-conf (cal-prob dataset classifier recordset-for-one (first troubled-source-sites))

                 ;; first-site {:site-id (ffirst troubled-source-sites)}  
                 first-ssite (ffirst troubled-source-sites)
                 ;; both-unconf {:both-unconf both-unconf}

                 ;;own-site-unconf-other-conf {:own-unconf|other-conf own-site-unconf-other-conf}

                 ;;other-site-unconf {:other-site-unconf  prob-other-site-unconf}
                 ;;other-site-conf {:other-site-conf  prob-other-site-conf}

                 getting-result (hash-map :site-id first-ssite :both-unconf both-unconf :own-unconf|other-conf own-site-unconf-other-conf :other-site-unconf prob-other-site-unconf :other-site-conf prob-other-site-conf :other-site-quant second-site-quant) 

                 getting-result1 {first-ssite {:both-unconf both-unconf :own-unconf|other-conf own-site-unconf-other-conf :other-site-unconf prob-other-site-unconf :other-site-conf prob-other-site-conf :other-site-quant second-site-quant :total total-records-reaching-ts}}

                 

                 ;;getting-result (conj getting-result {:site-id first-ssite} {:both-unconf both-unconf})  
                 ;;fgetting-result [conj getting-result i]

                 ;; getting-result1 (conj getting-result1 first-site both-unconf own-site-unconf-other-conf other-site-unconf other-site-conf) 
                 ;;calculations for second site
                 ;; second-site {:site-id (second troubled-source-sites) }
                 second-ssite (first (second troubled-source-sites))
                 both-unconf2 (cal-prob dataset classifier recordsets-for-two (second troubled-source-sites))

                 record-for-other (->> one-trouble-records
                                       (filter #(= (first %) (second troubled-source-sites)))
                                       
                                       )

                 record-id-for-other (->> record-for-other
                                          (map #(get % 1))
                                          )  


                 ;;record id +values of records where site2 is unconf and site1 is conf
                 recordset-for-other
                 (for [ x record-sets
                       y record-id-for-other :when (= y (:id x))] x)

                 own-site-unconf-other-conf2 (cal-prob dataset classifier recordset-for-other (second troubled-source-sites))

                 ;;     both-unconf2 {:both-unconf both-unconf2}

                 ;;  own-site-unconf-other-conf2 {:own-unconf|other-conf own-site-unconf-other-conf2}


                 only-other-site-unconf-records2 (->> all-records
                                                      (filter #(= (first %) (first troubled-source-sites)))                             (count)
                                                      )

                 other-site-unconf-records2 (+ only-other-site-unconf-records2 (count two-trouble-records))


                 prob-other-site-unconf2 other-site-unconf-records2

                 prob-other-site-conf2 (- total-records-reaching-ts other-site-unconf-records2)

                 first-site-quant (/ prob-other-site-conf2 total-records-reaching-ts)
                 ;;   prob-other-site-unconf2 (/ other-site-unconf-records2 i)
                 ;;   prob-other-site-conf2 (/ (- i other-site-unconf-records2) i)

                 ;;      other-site-unconf2 {:other-site-unconf  prob-other-site-unconf2}
                 ;;   other-site-conf2 {:other-site-conf  prob-other-site-conf2}
                 
                 ;; getting-result2 (conj getting-result2 second-site both-unconf2 own-site-unconf-other-conf2 other-site-unconf2 other-site-conf2)
                 getting (hash-map :site-id second-ssite :both-unconf both-unconf2 :own-unconf|other-conf own-site-unconf-other-conf2 :other-site-unconf prob-other-site-unconf2 :other-site-conf prob-other-site-conf2 :other-site-quant first-site-quant)
                 ;;historic-data (concat getting-result getting)
                 getting-result2 {second-ssite {:both-unconf both-unconf2 :own-unconf|other-conf own-site-unconf-other-conf2 :other-site-unconf prob-other-site-unconf2 :other-site-conf prob-other-site-conf2 :other-site-quant first-site-quant :total total-records-reaching-ts}}
                                        ; historic {sites11 getting-result1 getting-result2}}
                 historic {sites11 [getting-result1 getting-result2]}
                 ]       
           ;;  (println troubled-source-sites)
;;(println  (count one-trouble-site))
;;(spit (str output-dir "/" "records10.txt") (with-out-str (pr one-trouble-records)))
           ;;  (spit (str output-dir "/" "dis.txt") (with-out-str (pr historic)))
           ;;  (spit (str output-dir "/" "2-trou.txt") (with-out-str (pr troubled-source-sites  )))
          historic
       ;;   [sites11 getting-result1 getting-result2]
             )
           )
         )
       ] 

(recur (inc x) (conj getting-results 
historic
;; [sites11 getting-result1 getting-result2]
;;[sites11 getting-result1 getting-result2]
                     ) ;;(conj gettings getting-result2)
       )

;;getting-result1
)

)
[getting-results]
)

)
           
)

)
(defn final-prob1 [results tsites record-sets dataset classifier i batch1]

;;(def getting-result [:site-id "" :both-unconf ""])


(let [
sites1   tsites
;; (->> results
;;             (map #(vector (:site-communication %) 
;;                    (:id %)))
;;              (filter #(= (second %) i))
;;               (map first)
;;               last
;;               keys
;;            (filter #(>= % 9))
            
;; )
sites (get-all-sites classifier)
      
;;sites11 (nth sites1 x)
  all-records (->> results
             (map #(vector (get (:site-communication %) sites1
                                ) 
                   (:id %)))
)
total-non-nil-records (->> all-records
                                            (map #(get % 0))
                                            (filter #(not= % nil))
                                            (count)               
                                            )


;;  all-records10 (->> results
;;              (map #(vector (get (:site-communication %) 10
;;                                 ) 
;;                    (:id %)))
;; )


  historic
       (cond


         (>  total-non-nil-records (/ batch1 2))
         (do
       ;; (println "got" sites11)
           (let [   
                 ;;calculation of probability where both sites are unconf

                 ;;list of records by both troubled source-site
                 ;; list of records where both sites are troubled
                 ;;prob common in both sites calculation
                 two-trouble-records (->> all-records
                                          (filter #(= (count (first %)) 2)))  
                 ;;record-ids of records where both sites are troubled
                 record-ids-for-two  (->> two-trouble-records
                                          (map #(get % 1))
                                          )  

                 recordsets-for-two  (for [ x record-sets
                                           y record-ids-for-two :when (= y (:id x))] x)

                 ;;calculation of probability where first site is unconf

                 ;;list of records with only one troubled source site
                 one-trouble-records (->> all-records
                                          (filter #(= (count (first %)) 1))) 

 one-trouble-site (->> one-trouble-records
                                              (map #(first %))
                                            (distinct) 
                                                )
  tt (->> two-trouble-records
                                            (map #(first %))
                                            (distinct)
 (first)
  
                                            )



tt1 (list (first tt))
tt2 (list (second tt))
ttfi (list tt1 tt2)

     troubled-source-sites 
(cond 
(or (=  one-trouble-records ()) (= (count one-trouble-site) 1))
ttfi
:else 
(->> one-trouble-records
                                            (map #(first %))
                                            (distinct) 
                                            )

)



                 
                 
                 
                 ;;index of troubled source site 
                 ;;list of troubled source site  
                 ;;one trouble records are not there
                
                 ;; troubled-source-sites (->> two-trouble-records
                 
                 ;;                               (map #(first %))
                 ;;                                 (first) 
                 ;;                                 )

                 ;; second-site-quant (->> sites
                 
                 ;;  (filter #(= (get % 0) (first (second troubled-source-sites))))
                 ;;                   (map #(get % 1)) 
                 ;;                   (map :confidence-moving-average)                        ;; (first)
                 ;;                    ;;(map #(find-all-nested % :current-ma) ) 
                 ;;                      (map :current-ma)
                 ;;                      (first)
                 ;; )
                 ;; first-site-quant (->> sites
                 
                 ;;  (filter #(= (get % 0) (ffirst troubled-source-sites)))
                 ;;                   (map #(get % 1)) 
                 ;;                   (map :confidence-moving-average)                        ;; (first)
                 ;;                    ;;(map #(find-all-nested % :current-ma) ) 
                 ;;                      (map :current-ma)
                 ;;                      (first)
                 ;; )


                 ;;calculation of formuleas for 1st site

                 only-other-site-unconf-records (->> all-records
                                                     (filter #(= (first %) (second troubled-source-sites)))                             (count)
                                                     )
                 other-site-unconf-records (+ only-other-site-unconf-records (count two-trouble-records ))

                 total-records-reaching-ts (->> all-records
                                                (map #(get % 0))
                                                (filter #(not= % nil))
                                                (count)               
                                                )

                 prob-other-site-unconf other-site-unconf-records
                 prob-other-site-conf (- total-records-reaching-ts other-site-unconf-records)

                 second-site-quant (/ prob-other-site-conf total-records-reaching-ts)
                 ;;list of records for 1st troubled source site where site 1 is unconf and site2 is conf
                 ;;list of records for 1st troubled source site where site 1 is unconf and site2 is conf
                 record-for-one (->> one-trouble-records
                                     (filter #(= (first %) (first troubled-source-sites)))
                                     
                                     )
                 record-id-for-one (->> record-for-one
                                        (map #(get % 1))
                                        )  


                 ;;record id +values of records where site1 is unconf and site2 is conf
                 recordset-for-one
                 (for [ x record-sets
                       y record-id-for-one :when (= y (:id x))] x)

                 ;;getting prob result
                 both-unconf (cal-prob dataset classifier recordsets-for-two (first troubled-source-sites))

                 own-site-unconf-other-conf (cal-prob dataset classifier recordset-for-one (first troubled-source-sites))

                 ;; first-site {:site-id (ffirst troubled-source-sites)}  
                 first-ssite (ffirst troubled-source-sites)
                 ;; both-unconf {:both-unconf both-unconf}

                 ;;own-site-unconf-other-conf {:own-unconf|other-conf own-site-unconf-other-conf}

                 ;;other-site-unconf {:other-site-unconf  prob-other-site-unconf}
                 ;;other-site-conf {:other-site-conf  prob-other-site-conf}

                 getting-result (hash-map :site-id first-ssite :both-unconf both-unconf :own-unconf|other-conf own-site-unconf-other-conf :other-site-unconf prob-other-site-unconf :other-site-conf prob-other-site-conf :other-site-quant second-site-quant) 

                 getting-result1 {first-ssite {:both-unconf both-unconf :own-unconf|other-conf own-site-unconf-other-conf :other-site-unconf prob-other-site-unconf :other-site-conf prob-other-site-conf :other-site-quant second-site-quant :total total-records-reaching-ts}}

                 

                 ;;getting-result (conj getting-result {:site-id first-ssite} {:both-unconf both-unconf})  
                 ;;fgetting-result [conj getting-result i]

                 ;; getting-result1 (conj getting-result1 first-site both-unconf own-site-unconf-other-conf other-site-unconf other-site-conf) 
                 ;;calculations for second site
                 ;; second-site {:site-id (second troubled-source-sites) }
                 second-ssite (first (second troubled-source-sites))
                 both-unconf2 (cal-prob dataset classifier recordsets-for-two (second troubled-source-sites))

                 record-for-other (->> one-trouble-records
                                       (filter #(= (first %) (second troubled-source-sites)))
                                       
                                       )

                 record-id-for-other (->> record-for-other
                                          (map #(get % 1))
                                          )  


                 ;;record id +values of records where site2 is unconf and site1 is conf
                 recordset-for-other
                 (for [ x record-sets
                       y record-id-for-other :when (= y (:id x))] x)

                 own-site-unconf-other-conf2 (cal-prob dataset classifier recordset-for-other (second troubled-source-sites))

                 ;;     both-unconf2 {:both-unconf both-unconf2}

                 ;;  own-site-unconf-other-conf2 {:own-unconf|other-conf own-site-unconf-other-conf2}


                 only-other-site-unconf-records2 (->> all-records
                                                      (filter #(= (first %) (first troubled-source-sites)))                             (count)
                                                      )

                 other-site-unconf-records2 (+ only-other-site-unconf-records2 (count two-trouble-records))


                 prob-other-site-unconf2 other-site-unconf-records2

                 prob-other-site-conf2 (- total-records-reaching-ts other-site-unconf-records2)

                 first-site-quant (/ prob-other-site-conf2 total-records-reaching-ts)
                 ;;   prob-other-site-unconf2 (/ other-site-unconf-records2 i)
                 ;;   prob-other-site-conf2 (/ (- i other-site-unconf-records2) i)

                 ;;      other-site-unconf2 {:other-site-unconf  prob-other-site-unconf2}
                 ;;   other-site-conf2 {:other-site-conf  prob-other-site-conf2}
                 
                 ;; getting-result2 (conj getting-result2 second-site both-unconf2 own-site-unconf-other-conf2 other-site-unconf2 other-site-conf2)
                 getting (hash-map :site-id second-ssite :both-unconf both-unconf2 :own-unconf|other-conf own-site-unconf-other-conf2 :other-site-unconf prob-other-site-unconf2 :other-site-conf prob-other-site-conf2 :other-site-quant first-site-quant)
                 ;;historic-data (concat getting-result getting)
                 getting-result2 {second-ssite {:both-unconf both-unconf2 :own-unconf|other-conf own-site-unconf-other-conf2 :other-site-unconf prob-other-site-unconf2 :other-site-conf prob-other-site-conf2 :other-site-quant first-site-quant :total total-records-reaching-ts}}
                                        ; historic {sites11 getting-result1 getting-result2}}
                 historic {sites1 [getting-result1 getting-result2]}
                 ] 
     
             ;;(println troubled-source-sites)
           ;;  (spit (str output-dir "/" "dis.txt") (with-out-str (pr historic)))
           ;;  (spit (str output-dir "/" "2-trou.txt") (with-out-str (pr troubled-source-sites  )))
                 historic
              )
      
             )
           )
] 
        
      
;;(spit (str output-dir "/" "sites.txt") (with-out-str (pr sites)))
historic
;;getting-result1
)
  )




(defn train-classifier
  ([classifier dataset batch1]
   (train-classifier classifier dataset batch1 nil))
  ([classifier dataset batch1 progress-atom]
;;(println "batch1" batch1)
;;(def dataset-feature (dataset :schema))
   (loop [dataset dataset
          results []
          i 0 
          record-sets []
       ;; cond-probs []
          ]
     (let [[record rest-dataset] (pop-record-and-rest dataset) 


]
       (if record
         (let [raw-result (doall (process-record classifier record))
               result (assoc raw-result :truth (last (:values record)))
               record-id (:id record)
               record-values (drop-last (:values record)) 
             record-set {:id record-id :values record-values}
            ;; cond-prob (final-prob results record-sets dataset classifier i)
            ;;  fcond-prob (conj cond-prob :id i)
              
       
]
;;(spit (str output-dir "/" "checki.txt") (with-out-str (pr i)))
(when (= i 0)
(reset! prob-cond-check {})
)
(when (= i batch1)
;;(println "entering data" i)
;;(println (:id record))
(let [ 
trouble-try (->> (get-active-sites (get-all-sites classifier))
             (filter #(= (:type (get % 1)) :t-site
                                             ))
                               (map #(get % 0))
                         ;; (count)
             )
]
;;(println "trouble" trouble-try)
;;(swap! prob-cond-check conj (final-prob results record-sets dataset classifier i))
 (reset! prob-cond-check (final-prob results trouble-try record-sets dataset classifier i batch1))
(reset! prob-cond-check (first @prob-cond-check))
)
;;(spit (str output-dir "/" "cond1.txt") (with-out-str (pr (get-all-sites classifier)))) 
;;(println prob-cond-check)
;;(spit (str output-dir "/" "cond.txt") (with-out-str (pr prob-cond-check)))
)
(when (> i batch1)
;;(println "entering data" (:id record))
;;(update-prob prob-cond-check record result dataset classifier)

(let [prob-sites (->> @prob-cond-check
                         (count)

                 )

trouble-try (->> (get-active-sites (get-all-sites classifier))
             (filter #(= (:type (get % 1)) :t-site
                                             ))
                               (map #(get % 0))
                          (count)
             )

trouble-try-sites (->> (get-active-sites (get-all-sites classifier))
             (filter #(= (:type (get % 1)) :t-site
                                             ))
                               (map #(get % 0))
                          
             )
prob-sites-key (->> @prob-cond-check
                  (into (sorted-map))
                   (filter #(not= % nil))
                         (map key)

                 )



]
;;(println "prob" prob-sites-key)
;;(println "trouble" trouble-try-sites)
;; (if (= trouble-try 0)
;; (do
;; ;;(println "empty")
;; (reset! prob-cond-check ())
;; )
;; )
(if (and (not= trouble-try 0) (not= trouble-try-sites prob-sites-key)) ;;problem may arise
(do
;;(println "new" (:id record) "  " trouble-try-sites "    " prob-sites-key)
 ;;(reset! prob-cond-check (final-prob results trouble-try-sites record-sets dataset classifier i))
;;(reset! prob-cond-check (first @prob-cond-check))
(let [
equal-sites (seq (clojure.set/intersection (set trouble-try-sites) (set prob-sites-key)))
    non-equal-sites (seq (clojure.set/difference (set trouble-try-sites) (set prob-sites-key)))
equal-vector (vec equal-sites)
      filtered (map #(select-keys % equal-vector) @prob-cond-check) 
 filtered1 (filter #(not= % {}) filtered)
]
;;(println "equal" equal-vector "non" non-equal-sites "filtered" filtered1)
;;make this comment // for updating prob only on structure change
(if (not= filtered1 ())
(update-prob filtered1 record result dataset classifier)
)
(if (not= non-equal-sites ())

(doseq [y non-equal-sites]
;;(println "y" y)
(if (not= nil (final-prob1 results y record-sets dataset classifier i batch1))
(do

;;(println "its thre" (:id record) (final-prob1 results y record-sets dataset classifier i) )
;;(println "its replacing")
;;(spit (str output-dir "/" "old-value.txt") (with-out-str (pr prob-cond-check)))
(swap! prob-cond-check conj (final-prob1 results y record-sets dataset classifier i batch1))
;;(println prob-sites-key)
;;(spit (str output-dir "/" "prob-changed.txt") (with-out-str (pr prob-cond-check)))
) 
)
)
)
;;(spit (str output-dir "/" "cond2.txt") (with-out-str (pr prob-cond-check)))
)
)
;; (reset! prob-cond-check (final-prob results record-sets dataset classifier i))


)
(if (and (not= trouble-try 0) (= trouble-try-sites prob-sites-key) (not= @prob-cond-check ()) 
;;(not= (ffirst @prob-cond-check) nil)
)
(do
(let [filtered (filter #(not= % nil) @prob-cond-check)
    nil-sites  (seq (clojure.set/difference (set trouble-try-sites) (set prob-sites-key)))


]
;;(println "update" (:id record) "  " trouble-try-sites "    " prob-sites-key)
;;make this comment // for updating prob only on structure change
(update-prob filtered record result dataset classifier)

;;(println "filt" nil-sites)

(doseq [y nil-sites]
;;(println "y" y)
(if (not= nil (final-prob1 results y record-sets dataset classifier i batch1))
(do

;;(println "its thre" (:id record) (final-prob1 results y record-sets dataset classifier i) )
;;(println "its replacing")
;;(spit (str output-dir "/" "old-value.txt") (with-out-str (pr prob-cond-check)))
(swap! prob-cond-check conj (final-prob1 results y record-sets dataset classifier i batch1))
;;(println prob-sites-key)
;;(spit (str output-dir "/" "prob-change.txt") (with-out-str (pr prob-cond-check)))
) 
)
)

)
;;(spit (str output-dir "/" "cond1.txt") (with-out-str (pr prob-cond-check))) 
)


)


)
)


           (when (and (= (mod i 1000) 0) progress-atom)

;;adding mine 
          
             (reset! progress-atom i)
           )
           #_(debug
              (if (= (mod i 1000) 0)
                (println "Processed" i "records")))


           (recur rest-dataset (conj results result) (inc i) (conj record-sets record-set) 
;;(conj cond-probs fcond-prob)
                  )

)

         
        results
;;[results cond-probs]
)
)

 )

 )
 )
 



(defn build-classifier
  [batch1 base-classifier dataset base-site-structure
   {:keys [site-window-size site-training-time shared-sources?
           creation-window-size creation-time-threshold
           removal-window-size removal-time-threshold
           trouble-factor t-site-input-type creation-agreement-threshold
           removal-accuracy-threshold removal-usage-threshold]}
   & {:keys [p-site-aggregation-rule moving-average-generator
             detrend-confidence? confidence-jitter trouble-classifier
             disable-monitors disable-monitor-logging]}]
   ;; (println "sec" batch1)
  (let [creation-monitor-factory (make-creation-significant-agreement-monitor-factory
                                  creation-window-size
                                  creation-agreement-threshold
                                  creation-time-threshold)
        removal-monitor-factory (make-removal-accuracy-usage-monitor-factory
                                 removal-window-size
                                 removal-accuracy-threshold
                                 removal-usage-threshold
                                 removal-time-threshold)
        blacklist-monitor-factory (make-blacklist-source-accuracy-monitor-factory)]
    (dynamic-distributed-classifier batch1 base-site-structure
                                    (get-schema dataset)
                                    base-classifier
                                    moving-average-generator
                                    site-window-size
                                    trouble-factor
                                    t-site-input-type
                                    site-training-time
                                    shared-sources?
                                    creation-monitor-factory
                                    removal-monitor-factory
                                    blacklist-monitor-factory
                                    p-site-aggregation-rule
                                    detrend-confidence?
                                    confidence-jitter
                                    
                                    (or trouble-classifier base-classifier)
                                    
                                    :disable-monitors disable-monitors
                                    :disable-monitor-logging disable-monitor-logging)))

(defn build-naive-classifier
  [base-classifier dataset base-site-structure batch1
   & {:keys [p-site-aggregation-rule moving-average-generator
             detrend-confidence? confidence-jitter]}]
 ;;(println "first" batch1)
  (dynamic-distributed-classifier batch1 base-site-structure
                                  (get-schema dataset)
                                  base-classifier
                                  moving-average-generator
                                  1
                                  0
                                  nil
                                  1
                                  false
                                  (make-creation-significant-agreement-monitor-factory 1 2 1)
                                  (make-removal-inverted-agreement-monitor-factory)
                                  (make-blacklist-permanent-monitor-factory)
                                  p-site-aggregation-rule
                                  detrend-confidence?
                                  confidence-jitter
                                  
                                  base-classifier
                                  
                                  :disable-monitors true))

(defn String->Number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))
(defn parse-int [s]
   (Double/parseDouble (re-find  #"f+" s )))


(defn parse-float [s]
   (Double. s;;(re-find  #"f+" s )
    ))

(defn run-experiment
  ([base-classifier config]
   (run-experiment base-classifier config nil))
  ([base-classifier
    {:keys [label dataset-description dataset-fn
            base-site-structure system-config
            p-site-aggregation-rule detrend-confidence?
            confidence-jitter trouble-classifier
            disable-monitors disable-monitor-logging 
              batch1
            ]}

    progress-atom]
;;(println "hey" batch1)
   (let [dataset (dataset-fn)
         classifier (if (= :naive system-config)
                      (build-naive-classifier base-classifier dataset
                                              base-site-structure
                                              batch1
                                              :p-site-aggregation-rule p-site-aggregation-rule
                                              :detrend-confidence? detrend-confidence?
                                              :confidence-jitter confidence-jitter)
                      (build-classifier batch1 base-classifier dataset
                                        base-site-structure
                                        system-config
                                        
                                        :p-site-aggregation-rule p-site-aggregation-rule
                                        :detrend-confidence? detrend-confidence?
                                        :confidence-jitter confidence-jitter
                                        :trouble-classifier trouble-classifier
                                        :disable-monitors disable-monitors
                                        :disable-monitor-logging disable-monitor-logging))
        results (train-classifier classifier dataset batch1 progress-atom)
       ;; [results condp] (train-classifier classifier dataset progress-atom)
         sites (get-all-sites classifier)
         model-description (describe-model classifier)
     data (->> results
             (map #(vector (:site-communication %) 
                   (:id %))))
       

 tsites (->> sites

             (filter #(= (:type (get % 1)) :t-site))
               (map #(get % 0))
)
         
           
       commu (get-communication-to-sites results tsites) 
 ;; check-comm
 ;;         (->> results
 ;;              ;; (map #(:site-communication %))
 ;;              ;;  (map #(get (:site-communication %) 10))
 ;;              (map #(vector (get (:site-communication %) 10
 ;;                                 ) 
 ;;                            (:id %)))
 ;;              )
         ;; total-comm (get-communication-to-sites results (3 4))
         comm3 (->> results
                    (map #(get-in % [:site-communication 3]))
                    (remove nil?)
                    (flatten)
                    (frequencies)
                    (map-vals #(/ % (count results)))
                    (into {})) 

       
        ;; conf-matrix2 (get-class-performance-measures conf-matrix)
         r (->> results
              (filter #(> (:id %) batch1))
           )
 ;; conf-matrix (get-confusion-frequencies r)
      
        ;;  conf-matrix1 (print-confusion-matrix conf-matrix)
   accu (get-accuracy r) ;;results changed to r (accuracy)
         site-communication (get-communication-to-sites r (keys sites)) ;;results changed to r (communication)
         ;; percent (if pretty? percent identity)
         pro   (pretty-site-communication site-communication sites percent) 
         f (for [x  pro
                 :let [ y (second x)]  :when (not= y {})
                 ];;(str (second (first y)) (second (second y)))
             ;;(String->Number (second (second y)))
             (if (= (count y) 2)
               (+ (String->Number (second (first y))) (String->Number (second (second y))))
               (String->Number (second (first y)))
               )

             )
         af (reduce + f)
       ;; g (get-confusion-frequencies r)
        ;; gg (get-class-performance-measures g)
         ;; f (second (first pro)) 
         ]
;;(println site-communication)
;;(println "acc" accu)
;;(println af)
;;(println "freq" g)
;;(println "perf" gg)
;;(spit (str output-dir "/" "res_check.txt") (with-out-str (pr check-comm)))
;;(spit (str output-dir "/" "check_result.txt") (with-out-str (pr data)))
;;(spit (str output-dir "/" "final-check.txt") (with-out-str (pr conf-matrix)))
;;(def f (adaptive-random-forest (first (find-all-nested model-description :schema))))
;;(println f)
 ;;(spit (str output-dir "/" "site-label.txt") (with-out-str (pr model-description)))

     {:label label
      :timestamp (quot (System/currentTimeMillis) 1000)
      :dataset-description dataset-description
      :base-site-structure base-site-structure
      :system-config system-config
      :final-site-structure (->> sites
                                 (map-vals #(select-keys % [:type :label :attributes :source-sites :order])))
      :site-changelog (get-site-changelog classifier)
      :summary (results-summary batch1 sites results (get-site-changelog classifier)
                                :pretty? false)
      :model-description (if (map? (:AGGREGATOR model-description))
                           (update-in model-description
                                      [:AGGREGATOR :p-site-aggregation-rule]
                                      dissoc :classifier-generator)
                           model-description)
      :results results
     ;; :cond-prob condp
}
;;(println base-site-structure)
;;(def m "podsm.classifier.moa_classifier$wrap_moa_classifier$reify__7953 0x55ad460 "podsm.classifier.moa_classifier$wrap_moa_classifier$reify__7953@55ad460"" )
;;(println s)
;; (def s (first (find-all-nested model-description :schema)))
;;(println m)
;;(def f (moa-tree-model-string->tree m s))


;;(println f)

)))

(defn random-base-site-structure
  ([dataset-fn feature-count max-features-per-p-site]
   (random-base-site-structure dataset-fn feature-count max-features-per-p-site 1))
  ([dataset-fn feature-count max-features-per-p-site seed]
   (let [features (range 0 feature-count)
         class-index feature-count
         p-sites (->> (seeded-shuffle features seed)
                      (partition max-features-per-p-site
                                 max-features-per-p-site
                                 [])
                      (map-indexed
                       (fn [idx site-features]
                         (p-site (keyword (str "p-" (inc idx)))
                                 (vec site-features)))))]
     (apply make-site-structure class-index p-sites))))

(defn- p-site-pairs
  [feature-count]
  (let [p-site-count (/ feature-count 2)
        features (range feature-count)
        feature-pairs (clojure.math.combinatorics/combinations features 2)]
    (->> (clojure.math.combinatorics/combinations feature-pairs p-site-count)
         (filter (fn [pair-combos]
                   (= (count (flatten pair-combos))
                      (count (distinct (flatten pair-combos)))))))))

(defn p-site-permutations
  [feature-count]
  (->> (p-site-pairs feature-count)
       (map
        (fn [p-sites-features]
          (->> p-sites-features
               (map-indexed #(p-site (keyword (str "p" (inc %1)))
                                     (into [] %2))))))))

