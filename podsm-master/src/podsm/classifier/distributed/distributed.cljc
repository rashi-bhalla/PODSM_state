(ns podsm.classifier.distributed.distributed
  (:require [clojure.pprint :refer [pprint]]
            [podsm.classifier.base
             :refer [process-record describe-model]]
            [podsm.classifier.moa-classifier :refer :all]
            [podsm.classifier.distributed.base
             :refer [get-sites get-all-sites]]
            [podsm.dataset.base
             :refer [get-schema pop-record-and-rest]]
            [podsm.utils
             :refer [index-of safe-division seq-contains?
                     seq-is-subset? get-keys find-all-nested cal-no-value cal-nu-value positions]]
            [podsm.utils.random
             :refer [seeded-rng next-int! next-float!]]
            [podsm.utils.moving-average
             :refer [update-ma get-current-ma make-ewma-for-window]]
            [podsm.classifier.distributed.sites
             :refer [init-sites-atom site-attributes feature-count 
                     get-active-sites is-training? is-stacked-t-site? get-site-features site-attributes p-site]]
            [podsm.classifier.distributed.aggregation
             :refer [build-two-stage-aggregator describe-aggregator
                     aggregate-results-with-candidates]]
            [podsm.trees.base :refer [moa-tree-model-string->tree tree-branch-paths]]
            [podsm.classifier.moa-classifier :refer [hoeffding-tree naive-bayes adaptive-random-forest]]
            [podsm.trees.parse-model :refer [parse-moa-tree-model-string]]
    ;; [podsm.training :refer [prob-cond-check]]
)
  (:import [podsm.classifier.base Classifier]
           [podsm.classifier.distributed.base DistributedClassifier]
           [podsm.classifier.distributed.aggregation Aggregator]
           [podsm.dataset.base Dataset]))

;;(def output-dir (str "H:/data/nasa"))
(def prob-cond-check (atom {}))


(defn- test-site!
  "Apply the classifier at the given site to classify the given
  record (with the full list of sites provided as context to determine
  the values (a subset of features or site class results) that can be
  used at the site)."
  [site sites site-results record]
  (let [record-values (vec (:values record))
        local-values (if (is-stacked-t-site? site)
                       ;; Create record of classes from source-sites.
                       (as-> (:source-sites site) $
                         (get-keys site-results $)
                         (map :class $)
                         ;; Add in actual target class value
                         (concat $ [(last record-values)])
                         (vec $))
                       ;; Filter record features
                       (get-keys record-values (site-attributes site sites)))]
    ;; By processing a record at a site, we are also training the
    ;; site on that record.
    (process-record (:classifier site)
                    (assoc record :values local-values))))

(defn compute-agreement-factor
  "Given a trouble-site's agreement-window (each entry specifies the
  number of source-sites that agreed a particular record was
  'trouble') and number of source-sites to a given trouble-site,
  compute the level of agreement at the trouble-site.

  A primary-site should have an agreement window full of 1s and a
  source-site-count of 1 to produce an agreement value of 1 (full
  agreement)."
  [agreement-window source-site-count]
  (if (= 0 source-site-count)
    1
    (let [window-sum (reduce + agreement-window)]
      ;; Treat the special case of no transmission as 0% agreement.
      (if (= 0 window-sum)
        0
        (/ (count (filter #(= % source-site-count) agreement-window))
           (/ window-sum source-site-count))))))

#_(defn- compute-agreement-factor
    "Function for testing a naively assumed agreement factor of
     1 (i.e. full agreement)."
    [agreement-window source-site-count]
    1)

(defn default-moving-average-generator
  "The default moving average generator, defaults to an exponentially
  weighted moving average that takes into account approximately half
  of the window (alpha = (/ 2 (+ (/ window-size 2) 1))). See:
  https://en.wikipedia.org/wiki/Moving_average#Relationship_between_SMA_and_EMA

  Set the initial average to 0.5, as this is the mid-point for all
  possible confidence values."
  [window-size]
  (fn [] (make-ewma-for-window (/ window-size 2) 0.5)))

(defn is-troubled-by-confidence?
  "Determine if a given site is 'trouble' (i.e. will forward the current
  record (as defined by the list of site-results generated for the
  record up to this point)) based on:

  1. The source-site is not troubled if it does not have a site-result
  entry (indicating it must have been a trouble-site that was not
  'fired').

  2. The formula for determining whether a record is trouble is
  applied, which is based on agreement, quantile of the confidence
  value, feature counts, and trouble-factor."
  [site-idx sites site-results t-site-feature-count
   t-site-source-site-count t-site-trouble-factor
   t-site-input-type]
  (and (contains? site-results site-idx)
       (let [site (get sites site-idx)
             site-result (get site-results site-idx)
             source-site-input-size (if (is-stacked-t-site? site)
                                      (count (:source-sites site))
                                      (feature-count site sites))
             source-site-reduction-factor (case (:type site)
                                            :p-site 1
                                            :t-site (* (:agreement site-result)
                                                       (min 1
                                                            (/ (:trouble-factor site)
                                                               source-site-input-size))))
             t-site-input-size (if (= t-site-input-type :classes)
                                 t-site-source-site-count
                                 t-site-feature-count)
             max-quantile (safe-division t-site-trouble-factor
                                         (* source-site-reduction-factor
                                            t-site-input-size) 1)]
         (< (:quantile site-result) max-quantile))))

(defn is-troubled-by-class?
  "Determine if a given site is 'trouble' (i.e. will forward the current
  record (as defined by the list of site-results generated for the
  record up to this point)) based on:

  1. The source-site is not troubled if it does not have a site-result
  entry (indicating it must have been a trouble-site that was not
  'fired').

  2. There is no unanimous classification between this site and its
  fellow source-sites."
  [site-idx site-results source-site-indexes]
  (and (contains? site-results site-idx)
       (->> source-site-indexes
            (map #(get site-results %))
            (map :class)
            (apply not=))))

(def ^:dynamic *trouble-selector* :confidence)

(defn is-troubled?
  "Determine if a given site is 'trouble' (i.e. will forward the current
  record (as defined by the list of site-results generated for the
  record up to this point)).

  Method used to determine 'trouble' is given by *trouble-selector*"
  [site-idx sites site-results t-site-feature-count t-site-source-site-count
   t-site-trouble-factor source-site-indexes t-site-input-type]
  (case *trouble-selector*
    :confidence (is-troubled-by-confidence? site-idx sites site-results
                                            t-site-feature-count
                                            t-site-source-site-count
                                            t-site-trouble-factor
                                            t-site-input-type)
    :class (is-troubled-by-class? site-idx site-results source-site-indexes)))

(defn- get-troubled-source-sites
  "Return the indexes of the source-sites for the given site that
  consider the current record (as defined by the list of site-results
  generated for the record up to this point) to be 'trouble' and
  should be 'forwarded' to the given site."
  [site sites site-results]
  ;; If :source-sites is not set, this is a primary-site with no
  ;; trouble source-sites.
  (if-let [source-sites (:source-sites site)]
    (filter #(is-troubled? % sites site-results
                           (feature-count site sites)
                           (count source-sites)
                           (:trouble-factor site)
                           source-sites
                           (:input-type site))
            source-sites)
    []))

(defn compute-confidence-quantile
  "Default function for computing the quantile (position in the list on
  a scale of 0 to 1) of the given confidence value within the given
  confidence-window.

  Note that a true quantile represents a point with a certain
  percentage of data above and below, so we need to choose what side
  of the specific value our quantile is to be placed. To be
  conservative in the amount of data we send to trouble-sites, we
  imagine the quantile is placed just above the specific value, so the
  quantiles are generally higher and less likely to fall below the
  confidence threshold. The impact of this choice is very minor,
  particularly with a reasonably sized window. Note that this means
  the smallest confidence value in the list doesn't have a quantile of
  zero, but rather `1 / (count confidence-window)`.

  An alternative to the above approach could be to ignore the
  particular value in the list and compute the quantile as `(count
  items-below) / (count items-above)`.

  If the given confidence value appears more than once in the list, we
  place the quantile mark after ALL of its occurences (once again, to
  be conservative in the amount of data we send)."
  [confidence-window confidence]
  (safe-division (count (filter #(<= % confidence)
                                confidence-window))
                 (count confidence-window)
                 1))

(defn- dec-training-countdown
  "If a :training-countdown is set on the site, then decrement it and
  return the new site (or remove it if the counter has reached
  zero). Otherwise return the original site.

  We let the countdown reach zero and only remove it one record later
  because we still want to indicate to the aggregator not to use this
  site's classifier until the next record."
  [site]
  (if (contains? site :training-countdown)
    (if (<= (:training-countdown site) 0)
      (dissoc site :training-countdown)
      (update site :training-countdown dec))
    site))


;;calculation of prob of depending uopn the attributes 
(defn subset-attribute-cal [feature-name set-prob-cal value record]
;;(println "inside third")
;;(println value)
;;(println set-prob-cal)
(case (feature-name :options)
:numeric 

(do 
;;(println "numeric block")
(let [prob-both-unconf (cal-nu-value set-prob-cal value record)]
;;(println "prob in block" prob-both-unconf)
prob-both-unconf
;;(println prob-both-unconf)
)

)
:nominal
(do
;;(println "nominal block")
(let [prob-both-unconf (cal-no-value set-prob-cal value record)]
prob-both-unconf
)
)
)) 

(defn cal-attributes-prob [trouble-site-feature-set trouble-site-feature-count record set-prob-cal schema]
;;(println "inside second")
;;(println trouble-site-feature-set)
;;(println  trouble-site-feature-count)
;;(println record)
;;(println schema)
(loop [x 0 
      prob-attributes []]
  ;;   (println "xis" x)
(if (< x  trouble-site-feature-count)
(do

(let [
feature-name (nth trouble-site-feature-set x)
    feature-index (first (positions #{feature-name} schema) )
value (nth (:values record) feature-index)
historic-data (nth (first set-prob-cal)  x)
;;y (subset-attribute-cal feature-name set-prob-cal value)
;;y (reduce * prob-attributes)
]

;;(println "feature-index" feature-index)
;;(println value)
;;(println historic-data)


;(println y)
(recur (inc x) (conj prob-attributes (subset-attribute-cal feature-name historic-data value record)))

)

)
(reduce * prob-attributes)

)
)
)

(defn check-prob [prob-own-conf other-site-quant troubled-source-sites]
;;(println "prob" prob-own-conf "other" other-site-quant)
;;(println "prob inside is" prob-own-conf)
(cond
(>= prob-own-conf 0.50)
;;(>= prob-own-conf other-site-quant)
(do
;;(println "inside")
(let [troubled-sites ()]
troubled-sites
)
)

:else troubled-source-sites
)
)




;;adding some test
(defn get-updated-trouble-sites [troubled-source-sites record sites site schema level] 
;;(println "inside calculations" "troubled-site" troubled-source-sites "level" level)
;;(println record)

(let [
;;get historic data of troubled site


cond-pro (->> @prob-cond-check
            (map #(select-keys % [level])) 
               (filter #(not= % {})) 
            (first)
            (map #(nth % 1))
       
)
;; troubled-site (->> cond-pro
;;                    (first)
                   
;;                  (map #(select-keys % [troubled-source-sites]))  
;;                  (filter #(not= % {})) 
;;                  ;;  (:site-id)
;;                  ;;  (filter #(= (get % 0) troubled-source-sites))
;;                    )
                    
t-sites (cond
(= cond-pro ())
;;(println "inside")
troubled-source-sites
:else 
(do
(let [     

 troubled-site (->> cond-pro
                   (first)
                 ;; (map #(get % troubled-source-sites))  working
                 (map #(select-keys % [troubled-source-sites]))  
                 (filter #(not= % {})) 
                 ;;  (:site-id)
                 ;;  (filter #(= (get % 0) troubled-source-sites))
                   )
                    
                   
;; ;; ;;get schema of troubled site
trouble-site-feature-set (->> sites
                           
 (filter #(= (get % 0) troubled-source-sites))
                  (map #(get % 1)) 
                      (map :site-schema)
                     (first)
                   (drop-last)

)

other-site  (->> :source-sites site
             (filter #(not= % troubled-source-sites 0))    
               (first)
                
) 

other-site-quant (->> troubled-site
                     (first)
                   (map #(get % 1)) 
                 
                   (map :other-site-quant)
                   (first) 
                      ;;  (filter #(= (:site-id %) troubled-source-sites )) 
)
;; ;; other-site-conf (->> sites
                           
;; ;;  (filter #(= (get % 0) other-site))
;; ;;                   (map #(get % 1)) 
;; ;;                   (map :confidence-moving-average)                        ;; (first)
;; ;;                    ;;(map #(find-all-nested % :current-ma) ) 
;; ;;                      (map :current-ma)
;; ;;                      (first)
;; ;; )

trouble-site-feature-count (count trouble-site-feature-set) 

;; ;; ;;get both-unconf value 
both-unconf (->> troubled-site
                     (first)
                   (map #(get % 1)) 
                 
                   (map :both-unconf)
                   
                                       
 )

 own-unconf-other-conf (->> troubled-site
                     (first)
                   (map #(get % 1)) 
                 
                   (map :own-unconf|other-conf)
;;                     ;; first
;;                     (filter #(= (:site-id %) troubled-source-sites))
;;                   
                   
 )
feature-set (->>
                (nth trouble-site-feature-set 0)
                 (:options)
)

prob-both-unconf (cal-attributes-prob trouble-site-feature-set trouble-site-feature-count record both-unconf schema)
;;(get (cal-attributes-prob trouble-site-feature-set trouble-site-feature-count record both-unconf schema) 0)

prob-own-unconf (cal-attributes-prob trouble-site-feature-set trouble-site-feature-count record own-unconf-other-conf schema)
;;(get (cal-attributes-prob trouble-site-feature-set trouble-site-feature-count record own-unconf-other-conf schema) 0)
 
 other-site-unconf (->> troubled-site
                     (first)
                   (map #(get % 1)) 
                 
                   (map :other-site-unconf)
                           (first)

 
;;                      ;;first
;;                     (filter #(= (:site-id %) troubled-source-sites))
;;                   (map :other-site-unconf)
;;                   (first)
                   
 )
total (->> troubled-site
                     (first)
                   (map #(get % 1)) 
                 
                   (map :total)
                           (first)
          
            )
prob-other-site-unconf (/ other-site-unconf total)

 other-site-conf (->>  troubled-site
                     (first)
                   (map #(get % 1)) 
                  (map :other-site-conf)
                    (first)
                   


;;                     ;; first
;;                     (filter #(= (:site-id %) troubled-source-sites))
;;                  
 )
prob-other-site-conf (/ other-site-conf total)

prob-own-conf (cond 
(or (= 0  prob-own-unconf) (= 0 prob-other-site-conf) (= 0 prob-both-unconf) (= 0 prob-other-site-unconf))
0
:else
(do 
(try 
(/ (* prob-own-unconf prob-other-site-conf) (+ (* prob-own-unconf prob-other-site-conf) (* prob-both-unconf prob-other-site-unconf)))
(catch Exception e
0
)
)
)
)
t-sites (check-prob prob-own-conf other-site-quant troubled-source-sites)
]
;;(println "ts" cond-pro)
;;(println "t1" troubled-site "   " other-site-quant)
;;(println "troubled" both-unconf)
;;(println "values1" prob-both-unconf)
;;(println "values2" prob-own-conf)


;;(println "quant" other-site-quant)
t-sites

 )
)
)

;;these are comments only
;; (cond 
;; (> (:id record) 10001)
;; ;;(and (> (:id record) 10001) (< (:id record) 10007))
;; (first @prob-cond-check)
;; :else @prob-cond-check
;; )

 ]
;;(println   "tro" troubled-site)
t-sites 
)
) 


(defn- get-site-results!
  "Sequentially get the classification result from each site for the
  given record.

  If the site contains pre-computed results, use them instead of
  performing classification.

  Primary sites are always fired, but trouble-sites are only fired if
  all of their source-sites are forwarded.

  Returns:

  1. The individual results of each site that was 'fired'
  2. The list of sites with updated confidence and agreement windows.
  3. A record of any communication between sites when processing this
     record."
  [batch1 sites record window-size schema conf-quantile-fn
   & {:keys [detrend-confidence? confidence-jitter]}]
  (loop [sites sites
         site-results {}
         site-communication {}
         site-idxs (keys sites)
           
]
    (if-let [site-idx (first site-idxs)]
      (let [site (get sites site-idx)]
        (if (contains? site :computed-results)
          ;; Handle pre-computed site results. site-communication is NOT updated.
          (if-let [result (get-in site [:computed-results (:id record)])]
            ;; Pre-computed result found, add it to the site-results and
            ;; decrement any training counter.
            (recur (assoc sites site-idx (dec-training-countdown site))
                   (assoc site-results site-idx result)
                   site-communication (rest site-idxs))
            ;; No pre-computed result found, so this site does not fire
            ;; for this record.
            (recur sites site-results
                   site-communication (rest site-idxs)))
          (let [troubled-source-sites (get-troubled-source-sites site sites site-results)
                   
              trouble-try (->> sites
                            ;; (map #(get % 1))
                           
                              (filter #(= (:type (get % 1)) :p-site
                                             ))
                               (map #(get % 0))
                               )
              level (- site-idx (count trouble-try))
                
  troubled-source-sites         
                 (cond
                (and (> (:id record) (+ batch1 1)) (= (count troubled-source-sites) 2))
                (do
              ;; (println "site-idx")
              ;;  (println "troubled-sites initially" troubled-source-sites) 
               ;; (println "id" site-idx)
                ;; (println prob1)
                (let [tsite (when (= (count troubled-source-sites) 2) troubled-source-sites) 
                tsite11 (first tsite)
                tsite12 (second tsite)
                tsite21 (get-updated-trouble-sites tsite11 record sites site schema site-idx)
                tsite22 (get-updated-trouble-sites tsite12 record sites site schema site-idx)
                tsite3 (cond
                             (and (= tsite21 ()) (= tsite22 ()) )
                             ()
                              (and (not= tsite21 ()) (= tsite22 ()) )
                               (list tsite21)
                             (and (= tsite21 ()) (not= tsite22 ()) )
                            (list tsite22)
                           (and (not= tsite21 ()) (not= tsite22 ()) )
                
                             (list tsite21 tsite22))
                ] 
               ;; (println "afterwards" tsite3)
                tsite3
                )

                )

                (and (> (:id record) (+ batch1 1)) (= (count troubled-source-sites) 1))
                (do 
               ;; (println "troubled-sites-initially" troubled-source-sites)
                (let [tsite (when (= (count troubled-source-sites) 1) troubled-source-sites)
                      tsite1 (first tsite)
                      tsite2 (get-updated-trouble-sites tsite1 record sites site schema site-idx)
                      tsite3 (cond
                             (= tsite2 ())
                             tsite2 
                             :else 
                            (list tsite2))
                   tsite4   tsite3
                     
                      ]

         ;; (println "afterwards" tsite2)
                tsite4
                )
                )
                :else troubled-source-sites
                )


                source-site-count (count (:source-sites site))
                troubled-sources-count (count troubled-source-sites)

                new-site (if (= 0 troubled-sources-count)
                           site
                           ;; Update the agreement window for
                           ;; trouble-sites where at least one
                           ;; source-site forwarded a record. If no
                           ;; source-sites forwarded the record, then
                           ;; this site would not even know about the
                           ;; record.
                           (update site :agreement-window
                                   #(take window-size
                                          (conj % troubled-sources-count))))
                ;; Keep a record of which source-sites sent data to this
                ;; site.
                
                new-site-communication
                (assoc site-communication site-idx
                       troubled-source-sites) 
                
                ]


;; (if (= (:id record) 10002)
;; (do
;; ;;(println "10011 inside now")
;; ;;(println "prob" @prob-cond-check)
;; (println "after" troubled-source-sites)
;; (spit (str output-dir "/" "check_result.txt") (with-out-str (pr trouble-try)))
;; ;;(println "after" troubled-source-sites1)
;; ;;(println "source-count" source-site-count)
;; )
;; )
;; (if (= (:id record) 10002);; (do
;; (println "10002 inside now")
;; (println "prob" @prob-cond-check)
;; (println "10002" troubled-source-sites)
;; ;;(println "10002 afterwards" troubled-source-sites1)
;; ;;(println "troubled-source-count" (count troubled-source-sites1))
;; ;;(println "source-count" source-site-count)
;; )
;; )
;;(spit (str output-dir "/" "check_result.txt") (with-out-str (pr troubled-source-sites " " (:id record))))

;;ending some test

            (if (= troubled-sources-count source-site-count)
              ;; All source-sites declare trouble, so we need to
              ;; fire/classify at this site (or both are 0 for
              ;; primary-sites, which must always fire).
              (let [raw-result (test-site! site sites site-results record)
                    confidence (:confidence raw-result)
                    new-ma (-> (:confidence-moving-average new-site)
                               (update-ma confidence))
                    ;; Transform the confidence value according to
                    ;; configuration.
                    transformed-confidence
                    (cond-> confidence
                      ;; Subtract moving average if detrending is
                      ;; enabled.
                      detrend-confidence?
                      (- (get-current-ma new-ma))
                      ;; Add random jitter (between -1 and 1
                      ;; multiplied by confidence-jitter)
                      (> confidence-jitter 0)
                      (+ (* confidence-jitter
                            (- (* 2 (next-float! (:jitter-rng new-site))) 1))))
                    ;; Update the site's confidence window based on the
                    ;; new result, and decrement any training counter.
                    new-site (-> new-site
                                 (assoc :confidence-moving-average new-ma)
                                 (update :confidence-window
                                         #(take window-size
                                                (conj % transformed-confidence)))
                                 (dec-training-countdown))
                    result (-> raw-result
                               ;; Mark the quantile and agreement of this result
                               ;; for reference when determining if this record
                               ;; should be counted as "forwarded" to any
                               ;; trouble-sites.
                               (assoc :site-label (:label site))
                               (assoc :site-type (:type site))
                               (assoc :site-order (:order site))
                               (assoc :transformed-confidence transformed-confidence)
                               (assoc :confidence-ma (get-current-ma new-ma))
                               (assoc :quantile
                                      (conf-quantile-fn (:confidence-window new-site)
                                                        transformed-confidence))
                               (assoc :agreement
                                      (compute-agreement-factor (:agreement-window new-site)
                                                                source-site-count))
                               ;; Mark the current training-countdown
                               ;; state for reference when aggregating
                               ;; sites.
                               (assoc :training-countdown (:training-countdown new-site)))]
                (recur (assoc sites site-idx new-site)
                       (assoc site-results site-idx result)
                       new-site-communication (rest site-idxs)))
              (recur (assoc sites site-idx new-site) site-results
                     new-site-communication (rest site-idxs))))))
      [sites site-results site-communication])))

(defn distributed-classifier
  "Create a new distributed classifier for the given:

  1. The site-structure
  2. The dataset schema
  3. A function for generating the classifier to use
     at each site.
  4. The size/width of the agreement and confidence windows
  5. (optional) A custom function to compute the confidence quantile
  given a confidence value and a window of recent
  confidence-values (main use is to override it with a function that
  returns 0 or 1 depending on whether the confidence value is above or
  below a static threshold (ignoring the confidence-window)).
  6. (optional) A sites-atom can be built separately with
  [[podsm.classifier.distributed.sites/init-sites-atom]]
  and provided via a keyword argument. Providing the sites-atom allows
  the client to manipulate the sites-atom as needed.
  7. (optional) The p-site-aggregation-rule is a map that determines how
  classification aggregation is performed for p-site results, as used in
  [[podsm.classifier.distributed.aggregation/build-two-stage-aggregator]].
  8. (optional) The moving average generator is a function for
  generating a MovingAverage for each site to track the moving average
  of confidence values (defaults to that returned
  by [[podsm.classifier.distributed.distributed/default-moving-average-generator]]).
  9. (optional) detrend-confidence? determines whether confidence
  values will be detrended according to the computed
  moving-average (to prevent quantiles being affected by trends in
  confidence values - particularly when confidence varies little).
  10. (optional) confidence-jitter is the maximum bound of random
  noise added to confidence values to ensure quantile values still
  have high variance even when confidence varies little. Set to 0 to
  not add noise."
  [site-structure schema classifier-generator window-size batch1
   & {:keys [sites-atom conf-quantile-fn moving-average-generator
             p-site-aggregation-rule detrend-confidence? confidence-jitter]
      :or {conf-quantile-fn compute-confidence-quantile}}]

  (let [p-site-aggregation-rule (or p-site-aggregation-rule {:type :max-conf})
        default-detrend-confidence? false
        default-confidence-jitter 0
        detrend-confidence? (if (nil? detrend-confidence?) default-detrend-confidence? detrend-confidence?)
        confidence-jitter (if (nil? confidence-jitter) default-confidence-jitter confidence-jitter)
        moving-average-generator (or moving-average-generator
                                     (default-moving-average-generator window-size))
        sites-atom (or sites-atom
                       (init-sites-atom site-structure schema classifier-generator
                                        moving-average-generator))
        aggregator (build-two-stage-aggregator p-site-aggregation-rule
                                               classifier-generator
                                               schema @sites-atom)]
    (reify
      Classifier
      (process-record [this record]

     ;;testing results 
        (if (= (:id record) 10001)
          (println "reached 10001 in distributed")
        ;;   (let [model (describe-model this)]
;;      (spit (str output-dir "/" "check_prob.txt") (with-out-str (pr (get-sites this))))
;; )
)
;; ;;ending
        ;; Get the results of processing the record on the distributed
        ;; hierarchy of sites.
        (let [truth (last (:values record))
              [new-sites site-results site-communication]
              (get-site-results! batch1 (get-sites this) record
                                 window-size schema conf-quantile-fn
                                 :detrend-confidence? detrend-confidence?
                                 :confidence-jitter confidence-jitter)
              {:keys [final-result candidate-results]}
              (->> site-results
                   ;; Filter out results for sites that are
                   ;; still "training".
                   (filter #(not (is-training? (second %))))
                   (into (sorted-map))
                   (aggregate-results-with-candidates aggregator record))]
          ;; Update the sites-atom based on changes to the states of
          ;; sites (e.g. confidence/agreement windows).
          (doseq [[site-idx new-site] new-sites]
            (swap! sites-atom #(assoc % site-idx new-site)))
          ;; Find the final classification result.
          (->> final-result
               ((fn [result]
                  ;; Add metadata to the final result for later
                  ;; analysis.
                  (-> result

                      (assoc :truth truth)
                      (assoc :correct? (= (:class result) truth))
                      (assoc :breakdown site-results)
                      ;; Save candidate results so that they can be
                      ;; used when evaluating accuracy without
                      ;; particular trouble-site(s).
                      (assoc :candidate-results candidate-results)
                      (assoc :site-communication site-communication)))))))
      (describe-model [this]
        ;; Describe the model by describing the classifier at each site.
        (let [site-models (->> (vals (get-sites this))
                               (map #(vector (:label %)
                                             {:model (describe-model (:classifier %))
                                              :schema (:site-schema %)}))
                               (into {}))]
              ;; (struct-paths site-models)
          ;; Include aggregator's model (if one exists)
          (if-let [aggregator-model (describe-aggregator aggregator)]
            (assoc site-models :AGGREGATOR
                   {:model aggregator-model
                    :p-site-aggregation-rule p-site-aggregation-rule})
            site-models)))
      DistributedClassifier
      (get-sites [this]
        (get-active-sites (get-all-sites this)))
      (get-all-sites [this]
        @sites-atom)
      (get-site-changelog [this]
        []))))
