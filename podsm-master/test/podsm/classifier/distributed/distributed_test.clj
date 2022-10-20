(ns podsm.classifier.distributed.distributed-test
  (:use midje.sweet)
  (:require
   [podsm.classifier.distributed.distributed :as distributed]))

(fact "example"
      (conj [1 2] 3) => [1 2 3])
