(ns synthbiont.ontology
    (:use [tawny.owl])
  (:use [synthbiont.ontology])  
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

(defn addComment [ontology class value]
  (owl-class ontology class :comment value))

