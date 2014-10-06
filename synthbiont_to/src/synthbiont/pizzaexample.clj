(ns pizza.core

  (:use [tawny.owl])
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

(defontology pizzaontology
  :iri "http://www.ncl.ac.uk/pizza"
  :prefix "piz:"
  :comment "An example ontology modelled on the Pizza tutorial ontology from Manchester University,
written using the tawny-owl library"
  :versioninfo "Unreleased Version")

(defclass Pizza
  :label "Pizza1")

(defclass Pizza
  :label "PizzaGM")

(defclass PizzaTopping)

;; currently we have to use the annotation function with label to pass a
;; language in.
(defclass PizzaBase
  ;; the pizza ontology contains some Portuguese labels. The :label keyword
  ;; used above is a shortcut for English
  :annotation (label "BaseDaPizza" "pt"))
  ;;)

(declare-classes ChickenTopping
               HamTopping
               HotSpicedBeefTopping
               PeperoniSausageTopping)

(defoproperty hasBase
  ;:subpropertyof hasIngredient
  ;:characteristics functional
  :range PizzaBase
  :domain Pizza
  )

(save-ontology "ontology.owl" :owl)

(defn testgm2[]
  (println "calling the method")
;(foo "dsfdfg")
)
