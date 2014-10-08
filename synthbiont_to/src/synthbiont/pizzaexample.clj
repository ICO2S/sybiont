(ns pizza.core

  (:use [tawny.owl])
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

(defontology pizzaontology
  :iri "http://www.ncl.ac.uk/pizza"
  :prefix "piz:"
  :comment "An example ontology modelled on the Pizza tutorial ontology from
Manchester University, written using the tawny-owl library"
  :versioninfo "Unreleased Version"
  :seealso "Manchester Version"
  )

(defaproperty myOpinion
  :subproperty owl-comment-property
  :label "My Opinion"
  :comment "Do I think this is a good pizza to eat?"
  )

;; these classes are all siblings and should all be disjoint
;; using the as-disjoint macro makes things a little easier. 
(as-disjoint
 ;; we use :label here as it is easier and more straight forward
 
 (defclass Pizza
   :label "Pizza")


 (defclass PizzaTopping)

 ;; currently we have to use the annotation function with label to pass a
 ;; language in.
 (defclass PizzaBase
   ;; the pizza ontology contains some Portuguese labels. The :label keyword
   ;; used above is a shortcut for English
   :annotation (label "BaseDaPizza" "pt")))

;; now that we have our first classes we can specify our properties
(as-inverse
 (defoproperty hasIngredient
   :characteristic :transitive)
 (defoproperty isIngredientOf
   :characteristic :transitive
   ))

(defoproperty hasTopping
  :subproperty hasIngredient
  :range PizzaTopping
  :domain Pizza
  )

(defoproperty hasBase
  :subproperty hasIngredient
  :characteristic :functional
  :range PizzaBase
  :domain Pizza
  )


(defdproperty hasCalorificContentValue
  :range :XSD_INTEGER)

(defn cal [number]
  (has-value hasCalorificContentValue number))

(owl-class Pizza
          :subclass
          (owl-some hasCalorificContentValue :XSD_INTEGER)
          (owl-some hasTopping PizzaTopping)
          (owl-some hasBase PizzaBase))


;; define a set of subclasses which are all mutually disjoint
(as-disjoint-subclasses
 PizzaBase
 
 (defclass ThinAndCrispyBase
   :subclass (cal 150)
   :annotation (label "BaseFinaEQuebradica" "pt"))

 (defclass DeepPanBase
   :subclass (cal 250)
   :annotation (label  "BaseEspessa" "pt")))

(p/value-partition
 Spiciness
 [Mild
  Medium
  Hot]
 )


(as-disjoint-subclasses
 PizzaTopping

 ;; This section used to reflect the natural hierarchy within the lisp, by
 ;; embedding multiple 'as-disjoint-subclasses' forms. In the end, I have
 ;; unwound this for two reasons. First, the implementation used dynamic
 ;; binding and scoping in a way that I was not entirely happy with; second,
 ;; the deep embedding means that small.
 (defclass CheeseTopping)
 (defclass FishTopping)
 (defclass FruitTopping)
 (defclass HerbSpiceTopping)
 (defclass MeatTopping)
 (defclass NutTopping)
 (defclass SauceTopping)
 (defclass VegetableTopping))

(as-disjoint-subclasses
 CheeseTopping

 (declare-classes
  GoatsCheeseTopping
  GorgonzolaTopping
  MozzarellaTopping
  ParmesanTopping))

(as-disjoint-subclasses
 FishTopping

 (declare-classes AnchoviesTopping
                  MixedSeafoodTopping
                  PrawnsTopping))

(as-disjoint-subclasses
 FruitTopping
 (declare-classes PineappleTopping
                  SultanaTopping))

(as-disjoint-subclasses
 HerbSpiceTopping

 (declare-classes CajunSpiceTopping
                  RosemaryTopping))

(as-disjoint-subclasses
 MeatTopping

 (declare-classes ChickenTopping
                  HamTopping
                  HotSpicedBeefTopping
                  PeperoniSausageTopping))


(as-subclasses
 NutTopping
 (defclass PineKernels))

(as-subclasses
 SauceTopping
 (defclass TobascoPepperSauce))

(as-disjoint-subclasses
 VegetableTopping

 (declare-classes PepperTopping
                  GarlicTopping
                  PetitPoisTopping
                  AsparagusTopping
                  CaperTopping
                  SpinachTopping
                  ArtichokeTopping
                  OnionTopping
                  OliveTopping
                  MushroomTopping
                  RocketTopping
                  TomatoTopping
                  LeekTopping))

(as-disjoint-subclasses
 PepperTopping
 (declare-classes PeperonataTopping
                  JalapenoPepperTopping
                  SweetPepperTopping
                  GreenPepperTopping))

;; equivalent classes -- these are the main categories which will be reasoned under. 
(defclass CheesyPizza
  :equivalent
  (owl-and Pizza
          (owl-some hasTopping CheeseTopping)))

(defclass InterestingPizza
  :equivalent
  (owl-and Pizza
          (at-least 3 hasTopping PizzaTopping)))

(defclass FourCheesePizza
  :equivalent
  (owl-and Pizza
          (exactly 4 hasTopping CheeseTopping)))

(defclass VegetarianPizza
  :annotation 
  (annotation myOpinion "Always a good start.")
  :equivalent
  (owl-and Pizza
          (owl-not 
           (owl-some hasTopping MeatTopping))
          (owl-not 
           (owl-some hasTopping FishTopping))))

(defclass NonVegetarianPizza
  :annotation 
  (annotation myOpinion "Not a good start.")
  :equivalent
  (owl-and Pizza (owl-not VegetarianPizza)))

;; different, but equivalent, definition
(defclass VegetarianPizza2
  :equivalent 
  (owl-and Pizza
          (only hasTopping 
                (owl-not (owl-or MeatTopping FishTopping)))))


(defclass HighCaloriePizza
  :equivalent
  (owl-some hasCalorificContentValue
           (span >= 700)))

(defclass MediumCaloriePizza
  :equivalent
  (owl-some hasCalorificContentValue
           (span >=< 400 700)))

(defclass LowCaloriePizza
  :equivalent
  (owl-some hasCalorificContentValue
           (span <= 400)))

;; named pizzas
(defclass NamedPizza
  :subclass Pizza)

(print "printing" (subclasses Pizza))
(save-ontology "pizza.omn" :omn)

(defn testgm2[]
  (println "calling the method")
  ;(print "printing" (subclasses Pizza))
;(foo "dsfdfg")
)
