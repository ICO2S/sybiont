(ns synthbiont.ontology
    (:use [tawny.owl])
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

(defn addComment [ontology class value]
  (owl-class ontology class :comment value))


(defn addLabel [ontology class value]
  (owl-class ontology class :label value))

(defn addAnnotationProperty [ontology class predicateOntology predicate value]
  (annotation-property predicateOntology predicate)
  (owl-class ontology class :annotation (annotation predicateOntology predicate value)))

(defn addCommentForProperty [ontology predicate value propertyType]
  (if (= propertyType "object")
    (object-property ontology predicate :comment value))
  
   (if (= propertyType "datatype")
    (datatype-property ontology predicate :comment value))
   
   (if (= propertyType "annotation")  
      (annotation-property ontology predicate :comment value))
   )

(defn addLabelForProperty [ontology predicate value propertyType]
  (if (= propertyType "object")
    (object-property ontology predicate :label value))
  
   (if (= propertyType "datatype")
    (datatype-property ontology predicate :label value))
   
   (if (= propertyType "annotation")  
      (annotation-property ontology predicate :label value))   
    )

(defn addAnnotationPropertyForProperty [ontology predicate predicateAnnotateWith value propertyType]
  (println "annotating property with:"  predicateAnnotateWith " value:" value)
  (annotation-property ontology predicateAnnotateWith)
  (if (= propertyType "object")
    (object-property ontology predicate :annotation (annotation ontology predicateAnnotateWith value)))
  
  (if (= propertyType "datatype")  
    (datatype-property ontology predicate :annotation (annotation ontology predicateAnnotateWith value)))
  
  (if (= propertyType "annotation")  
    (annotation-property ontology predicate :annotation (annotation ontology predicateAnnotateWith value)))
  
    )

(defn addDatatypeProperty [ontology class predicateOntology predicate value]
  (datatype-property predicateOntology predicate)
  (owl-class ontology class :subclass (has-value predicateOntology predicate value)))

(defn addObjectProperty [ontology class valueOntology valueClass predicateOntology predicate]
  (object-property predicateOntology predicate)
  (owl-class ontology class :subclass (owl-some predicateOntology predicate (owl-class valueOntology valueClass)))
)