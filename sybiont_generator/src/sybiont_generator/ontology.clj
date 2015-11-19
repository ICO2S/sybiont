(ns sybiont_generator.ontology
    (:use [tawny.owl])
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

;(defn addComment [owlClass value]
;  (owl-class (.getIRI owlClass) :comment value))


(defn addComment [ontology owlClass value]
  (owl-class ontology owlClass :comment value))

;(defn addLabel [owlClass value]
  ;(owlClass :label value))

(defn addLabel [ontology owlClass value]
  (owl-class ontology owlClass :label value))

(defn addAnnotationProperty [owlClass predicateOntology predicate value]
  (annotation-property predicateOntology predicate)
  (owl-class (.getIRI owlClass) :annotation (annotation predicateOntology predicate value)))

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
    (annotation-property ontology predicate :annotation (annotation ontology predicateAnnotateWith value))))

(defn addDatatypeProperty [owlClass predicateOntology predicate value]
  (datatype-property predicateOntology predicate)
  (owl-class (.getIRI owlClass) :subclass (has-value predicateOntology predicate value)))

(defn addObjectProperty [owlClass valueClass predicateOntology predicate]
  (object-property predicateOntology predicate)
  (owl-class (.getIRI owlClass) :subclass (owl-some predicateOntology predicate (owl-class (.getIRI valueClass))))
)

(defn addObjectProperty [owlClass valueClass predicateOntology predicate]
  (object-property predicateOntology predicate)
  (owl-class (.getIRI owlClass) :subclass (owl-some predicateOntology predicate (owl-class (.getIRI valueClass))))
)

(defn getClassIri [ontology className]
  (iri (str (.toString (.getOntologyIRI (.getOntologyID ontology))) "#" className))
  )

(defn getOntologyIri [ontology] 
  (.getOntologyIRI(.getOntologyID ontology))
  )