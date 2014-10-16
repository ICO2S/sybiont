(ns synthbiont.reasoner
      (:use [tawny.owl])
        (:use [synthbiont.ontology])  
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

(import '(java.io FileInputStream File))    
(import '(org.semanticweb.owlapi.model OWLOntology ))
(import '(org.semanticweb.owlapi.apibinding OWLManager ))


(def localOntology)
(defn loadOntology [filePath]
  ;(def localOntology (.loadOntologyFromOntologyDocument (OWLManager/createOWLOntologyManager) (File. filePath)))
  (def localOntology (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. filePath)))
  
  ;(print (.getOntologyID localOntology))      
  )

(defn removeClassesExcept [classList]
 (doseq [owlClass (.getClassesInSignature localOntology)] 
   (doseq [superClass classList] 
   (if-not (superclass? localOntology (.getIRI owlClass) (iri (str "http://www.sybio.ncl.ac.uk#" superClass)))       
   ;(if (superclass? localOntology (.getIRI owlClass) (getClassIri localOntology "Promoter"))       
     ;(println superClass ":" owlClass)
     (if-not (= (.getIRI owlClass) (iri (str "http://www.sybio.ncl.ac.uk#" superClass)) )
     (try
       (remove-entity localOntology owlClass)
        (catch Exception e (str "caught exception: " (.getMessage e) "Class IRI:"(.getIRI owlClass))))
     )
     )
    )
   )
  )

(defn operatorReasoning[]
  (loadOntology "Operators.omn")
  (owl-import localOntology (iri(File."synthbiont.omn")))
  ;(save-ontology localOntology "OperatorsDel.omn" :omn)
  (r/reasoner-factory :elk)
  (subclasses localOntology (iri (str "http://www.sybio.ncl.ac.uk#NegativelyRegulatedOperator")))
  )

(defn executeReasoning[]

  )




(defn prepareFiles[]
  (loadOntology "bacillondexontology.omn")
  ;(loadOntology "synthbiont.omn")
  (removeClassesExcept  ["Operator"])
  (println "Removed the ontology classes")
  
  ;(owl-import localOntology (iri(File."synthbiont.omn")))
  (save-ontology localOntology "Operators.omn" :omn)
  (save-ontology localOntology "Operators.rdf" :rdf)
  
  
  )

