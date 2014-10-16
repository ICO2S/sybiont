(ns synthbiont.reasoner
      (:use [tawny.owl])
        (:use [synthbiont.ontology])  
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

(import '(java.io FileInputStream File))    
;(import '(org.semanticweb.owlapi.model OWLManager ))
(import '(org.semanticweb.owlapi.apibinding OWLManager ))


(def localOntology)
(defn loadOntology [filePath]
  (def localOntology (.loadOntologyFromOntologyDocument (OWLManager/createOWLOntologyManager) (File. filePath)))
  (print (.getOntologyID localOntology))      
  )

(defn removeClassesExcept [classList]
 (doseq [owlClass (.getClassesInSignature localOntology)] 
   (doseq [superClass classList] 
   (if-not (superclass? localOntology (.getIRI owlClass) (iri (str "http://www.sybio.ncl.ac.uk#" superClass)))       
   ;(if (superclass? localOntology (.getIRI owlClass) (getClassIri localOntology "Promoter"))       
     (println superClass ":" owlClass)
     (remove-entity localOntology owlClass)    
     )
    )
   )
  )

(defn executeReasoning[]

  )

(defn prepareFiles[]
  (loadOntology "bacillondexontology.rdf")
  ;(loadOntology "synthbiont.omn")
  (removeClassesExcept  ["Operator"])
  (save-ontology localOntology "Operators.omn" :omn)
  (save-ontology localOntology "Operators.rdf" :rdf)
  
  
  )

