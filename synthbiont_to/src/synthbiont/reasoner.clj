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
(import '(org.semanticweb.owlapi.util OWLOntologyMerger ))
(import '(org.semanticweb.owlapi.io RDFXMLOntologyFormat ))




(def localOntology)
(defn loadOntology [filePath]
  ;(def localOntology (.loadOntologyFromOntologyDocument (OWLManager/createOWLOntologyManager) (File. filePath)))
  (def localOntology (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. filePath)))
  
  ;(print (.getOntologyID localOntology))      
  )

(defn removeClassesExcept [classList]
 (doseq [owlClass (.getClassesInSignature localOntology)] 
   (def  foundSuperClass false)
   (doseq [superClass classList] 
	   (if (superclass? localOntology (.getIRI owlClass) (iri (str "http://www.sybio.ncl.ac.uk#" superClass)))    
	     (def foundSuperClass true))
	   ;(if (superclass? localOntology (.getIRI owlClass) (getClassIri localOntology "Promoter"))       
	     ;(println superClass ":" owlClass)
	     (if (= (.getIRI owlClass) (iri (str "http://www.sybio.ncl.ac.uk#" superClass)) )
	            (def foundSuperClass true))     
    )
   
   (if-not (= foundSuperClass true)
     (try
       (remove-entity localOntology owlClass)
        (catch Exception e (str "caught exception: " (.getMessage e) "Class IRI:"(.getIRI owlClass))))
     )
   
   )
  )

(defn operatorReasoning2[]
 (def bacillondex (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. "Operators.omn")))
 (def synthbiont (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. "synthbiont.omn")))
 (def merger (OWLOntologyMerger. (owl-ontology-manager)))
 
 (def merged (.createMergedOntology merger (owl-ontology-manager) (iri (str "http://www.bacillondex.orgGM")) )) 
 (save-ontology merged "OperatorsMerged.omn" :omn)
 
  )

(defn operatorReasoning[]
  (loadOntology "Operators.omn")
  (owl-import localOntology (iri(File."synthbiont.omn")))

  ;(save-ontology localOntology "OperatorsDel.omn" :omn)
  
  (r/reasoner-factory :hermit)
  
  (println "inferred sub classes")
  (r/isubclasses localOntology (iri (str "http://www.sybio.ncl.ac.uk#NegativelyRegulatedOperator")))
  
  (println "inferred super classes")
  (r/isuperclasses localOntology (iri (str "http://www.sybio.ncl.ac.uk#NegativelyRegulatedOperator")))

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

; To classify promoters based on their regulatory relationships with TFs
(defn preparePromoters[]
  (loadOntology "bacillondexontology.omn")
  ;(loadOntology "synthbiont.omn")
  (removeClassesExcept  ["Operator" "Promoter"])
  (println "Removed the ontology classes")
  
  ;(owl-import localOntology (iri(File."synthbiont.omn")))
  (save-ontology localOntology "Promoters.omn" :omn)
  ;(save-ontology localOntology "Promoters.rdf" :rdf)    
  )

; To classify promoters based sigma factors
(defn preparePromoters2[]
  (loadOntology "bacillondexontology.omn")
  ;(loadOntology "synthbiont.omn")
  (removeClassesExcept  ["Promoter" "TF" "Protein"])
  (println "Removed the ontology classes")
  
  ;(owl-import localOntology (iri(File."synthbiont.omn")))
  (save-ontology localOntology "Promoters2.omn" :omn)
  ;(save-ontology localOntology "Promoters.rdf" :rdf)    
  )


