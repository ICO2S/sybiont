(ns synthbiont.reasoner
      (:use [tawny.owl])
        (:use [synthbiont.ontology])  
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

(import '(java.io FileInputStream File))    
(import '(org.semanticweb.owlapi.model OWLOntology PrefixManager SetOntologyID ))
(import '(org.semanticweb.owlapi.apibinding OWLManager ))
(import '(org.semanticweb.owlapi.util OWLOntologyMerger DefaultPrefixManager))
(import '(org.semanticweb.owlapi.io RDFXMLOntologyFormat ))




(def localOntology)
(defn loadOntology [filePath]
  (def localOntology (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. filePath))))

(defn removeClassesExcept [ontology classList]
 (doseq [owlClass (.getClassesInSignature ontology)] 
   (def  foundSuperClass false)
   (doseq [superClass classList] 
	   (if (superclass? ontology (.getIRI owlClass) (iri (str "http://www.sybio.ncl.ac.uk#" superClass)))    
	     (def foundSuperClass true))
	     (if (= (.getIRI owlClass) (iri (str "http://www.sybio.ncl.ac.uk#" superClass)) )
	            (def foundSuperClass true))     
    )
   
   (if-not (= foundSuperClass true)
     (try
       (remove-entity ontology owlClass)
        (catch Exception e (str "caught exception: " (.getMessage e) "Class IRI:"(.getIRI owlClass))))
     )   
   )
  )


(defn set-prefix
  "Sets a prefix for the ontology."
  [^OWLOntology o ^String p]
  (if p
    (.setPrefix
     (.asPrefixOWLOntologyFormat
      (.getOntologyFormat
       (owl-ontology-manager) o))
     p (str (getOntologyIri o)
            ))))

(defn set-namespaceprefix
  "Sets a prefix for the ontology."
  [^OWLOntology o ^String namespaceuri ^String p]
  (if p
    (.setPrefix
     (.asPrefixOWLOntologyFormat
      (.getOntologyFormat
       (owl-ontology-manager) o))
     p namespaceuri)))

(defn printReasoning [label inferredClasses]
  (println label " classes:")
  (doseq [subclass inferredClasses]
    (println (.getIRI subclass))
    )
  )

(defn printReasoningSummary [label inferredClasses]
  (println label ":" (count inferredClasses) " classes inferred")
  )

(defn mergeOntologies [file1 file2 namespace prefix] 
  (def ontology1 (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. "Operators.omn")))
 (def ontology2 (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. "synthbiont.omn")))
 (def merger (OWLOntologyMerger. (owl-ontology-manager)))
 (def merged (.createMergedOntology merger (owl-ontology-manager) (iri namespace) )) 
 (remove-ontology-maybe  (.getOntologyID ontology1))
 (remove-ontology-maybe  (.getOntologyID ontology2)) 
 (set-prefix merged prefix)
  )

(defn inferOperators[]
 (mergeOntologies "Operators.omn" "synthbiont.omn" "http://www.bacillondex.org_operators" "bom")  
 (r/reasoner-factory :hermit)  
 (printReasoningSummary "NegativelyRegulatedOperator" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#NegativelyRegulatedOperator"))))  
 (printReasoningSummary "PositivelyRegulatedOperator" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#PositivelyRegulatedOperator")))) 
 (printReasoning "NegativelyRegulatedOperator" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#NegativelyRegulatedOperator"))))  
 (printReasoning "PositivelyRegulatedOperator" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#PositivelyRegulatedOperator")))) 
 
  )


(defn inferPromotersByRegulationType[]
 (mergeOntologies "PromotersByRegulationTypesOnly.omn" "synthbiont.omn" "http://www.bacillondex.org_promotersbyregulationtype" "bom")  
 (r/reasoner-factory :hermit)  
 (printReasoningSummary "InduciblePromoter" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#InduciblePromoter"))))  
 
  )

(defn inferPromoters[]
 (mergeOntologies "Promoters.omn" "synthbiont.omn" "http://www.bacillondex.org_promoters" "bom")  
 (r/reasoner-factory :elk)  
 (printReasoningSummary "NegativelyRegulatedOperator" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#NegativelyRegulatedOperator"))))  
 (printReasoningSummary "PositivelyRegulatedOperator" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#PositivelyRegulatedOperator")))) 
 (printReasoningSummary "InduciblePromoter" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#InduciblePromoter"))))  
 (printReasoningSummary "SigAPromoter" (r/isubclasses merged (iri (str "http://www.sybio.ncl.ac.uk#SigAPromoter")))) 
 
 
  )


(defn executeReasoning[]

  )




(defn subsetForOperators[]
  (print "Creating the subset of the ontology to classify operators only...")
  (loadOntology "bacillondexontology.omn")
  (removeClassesExcept localOntology ["Operator"])
  ;(SetOntologyID. o (iri (str (getOntologyIri ontology) "_operators"))  )
  (save-ontology localOntology "Operators.omn" :omn)  
  (remove-ontology-maybe   (.getOntologyID localOntology))
  (println "done!")
  )

; To classify promoters based on their regulatory relationships with TFs
(defn subsetForPromotersByRegulationTypes[]
  (print "Creating the subset of the ontology to classify promoters by their regulation type only...")  
  (loadOntology "bacillondexontology.omn")
  (removeClassesExcept  localOntology ["Operator" "Promoter"])  
  (save-ontology localOntology "PromotersByRegulationTypesOnly.omn" :omn)
  (remove-ontology-maybe   (.getOntologyID localOntology))
  (println "done!")  
  )  

; To classify promoters based sigma factors
(defn subsetForPromotersBySigmaFactors[]
  (print "Creating the subset of the ontology to classify promoters by sigma factors only...")  
  (loadOntology "bacillondexontology.omn")
  (removeClassesExcept localOntology ["Promoter" "TF" "Protein"])  
  (save-ontology localOntology "PromotersBySigmaFactorsOnly.omn" :omn)
    (remove-ontology-maybe   (.getOntologyID localOntology))
  (println "done!")
  )

;To classify all promoters
;Takes around 43 seconds,
(defn subsetForPromoters[]
  (print "Creating the subset of the ontology to classify all promoters...")  
  (loadOntology "bacillondexontology.omn")
  (removeClassesExcept localOntology ["Promoter" "TF" "Protein" "Operator"])
  (save-ontology localOntology "Promoters.omn" :omn)
  (remove-ontology-maybe   (.getOntologyID localOntology))
  (println "done!")
  )

;Creates a subset of the ontology to infer CDSs by molecular functions of the encoded products.
; Reasoning takes 330 seconds,
(defn subsetForCDSsByMolecularFunction[]
  (print "Creating the subset of the ontology to classify CDSs by the molecular functions of their encoded products...")  
  (loadOntology "bacillondexontology.omn")
  (removeClassesExcept localOntology ["Protein" "CDS" "MolFunc"])  
  (save-ontology localOntology "CDSsByMolecularFunctionOnly.omn" :omn)
  (remove-ontology-maybe   (.getOntologyID localOntology))  
  (println "done!")
  )

;Creates a subset of the ontology to infer CDSs by transcriptional activity.
;1390 seconds
(defn subsetForCDSsByTrancriptionalActivity[]
  (print "Creating the subset of the ontology to classify CDSs by the transcriptional activity of their encoded products...")  
  (loadOntology "bacillondexontology.omn")
  (removeClassesExcept localOntology ["Protein" "CDS" "TF" "Operator"])  
  (save-ontology localOntology "CDSsByTrancriptionalActivityOnly.omn" :omn)
  (remove-ontology-maybe   (.getOntologyID localOntology))  
  (println "done!")
  )

(defn createOntologySubsets[]
  (subsetForOperators)  
  (subsetForPromotersByRegulationTypes)
  (subsetForPromotersBySigmaFactors)
  (subsetForPromoters) 
  (subsetForCDSsByMolecularFunction)
  (subsetForCDSsByTrancriptionalActivity)
   )
  
  