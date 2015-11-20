(ns sybiont_generator.reasoner
      (:use [tawny.owl])
      (:use [sybiont_generator.ontology])  
      (:use [sybiont_generator.param])  
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))
 
(import '(java.io FileInputStream File))    
(import '(org.semanticweb.owlapi.model OWLOntology PrefixManager SetOntologyID OWLOntologyID))
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
	   (if (superclass? ontology (.getIRI owlClass) (iri (str ONTOLOGY_URI "#" superClass)))    
	     (def foundSuperClass true))
	     (if (= (.getIRI owlClass) (iri (str ONTOLOGY_URI "#" superClass)) )
	            (def foundSuperClass true)))
   
   (if-not (= foundSuperClass true)
     (try
        (remove-entity ontology owlClass)
        (catch Exception e (str "caught exception: " (.getMessage e) "Class IRI:"(.getIRI owlClass)))))))

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
    (println (.getIRI subclass))))

(defn printReasoningSummary [label inferredClasses]
  (println label ":" (count inferredClasses) " classes inferred"))

(defn mergeOntologies [file1 file2 namespace prefix] 
  (def ontology1 (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. file1)))
 (def ontology2 (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. file2)))
 (def merger (OWLOntologyMerger. (owl-ontology-manager)))
 (def merged (.createMergedOntology merger (owl-ontology-manager) (iri namespace) )) 
 (remove-ontology-maybe  (.getOntologyID ontology1))
 (remove-ontology-maybe  (.getOntologyID ontology2)) 
 (set-prefix merged prefix))

(defn inferOperatorsHermit[]
 (println "Inferring using HermiT")
 (loadOntology "Operators_Reasoning.omn")
 (r/reasoner-factory :hermit) 
 (printReasoningSummary "NegativelyRegulatedOperator" (r/isubclasses localOntology (iri (str ONTOLOGY_URI "#NegativelyRegulatedOperator"))))  
 )

(defn inferOperatorsFact[]
  (println "Inferring using JFact")
  (loadOntology "Operators_Reasoning.omn") 
 (r/reasoner-factory :jfact)
 ;(r/reasoner-factory :elk) 
 ;(r/reasoner-factory :jfact) 
 (printReasoningSummary "NegativelyRegulatedOperator" (r/isubclasses localOntology (iri (str ONTOLOGY_URI "#NegativelyRegulatedOperator"))))   
  )

(defn inferOperators[]
  (loadOntology "Operators_Reasoning.omn") 
 ;(r/reasoner-factory :hermit)
 ;(r/reasoner-factory :elk) 
 (r/reasoner-factory :jfact) 
 (printReasoningSummary "NegativelyRegulatedOperator" (r/isubclasses localOntology (iri (str ONTOLOGY_URI "#NegativelyRegulatedOperator"))))  
 (printReasoningSummary "PositivelyRegulatedOperator" (r/isubclasses localOntology (iri (str ONTOLOGY_URI "#PositivelyRegulatedOperator")))) 
 (printReasoning "NegativelyRegulatedOperator" (r/isubclasses localOntology (iri (str ONTOLOGY_URI "#NegativelyRegulatedOperator"))))  
 (printReasoning "PositivelyRegulatedOperator" (r/isubclasses localOntology (iri (str ONTOLOGY_URI "#PositivelyRegulatedOperator")))))


(defn inferPromotersByRegulationType[]
 (loadOntology "PromotersByRegulationTypes_Reasoning.omn")
 (r/reasoner-factory :hermit)  
 (printReasoningSummary "InduciblePromoter" (r/isubclasses localOntology (iri (str ONTOLOGY_URI "#InduciblePromoter")))))

;To generate subsets of the ontology which can be submitted to reasoners directly
(defn subsetForClasses[classesToKeep nssuffix targetFile message]
  (print message)
  (loadOntology SYBIONTKB)
  (removeClassesExcept localOntology classesToKeep)
  
  (save-ontology localOntology (str "temp" targetFile) :omn)  
  (remove-ontology-maybe   (.getOntologyID localOntology))
  
  (mergeOntologies (str "temp" targetFile) SYBIONT (str "http://www.bacillondex.org_" nssuffix) "bomerged")  
  (save-ontology merged targetFile :omn) 
  (remove-ontology-maybe   (.getOntologyID merged)) 
  
  (clojure.java.io/delete-file (str "temp" targetFile))
  (println "done!")
  )


(defn createOntologySubsets[]
  ;Hermit:1.3.8.3, FaCT:1.6.4   
  ;Hermit:979 ms, FaCT++:192 ms
  (remove-ontology-maybe  (new OWLOntologyID (iri (str ONTOLOGY_DATA_URI))))
  (remove-ontology-maybe  (new OWLOntologyID (iri (str ONTOLOGY_URI))))
  
  (subsetForClasses ["Operator"] "operators" "Operators_Reasoning.omn" "Creating the subset of the ontology to classify operators only...")
  
   ;Hermit:120 second, Fact++:1 second
  (subsetForClasses ["Operator" "Promoter"] "promotersbyrt" "PromotersByRegulationTypes_Reasoning.omn" "Creating the subset of the ontology to classify promoters by their regulation type only...")
  
  ;Hermit:? seconds, Fact++:108 seconds
  (subsetForClasses ["Promoter" "TF" "Protein"] "promotersbysf" "PromotersBySigmaFactors_Reasoning.omn" "Creating the subset of the ontology to classify promoters by sigma factors only...")
   
  ;Hermit:? seconds, Fact++:394 seconds
  (subsetForClasses ["Promoter" "TF" "Protein" "Operator"] "promoters" "Promoters_Reasoning.omn" "Creating the subset of the ontology to classify all promoters...")
  
  ;Hermit:? seconds, Fact++:438 seconds
  (subsetForClasses ["Protein" "CDS" "MolecularFunction"] "cdssbymf" "CDSsByMolecularFunction_Reasoning.omn" "Creating the subset of the ontology to classify CDSs by the molecular functions of their encoded products...")
  
  ;Hermit:? seconds, Fact++:1387 seconds
  (subsetForClasses ["Protein" "CDS" "TF" "Operator"] "cdssbyta" "CDSsByTrancriptionalActivity_Reasoning.omn" "Creating the subset of the ontology to classify CDSs by the transcriptional activity of their encoded products...")
    )

