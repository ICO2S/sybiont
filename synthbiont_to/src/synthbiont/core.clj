(ns synthbiont.core
  (:use [tawny.owl])
  (:use [synthbiont.ontology])  
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))


(import '(java.io FileInputStream File))       
(import '(com.hp.hpl.jena.rdf.model Model ModelFactory ResourceFactory))
(import '(com.hp.hpl.jena.vocabulary RDF RDFS))
 ;(:import (org.semanticweb.owlapi.model IRI OWLNamedObject OWLOntologyID)
 ;          (org.semanticweb.owlapi.util SimpleIRIMapper))
 

 (def ^:dynamic model) 
 
; ontology URIs
(def ONTOLOGY_URI "http://www.sybio.ncl.ac.uk")
(def ONTOLOGY_DATA_URI "http://www.bacillondex.org")
(def GO_URI "http://purl.org/obo/owl/GO")


;Namespaces
(def conceptNS (str ONTOLOGY_DATA_URI  "/concept/"))
(def relationTypeNS (str ONTOLOGY_DATA_URI  "/relationType/"))
(def attributeNS (str ONTOLOGY_DATA_URI  "/attributeName/"))
(def evidenceTypeNS (str ONTOLOGY_DATA_URI  "/evidenceType/"))
(def cvNS (str ONTOLOGY_DATA_URI  "/cv/"))
(def ondexcoreNS "http://ondex.sourceforge.net/ondex-core#")

;RDF Data Property definitions
(def RDFS_COMMENT_PROPERTIES (hash-set "description" "annotation" "comment" "conceptDescription"))
(def RDFS_LABEL_PROPERTIES (hash-set "title" "conceptName"))
(def ANNOTATION_PROPERTIES (hash-set "pid" "evidence" "elementOf" "TAXID" "annotatedsequence" "originalannotatedsequence" "AA" "URL" "operongenes" "BEGIN" "END" "ReadingFrame" "BM" "Control" "Target" "DELTAGO" "EndOfBindingSite" "Length" "StartOfBindingsite" "UnnormalizedMaxExpression" "UnnormalizedMinExpression" "COGProcess" "title" "conceptName" "description" "annotation" "comment" "conceptDescription"))
(def PROPERTIES_TO_IGNORE (hash-set "identifier"))
(def GO_CLASSES (hash-set "MolFunc" "BioProc" "CelComp"))
(def SUBCLASS_RELATION (hash-set "is_a"))
(def SAMECLASS_RELATION (hash-set "equ"))

;New Class definitions
(def EVIDENCE_TYPE_PARENT_CLASS "EvidenceType")
(def DATA_SOURCE_PARENT_CLASS "DataSource")
    
;TODO (def INVERSE_PROPERTIES {:"part_of" "has_part", :"en_by" "encodes", :"equ" "equ",:"bi_to" "bound_by"})

;(def ^:dynamic objectPropertyUris {})
;(def ^:dynamic datatypePropertyUris '())
;(def datatypePropertyUris (hash-set "equ"))       
( def  datatypePropertyUris (set [])) 
( def  objectPropertyUris (set []))   

(defontology synthbiont
	 :iri "http://www.sybio.ncl.ac.uk"
	 :prefix "sybio:"
	 :comment "An ontology for synthetic biology"
	 :versioninfo "1.0"
  )  
 
 	(defontology bacillondex
	  :iri "http://www.bacillondex.org"
	  :prefix "bo:"
	  :comment "An ontology for Bacillus subtilis parts"
	  :versioninfo "1.0"    
   )  
  
 ;*******GENERIC ONTOLOGY METHODS**************************
;BEGIN


(defn addLabel [ontology class value]
  (owl-class ontology class :label value))

(defn addAnnotationProperty [ontology class predicate value]
  (annotation-property synthbiont predicate)
  (owl-class ontology class :annotation (annotation synthbiont predicate value)))

(defn addCommentForProperty [ontology predicate value isObjectProperty]
  (if (= isObjectProperty true)
    (object-property ontology predicate :comment value)
    (datatype-property ontology predicate :comment value)    
    ))

(defn addLabelForProperty [ontology predicate value isObjectProperty]
  (if (= isObjectProperty true)
    (object-property ontology predicate :label value)
    (datatype-property ontology predicate :label value)    
    ))

(defn addAnnotationPropertyForProperty [ontology predicate predicateAnnotateWith value isObjectProperty]
  (println "annotating property with:"  predicateAnnotateWith " value:" value)
  (annotation-property synthbiont predicateAnnotateWith)
  (if (= isObjectProperty true)
    (object-property synthbiont predicate :annotation (annotation synthbiont predicateAnnotateWith value))
    (datatype-property synthbiont predicate :annotation (annotation synthbiont predicateAnnotateWith value))
    ))

(defn addDatatypeProperty [ontology class predicate value]
  (datatype-property synthbiont predicate)
  (owl-class ontology class :subclass (has-value synthbiont predicate value)))

(defn addObjectProperty [ontology class valueOntology valueClass predicate]
  (object-property synthbiont predicate)
  (owl-class ontology class :subclass (owl-some synthbiont predicate (owl-class valueOntology valueClass)))
)
; END ****************************************************

(defn getRDFModel []
  (let [rdfFilePath "/home/goksel/work/sbolstack/DataFiles/BacillOndexPlus.rdf"
       RDF_BASE_URI "http://www.bacillondex.org"
       stream (FileInputStream. (File. rdfFilePath))
       model (ModelFactory/createDefaultModel)]               
  (.setNsPrefix model "" RDF_BASE_URI)  
  (.read model stream (RDFS/getURI))
  ))


(defn getID [resourceURI]
  (let [index (.lastIndexOf resourceURI "/")]
   (.substring resourceURI (+ index 1))))

(defn addAnnotation [ontology class predicate value]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicate)
   (addComment ontology class value)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicate)
		   (addLabel ontology class value)
       (addAnnotationProperty ontology class predicate value))       
     )   
 ))

(defn addLiteralPropertyToClass [ontology stmt]
  (let [predicateName (str (.getLocalName(.getPredicate stmt)))
        objectValue (str (.toString (.getValue (.asLiteral (.getObject stmt)))))
         class (getID (.getURI (.getSubject stmt)))]
  (if-not (contains? PROPERTIES_TO_IGNORE  predicateName)
    (if (contains? ANNOTATION_PROPERTIES  predicateName)
      (addAnnotation ontology class predicateName objectValue) 
      ( do        
        (def datatypePropertyUris (conj datatypePropertyUris  (.getURI (.getPredicate stmt))))
        (addDatatypeProperty ontology class predicateName objectValue) 
      )
    )
  )
  ))

(defn handleMetaResource [ontology resource parentOntology parentClass]
  (let [typeId (getID (.getURI resource))]  
   (owl-class ontology typeId)  
  (if not (nil? parentClass)
    (owl-class ontology typeId :subclass (owl-class parentOntology parentClass)))
  ;TODO Change it to let  
  (def typeResourceProperties (iterator-seq (.listProperties resource))) 
  (doseq [stmt typeResourceProperties] 
     (if (.isLiteral (.getObject stmt)) 
       (addLiteralPropertyToClass synthbiont stmt))
  )  
  ))

(defn getGoTerm [resource]
  (let [
        pidProperty (.getProperty model (str ondexcoreNS "pid"))       
        stmt (.getProperty resource pidProperty)
        goTerm (.toString (.getValue (.asLiteral (.getObject stmt))))]       
    (.replace goTerm ":" "_")
    ))

(defn addOntologyClass [classResource  parentClass]
  (if (contains? GO_CLASSES parentClass)
    (owl-class (iri (str GO_URI "#" (getGoTerm classResource))) :subclass (owl-class synthbiont parentClass))
    (owl-class bacillondex (getID (.getURI classResource)) :subclass (owl-class synthbiont parentClass))     
  ))

(defn handleRelationStatement [stmt]
  (let [predicate (.getLocalName (.getPredicate stmt))
        valueResource (.getResource model (.getURI (.asResource (.getObject stmt))))
        class (getID (.getURI (.getSubject stmt)))
        valueResourceId (getID (.getURI valueResource))]
        ;TODO:  Not adding the class for the related object here
        ;TODO: Not adding the related class' super class        
        (if (contains? SUBCLASS_RELATION predicate)
          (owl-class bacillondex class :subclass (owl-class bacillondex valueResourceId )))
        
        (if (contains? SAMECLASS_RELATION predicate) 
          (owl-class bacillondex class :equivalent (owl-class bacillondex valueResourceId )))
      
        (if-not (or (contains? SUBCLASS_RELATION predicate) (contains? SAMECLASS_RELATION predicate)) 
          (do 
            (def objectPropertyUris (conj objectPropertyUris  (.getURI (.getPredicate stmt))))
            (addObjectProperty bacillondex class bacillondex valueResourceId predicate)
          )
           ; TODO: Add the properties for this property
           ; TODO: Add the inverse properties in the beginning or after        
          )                          
        ))

(defn handleOndexCoreResource [stmt]
  (let [resource (.asResource (.getObject stmt))
        resourceUri (.getURI resource)
        predicate (.getLocalName (.getPredicate stmt))
        class (getID (.getURI (.getSubject stmt)))]
    (if (.startsWith resourceUri evidenceTypeNS)
      (handleMetaResource bacillondex resource synthbiont EVIDENCE_TYPE_PARENT_CLASS))
    (if (.startsWith resourceUri cvNS)
        (handleMetaResource bacillondex resource synthbiont DATA_SOURCE_PARENT_CLASS))    
    (if (contains? ANNOTATION_PROPERTIES predicate)
      (addAnnotation bacillondex class predicate resourceUri)
      (print "TODO: Handle core resources that are not annotations")
      )
    ))

(defn handleOndexCoreStatement [stmt]
    (if (.isLiteral (.getObject stmt))
      (addLiteralPropertyToClass bacillondex stmt)
      (handleOndexCoreResource stmt))
  )

(defn handleStatement [stmt]
  (let [predicateNS (.getNameSpace (.getPredicate stmt))]
    (if (= predicateNS relationTypeNS)
      (handleRelationStatement stmt)
    )
    (if (= predicateNS attributeNS)
     (addLiteralPropertyToClass bacillondex stmt))
   (if (= predicateNS ondexcoreNS)
    (handleOndexCoreStatement stmt))    
  ))
  
(defn handleConceptResource [resource]
   (let [typeResource (.asResource (.getObject (.getProperty resource (RDF/type))))
         type (getID (.getURI typeResource))]
    (handleMetaResource synthbiont typeResource nil  nil)
    (addOntologyClass resource type)
    (doseq [stmt (iterator-seq (.listProperties resource))] 
      (handleStatement stmt )      
      )    
    ))

(defn addPropertyAnnotation [ontology predicate predicateAnnotateWith value isObjectProperty]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicateAnnotateWith)
   (addCommentForProperty ontology predicate value isObjectProperty)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicateAnnotateWith)
		   (addLabelForProperty ontology predicate value isObjectProperty)
       (addAnnotationPropertyForProperty ontology predicate predicateAnnotateWith value isObjectProperty))       
     )   
 ))

(defn handleProperty [propertyUri isObjectProperty]
  (let [ propertyResource (.getResource model propertyUri)
        propertyName (.getLocalName propertyResource)                
        ]
       (if isObjectProperty
         (object-property synthbiont propertyName)
         (datatype-property synthbiont propertyName)         
         ) 
       (doseq [stmt (iterator-seq (.listProperties propertyResource))] 
         (if (.isLiteral (.getObject stmt))
           (if-not (contains? PROPERTIES_TO_IGNORE (.getLocalName (.getPredicate stmt)))
             (addPropertyAnnotation synthbiont propertyName (.getLocalName (.getPredicate stmt)) (str (.getValue (.asLiteral (.getObject stmt)))) isObjectProperty)
             )
           )     
       )
    ))

(defn convert []
   (def model (getRDFModel))  
   (let [ subjects (iterator-seq (.listSubjects model))]
      
   ;Phil's version (doseq [subjectURI (map #(.getURI %) subjects)] 
   (doseq [subject subjects]
	   ( let [subjectURI (.getURI subject)]     	      
      (if (.startsWith subjectURI conceptNS )
	       (handleConceptResource subject)))
     )
   )
   (print "Datatype properties:" datatypePropertyUris)
   (doseq [propertyUri datatypePropertyUris]
     (handleProperty propertyUri false)
     ;(handleProperty objectPropertyUris true)
     )
   
    (doseq [propertyUri objectPropertyUris]
    (handleProperty propertyUri true)
    )
    (println "converted")    
 )

;(defn get-go-ontology []
;  (tawny.owl/remove-ontology-maybe
;   (OWLOntologyID. (IRI/create "http://purl.obolibrary.org/obo/go.owl")))
;  (.loadOntologyFromOntologyDocument
;   (tawny.owl/owl-ontology-manager)
;   (IRI/create (clojure.java.io/resource "go-snippet.owl"))))

(defn createont []

 
    (convert)
   ; (annotation-property "annotationproperty1")    
   ;(owl-class "testclass"
   ;         :annotation (annotation "annotationproperty1" "annotationproperty value"))
   
   ;(datatype-property "datatypeproperty1")
   ;(owl-class synthbiont "testclass"
   ;         :subclass (has-value "datatypeproperty1" "datatype property value" ))
 
   
  ;(owl-class  bacillondex "testclass2"
  ;        :subclass (has-value "datatypeproperty1" "datatype property value2" )
  ;        )
  
  ; WORKS (owl-class bacillondex "a" :comment "This is a comment")
  ; WORKS (owl-class bacillondex "a" :label "gmlabel2")  
  ;(owl-class  bacillondex "testclass2"
  ;        :label "gmlabel2"
  ;)
  
  ;WORKS:
  ;(defclass testclass2)
  ;(add-label bacillondex testclass2 "gmlabel")
  
  ;WORKS
  ;(owl-class synthbiont "promoter")
  ;(owl-class bacillondex "spo0Apromoter"
  ;          :subclass (owl-class synthbiont "promoter")              
  ;          )
  
; (owl-import (get-go-ontology))
 ;WORKS 
 ;(defclass A
 ;   :subclass
 ;       (owl-class (iri "http://purl.obolibrary.org/obo/GO_0000002")))
  
; WORKS (owl-class bacillondex "class1" :equivalent (owl-class bacillondex "class2" ))
 
;WORKS
 ;(datatype-property synthbiont "datatypeproperty1")
 ;(owl-class  bacillondex "testclass2"
 ;        :subclass (has-value synthbiont "datatypeproperty1" "datatype property value2" )
 ;        )
  
 ; (def ae (list "one" "two"))
 ; (println "Before adding:" ae)
 ;(conj ae "three")
 ;(println "after adding:" ae)
 
  ; (def aes (hash-set "one" "two"))
 ; (println "Before adding:" aes)
 ;(conj aes "three")
 ;(println "after adding:" aes)
  
  ;(def baselist (list "barnabas" :adam))
;(def lst1 (cons :willie baselist))
  
;  ( def  datatypePropertyUris (list "barnabas" :adam))
  
  ;(def datatypePropertyUris  (conj  datatypePropertyUris "willie"))
  ;(def datatypePropertyUris  (conj  datatypePropertyUris "willie"))
  


;(print "RESULT:" datatypePropertyUris)

 (save-ontology synthbiont "synthbiont.owl" :omn)
 (save-ontology bacillondex "bacillondex.owl" :omn)
  
)
