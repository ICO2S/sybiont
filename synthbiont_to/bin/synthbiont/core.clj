(ns synthbiont.core
  (:use [tawny.owl])
  (:use [synthbiont.ontology])  
  (:use [synthbiont.rdf])
  (:use [synthbiont.param])  
  (:use [synthbiont.definition]) 
  (:use [synthbiont.reasoner])   
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))

(import '(java.io FileInputStream File))       
(import '(com.hp.hpl.jena.rdf.model Model ModelFactory ResourceFactory))
(import '(com.hp.hpl.jena.vocabulary RDF RDFS))
(import '(org.semanticweb.owlapi.model ClassExpressionType OWLObjectSomeValuesFrom))
(import '(org.semanticweb.owlapi.util SimpleIRIMapper NamespaceUtil DefaultPrefixManager))
(import '(org.coode.owlapi.manchesterowlsyntax ManchesterOWLSyntaxOntologyFormat))

(def ^:dynamic model)  
(def INVERSE_PROPERTIES {"part_of" "has_part", "en_by" "encodes", "equ" "equ","bi_to" "bound_by"})   
( def  datatypePropertyUris (set [])) 
( def  objectPropertyUris (set []))  
( def  annotationPropertyUris (set []))  
( def  resourceTypeUris (set []))  

 	(defontology bacillondex
	  :iri "http://www.bacillondex.org"
	  :prefix "bo:"
	  :comment "An ontology for Bacillus subtilis parts"
	  :versioninfo "1.0"    
   )  

(defn getID [resourceURI]
  (let [index (.lastIndexOf resourceURI "/")]
   (.substring resourceURI (+ index 1))))

(defn addAnnotation [owlClass predicateOntology predicate predicateURI value]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicate)
   (addComment owlClass value)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicate)
		   (addLabel owlClass value)
       (do
          (addAnnotationProperty owlClass predicateOntology predicate value)
          (def annotationPropertyUris (conj annotationPropertyUris  predicateURI));GMGMGM
        )))   
 ))

(defn addLiteralPropertyToClass [stmt owlClass]
  (let [predicateName (str (.getLocalName(.getPredicate stmt)))
        objectValue (str (.toString (.getValue (.asLiteral (.getObject stmt)))))
         predicateURI (.getURI (.getPredicate stmt))]
  (if-not (contains? PROPERTIES_TO_IGNORE  predicateName)
    (if (contains? ANNOTATION_PROPERTIES  predicateName)
      (addAnnotation owlClass synthbiont predicateName predicateURI objectValue) 
      ( do        
        (def datatypePropertyUris (conj datatypePropertyUris  predicateURI))
        (addDatatypeProperty owlClass synthbiont predicateName objectValue) 
      )))
  ))

(defn handleMetaResource [ontology resource parentOntology parentClass]
  (let [typeId (getID (.getURI resource))
        typeResourceProperties (iterator-seq (.listProperties resource))]  
  (owl-class ontology typeId)  
  (if-not (nil? parentClass)
    (owl-class ontology typeId :subclass (owl-class parentOntology parentClass)))
  (doseq [stmt typeResourceProperties] 
     (if (.isLiteral (.getObject stmt))
         (addLiteralPropertyToClass  stmt (owl-class ontology typeId)))
  )))

(defn getGoTerm [resource]
  (let [
        pidProperty (.getProperty model (str ondexcoreNS "pid"))       
        stmt (.getProperty resource pidProperty)
        goTerm (.toString (.getValue (.asLiteral (.getObject stmt))))]       
    (.replace goTerm ":" "_")))

(defn addOntologyClass [classResource  parentClass]
  (if (contains? GO_CLASSES parentClass)
    (owl-class (iri (str GO_URI "#" (getGoTerm classResource))) :subclass (owl-class synthbiont parentClass))
    (owl-class bacillondex (getID (.getURI classResource)) :subclass (owl-class synthbiont parentClass))     
  ))

  (defn handleInverseProperty [classIri valueClassIri predicateOntology predicate]
    (let [inversePredicate (.get INVERSE_PROPERTIES predicate)]
        (if-not (nil? inversePredicate)
          (do
	          (as-inverse
	             (object-property predicateOntology predicate)
	             (object-property predicateOntology inversePredicate))          
               (owl-class valueClassIri :subclass (owl-some predicateOntology inversePredicate (owl-class classIri)))                 		      
             (if (= inversePredicate "has_part")
               (do
                 (owl-class valueClassIri :subclass ( exactly predicateOntology 1 inversePredicate (owl-class classIri)))
                 ;TODO Enhance this
                 (owl-class valueClassIri :subclass ( owl-some predicateOntology inversePredicate (owl-class classIri))))
               )))
    ))         
            
(defn handleRelationStatement [stmt owlClass]
  (let [predicate (.getLocalName (.getPredicate stmt))
        valueResource (.getResource model (.getURI (.asResource (.getObject stmt))))
        valueResourceId (getID (.getURI valueResource))
        valueTypeResource (.asResource (.getObject (.getProperty valueResource (RDF/type))))
        valueType (getID (.getURI valueTypeResource))
        valueClass (addOntologyClass valueResource valueType)]
        (if (contains? SUBCLASS_RELATION predicate)
          (owl-class (.getIRI owlClass) :subclass (owl-class (.getIRI valueClass) )))
        
        (if (contains? SAMECLASS_RELATION predicate) 
          (owl-class (.getIRI owlClass) :equivalent (owl-class (.getIRI valueClass) )))
      
        (if-not (or (contains? SUBCLASS_RELATION predicate) (contains? SAMECLASS_RELATION predicate)) 
          (do 
            (def objectPropertyUris (conj objectPropertyUris  (.getURI (.getPredicate stmt))))
            (object-property synthbiont predicate)
             (owl-class (.getIRI owlClass) :subclass (owl-some synthbiont predicate (owl-class (.getIRI valueClass))))  
             (handleInverseProperty (.getIRI owlClass)  (.getIRI valueClass) synthbiont predicate)                                   
          ))                          
        ))


(defn handleOndexCoreResource [stmt owlClass]
  (let [resource (.asResource (.getObject stmt))
        resourceUri (.getURI resource)
        predicate (.getLocalName (.getPredicate stmt))
        predicateUri (.getURI (.getPredicate stmt))]
    (if (.startsWith resourceUri evidenceTypeNS)
      (handleMetaResource bacillondex resource synthbiont EVIDENCE_TYPE_PARENT_CLASS))
    (if (.startsWith resourceUri cvNS)
        (handleMetaResource bacillondex resource synthbiont DATA_SOURCE_PARENT_CLASS))    
    (if (contains? ANNOTATION_PROPERTIES predicate)
      (addAnnotation owlClass synthbiont predicate predicateUri resourceUri)
      (println "TODO: Handle core resources that are not annotations")
      )
    ))

(defn handleOndexCoreStatement [stmt owlClass]
    (if (.isLiteral (.getObject stmt))
      (addLiteralPropertyToClass stmt owlClass)
      (handleOndexCoreResource stmt owlClass))
  )

(defn handleStatement [stmt owlClass]
  (let [predicateNS (.getNameSpace (.getPredicate stmt))]
    (if (= predicateNS relationTypeNS)
      (handleRelationStatement stmt owlClass)
    )
    (if (= predicateNS attributeNS)
     (addLiteralPropertyToClass stmt owlClass))
   (if (= predicateNS ondexcoreNS)
    (handleOndexCoreStatement stmt owlClass))    
  ))
  
(defn handleConceptResource [resource]
   (let [typeResource (.asResource (.getObject (.getProperty resource (RDF/type))))
         type (getID (.getURI typeResource))]
    (def resourceTypeUris (conj resourceTypeUris  (.getURI typeResource)))
    (let [owlClass (addOntologyClass resource type)]
      (doseq [stmt (iterator-seq (.listProperties resource))] 
        (handleStatement stmt owlClass)))    
    ))

(defn addPropertyAnnotation [ontology predicate predicateAnnotateWith value propertyType]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicateAnnotateWith)
   (addCommentForProperty ontology predicate value propertyType)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicateAnnotateWith)
		   (addLabelForProperty ontology predicate value propertyType)
       (addAnnotationPropertyForProperty ontology predicate predicateAnnotateWith value propertyType)))   
 ))

(defn handleProperty [propertyUri propertyType]
  (let [ propertyResource (.getResource model propertyUri)
        propertyName (.getLocalName propertyResource)                
        ]
       (if (= propertyType "object")
         (object-property synthbiont propertyName))
       
       (if (= propertyType "datatype")       
         (datatype-property synthbiont propertyName))
       
       (if (= propertyType "annotation")       
         (annotation-property synthbiont propertyName))
       
       (doseq [stmt (iterator-seq (.listProperties propertyResource))] 
         (if (.isLiteral (.getObject stmt))
           (if-not (contains? PROPERTIES_TO_IGNORE (.getLocalName (.getPredicate stmt)))
             (addPropertyAnnotation synthbiont propertyName (.getLocalName (.getPredicate stmt)) (str (.getValue (.asLiteral (.getObject stmt)))) propertyType)
             )))
    ))

(defn convert []
   (print "Reading the file ...")   
   (def model (getRDFModel))  
   (println "done!")   
   
   (let [ subjects (iterator-seq (.listSubjects model))]
   ;Phil's version (doseq [subjectURI (map #(.getURI %) subjects)] 
   (doseq [subject subjects]
	   ( let [subjectURI (.getURI subject)]     	      
      (if (.startsWith subjectURI conceptNS )
	       (handleConceptResource subject)))
     )
   )
   
   (print "Adding the class types ()...")
   (doseq [typeUri resourceTypeUris]
       (handleMetaResource synthbiont (.getResource model typeUri) nil  nil))
   (println "done!")
      
   (print "Adding the datatype properties ...")      
   (doseq [propertyUri datatypePropertyUris]
     (handleProperty propertyUri "datatype"))
   (println "done!")
   
   (print "Adding the object properties ...")          
   (doseq [propertyUri objectPropertyUris]
     (handleProperty propertyUri "object"))
    (println "done!")
    
   (print "Adding the annotation properties ...")          
   (doseq [propertyUri annotationPropertyUris]
     (handleProperty propertyUri "annotation"))
    (println "done!")            
    (println "converted")    
 )
 


(defn getIriFragment[owlClass]
  (let [classIri (.getIRI owlClass)
        suffixIndex (.lastIndexOf (str classIri) "#")]
    (subs (str classIri) (+ suffixIndex 1))))

(defn addDisjointAxioms[ontology classOntology class propertyOntology property targetClassOntology targetClass]
 ( def  unionClasses []) 
 (let [classIri (iri (str (.toString (.getOntologyIRI (.getOntologyID classOntology))) "#" class))]
 (doseq [subClass (subclasses ontology classIri)]   
   (doseq [superClass (superclasses ontology subClass)]
       (if  (= (.getClassExpressionType superClass) (ClassExpressionType/OBJECT_SOME_VALUES_FROM) )
            (if (= property (.getFragment(.getIRI(.getProperty (cast OWLObjectSomeValuesFrom superClass)))))
               (let [relatedClass (getIriFragment (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass))))] 
                 (if (superclass? ontology relatedClass (getClassIri targetClassOntology targetClass))
                     (def  unionClasses (conj unionClasses  (owl-class ontology relatedClass)))
                 )))))   
   (if (>= (count unionClasses) 2)
       (as-disjoint unionClasses))
    ( def  unionClasses [])   
   )))

(defn addClosureAxiomsToSuperClass[ontology classOntology class propertyOntology property]
 ( def  unionClasses []) 
 (let [classIri (iri (str (.toString (.getOntologyIRI (.getOntologyID classOntology))) "#" class))]
 (doseq [subClass (subclasses ontology classIri)]   
   (doseq [superClass (superclasses ontology subClass)]
       (if  (= (.getClassExpressionType superClass) (ClassExpressionType/OBJECT_SOME_VALUES_FROM) )
         (do   
         (if (= property (.getFragment(.getIRI(.getProperty (cast OWLObjectSomeValuesFrom superClass)))))
             (def  unionClasses (conj unionClasses  (owl-class ontology (getIriFragment (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass)))))))                      
           ))))
   
   (if-not (empty? unionClasses)
     (do 
       (owl-class ontology (getIriFragment subClass) :subclass (owl-only propertyOntology property (owl-or unionClasses) ))   
       ( def  unionClasses [])))
   )))


(defn addCountAxioms[ontology classOntology class propertyOntology property targetClassOntology targetClass countCondition]
 ( def  unionClasses []) 
 (let [classIri (iri (str (.toString (.getOntologyIRI (.getOntologyID classOntology))) "#" class))]
 (doseq [subClass (subclasses ontology classIri)]   
   (doseq [superClass (superclasses ontology subClass)]
       (if  (= (.getClassExpressionType superClass) (ClassExpressionType/OBJECT_SOME_VALUES_FROM) )
            (if (= property (.getFragment(.getIRI(.getProperty (cast OWLObjectSomeValuesFrom superClass)))))
               (let [relatedClass (getIriFragment (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass))))] 
                 (if (superclass? ontology relatedClass (getClassIri targetClassOntology targetClass))
                     (def  unionClasses (conj unionClasses  (owl-class ontology relatedClass)))
                 )))))
   (if (= (count unionClasses) (long countCondition))
       (owl-class (.getIRI subClass) :subclass (exactly propertyOntology (long countCondition) property (owl-class (getClassIri targetClassOntology targetClass)))))
    ( def  unionClasses [])   
   )))

(defn addClosureAxioms[]
  (println "Adding the closure axioms for promoters" )
  (addClosureAxiomsToSuperClass bacillondex synthbiont "Promoter" synthbiont "has_part")
  (addDisjointAxioms bacillondex synthbiont "Promoter" synthbiont "has_part" synthbiont "Operator");
  (addCountAxioms bacillondex synthbiont "Promoter" synthbiont "has_part" synthbiont "Operator" (long 0));  
  (println "Added the closure axioms for promoters" )  
  )

;(defn get-go-ontology []
;  (tawny.owl/remove-ontology-maybe
;   (OWLOntologyID. (IRI/create "http://purl.obolibrary.org/obo/go.owl")))
;  (.loadOntologyFromOntologyDocument
;   (tawny.owl/owl-ontology-manager)
;   (IRI/create (clojure.java.io/resource "go-snippet.owl"))))

(defn createontORG []
    (convert)
    (addClosureAxioms)  
    (save-ontology synthbiont "synthbiont.omn" :omn)
    (save-ontology bacillondex "bacillondexontology.omn" :omn)    
)

(import '(org.semanticweb.owlapi.model OWLOntology PrefixManager SetOntologyID ))
(import '(org.semanticweb.owlapi.apibinding OWLManager ))
(import '(org.semanticweb.owlapi.util OWLOntologyMerger DefaultPrefixManager))
(import '(org.semanticweb.owlapi.io RDFXMLOntologyFormat ))


(defn mergeOntologies2 [file1 file2 namespace prefix] 
  (def ontologyA (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. file1)))
 (def ontologyB (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. file2)))
 (def mergerAB (OWLOntologyMerger. (owl-ontology-manager)))
 (def mergedAB (.createMergedOntology mergerAB (owl-ontology-manager) (iri namespace) )) 
 (remove-ontology-maybe  (.getOntologyID ontologyA))
 (remove-ontology-maybe  (.getOntologyID ontologyB)) 
 (set-prefix mergedAB prefix))


(defn createOntologyWithSequenceClasses [from to]
  (let [tempOntology (.loadOntologyFromOntologyDocument (owl-ontology-manager) (File. from))]
       (removeClassesExcept  tempOntology ["Operator" "Promoter" "CDS" "Shim" "Terminator" "RBS"])  
       (save-ontology tempOntology to :omn)    
     )
  )
(defn addSBOLClasses []
    ; bacillondex file does not have the SO class definitions required by the SparQL queries. Here synthbiont is merged into bacillondexontologytemp.rdf
    ;(remove-ontology-maybe  (.getOntologyID synthbiont))
    ;(remove-ontology-maybe  (.getOntologyID bacillondex))    
    
       
    ;(mergeOntologies2 "synthbiont.omn" "bacillondexontology_sequenceclasses.rdf" "http://www.bacillondex_withontology.org" "bowo") 
    ;(save-ontology mergedAB "bacillondex_sequenceclasses_withontology.rdf" :rdf) 
    
    ;Query the newly merged RDF file and add the results to the knowledge base file without the class definitions
    (println "Querying for SBOL resources")
    (
      let [nosbolmodel (getRDFModel2 "bacillondexontology_nosbol.rdf")
           modelwithontology (getRDFModel2 "bacillondex_sequenceclasses_withontology.rdf") 
           ]
      (addSPARQLConstructQueryResult nosbolmodel modelwithontology "SBOLDnaComponents.sparql" "bacillondexontology.rdf")
      (println "Loaded the rdf files")
      )
    ;;(addSPARQLConstructQueryResult (getRDFModel2 "bacillondexontology_nosbol.rdf") (getRDFModel2 "bacillondexontology_withontology.rdf") "SBOLDnaComponents.sparql", "bacillondexontology.rdf")
    
    ;Create the omn version of the rdf file
    ;(loadOntology "bacillondexontology.rdf")
    ;(save-ontology bacillondex "bacillondexontology.omn" :omn)    
  )
 
(defn createont []
    (print "converting")
    (convert)
    (addClosureAxioms)  
    (save-ontology synthbiont "synthbiont.omn" :omn)
    (save-ontology bacillondex "bacillondexontology.omn" :omn)    
    (save-ontology bacillondex "bacillondexontology_nosbol.rdf" :rdf)    
    (removeClassesExcept  bacillondex ["Operator" "Promoter" "CDS" "Shim" "Terminator" "RBS"]) 
    (save-ontology bacillondex "bacillondexontology_sequenceclasses.rdf" :rdf)    
    
)
