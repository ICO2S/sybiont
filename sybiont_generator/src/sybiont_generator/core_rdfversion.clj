(ns sybiont_generator.corerdfversion
  (:use [tawny.owl])
  (:use [sybiont_generator.ontology])  
  (:use [sybiont_generator.rdf])
  (:use [sybiont_generator.param])  
  (:use [sybiont_generator.definition]) 
  (:use [sybiont_generator.reasoner])   
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


( def  datatypePropertyUris (set [])) 
( def  objectPropertyUris (set []))  
( def  annotationPropertyUris (set []))  
( def  resourceTypeUris (set []))  
( def  handle (set []))  
( def  coreResourceTypeUris (set []))  

 	(defontology bacillondex
	  :iri "http://www.bacillondex.org"
	  :prefix "bo:"
	  :comment "An ontology for Bacillus subtilis parts"
	  :versioninfo "1.0"    
   )  


  
  (def CONCEPTS_AS_CLASSES (hash-set "MolFunc" "BioProc" "CelComp" "COG" "COGCategory" "KOEN" "EC" "Path"))

  
(defn getID [resourceURI]
  (let [index (.lastIndexOf resourceURI "/")]
   (.substring resourceURI (+ index 1))))

(defn getUpdatedClassName[className]
  (let [newName (.get CLASS_MAPPINGS className)]
        (if-not (nil? newName)
          (str newName)
          (str className)
        )))

(defn getUpdatedPropertyName[propertyName]
  (let [newName (.get PROPERTY_MAPPINGS propertyName)]
        (if-not (nil? newName)
          (str newName)
          (str propertyName)
        )))

(defn addAnnotation [ontology owlClass predicateOntology predicate predicateURI value]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicate)
   (addComment ontology owlClass value)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicate)
		   (addLabel ontology owlClass value)
       (do
          (addAnnotationProperty owlClass predicateOntology (getUpdatedPropertyName predicate) value)
          (def annotationPropertyUris (conj (set annotationPropertyUris)  predicateURI));GMGMGM
        )))   
 ))

(defn addAnnotationForIndividual [ontology owlIndividual predicateOntology predicate predicateURI value]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicate)
   ;(add-comment ontology owlIndividual value)
   (individual owlIndividual :comment (str value))
   ;(println (.getIRI owlIndividual ) value)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicate)
		   (individual owlIndividual :label (str value))
       (do
         (individual owlIndividual :annotation (annotation predicateOntology (annotation-property predicateOntology predicate) value)) 
          (def annotationPropertyUris (conj (set annotationPropertyUris)  predicateURI));GMGMGM
        )))   
 ))

(defn addLiteralPropertyToClass [stmt ontology owlClass]
  (let [predicateName (str (.getLocalName(.getPredicate stmt)))
        objectValue (str (.toString (.getValue (.asLiteral (.getObject stmt)))))
         predicateURI (.getURI (.getPredicate stmt))]
  (if-not (contains? PROPERTIES_TO_IGNORE  predicateName)
    (if (contains? ANNOTATION_PROPERTIES  predicateName)
      (addAnnotation ontology owlClass synthbiont (getUpdatedPropertyName predicateName) predicateURI objectValue) 
      ( do        
        (def datatypePropertyUris (conj (set datatypePropertyUris)  predicateURI))
        (addDatatypeProperty owlClass synthbiont (getUpdatedPropertyName predicateName) objectValue) 
      )))
  ))

(defn addLiteralPropertyToIndividual [stmt ontology owlIndividual]
  (let [predicateName (str (.getLocalName(.getPredicate stmt)))
        objectValue (str (.toString (.getValue (.asLiteral (.getObject stmt)))))
         predicateURI (.getURI (.getPredicate stmt))]
  (if-not (contains? PROPERTIES_TO_IGNORE  predicateName)
    (if (contains? ANNOTATION_PROPERTIES  predicateName)
      (addAnnotationForIndividual ontology owlIndividual synthbiont (getUpdatedPropertyName predicateName) predicateURI objectValue) 
      ( do        
        (def datatypePropertyUris (conj (set datatypePropertyUris)  predicateURI))
         (individual owlIndividual :fact (fact bacillondex  (datatype-property synthbiont (getUpdatedPropertyName predicateName)) objectValue))
      )))
  ))

(defn handleMetaResource [ontology resource parentOntology parentClass]
  (let [typeId (getID (.getURI resource))
        typeResourceProperties (iterator-seq (.listProperties resource))]  
  (owl-class ontology (getUpdatedClassName typeId)) 
  (println (str "  Creating the class " typeId))
  (if-not (nil? parentClass)
    (owl-class ontology (getUpdatedClassName typeId) :subclass (owl-class parentOntology (getUpdatedClassName parentClass))))
  (doseq [stmt typeResourceProperties] 
     (if (.isLiteral (.getObject stmt))
       (do                   
         (addLiteralPropertyToClass  stmt ontology (owl-class ontology (getUpdatedClassName typeId)))                
       ))
  )))


(defn getGoTerm [resource]
  (let [
        pidProperty (.getProperty model (str ondexcoreNS "pid"))       
        stmt (.getProperty resource pidProperty)
        goTerm (.toString (.getValue (.asLiteral (.getObject stmt))))]       
    (.replace goTerm ":" "_")))

(defn addOntologyClass [classResource  parentClass]
  (if (contains? GO_CLASSES parentClass)
    (owl-class (iri (str GO_URI "#" (getGoTerm classResource))) :subclass (owl-class synthbiont (getUpdatedClassName parentClass)))
    (owl-class bacillondex (getID (.getURI classResource)) :subclass (owl-class synthbiont (getUpdatedClassName parentClass)))     
  ))

(defn addOntologyIndividualOnly [individualResource parentClassName]
   (if-not (contains? GO_CLASSES parentClassName)
    (individual bacillondex (getID (.getURI individualResource)))
    (individual (iri (str GO_URI "#" (getGoTerm individualResource))))     
    )
  )
  
(defn addOntologyIndividual [individualResource parentClassName]
  ;(println  (.getURI individualResource) parentClassName)
   (let [individualEntity (addOntologyIndividualOnly individualResource parentClassName)]
      (add-type bacillondex individualEntity (owl-class synthbiont (getUpdatedClassName parentClassName)))  
      individualEntity
   )
   ; (add-type 
   ;   bacillondex 
   ;   (individual bacillondex (getID (.getURI individualResource))) 
   ;   (owl-class synthbiont (getUpdatedClassName parentClassName))
   ; )
    ;return value is the individual itseld
    ;(individual bacillondex (getID (.getURI individualResource))) 
                                      ;TODO
                                     ;:subclass (owl-class synthbiont (getUpdatedClassName parentClass))                                      
  )

  (defn handleInverseProperty [classIri valueClassIri predicateOntology predicate]
    (let [inversePredicate (.get INVERSE_PROPERTIES predicate)]
        (if-not (nil? inversePredicate)
          (do
	          (as-inverse
	             (object-property predicateOntology (getUpdatedPropertyName predicate))
	             (object-property predicateOntology inversePredicate))          
               (owl-class valueClassIri :subclass (owl-some predicateOntology inversePredicate (owl-class classIri)))                 		      
             (if (= inversePredicate HAS_PART)
               (do
                 (owl-class valueClassIri :subclass ( exactly predicateOntology 1 inversePredicate (owl-class classIri)))                 
                 (owl-class valueClassIri :subclass ( owl-some predicateOntology inversePredicate (owl-class classIri))))
               )))
    ))  
  
  (defn handleInversePropertyForIndividuals [individualOriginal valueIndividual predicateOntology predicate]
    (let [inversePredicateName (.get INVERSE_PROPERTIES predicate)]
        (if-not (nil? inversePredicateName)          
           (individual valueIndividual :fact (fact predicateOntology (object-property predicateOntology inversePredicateName)  individualOriginal))                    
    )))
                
  (defn addIndividualRelationshipForClass [stmt owlClass predicate valueResource valueType]
   (let [valueIndividual (addOntologyIndividual  valueResource valueType)]
      (addLiteralPropertyToClass stmt bacillondex owlClass)     
   )
)
  
(defn addClassRelationshipForClass [stmt owlClass predicate valueResource valueType]
   (let [valueClass (addOntologyClass valueResource valueType)]
	  (if (contains? SUBCLASS_RELATION predicate)
	          ;(owl-class (.getIRI owlClass) :subclass (owl-class (.getIRI valueClass) )))
	          (owl-class bacillondex (.getIRI owlClass) :subclass (owl-class bacillondex (.getIRI valueClass) )))
	        
	        (if (contains? SAMECLASS_RELATION predicate) 
	          (owl-class (.getIRI owlClass) :equivalent (owl-class (.getIRI valueClass) )))
	      
	        (if-not (or (contains? SUBCLASS_RELATION predicate) (contains? SAMECLASS_RELATION predicate)) 
	          (do 
	            (def objectPropertyUris (conj (set objectPropertyUris)  (.getURI (.getPredicate stmt))))
	            (object-property synthbiont (getUpdatedPropertyName predicate))
	            ;TODO The righthand should be an individual
	            (if (contains? CONCEPTS_AS_CLASSES  type) 
               ;if contains
               (do
                 (owl-class (.getIRI owlClass) :subclass (owl-some synthbiont (getUpdatedPropertyName predicate) (owl-class (.getIRI valueClass))))  
	               (handleInverseProperty (.getIRI owlClass)  (.getIRI valueClass) synthbiont predicate)   
               )
               ;else -onlt add the inverse relationship from the individual to the class
               (do
                 (let [valueIndividual (addOntologyIndividual valueResource valueType)
                       owlClassIndividual (individual (.getIRI owlClass))] 
                    (individual owlClassIndividual :fact (fact synthbiont  (object-property synthbiont (getUpdatedPropertyName predicate)) valueIndividual))
                    (handleInversePropertyForIndividuals owlClassIndividual valueIndividual synthbiont predicate)
                  )   
                 )
               )
	          ))     
	        )
         ;(println  "end")
  )
   


(defn handleRelationStatement [stmt owlClass]
  ;(println   "start")
  (let [predicate (.getLocalName (.getPredicate stmt))        
        valueResource (.getResource model (.getURI (.asResource (.getObject stmt))))
        valueResourceId (getID (.getURI valueResource))
        valueTypeResource (.asResource (.getObject (.getProperty valueResource (RDF/type))))
        valueType (getID (.getURI valueTypeResource))
        ]
        (if (contains? CONCEPTS_AS_CLASSES  valueType)  
          (addClassRelationshipForClass stmt owlClass predicate valueResource valueType)
          ;TODO
          ;(addIndividualRelationshipForClass owlClass predicate valueResource valueType)          
        )
      )
  )

(defn handleRelationStatementForIndividual [stmt owlIndividual]
  (let [predicate (.getLocalName (.getPredicate stmt))
        valueResource (.getResource model (.getURI (.asResource (.getObject stmt))))
        valueResourceId (getID (.getURI valueResource))
        valueTypeResource (.asResource (.getObject (.getProperty valueResource (RDF/type))))
        valueType (getID (.getURI valueTypeResource))
        ]
        (if (contains? SUBCLASS_RELATION predicate)
             (if-not (contains? CONCEPTS_AS_CLASSES  valueType)
               (let [relatedIndividual (addOntologyIndividual valueResource  valueType)]
                 (add-same bacillondex owlIndividual relatedIndividual)
                 (add-same bacillondex relatedIndividual owlIndividual) 
               )
               (do
                 (add-type bacillondex  owlIndividual (owl-class bacillondex valueResourceId))
                 ;(println (.getIRI owlIndividual) valueType)
               )
               ;(addOntologyIndividual (str (.getIRI owlIndividual))  valueType)
              )
           )
                
        (if (contains? SAMECLASS_RELATION predicate) 
          (let [relatedIndividual (addOntologyIndividual valueResource valueType)]
            (add-same bacillondex owlIndividual relatedIndividual)
            (add-same bacillondex relatedIndividual owlIndividual)                
          ))
              
        (if-not (or (contains? SUBCLASS_RELATION predicate) (contains? SAMECLASS_RELATION predicate)) 
          (do 
            (def objectPropertyUris (conj (set objectPropertyUris)  (.getURI (.getPredicate stmt))))
            (let [relationshipProperty (object-property synthbiont (getUpdatedPropertyName predicate))]
              (if-not (contains? CONCEPTS_AS_CLASSES  valueType)
                ;if not contains 
                (let [relatedIndividual (addOntologyIndividual valueResource  valueType)]
                  (individual owlIndividual :fact (fact synthbiont relationshipProperty relatedIndividual))              
                   (handleInversePropertyForIndividuals owlIndividual relatedIndividual synthbiont predicate)
                  )
                ;if contains
                  ;(let [relatedClass (addOntologyClass valueResource valueType)]
                  (let [relatedIndividual (addOntologyIndividual valueResource  valueType)]
                  ;(individual owlIndividual :fact (fact synthbiont relationshipProperty (.getURI relatedClass)))      
                  (individual owlIndividual :fact (fact synthbiont relationshipProperty relatedIndividual))      
                   
                  ;(handleInversePropertyForIndividuals owlIndividual relatedIndividual synthbiont predicate)
                  )
                                       
              )              
            )
            )
          )
        
        )
  )

(defn handleOndexCoreResource [stmt owlClass]
  (let [resource (.asResource (.getObject stmt))
        resourceUri (.getURI resource)
        predicate (.getLocalName (.getPredicate stmt))
        predicateUri (.getURI (.getPredicate stmt))]
        
    (if-not (contains? coreResourceTypeUris resourceUri)
      (do
        (def coreResourceTypeUris (conj (set coreResourceTypeUris)  resourceUri))
        (if (.startsWith resourceUri evidenceTypeNS)
		      (handleMetaResource synthbiont resource synthbiont EVIDENCE_TYPE_PARENT_CLASS))
		    (if (.startsWith resourceUri cvNS)
		        (handleMetaResource bacillondex resource synthbiont DATA_SOURCE_PARENT_CLASS))            
        )      
      )    
    (if (contains? ANNOTATION_PROPERTIES predicate)
      (addAnnotation bacillondex owlClass synthbiont predicate predicateUri resourceUri)
      (println "TODO: Handle core resources that are not annotations")
      )
    ))

(defn handleOndexCoreResourceForIndividual [stmt owlIndividual]
  (let [resource (.asResource (.getObject stmt))
        resourceUri (.getURI resource)
        predicate (.getLocalName (.getPredicate stmt))
        predicateUri (.getURI (.getPredicate stmt))]
        
    (if-not (contains? coreResourceTypeUris resourceUri)
      (do
        (def coreResourceTypeUris (conj (set coreResourceTypeUris)  resourceUri))
        (if (.startsWith resourceUri evidenceTypeNS)
		      (handleMetaResource synthbiont resource synthbiont EVIDENCE_TYPE_PARENT_CLASS))
		    (if (.startsWith resourceUri cvNS)
		        (handleMetaResource bacillondex resource synthbiont DATA_SOURCE_PARENT_CLASS))            
        )      
      )    
    (if (contains? ANNOTATION_PROPERTIES predicate)
      (addAnnotationForIndividual bacillondex owlIndividual synthbiont predicate predicateUri resourceUri)
      (println "TODO: Handle core resources that are not annotations")
      )
    ))

(defn handleOndexCoreStatement [stmt ontology owlClass]
    (if (.isLiteral (.getObject stmt))
      (addLiteralPropertyToClass stmt ontology owlClass)
      (handleOndexCoreResource stmt owlClass))
  )

(defn handleOndexCoreStatementForIndividual [stmt ontology owlIndividual]
    (if (.isLiteral (.getObject stmt))
      (addLiteralPropertyToIndividual stmt ontology owlIndividual)
      (handleOndexCoreResourceForIndividual stmt owlIndividual))
  )




(defn addPropertyAnnotation [ontology predicate predicateAnnotateWith value propertyType]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicateAnnotateWith)
   (addCommentForProperty ontology (getUpdatedPropertyName predicate) value propertyType)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicateAnnotateWith)
		   (addLabelForProperty ontology (getUpdatedPropertyName predicate) value propertyType)
       (addAnnotationPropertyForProperty ontology (getUpdatedPropertyName predicate) predicateAnnotateWith value propertyType)))   
 ))

(defn handleProperty [propertyUri propertyType]
  (let [ propertyResource (.getResource model propertyUri)
        propertyName (.getLocalName propertyResource)                
        ]
       (if (= propertyType "object")
         (object-property synthbiont (getUpdatedPropertyName propertyName)))
       
       (if (= propertyType "datatype")       
         (datatype-property synthbiont (getUpdatedPropertyName propertyName)))
       
       (if (= propertyType "annotation")       
         (annotation-property synthbiont (getUpdatedPropertyName propertyName)))
       
       (doseq [stmt (iterator-seq (.listProperties propertyResource))] 
         (if (.isLiteral (.getObject stmt))
           (if-not (contains? PROPERTIES_TO_IGNORE (.getLocalName (.getPredicate stmt)))
             (addPropertyAnnotation synthbiont (getUpdatedPropertyName propertyName) (.getLocalName (.getPredicate stmt)) (str (.getValue (.asLiteral (.getObject stmt)))) propertyType)
             )))
    ))


 
(defn getIriFragment[owlClass]
  (let [classIri (.getIRI owlClass)
        suffixIndex (.lastIndexOf (str classIri) "#")]
    (subs (str classIri) (+ suffixIndex 1))))

 
(defn handleStatementForIndividual [stmt ontology owlIndividual]
(let [predicateNS (.getNameSpace (.getPredicate stmt))]
  (if (= predicateNS relationTypeNS)
    (handleRelationStatementForIndividual stmt owlIndividual)
  )
  ;TODO
  (if (= predicateNS attributeNS)
   (addLiteralPropertyToIndividual stmt ontology owlIndividual))
  ;TODO
 (if (= predicateNS ondexcoreNS)
 (handleOndexCoreStatementForIndividual stmt ontology owlIndividual))    
))
 

  (defn handleStatement [stmt ontology owlClass]
  (let [predicateNS (.getNameSpace (.getPredicate stmt))]
    ;(println "in handle statement:" (.getIRI owlClass) (.getPredicate stmt))
    (if (= predicateNS relationTypeNS)
      (handleRelationStatement stmt owlClass)
    )
    ;(println   "after handleRelationStatement")
    (if (= predicateNS attributeNS)
     (addLiteralPropertyToClass stmt ontology owlClass))  
     ; (println   "after addLiteralPropertyToClass")
  
   ;TODO
    (if (= predicateNS ondexcoreNS)
    (handleOndexCoreStatement stmt ontology owlClass))    
  ))
  
  
  (defn handleConceptResource [resource]
   (let [typeResource (.asResource (.getObject (.getProperty resource (RDF/type))))
         type (getID (.getURI typeResource))]
    (def resourceTypeUris (conj (set resourceTypeUris)  (.getURI typeResource)))
    ;(println  (.getURI resource) type)
    (if (contains? CONCEPTS_AS_CLASSES  type)       
       ; if contains
	       (let [owlClass (addOntologyClass resource type)]
			       (doseq [stmt (iterator-seq (.listProperties resource))] 
			         (handleStatement stmt bacillondex owlClass)
	           )
	        )       
       ;else

      (let [owlIndividual (addOntologyIndividual resource type)]
		    (doseq [stmt (iterator-seq (.listProperties resource))] 
		       (handleStatementForIndividual stmt bacillondex owlIndividual)
        )
       )
      
    )
     ;(println  "---end of" (.getURI resource) type)
    ))
  
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
  (addClosureAxiomsToSuperClass bacillondex synthbiont "Promoter" synthbiont HAS_PART)
  (addDisjointAxioms bacillondex synthbiont "Promoter" synthbiont HAS_PART synthbiont "Operator");
  (addCountAxioms bacillondex synthbiont "Promoter" synthbiont HAS_PART synthbiont "Operator" (long 0));  
  (println "Added the closure axioms for promoters" )  
  )


      
(defn addSBOLClasses []
    ; bacillondex file does not have the SO class definitions required by the SparQL queries. Here synthbiont is merged into bacillondexontologytemp.rdf
    (remove-ontology-maybe  (.getOntologyID synthbiont))
    (remove-ontology-maybe  (.getOntologyID bacillondex))    
           
    (mergeOntologies SYBIONT "sybiontkb_sequenceclasses.rdf" "http://www.bacillondex_withontology.org" "bowo") 
    (save-ontology merged "sybiontkb_sequenceclasses_withontology.rdf" :rdf) 
    
    ;Query the newly merged RDF file and add the results to the knowledge base file without the class definitions
    (println "Querying for SBOL resources")
    (
      let [nosbolmodel (getRDFModel "sybiontkb_nosbol.rdf")
           modelwithontology (getRDFModel "sybiontkb_sequenceclasses_withontology.rdf") 
           newmodel_withcomponents (addSPARQLConstructQueryResult nosbolmodel modelwithontology "SBOLDnaComponents.sparql")
           newmodel_withannotations (addSPARQLConstructQueryResult newmodel_withcomponents modelwithontology "SBOLAnnotations.sparql")           
           ]      
        (save newmodel_withannotations "sybiontkb_sbol.ttl")      
      )    
     (clojure.java.io/delete-file "sybiontkb_nosbol.rdf")
     (clojure.java.io/delete-file "sybiontkb_sequenceclasses.rdf")
     (clojure.java.io/delete-file "sybiontkb_sequenceclasses_withontology.rdf")
       
  )
 
  (defn convert []
   (print "Reading the file ...")   
   (def model (getRDFModel "BacillOndexPlus.rdf"))  
   (println "done!!!")   
   
   (let [ subjects (iterator-seq (.listSubjects model))]
   ;Phil's version (doseq [subjectURI (map #(.getURI %) subjects)] 
   (doseq [subject subjects]
	   ( let [subjectURI (.getURI subject)]     	      
      (if (.startsWith subjectURI conceptNS )
	       (handleConceptResource subject)))
     )
   )
   
   (print "Adding the class types ()...")
   (doseq [typeUri (set resourceTypeUris)]
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
  
(defn createrdf []
    (println "converting")
    (convert)
    ;(addClosureAxioms)  
    ;(save-ontology synthbiont SYBIONT :omn)
    ;(save-ontology synthbiont SYBIONT_OWL :rdf)    
    ;(save-ontology bacillondex SYBIONTKB :omn)    
    (save-ontology bacillondex "sybiontkb_rdfversion.rdf" :rdf)    
    ;(removeClassesExcept  bacillondex ["Operator" "Promoter" "CDS" "Shim" "Terminator" "RBS"]) 
    ;(save-ontology bacillondex "sybiontkb_sequenceclasses.rdf" :rdf)        
)
;https://crossclj.info/doc/uk.org.russet/tawny-owl/1.5.0/tawny.owl.html#_add-comment
;http://www.bacillondex.org#6634

(defn addReasoningResults []
  
   (let [rdfModel (getRDFModel "sybiontkb_rdfversion.rdf")]      
      (addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/NegativelyRegulatedOperator.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/PositivelyRegulatedOperator.sparql")    	
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/RepressiblePromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/InduciblePromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/InduciblePromoterWith2Operators.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/RepressiblePromoterWith2Operators.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/RepressibleInduciblePromoterWith2Operators.sparql")    	
     
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigAPromoter.sparql")
      (addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigBPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigDPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigEPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigFPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigGPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigHPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigIPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigKPromoter.sparql")
      (addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigLPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigMPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigVPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigWPromoter.sparql")     
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigXPromoter.sparql")     
      (addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigYPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/SigZPromoter.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/YlaCPromoter.sparql")    	
     
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/ConstitutiveSigAPromoter.sparql")    	
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/RepressorEncodingCDS.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/ActivatorEncodingCDS.sparql")
    	(addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/KinaseEncodingCDS.sparql")
      (addSPARQLConstructQueryResult rdfModel rdfModel "rdfinference/ResponseRegulatorEncodingCDS.sparql")            
      (save rdfModel "sybiontkb_rdfversion_inferred.ttl")      
     ) 

  
 	    	
    )
    
;Steps to execute
;(createrdf)
;(addReasoningResults)
