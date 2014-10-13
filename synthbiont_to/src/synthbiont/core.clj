(ns synthbiont.core
  (:use [tawny.owl])
  (:use [synthbiont.ontology])  
  (:use [synthbiont.rdf])
  (:use [synthbiont.param])  
  (:use [synthbiont.definition])    
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))


(import '(java.io FileInputStream File))       
(import '(com.hp.hpl.jena.rdf.model Model ModelFactory ResourceFactory))
(import '(com.hp.hpl.jena.vocabulary RDF RDFS))
(import '(org.semanticweb.owlapi.model ClassExpressionType OWLObjectSomeValuesFrom))

 ;(:import (org.semanticweb.owlapi.model IRI OWLNamedObject OWLOntologyID)
 ;          (org.semanticweb.owlapi.util SimpleIRIMapper))
 

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

(defn addAnnotation [ontology class predicateOntology predicate predicateURI value]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicate)
   (addComment ontology class value)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicate)
		   (addLabel ontology class value)
       (do
          (addAnnotationProperty ontology class predicateOntology predicate value)
          (def annotationPropertyUris (conj annotationPropertyUris  predicateURI));GMGMGM
        ))       
     )   
 ))

(defn addLiteralPropertyToClass [ontology stmt]
  (let [predicateName (str (.getLocalName(.getPredicate stmt)))
        objectValue (str (.toString (.getValue (.asLiteral (.getObject stmt)))))
         class (getID (.getURI (.getSubject stmt)))
         predicateURI (.getURI (.getPredicate stmt))
         ]
  (if-not (contains? PROPERTIES_TO_IGNORE  predicateName)
    (if (contains? ANNOTATION_PROPERTIES  predicateName)
      (addAnnotation ontology class synthbiont predicateName predicateURI objectValue) 
      ( do        
        (def datatypePropertyUris (conj datatypePropertyUris  predicateURI))
        (addDatatypeProperty ontology class synthbiont predicateName objectValue) 
      )
    )
  )
  ))

(defn handleMetaResource [ontology resource parentOntology parentClass]
  (let [typeId (getID (.getURI resource))
        typeResourceProperties (iterator-seq (.listProperties resource))
        ]  
  (owl-class ontology typeId)  
  (if-not (nil? parentClass)
    (owl-class ontology typeId :subclass (owl-class parentOntology parentClass)))
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

  (defn handleInverseProperty [ontology class valueOntology valueClass predicateOntology predicate]
    (let [inversePredicate (.get INVERSE_PROPERTIES predicate)]
        (if-not (nil? inversePredicate)
          (do
	          (as-inverse
	             (object-property predicateOntology predicate)
	             (object-property predicateOntology inversePredicate))
          
             (addObjectProperty valueOntology valueClass  ontology class predicateOntology inversePredicate)          
		      
             (if (= inversePredicate "has_part")                 
                 (owl-class valueOntology valueClass :subclass ( exactly predicateOntology 1 inversePredicate (owl-class ontology class) ))
                 ;(owl-class valueOntology valueClass :subclass (owl-some predicateOntology predicate (owl-class ontology class)))                 
                 ;(owl-class valueOntology valueClass :subclass (exactly 1 predicateOntology inversePredicate ontology class )) 
               )                 
          ))
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
            (addObjectProperty bacillondex class bacillondex valueResourceId synthbiont predicate)
            (handleInverseProperty bacillondex class bacillondex valueResourceId synthbiont predicate)  
          ))                          
        ))

(defn handleOndexCoreResource [stmt]
  (let [resource (.asResource (.getObject stmt))
        resourceUri (.getURI resource)
        predicate (.getLocalName (.getPredicate stmt))
        class (getID (.getURI (.getSubject stmt)))
        predicateUri (.getURI (.getPredicate stmt))        
        ]
    (if (.startsWith resourceUri evidenceTypeNS)
      (handleMetaResource bacillondex resource synthbiont EVIDENCE_TYPE_PARENT_CLASS))
    (if (.startsWith resourceUri cvNS)
        (handleMetaResource bacillondex resource synthbiont DATA_SOURCE_PARENT_CLASS))    
    (if (contains? ANNOTATION_PROPERTIES predicate)
      (addAnnotation bacillondex class synthbiont predicate predicateUri resourceUri)
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
    ;(handleMetaResource synthbiont typeResource nil  nil)
    (def resourceTypeUris (conj resourceTypeUris  (.getURI typeResource)))
    (addOntologyClass resource type)
    (doseq [stmt (iterator-seq (.listProperties resource))] 
      (handleStatement stmt )      
      )    
    ))

(defn addPropertyAnnotation [ontology predicate predicateAnnotateWith value propertyType]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicateAnnotateWith)
   (addCommentForProperty ontology predicate value propertyType)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicateAnnotateWith)
		   (addLabelForProperty ontology predicate value propertyType)
       (addAnnotationPropertyForProperty ontology predicate predicateAnnotateWith value propertyType))       
     )   
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
             )
           )     
       )
    ))

(defn addInverseProperties []
  
  )

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
 
(defn getClassIri [ontology className]
  (iri (str (.toString (.getOntologyIRI (.getOntologyID ontology))) "#" className))
  )

(defn getIriFragment[owlClass]
  (let [classIri (.getIRI owlClass)
        suffixIndex (.lastIndexOf (str classIri) "#")
        ]
    (subs (str classIri) (+ suffixIndex 1))  
    )
  )

(defn addDisjointAxioms[ontology classOntology class propertyOntology property targetClassOntology targetClass]
 ( def  unionClasses []) 
 (let [classIri (iri (str (.toString (.getOntologyIRI (.getOntologyID classOntology))) "#" class))]
 (doseq [subClass (subclasses ontology classIri)]   
   (println "subclass" subClass)
   (doseq [superClass (superclasses ontology subClass)]
       ;(println "-----" "superclass" superClass)
       (if  (= (.getClassExpressionType superClass) (ClassExpressionType/OBJECT_SOME_VALUES_FROM) )
            (if (= property (.getFragment(.getIRI(.getProperty (cast OWLObjectSomeValuesFrom superClass)))))
               (let [relatedClass (getIriFragment (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass))))] 
                 ;(println "-----" "-----" "related class:" relatedClass)
                 (if (superclass? ontology relatedClass (getClassIri targetClassOntology targetClass))
                   (do
                     ;(println "-----" "-----" "-----" "adding the related class:" relatedClass)
                     (def  unionClasses (conj unionClasses  (owl-class ontology relatedClass)))
                   )
                 )
               )
             )
          )
	  )
   (println "-----union classes:" unionClasses)
   (if (>= (count unionClasses) 2)
       (as-disjoint unionClasses)   
    )
    ( def  unionClasses [])   
   )))


(defn addClosureAxiomsToSuperClass[ontology classOntology class propertyOntology property]
 ( def  unionClasses []) 
 (let [classIri (iri (str (.toString (.getOntologyIRI (.getOntologyID classOntology))) "#" class))]
 (doseq [subClass (subclasses ontology classIri)]   
   (doseq [superClass (superclasses ontology subClass)]
       (if  (= (.getClassExpressionType superClass) (ClassExpressionType/OBJECT_SOME_VALUES_FROM) )
            (if (= property (.getFragment(.getIRI(.getProperty (cast OWLObjectSomeValuesFrom superClass)))))
                (def  unionClasses (conj unionClasses  (owl-class ontology (getIriFragment (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass)))))))                      
              )
          )
	  )
   (if-not (empty? unionClasses)
     (do 
       (owl-class ontology (getIriFragment subClass) :subclass (owl-only propertyOntology property (owl-or unionClasses) ))   
       ( def  unionClasses [])      
     )
    )
   )))



(defn addClosureAxioms[]
  (println "Adding the closure axioms for promoters" )
  (addClosureAxiomsToSuperClass bacillondex synthbiont "Promoter" synthbiont "has_part")
  (addDisjointAxioms bacillondex synthbiont "Promoter" synthbiont "has_part", synthbiont "Operator");
  (println "Added the closure axioms for promoters" )
  
  )


;(defn get-go-ontology []
;  (tawny.owl/remove-ontology-maybe
;   (OWLOntologyID. (IRI/create "http://purl.obolibrary.org/obo/go.owl")))
;  (.loadOntologyFromOntologyDocument
;   (tawny.owl/owl-ontology-manager)
;   (IRI/create (clojure.java.io/resource "go-snippet.owl"))))

(defn createont []

 
    (convert)
    (addClosureAxioms)
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
  
 ;(print INVERSE_PROPERTIES)  
 ;(println "bi_to:" (get INVERSE_PROPERTIES "bi_to") )
 ;(println "bi_to2:" (get INVERSE_PROPERTIES "bi_to2") )
 
  ; (let [inversePredicate (.get INVERSE_PROPERTIES "part_of")]
  ;      (if-not (nil? inversePredicate)
  ;        (print "inverse:" inversePredicate)
  ;        )
   ;     )
 
 ;(owl-class synthbiont "class1" :subclass ( exactly 1 synthbiont "has_part" (owl-class synthbiont "class2") ))
 ; WORKS (owl-class synthbiont "class1" :subclass ( exactly 1  "has_part" (owl-class synthbiont "class2") ))
 
 ;WORKS
;(owl-class synthbiont "Promoter")
;(owl-class synthbiont "Operator")
;(owl-class bacillondex "12" :subclass (owl-some synthbiont "has_part" (owl-class bacillondex "13" :subclass (owl-class synthbiont "Operator"))))
;(owl-class bacillondex "12" :subclass (owl-some synthbiont "has_part" (owl-class bacillondex "17" :subclass (owl-class synthbiont "Operator"))))
;(owl-class bacillondex "12" :subclass (owl-class synthbiont "Promoter"))
;(owl-class bacillondex "14" :subclass (owl-class synthbiont "Promoter"))
;(owl-class bacillondex "14" :subclass (owl-some synthbiont "has_part" (owl-class bacillondex "15" :subclass (owl-class synthbiont "Operator"))))
;(owl-class bacillondex "16" :subclass (owl-class synthbiont "Promoter"))
;(as-disjoint  (owl-class bacillondex "13") (owl-class bacillondex "15"))
;(addClosureAxiomsToSuperClass bacillondex synthbiont "Promoter" synthbiont "has_part")
;(addDisjointAxioms bacillondex synthbiont "Promoter" synthbiont "has_part", synthbiont "Operator");


;(owl-class  "test1" :super (owl-class "test2"))
;(owl-class  synthbiont "test3" :super (owl-class bacillondex "test4"))
;(owl-class  bacillondex "test5" :super (owl-class bacillondex "test6"))
;(owl-class  bacillondex "test7" :subclass (owl-class synthbiont "test8"))





;(println "1- test1 superclass of test2" (superclass? bacillondex "test1" "test2"))
;(print "2- test1 superclass of test2" (superclass? (iri("http://www.sybio.ncl.ac.uk#test1")) (iri("http://www.sybio.ncl.ac.uk#test2"))))
;(println "2- test1 superclass of test2" (superclass? bacillondex (iri "http://www.bacillondex.org#test1") (iri "http://www.bacillondex.org#test2")))
;(println "3- test3 superclass of test4" (superclass? bacillondex (iri "http://www.sybio.ncl.ac.uk#test3") (iri "http://www.bacillondex.org#test4")))

;(print "superclasses" (superclasses  bacillondex "test4"))
;(print "superclasses" (superclasses  bacillondex "test6"))

;(superclass? bacillondex (iri "http://www.sybio.ncl.ac.uk#test8") (iri "http://www.bacillondex.org#test7"))

;(superclass? bacillondex "test7" (iri "http://www.sybio.ncl.ac.uk#test8"))

;(print "subclasses" (subclasses bacillondex (iri (str (.toString (.getOntologyIRI (.getOntologyID synthbiont))) "#Promoter"))))
 

 
 (save-ontology synthbiont "synthbiont.omn" :omn)
 (save-ontology bacillondex "bacillondex.omn" :omn)
  
)

(defn testontology[]
 


  
  
 	(defontology ontology1
     :iri "http://www.ontology1.org"
	  :prefix "o1:"
    )    


      
  (defontology ontology2
         :iri "http://www.ontology2.org"
	  :prefix "o2:"
    ) 

  ;(owl-class ontology1 "Class1" :subclass ( exactly ontology2 1  "predicate1" (owl-class ontology1 "Class2") ))
 (owl-class ontology1 "Class1" :subclass ( owl-some  ontology2 "predicate2" (owl-class ontology2 "Class2") ))
 (owl-class ontology1 "Class1" :subclass ( owl-some  ontology2 "predicate2" (owl-class ontology2 "Class5") )) 
 (owl-class ontology1 "Class3" :subclass (owl-class ontology1 "Class1"))
 (owl-class ontology1 "Class1" :subclass (owl-class ontology1 "Class4"))
 (owl-class ontology2 "Class5" :subclass (owl-class ontology1 "Class1"))
 

;(owl-class ontology1 "Class1" :subclass (owl-only ontology2 "predicate2" (owl-class ontology1 "Class2")))     
;(owl-class ontology1 "Class1" :subclass (owl-only ontology2 "predicate2" (owl-class ontology1 "Class5")))     
;(owl-class ontology1 "Class1" :subclass (owl-only ontology2 "predicate2" (owl-or [(owl-class ontology1 "Class5") (owl-class ontology1 "Class2")]) ))     


  ;(save-ontology ontology1 "ontology1.owl" :omn)
  ;(save-ontology ontology2 "ontology2.owl" :omn)
 
 ;(defoproperty predicate2)
 ;(defclass ontology1 Class1
   ;:subclass (exactly 1 predicate2 (defclass ontology1 Class2))
   ;)
 
 ;(owl-class (iri "http://purl.obolibrary.org/obo/GO_0000002" :prefix "go"))


 ;(println "subclasses" (subclasses ontology1 "Class1"))
 (println "superclasses" (superclasses ontology1 "Class1"))
 (println "subclasses" (subclasses ontology1 "Class1"))
 
 ;( def  unionClasses (set [(owl-class ontology1 "Class2") (owl-class ontology1 "Class5")])) 
 ;( def  unionClasses [(owl-class ontology1 "Class2") (owl-class ontology1 "Class5")]) 
 ( def  unionClasses []) 
 

 
 (doseq [superClass (superclasses ontology1 "Class1")]
    ;(println "A super class:" superClass)
    ;(println (.getClassExpressionType superClass))
    (if  (= (.getClassExpressionType superClass) (ClassExpressionType/OBJECT_SOME_VALUES_FROM) )
      (do
	      ;(println "found:" superClass)
	      ;(println "property:" (.getFragment(.getIRI(.getProperty (cast OWLObjectSomeValuesFrom superClass)))))
;        (println "object:" ((.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass))))
        ;(println "object:" (.getFragment (.getIRI (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass))))))
        (let [class (.getFragment (.getIRI (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass)))))]
          (def  unionClasses (conj unionClasses  (owl-class ontology1 class)))
          )
        ;(def  unionClasses (conj unionClasses  (.getFragment (.getIRI (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass)))))))
           ;(print "union classes inner" unionClasses)
          
           
       
	      )
      ) 
 )
 
  (print "union classes" unionClasses)
  (owl-class ontology1 "Class1" :subclass (owl-only ontology2 "predicate2" (owl-or unionClasses) ))     

  
  
  (save-ontology ontology1 "ontology1.omn" :omn)
 ;(println "superclasses" (superclasses ontology1 "Class1"))
  
  
  
  )
