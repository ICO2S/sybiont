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
(import '(org.semanticweb.owlapi.util SimpleIRIMapper NamespaceUtil DefaultPrefixManager))
(import '(org.coode.owlapi.manchesterowlsyntax ManchesterOWLSyntaxOntologyFormat))



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

(defn addAnnotation [owlClass predicateOntology predicate predicateURI value]
 (if (contains? RDFS_COMMENT_PROPERTIES  predicate)
   (addComment owlClass value)
   (do
     (if (contains? RDFS_LABEL_PROPERTIES  predicate)
		   (addLabel owlClass value)
       (do
          (addAnnotationProperty owlClass predicateOntology predicate value)
          (def annotationPropertyUris (conj annotationPropertyUris  predicateURI));GMGMGM
        ))       
     )   
 ))

(defn addLiteralPropertyToClass [stmt owlClass]
  (let [predicateName (str (.getLocalName(.getPredicate stmt)))
        objectValue (str (.toString (.getValue (.asLiteral (.getObject stmt)))))
         predicateURI (.getURI (.getPredicate stmt))
         ]
  (if-not (contains? PROPERTIES_TO_IGNORE  predicateName)
    (if (contains? ANNOTATION_PROPERTIES  predicateName)
      (addAnnotation owlClass synthbiont predicateName predicateURI objectValue) 
      ( do        
        (def datatypePropertyUris (conj datatypePropertyUris  predicateURI))
        (addDatatypeProperty owlClass synthbiont predicateName objectValue) 
      )
    )
  )
  ))

(defn handleMetaResource [ontology resource parentOntology parentClass]
  (let [typeId (getID (.getURI resource))
        typeResourceProperties (iterator-seq (.listProperties resource))
        ]  
    ;(println "In handle meta resource")
  (owl-class ontology typeId)  
  (if-not (nil? parentClass)
    (owl-class ontology typeId :subclass (owl-class parentOntology parentClass)))
  (doseq [stmt typeResourceProperties] 
     (if (.isLiteral (.getObject stmt))
       (do
         ;(print "Adding the literal...")
         (addLiteralPropertyToClass  stmt (owl-class ontology typeId)))
         ;(println "done!") 
       )
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

  (defn handleInverseProperty [classIri valueClassIri predicateOntology predicate]
    (let [inversePredicate (.get INVERSE_PROPERTIES predicate)]
        (if-not (nil? inversePredicate)
          (do
	          ;(println "inverseProperty:" inversePredicate)
            (as-inverse
	              (object-property predicateOntology predicate)
	              (object-property predicateOntology inversePredicate))          
               ;(owl-class classIri :subclass (owl-some predicateOntology predicate (owl-class valueClassIri)))
               (owl-class valueClassIri :subclass (owl-some predicateOntology predicate (owl-class classIri)))
               
  		      
             (if (= inversePredicate "has_part")
               (do
                 (owl-class valueClassIri :subclass ( exactly predicateOntology 1 inversePredicate (owl-class classIri) ))
                 ;TODO Enhance this
                 (owl-class valueClassIri :subclass ( owl-some predicateOntology inversePredicate (owl-class classIri) ))      
                )
               )                 
          ))
    ))         
            
(defn handleRelationStatement [stmt owlClass]
  (let [predicate (.getLocalName (.getPredicate stmt))
        valueResource (.getResource model (.getURI (.asResource (.getObject stmt))))
        valueResourceId (getID (.getURI valueResource))
        valueTypeResource (.asResource (.getObject (.getProperty valueResource (RDF/type))))
        valueType (getID (.getURI valueTypeResource))
        valueClass (addOntologyClass valueResource valueType)
        ]
        ;(print "valueClass:" valueClass)
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
        predicateUri (.getURI (.getPredicate stmt))        
        ]
    (if (.startsWith resourceUri evidenceTypeNS)
      (handleMetaResource bacillondex resource synthbiont EVIDENCE_TYPE_PARENT_CLASS))
    (if (.startsWith resourceUri cvNS)
        (handleMetaResource bacillondex resource synthbiont DATA_SOURCE_PARENT_CLASS))    
    (if (contains? ANNOTATION_PROPERTIES predicate)
      (addAnnotation owlClass synthbiont predicate predicateUri resourceUri)
      (print "TODO: Handle core resources that are not annotations")
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
    ;(handleMetaResource synthbiont typeResource nil  nil)
    (def resourceTypeUris (conj resourceTypeUris  (.getURI typeResource)))
    (let [owlClass (addOntologyClass resource type)]
      (doseq [stmt (iterator-seq (.listProperties resource))] 
        (handleStatement stmt owlClass)      
        )
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
   ;(try
   ;  (
   (doseq [typeUri resourceTypeUris]
       (handleMetaResource synthbiont (.getResource model typeUri) nil  nil))
   (println "done!")
   ;)
    ; (catch Exception e (str "caught exception: " (.getMessage e) (.printStackTrace e))))
   
   
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
        suffixIndex (.lastIndexOf (str classIri) "#")
        ]
    (subs (str classIri) (+ suffixIndex 1))  
    )
  )

(defn addDisjointAxioms[ontology classOntology class propertyOntology property targetClassOntology targetClass]
 ( def  unionClasses []) 
 (let [classIri (iri (str (.toString (.getOntologyIRI (.getOntologyID classOntology))) "#" class))]
 (doseq [subClass (subclasses ontology classIri)]   
   ;(println "subclass" subClass)
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
   ;(println "-----union classes:" unionClasses)
   (if (>= (count unionClasses) 2)
       (as-disjoint unionClasses)   
    )
    ( def  unionClasses [])   
   )))

(defn addClosureAxiomsToSuperClass[ontology classOntology class propertyOntology property]
 ;(println "In addClosureAxiomsToSuperClass")
 ( def  unionClasses []) 
 (let [classIri (iri (str (.toString (.getOntologyIRI (.getOntologyID classOntology))) "#" class))]
 (doseq [subClass (subclasses ontology classIri)]   
   ;(println "subclass:"  (getClassIri ontology subClass))
   (doseq [superClass (superclasses ontology subClass)]
       (if  (= (.getClassExpressionType superClass) (ClassExpressionType/OBJECT_SOME_VALUES_FROM) )
         (do   
           ;(println "SomeValuesExpression:")
         (if (= property (.getFragment(.getIRI(.getProperty (cast OWLObjectSomeValuesFrom superClass)))))
           (do  
             ;(println "Adding the union class")
             (def  unionClasses (conj unionClasses  (owl-class ontology (getIriFragment (.asOWLClass (.getFiller (cast OWLObjectSomeValuesFrom superClass)))))))                      
           )
           )
         )
          )
	  )
   (if-not (empty? unionClasses)
     (do 
       ;(println unionClasses)
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
 ;(owl-import bacillondex (iri(File."synthbiont.omn")))
 ;(owl-import bacillondex (iri(str "file:synthbiont.omn"))) 
 
 ;(save-ontology bacillondex "bacillondexontology.rdf" :rdf)   
 (save-ontology bacillondex "bacillondexontology.omn" :omn)   
 
)

(defn getclass[o cls]
  (owl-class o cls)
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
 (owl-class ontology1 "Class1"
            :label "Class 1"
            ) 
 ; (owl-class ontology2 "Class2")
 (owl-class ontology2 "Class2" 
            :label "Class 2"
            :subclass (owl-class ontology1 "Class1"))

 ;((getclass ontology2 "Class3") :label (str "Class 3"))
 (let [owlclass (getclass ontology2 "Class3")]
   ;(owlclass :label "Class 3")
   ;((cast owl-class owlclass) :label "Class 3")
   (owl-class (.getIRI (getclass ontology2 "Class3")) :label "Class 3")
   )
 

 
;(let [ 
;      iriMapper1 (SimpleIRIMapper. (iri(str "http://www.ontology1.org")) (iri(str "ontology1.omn")))
;      iriMapper2 (SimpleIRIMapper. (iri(str "http://www.ontology2.org")) (iri(str "ontology2.omn")))      
;      ]

;(.removeIRIMapper (owl-ontology-manager) iriMapper1)   
;(.removeIRIMapper (owl-ontology-manager) iriMapper2)   
  
(save-ontology ontology1 "ontology1.omn" :omn)
;(save-ontology ontology1 "ontology1.rdf" :rdf)
;(save-ontology ontology1 "ontology1.oml" :owl)

;(owl-import ontology1)
;(owl-import ontology2 (iri(str "ontology1.omn")))
(owl-import ontology2 (iri(File."ontology1.omn")))

;(.addIRIMapper (owl-ontology-manager) (SimpleIRIMapper. (iri(str "http://www.ontology1.org")) (iri(str "ontology1.omn"))))
;(.setPrefix (NamespaceUtil.) (str "http://www.ontology1.org") (str "o1"))
;(.setPrefix (NamespaceUtil.) (str "http://www.ontology3.org") (str "o3"))
;(.setPrefix (.getOntologyFormat (owl-ontology-manager) ontology2) "o1" "http://www.ontology1.org")
;(.addPrefixes (DefaultPrefixManager.) (.setPrefix (ManchesterOWLSyntaxOntologyFormat.) "http://www.ontology1.org" "o1"))

(save-ontology ontology2 "ontology2.omn" :omn)
;(save-ontology ontology2 "ontology2.rdf" :rdf)
;(save-ontology ontology2 "ontology2.owl" :owl)

;(.addIRIMapper (owl-ontology-manager) iriMapper)     
 ;)
)
