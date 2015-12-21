(ns sybiont_generator.definition
      (:use [tawny.owl])
      (:use [sybiont_generator.param]) 
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))
  
(defontology synthbiont
	 :iri "http://w3id.org/synbio/ont"
	 :prefix "sybio:"
	 :comment "An ontology for synthetic biology"
	 :versioninfo "1.0"
  )  

(defdproperty NA)
(defdproperty regulationType)
(defdproperty accession)
(defoproperty hasPart)
(defoproperty hasFunction)
(defoproperty encodes)
(defoproperty bindsTo)
(defoproperty boundBy)

(defclass BiologicalProcess :equivalent (owl-class (iri (str GO_URI "#GO_0008150"))))
(defclass CellularComponent :equivalent (owl-class (iri (str GO_URI "#GO_0005575"))))
(defclass MolecularFunction :equivalent (owl-class (iri (str GO_URI "#GO_0003674"))))

(defclass Promoter :subclass (owl-class (iri (str SO_URI "#SO_0000167"))))
(defclass CDS :subclass (owl-class (iri (str SO_URI "#SO_0000316"))))
(defclass Terminator :subclass (owl-class (iri (str SO_URI "#SO_0000614"))))
(defclass RBS :subclass (owl-class (iri (str SO_URI "#SO_0000139"))))
(defclass Shim :subclass (owl-class (iri (str SO_URI "#SO_0000997"))))
(defclass Operator :subclass (owl-class (iri (str SO_URI "#SO_0000057"))))
(defclass Operon :subclass (owl-class (iri (str SO_URI "#SO_0000178"))))
(defclass RNA :subclass (owl-class (iri (str SO_URI "#SO_0000356"))))

(defclass Protein)
(defclass TF)
(defclass DataSource :label "DataSource" :comment "Data source, such as a biological database" )
(defclass EvidenceType :label "EvidenceType" :comment "Evidence type, such as electronically inferred or imported from a database")




(owl-class (iri (str SBOL_URI "#DnaComponent")))
(owl-class (iri (str SBOL_URI "#SequenceAnnotation")))
(owl-class (iri (str SBOL_URI "#DnaSequence")))

(as-disjoint Operator Promoter)
; TODO Remove:
;(owl-class (iri(str "http://purl.org/obo/owl/GO#GO_0000156")))

;Operator Definitions
(defclass PositivelyRegulatedOperator
  :label "Positively regulated operator"
  :comment "Operator site for an activator"
  :equivalent (owl-and Operator
                       (owl-some NA  (iri(str "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")))
                       ( has-value regulationType "Positive")))

(defclass NegativelyRegulatedOperator
  :label "Negatively regulated operator"
  :comment "Operator site for an repressor"
  :equivalent (owl-and Operator
                       (owl-some NA  (iri(str "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")))
                       ( has-value regulationType "Negative")))
 
;Promoter Definitions
(defclass InduciblePromoter
 :label "Inducible promoter"
 :comment "Promoter with an activator site"
 :equivalent (owl-and Promoter
                     (exactly 1 hasPart Operator)
                     (exactly 1 hasPart PositivelyRegulatedOperator)))

(defclass RepressiblePromoter
 :label "Repressible promoter"
 :comment "Promoter with an repressor site"
 :equivalent (owl-and Promoter
                     (exactly 1 hasPart Operator)
                     (exactly 1 hasPart NegativelyRegulatedOperator)))

(defclass InduciblePromoterWith2Operators
 :label "Inducible promoter with 2 operators"
 :comment "Inducible promoter with two activator sites"
 :equivalent (owl-and Promoter
                     (exactly 2 hasPart Operator)
                     (exactly 2 hasPart PositivelyRegulatedOperator)))

(defclass RepressiblePromoterWith2Operators
 :label "Repressible promoter with 2 operators"
 :comment "Repressible promoter with two Repressible sites"
 :equivalent (owl-and Promoter
                     (exactly 2 hasPart Operator)
                     (exactly 2 hasPart NegativelyRegulatedOperator)))

(defclass RepressibleInduciblePromoterWith2Operators
 :label "Repressible inducible promoter with 2 operators"
 :comment "A promoter that has an activator and a repressor site"
 :equivalent (owl-and Promoter
                     (exactly 2 hasPart Operator)
                     (exactly 1 hasPart NegativelyRegulatedOperator)
                     (exactly 1 hasPart PositivelyRegulatedOperator)))

(defclass ANDGatePromoter
 :label "AND gate promoter"
 :comment "AND gate promoter"
 :subclass InduciblePromoterWith2Operators
 )

(defclass ORGatePromoter
 :label "OR gate promoter"
 :comment "OR gate promoter"
 :subclass InduciblePromoterWith2Operators
 )

(defclass NORGatePromoter
 :label "NOR gate promoter"
 :comment "NOR gate promoter"
 :subclass RepressiblePromoterWith2Operators
 )

(defclass NANDGatePromoter
 :label "NAND gate promoter"
 :comment "NAND gate promoter"
 :subclass RepressiblePromoterWith2Operators
 )

(defclass ANDNGatePromoter
 :label "ANDN gate promoter"
 :comment "ANDN gate promoter"
 :subclass RepressibleInduciblePromoterWith2Operators
 )

; Promoter definitions for Sigma factors
(defclass SigAPromoter)
(defclass SigBPromoter)
(defclass SigDPromoter)
(defclass SigEPromoter)
(defclass SigFPromoter)
(defclass SigGPromoter)
(defclass SigHPromoter)
(defclass SigIPromoter)
(defclass SigKPromoter)
(defclass SigLPromoter)
(defclass SigMPromoter)
(defclass SigWPromoter)
(defclass SigXPromoter)
(defclass SigVPromoter)
(defclass SigYPromoter)
(defclass SigZPromoter)
(defclass YlaCPromoter)

(defclass SigAPromoter
 :label "SigA promoter"
 :comment "SigA dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU25200")))
                              (owl-some hasPart SigAPromoter))))

(defclass SigBPromoter
 :label "SigB promoter"
 :comment "SigB dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU04730")))
                              (owl-some hasPart SigBPromoter))))

(defclass SigDPromoter
 :label "SigD promoter"
 :comment "SigD dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU16470")))
                              (owl-some hasPart SigDPromoter))))

(defclass SigEPromoter
 :label "SigE promoter"
 :comment "SigE dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU15320")))
                              (owl-some hasPart SigEPromoter))))

(defclass SigFPromoter
 :label "SigF promoter"
 :comment "SigF dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU23450")))
                              (owl-some hasPart SigFPromoter))))

(defclass SigGPromoter
 :label "SigG promoter"
 :comment "SigG dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU15330")))
                              (owl-some hasPart SigGPromoter))))

(defclass SigHPromoter
 :label "SigH promoter"
 :comment "SigH dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU00980")))
                              (owl-some hasPart SigHPromoter))))

(defclass SigIPromoter
 :label "SigI promoter"
 :comment "SigI dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU13450")))
                              (owl-some hasPart SigIPromoter))))
(defclass SigKPromoter
 :label "SigK promoter"
 :comment "SigK dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "P12254")))
                              (owl-some hasPart SigKPromoter))))

(defclass SigLPromoter
 :label "SigL promoter"
 :comment "SigL dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU34200")))
                              (owl-some hasPart SigLPromoter))))

(defclass SigMPromoter
 :label "SigM promoter"
 :comment "SigM dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU09520")))
                              (owl-some hasPart SigMPromoter))))

(defclass SigWPromoter
 :label "SigW promoter"
 :comment "SigW dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU01730")))
                              (owl-some hasPart SigWPromoter))))

(defclass SigXPromoter
 :label "SigX promoter"
 :comment "SigX dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU23100")))
                              (owl-some hasPart SigXPromoter))))

(defclass SigVPromoter
 :label "SigV promoter"
 :comment "SigV dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU27120")))
                              (owl-some hasPart SigVPromoter))))

(defclass SigYPromoter
 :label "SigY promoter"
 :comment "SigY dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU38700")))
                              (owl-some hasPart SigYPromoter))))

(defclass SigZPromoter
 :label "SigZ promoter"
 :comment "SigZ dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU26840")))
                              (owl-some hasPart SigZPromoter))))

(defclass YlaCPromoter
 :label "YlaC promoter"
 :comment "YlaC dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some boundBy (owl-and TF (has-value accession "BSU14730")))
                              (owl-some hasPart YlaCPromoter))))


(defclass ConstitutivePromoter
 :label "Constitutive promoter"
 :comment "Constitutive promoter"
 :subclass Promoter
 )

(defclass ConstitutiveSigAPromoter
 :label "Constitutive SigA promoter"
 :comment "Constitutive SigA promoter"
 :subclass SigAPromoter ConstitutivePromoter
 :equivalent (owl-and SigAPromoter
                      (owl-not (owl-some hasPart Operator))))

; CDS definitions
(defclass ResponseRegulatorEncodingCDS
 :label "Response regulator encoding CDS"
 :comment "Coding sequences that encode for response regulators"
 :equivalent (owl-and CDS
                     (owl-some encodes (owl-and Protein (owl-some hasFunction (iri(str "http://purl.org/obo/owl/GO#GO_0000156")))))                    
                     ;(owl-some encodes (owl-and Protein (owl-some hasFunction (owl-class (iri(str "http://purl.org/obo/owl/GO#GO_0000156"))))))                   
             ))

(defclass KinaseEncodingCDS
 :label "Kinase encoding CDS"
 :comment "Coding sequences that encode for kinases"
 :equivalent (owl-and CDS
                     (owl-some encodes (owl-and Protein (owl-some hasFunction (iri(str "http://purl.org/obo/owl/GO#GO_0000155")))))))

(defclass RepressorEncodingCDS
 :label "Repressor encoding CDS"
 :comment "Coding sequences that encode for transcriptional repressors"
 :equivalent (owl-and CDS
                     (owl-some encodes (owl-and Protein (owl-some bindsTo NegativelyRegulatedOperator)))))

(defclass ActivatorEncodingCDS
 :label "Activator encoding CDS"
 :comment "Coding sequences that encode for transcriptional activators"
 :equivalent (owl-and CDS
                     (owl-some encodes (owl-and Protein (owl-some bindsTo PositivelyRegulatedOperator)))))