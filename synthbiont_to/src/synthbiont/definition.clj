(ns synthbiont.definition
      (:use [tawny.owl])
      (:use [synthbiont.param]) 
  (:require [tawny
             [polyglot]
             [reasoner :as r]
             [pattern :as p]]))
  
(defontology synthbiont
	 :iri "http://www.sybio.ncl.ac.uk"
	 :prefix "sybio:"
	 :comment "An ontology for synthetic biology"
	 :versioninfo "1.0"
  )  

(defdproperty NA)
(defdproperty RT)
(defdproperty conceptAccession)
(defoproperty has_part)
(defoproperty has_function)
(defoproperty encodes)
(defoproperty bi_to)
(defoproperty bound_by)

(defclass BioProc :equivalent (owl-class (iri (str GO_URI "#GO_0008150"))))
(defclass CelComp :equivalent (owl-class (iri (str GO_URI "#GO_0005575"))))
(defclass MolFunc :equivalent (owl-class (iri (str GO_URI "#GO_0003674"))))

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

(as-disjoint Operator Promoter)
; TODO Remove:
;(owl-class (iri(str "http://purl.org/obo/owl/GO#GO_0000156")))

;Operator Definitions
(defclass PositivelyRegulatedOperator
  :label "Positively regulated operator"
  :comment "Operator site for an activator"
  :equivalent (owl-and Operator
                       (owl-some NA  (iri(str "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")))
                       ( has-value RT "Positive")))

(defclass NegativelyRegulatedOperator
  :label "Negatively regulated operator"
  :comment "Operator site for an repressor"
  :equivalent (owl-and Operator
                       (owl-some NA  (iri(str "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")))
                       ( has-value RT "Negative")))
 
;Promoter Definitions
(defclass InduciblePromoter
 :label "Inducible promoter"
 :comment "Promoter with an activator site"
 :equivalent (owl-and Promoter
                     (exactly 1 has_part Operator)
                     (exactly 1 has_part PositivelyRegulatedOperator)))

(defclass RepressiblePromoter
 :label "Repressible promoter"
 :comment "Promoter with an repressor site"
 :equivalent (owl-and Promoter
                     (exactly 1 has_part Operator)
                     (exactly 1 has_part NegativelyRegulatedOperator)))

(defclass InduciblePromoterWith2Operators
 :label "Inducible promoter with 2 operators"
 :comment "Inducible promoter with two activator sites"
 :equivalent (owl-and Promoter
                     (exactly 2 has_part Operator)
                     (exactly 2 has_part PositivelyRegulatedOperator)))

(defclass RepressiblePromoterWith2Operators
 :label "Repressible promoter with 2 operators"
 :comment "Repressible promoter with two Repressible sites"
 :equivalent (owl-and Promoter
                     (exactly 2 has_part Operator)
                     (exactly 2 has_part NegativelyRegulatedOperator)))

(defclass RepressibleInduciblePromoterWith2Operators
 :label "Repressible inducible promoter with 2 operators"
 :comment "A promoter that has an activator and a repressor site"
 :equivalent (owl-and Promoter
                     (exactly 2 has_part Operator)
                     (exactly 1 has_part NegativelyRegulatedOperator)
                     (exactly 1 has_part PositivelyRegulatedOperator)))

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
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU25200")))
                              (owl-some has_part SigAPromoter))))

(defclass SigBPromoter
 :label "SigB promoter"
 :comment "SigB dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU04730")))
                              (owl-some has_part SigBPromoter))))

(defclass SigDPromoter
 :label "SigD promoter"
 :comment "SigD dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU16470")))
                              (owl-some has_part SigDPromoter))))

(defclass SigEPromoter
 :label "SigE promoter"
 :comment "SigE dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU15320")))
                              (owl-some has_part SigEPromoter))))

(defclass SigFPromoter
 :label "SigF promoter"
 :comment "SigF dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU23450")))
                              (owl-some has_part SigFPromoter))))

(defclass SigGPromoter
 :label "SigG promoter"
 :comment "SigG dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU15330")))
                              (owl-some has_part SigGPromoter))))

(defclass SigHPromoter
 :label "SigH promoter"
 :comment "SigH dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU00980")))
                              (owl-some has_part SigHPromoter))))

(defclass SigIPromoter
 :label "SigI promoter"
 :comment "SigI dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU13450")))
                              (owl-some has_part SigIPromoter))))
(defclass SigKPromoter
 :label "SigK promoter"
 :comment "SigK dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "P12254")))
                              (owl-some has_part SigKPromoter))))

(defclass SigLPromoter
 :label "SigL promoter"
 :comment "SigL dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU34200")))
                              (owl-some has_part SigLPromoter))))

(defclass SigMPromoter
 :label "SigM promoter"
 :comment "SigM dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU09520")))
                              (owl-some has_part SigMPromoter))))

(defclass SigWPromoter
 :label "SigW promoter"
 :comment "SigW dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU01730")))
                              (owl-some has_part SigWPromoter))))

(defclass SigXPromoter
 :label "SigX promoter"
 :comment "SigX dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU23100")))
                              (owl-some has_part SigXPromoter))))

(defclass SigVPromoter
 :label "SigV promoter"
 :comment "SigV dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU27120")))
                              (owl-some has_part SigVPromoter))))

(defclass SigYPromoter
 :label "SigY promoter"
 :comment "SigY dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU38700")))
                              (owl-some has_part SigYPromoter))))

(defclass SigZPromoter
 :label "SigZ promoter"
 :comment "SigZ dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU26840")))
                              (owl-some has_part SigZPromoter))))

(defclass YlaCPromoter
 :label "YlaC promoter"
 :comment "YlaC dependent promoter"
 :subclass Promoter
 :equivalent (owl-and Promoter
                      (owl-or (owl-some bound_by (owl-and TF (has-value conceptAccession "BSU14730")))
                              (owl-some has_part YlaCPromoter))))


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
                      (owl-not (owl-some has_part Operator))))

; CDS definitions
(defclass ResponseRegulatorEncodingCDS
 :label "Response regulator encoding CDS"
 :comment "Coding sequences that encode for response regulators"
 :equivalent (owl-and CDS
                     (owl-some encodes (owl-and Protein (owl-some has_function (iri(str "http://purl.org/obo/owl/GO#GO_0000156")))))                    
                     ;TODO: Remove
                     ;(owl-some encodes (owl-and Protein (owl-some has_function (owl-class (iri(str "http://purl.org/obo/owl/GO#GO_0000156"))))))                   
             )
 )

(defclass KinaseEncodingCDS
 :label "Kinase encoding CDS"
 :comment "Coding sequences that encode for kinases"
 :equivalent (owl-and CDS
                     (owl-some encodes (owl-and Protein (owl-some has_function (iri(str "http://purl.org/obo/owl/GO#GO_0000155")))))))

(defclass RepressorEncodingCDS
 :label "Repressor encoding CDS"
 :comment "Coding sequences that encode for transcriptional repressors"
 :equivalent (owl-and CDS
                     (owl-some encodes (owl-and Protein (owl-some bi_to NegativelyRegulatedOperator)))))

(defclass ActivatorEncodingCDS
 :label "Activator encoding CDS"
 :comment "Coding sequences that encode for transcriptional activators"
 :equivalent (owl-and CDS
                     (owl-some encodes (owl-and Protein (owl-some bi_to PositivelyRegulatedOperator)))))