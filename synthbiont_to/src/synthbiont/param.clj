(ns synthbiont.param)

; ontology URIs
(def ONTOLOGY_URI "http://www.sybio.ncl.ac.uk")
(def ONTOLOGY_DATA_URI "http://www.bacillondex.org")
(def GO_URI "http://purl.org/obo/owl/GO")
(def SO_URI "http://purl.org/obo/owl/SO")
(def SBOL_URI "http://sbols.org/sbol.owl")




;Namespaces
(def conceptNS (str ONTOLOGY_DATA_URI  "/concept/"))
(def relationTypeNS (str ONTOLOGY_DATA_URI  "/relationType/"))
(def attributeNS (str ONTOLOGY_DATA_URI  "/attributeName/"))
(def evidenceTypeNS (str ONTOLOGY_DATA_URI  "/evidenceType/"))
(def cvNS (str ONTOLOGY_DATA_URI  "/cv/"))
(def ondexcoreNS "http://ondex.sourceforge.net/ondex-core#")

;RDF Data Property definitions
(def RDFS_COMMENT_PROPERTIES (hash-set "description" "annotation" "comment" "conceptDescription" "Comment"))
(def RDFS_LABEL_PROPERTIES (hash-set "title" "conceptName"))
(def ANNOTATION_PROPERTIES (hash-set "evidence" "elementOf" "TAXID" "annotatedsequence" "originalannotatedsequence" "AA" "URL" "operongenes" "BEGIN" "END" "ReadingFrame" "BM" "Control" "Target" "DELTAGO" "EndOfBindingSite" "Length" "StartOfBindingsite" "UnnormalizedMaxExpression" "UnnormalizedMinExpression" "COGProcess" "title" "conceptName" "description" "annotation" "comment" "conceptDescription" "Comment"))
(def PROPERTIES_TO_IGNORE (hash-set "identifier" "pid"))
(def GO_CLASSES (hash-set "MolFunc" "BioProc" "CelComp"))
(def SUBCLASS_RELATION (hash-set "is_a"))
(def SAMECLASS_RELATION (hash-set "equ"))

;New Class definitions
(def EVIDENCE_TYPE_PARENT_CLASS "EvidenceType")
(def DATA_SOURCE_PARENT_CLASS "DataSource")

(def INVERSE_PROPERTIES {"part_of" "hasPart", "en_by" "encodes", "equ" "equ","bi_to" "boundBy"})  
(def HAS_PART "hasPart")

(def CLASS_MAPPINGS {"BioProc" "BiologicalProcess", "MolFunc" "MolecularFunction", "CelComp" "CellularCompartment", 
                     "Comp" "Compound", "MicroArrayExperiment" "MicroarrayExperiment","Path" "Pathway", "Protcmplx" "ProteinComplex"})

(def PROPERTY_MAPPINGS {"ac_by" "activatedBy", 
                        "adjacent_to" "adjacentTo", 
                        "bi_to" "bindsTo", 
                        "ca_by" "catalyzedBy", 
                        "cat_c" "catalyzingClass",
                        "cs_by" "consumedBy", 
                        "demethylated_by" "demethylatedBy",
                        "dephosphorylated_by" "dephosphorylatedBy",
                        "derives_from" "derivesFrom",
                        "di_fr" "dissociatedFrom",
                        "en_by" "encodedBy",
                        "ex_by" "expressedBy",
                        "has_part" "hasPart",
                        "has_participant" "hasParticipant",
                        "in_by" "inhibitedBy",
                        "isfirstgeneof" "firstGeneOf",
                        "issecondgeneof" "secondGeneOf",
                        "isthirdgeneof" "thirdGeneOf",
                        "it_wi" "interactsWith",
                        "located_in" "locatedIn",
                        "max_expressed_in" "maxExpressedIn",
                        "member_of" "memberOf",
                        "methylated_by" "methylatedBy",
                        "min_expressed_in" "minExpressedIn",
                        "part_of" "partOf",
                        "pd_by" "producedBy",
                        "phosphorylated_by" "phosphorylatedBy",
                        "r" "relatedTo",
                        "rg_by" "regulatedBy",
                        "sh_im" "sharesIntermediate",
                        "terminates" "terminates",
                        "transcribes_to" "transcribesTo",
                        "upstream" "upstreamOf",                        
                        "has_function" "hasFunction",   
                        ;Data properties
                        "BioProcess" "biologicalProcess",
                        "conceptAccession" "accession",
                        "ffltype" "fflType",
                        "GC3Content" "gc3Content",
                        "GCContent" "gcContent",
                        "is_fCDS" "fCDS",
                        "LOC" "localisation",
                        "MaxExpression" "maxExpression",
                        "MinExpression" "minExpression",
                        "MW" "molecularWeight",
                        "negativefeedback" "hasNegativeFeedback",
                        "positivefeedback" "hasPositiveFeedback", 
                        "PMID" "pmid", 
                        "ProductType" "productType",
                        "RoleClassification" "hasRole",
                        "RT" "regulationType",
                        "SF" "sigmaFactor",
                        "StartOfBindingSite" "tfbsStart",
                        "TFDomain" "tfDomain",
                        "TFFamily" "tfFamily",
                        ;Annotations
                        "annotatedsequence" "annotatedSequence",
                        "BEGIN" "begin",
                        "BM" "bindingMotif",
                        "COGProcess" "cogProcess",
                        "Control" "controlExperiment",
                        "DELTAGO" "deltaGo",
                        "END" "end",
                        "EndOfBindingSite" "tfbsEnd",
                        "Length" "length",
                        "operongenes" "operonGenes",
                        "originalannotatedsequence" "originalAnnotatedSequence",
                        "ReadingFrame" "readingFrame",
                        "Target" "targetExperiment",
                        "TAXID" "taxId",
                        "UnnormalizedMaxExpression" "unnormalizedMaxExpression",
                        "UnnormalizedMinExpression" "unnormalizedMinExpression",
                        "URL" "url"
                        })


    