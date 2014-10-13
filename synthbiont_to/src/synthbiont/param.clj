(ns synthbiont.param)

; ontology URIs
(def ONTOLOGY_URI "http://www.sybio.ncl.ac.uk")
(def ONTOLOGY_DATA_URI "http://www.bacillondex.org")
(def GO_URI "http://purl.org/obo/owl/GO")
(def SO_URI "http://purl.org/obo/owl/SO")



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
    