(ns synthbiont.rdf)

(import '(java.io FileInputStream File FileOutputStream))       
(import '(com.hp.hpl.jena.rdf.model Model ModelFactory ResourceFactory))
(import '(com.hp.hpl.jena.vocabulary RDF RDFS))

(import '(com.hp.hpl.jena.query QueryFactory QueryExecutionFactory Syntax Query))


(defn getRDFModel []
  (let [rdfFilePath "/home/goksel/work/sbolstack/DataFiles/BacillOndexPlus.rdf"
       RDF_BASE_URI "http://www.bacillondex.org"
       stream (FileInputStream. (File. rdfFilePath))
       model (ModelFactory/createDefaultModel)]               
  (.setNsPrefix model "" RDF_BASE_URI)
  (.read model stream (RDFS/getURI))
  ))


(defn executeSPARQLConstructQuery[model sparqlFilePath]
  (
    let [syntax (Syntax/syntaxARQ) 
         query (QueryFactory/read sparqlFilePath syntax)
         qe (QueryExecutionFactory/create query model)
         ]
    (println "Executing the query")
    ;(println query)
   (.execConstruct qe)  
 ))

;(defn mergeModels[model1 model2])

(defn save[model filePath]
  ;(.write model (new FileOutputStream (new File filePath)) "RDF/XML-ABBREV")
  (.write model (new FileOutputStream (new File filePath)) "Turtle")
  
  )


(defn addSPARQLConstructQueryResult[modelToAdd modelToQuery sparqlFilePath]
  (
    let [resultModel (executeSPARQLConstructQuery modelToQuery sparqlFilePath)]
    (if (.isEmpty resultModel)
      (println (concat sparqlFilePath  " did not return any results!"))
      (do
        (println "Adding query results")
        (.add modelToAdd resultModel)
        (println "Added the result")        
        ;(println (str "Saving the file " (str filePath)))        
        ;(save modelToAdd  filePath)
        ;(println "Saved the file")
       )
    )
 ))


(defn getRDFModel2 [rdfFilePath]
  (let [
       RDF_BASE_URI "http://www.bacillondex.org"
       stream (FileInputStream. (File. rdfFilePath))
       model (ModelFactory/createDefaultModel)]               
  (.setNsPrefix model "" RDF_BASE_URI)
  (println (str "Reading file from " rdfFilePath))
  (.read model stream (RDFS/getURI))
  ))



(defn test1[]
  (let [ model (getRDFModel2 "bacillondexsequenceclassesonly_withontology.rdf")       
        file "SBOLDnaComponents.sparql"
        result (executeSPARQLConstructQuery model file)
        ]
  (print "hello3")
   ;(print(executeSPARQLConstructQuery model file))
   ;(print (.isEmpty result))
   (print result)
   
  ))


(defn test2[]
  (def modelGlobal (getRDFModel2 "bacillondex_sequenceclasses_withontology.rdf"))  
  )

(defn test3[]
  (let [ file "SBOLDnaComponents2.sparql"
        result (executeSPARQLConstructQuery modelGlobal file)
        ]
  (println "Got the result")
   ;(print(executeSPARQLConstructQuery model file))
   ;(print (.isEmpty result))
   ;(println result)
   (println "Saving the result")   
   (save result "testdel.rdf")
   (println "done!")
   
   
  ))


