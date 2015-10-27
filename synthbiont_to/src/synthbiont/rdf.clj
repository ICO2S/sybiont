(ns synthbiont.rdf)

(import '(java.io FileInputStream File FileOutputStream))       
(import '(com.hp.hpl.jena.rdf.model Model ModelFactory ResourceFactory))
(import '(com.hp.hpl.jena.vocabulary RDF RDFS))

(import '(com.hp.hpl.jena.query QueryFactory QueryExecutionFactory Syntax Query))

(defn executeSPARQLConstructQuery[model sparqlFilePath]
  (
    let [syntax (Syntax/syntaxARQ) 
         query (QueryFactory/read sparqlFilePath syntax)
         qe (QueryExecutionFactory/create query model)
         ]
    (println "Executing the query")
   (.execConstruct qe)  
 ))


(defn save[model filePath]
  ;(.write model (new FileOutputStream (new File filePath)) "RDF/XML-ABBREV")
  (.write model (new FileOutputStream (new File filePath)) "Turtle")
  (println "Wrote the model")  
  )


(defn addSPARQLConstructQueryResult[modelToAdd modelToQuery sparqlFilePath]
  (
    let [resultModel (executeSPARQLConstructQuery modelToQuery sparqlFilePath)]
    (if (.isEmpty resultModel)
      (println (concat sparqlFilePath  " did not return any results!"))
      (do
        (println "Adding query results")
        (.add modelToAdd resultModel)
       )
    )
 ))


(defn getRDFModel [rdfFilePath]
  (let [
       RDF_BASE_URI "http://www.bacillondex.org"
       stream (FileInputStream. (File. rdfFilePath))
       model (ModelFactory/createDefaultModel)]               
  (.setNsPrefix model "" RDF_BASE_URI)
  (println (str "Reading file from " rdfFilePath))
  (.read model stream (RDFS/getURI))
  ))
