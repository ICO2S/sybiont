(ns synthbiont.rdf)

(import '(java.io FileInputStream File))       
(import '(com.hp.hpl.jena.rdf.model Model ModelFactory ResourceFactory))
(import '(com.hp.hpl.jena.vocabulary RDF RDFS))

(defn getRDFModel []
  (let [rdfFilePath "/home/goksel/work/sbolstack/DataFiles/BacillOndexPlus.rdf"
       RDF_BASE_URI "http://www.bacillondex.org"
       stream (FileInputStream. (File. rdfFilePath))
       model (ModelFactory/createDefaultModel)]               
  (.setNsPrefix model "" RDF_BASE_URI)
  (.read model stream (RDFS/getURI))
  ))
