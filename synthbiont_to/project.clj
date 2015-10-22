(defproject synthbiont_to "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.apache.jena/jena-core "2.11.1"]
                 ;[org.apache.jena/jena-core "3.0.0"]
                 [org.apache.jena/jena-tdb "1.0.1"]
                 ;[org.apache.jena/jena-tdb "3.0.0"]
                 [uk.org.russet/tawny-owl "1.1.0"]
                 ;;[net.sourceforge.owlapi/owlapi-distribution "4.0.0"]
                 [net.sourceforge.owlapi/owlapi-distribution "3.5.1"]
                 
                                  ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.4.1"]
                 [com.hermit-reasoner/org.semanticweb.hermit "1.3.8.4"]
                 [net.sourceforge.owlapi/jfact "1.2.2"]
                 ]
   
  )
