(ns net.wikipunk.d3fend
  (:require
   [com.stuartsierra.component :as com]
   [net.wikipunk.rdf]
   [net.wikipunk.rdf.rdf]
   [net.wikipunk.rdf.rdfs]
   [net.wikipunk.rdf.owl]
   [net.wikipunk.rdf.xsd]
   [net.wikipunk.rdf.dcterms]
   [net.wikipunk.rdf.d3f]
   [net.wikipunk.rdf.skos]))

(defrecord D3FEND []
  com/Lifecycle
  (start [this]    
    this)
  (stop [this]
    this))
