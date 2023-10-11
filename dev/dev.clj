(ns dev
  "Tools for interactive development with the REPL. This file should
  not be included in a production build of the application.
  Call `(reset)` to reload modified code and (re)start the system.
  The system under development is `system`, referred from
  `com.stuartsierra.component.repl/system`.
  See also https://github.com/stuartsierra/component.repl"
  (:require
   [arachne.aristotle :as a]
   [arachne.aristotle.graph :as g]
   [clj-http.client :as http]
   [clojure.data.csv :as csv]
   [clojure.data.json :as json]
   [clojure.datafy :refer [datafy]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.javadoc :refer [javadoc]]
   [clojure.pprint :refer [pprint pp]]
   [clojure.reflect :refer [reflect]]
   [clojure.repl :refer [apropos dir find-doc pst source]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.tools.namespace.repl :refer [refresh refresh-all clear]]
   [clojure.walk :as walk]
   [com.stuartsierra.component :as com]
   [com.stuartsierra.component.repl :refer [reset set-init start stop system]]
   [com.walmartlabs.schematic :as sc]
   [datomic.client.api :as d]
   [net.wikipunk.boot]
   [net.wikipunk.ext]
   [net.wikipunk.mop :as mop]
   [net.wikipunk.rdf :as rdf :refer [doc]]
   [net.wikipunk.datomic.boot :as db]
   [net.wikipunk.qdrant :as qdrant]
   [net.wikipunk.openai :as openai]
   [zprint.core :as zprint]
   [asami.core :as asami]
   [michelangelo.core :as ma]
   [donatello.ttl :as ttl]
   [wkok.openai-clojure.api :as api]))

(set-init
  (fn [_]
    (set! *print-namespace-maps* nil)
    (if-let [r (io/resource "system.edn")]
      (-> (slurp r)
          (edn/read-string)
          (sc/assemble-system))
      (throw (ex-info "system.edn is not on classpath" {})))))

(defmacro inspect
  "Evaluate forms in an implicit do and inspect the value of the last
  expression using Reveal."
  [& body]
  `(do (@user/reveal (do ~@body))
       true))

(defn base
  "Returns the base of the IRI."
  [k]
  (str/replace-first (rdf/iri k) (re-pattern (str (name k) "$")) ""))

(defn keywords
  "Returns keywords used in the metaobject."
  [form]
  (cond
    (qualified-keyword? form)
    #{form}

    (coll? form)
    (reduce set/union (map keywords form))

    :else #{}))

(defn prefixes
  "Return prefixes for metaobject."
  [mo]
  (reduce (fn [m k]
            (let [prefix (namespace k)]
              (if (contains? m prefix)
                m
                (assoc m prefix (base k)))))
          {} (keywords mo)))

(def datatypes
  (descendants :rdfs/Datatype))

(defn datafy-ttl
  "datafy the ident for turtle"
  [x]
  (if (qualified-keyword? x)
    (recur (datafy x))
    (walk/postwalk (fn [form]
                     (cond
                       (and (:rdf/type form) (:rdf/value form))
                       (ttl/->TypedLiteral (:rdf/value form) (:rdf/type form))
                       (and (:rdf/value form) (:rdf/language form))
                       (ttl/->LangLiteral (:rdf/value form) (:rdf/language form))
                       (and (:xsd/anyURI form) (== (count form) 1))
                       (java.net.URI/create (:xsd/anyURI form))
                       (and (:rdf/value form) (== (count form) 1))
                       (:rdf/value form)
                       (and (map? form)
                            (== (count form) 1)
                            (some datatypes (keys form)))
                       (ttl/->TypedLiteral (first (vals form)) (first (keys form)))
                       :else form))
                   x)))

(defn print-ttl
  [x]
  (let [mo (datafy-ttl x)]
    (ttl/write-prefixes! *out* (dissoc (prefixes mo) "db"))
    (ttl/write-triples! *out* (:db/ident mo) (dissoc mo :db/ident))))

(def system-message
  {:role    "system",
   :content "You are an RDF Turtle generator and creative wizard of the metaobject protocol on the Semantic Web. Your task is to materialize RDF Turtle for a single resource at a time, based on the prefixes provided by the user. Employ these prefixes to craft valid RDF Turtle descriptions of the requested resources for the user, weaving in relevant relationships, properties, and its semantics using OWL, the Web Ontology Language. Ensure that when your response is concatenated with the provided prefixes, it forms syntactically correct RDF Turtle that can be successfully parsed. While your creativity is valued, ensure you do not include predicates you do not fully understand in your responses, or repeat prefixes, or stray from RDF Turtle. No markdown or explanatory text should be included in your responses."})

(def d3f-prefixes
  {"d3f"     "http://d3fend.mitre.org/ontologies/d3fend.owl#",
   "dcterms" "http://purl.org/dc/terms/",
   "owl"     "http://www.w3.org/2002/07/owl#",
   "rdf"     "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
   "rdfs"    "http://www.w3.org/2000/01/rdf-schema#",
   "skos"    "http://www.w3.org/2004/02/skos/core#",
   "xsd"     "http://www.w3.org/2001/XMLSchema#"})

(def d3f-prefixes-message
  {:role    "user"
   :content (with-out-str (ttl/write-prefixes! *out* d3f-prefixes))})

(defn example
  [x]
  (let [mo (datafy-ttl x)
        id (or (:db/ident mo) (some-> (:xsd/anyURI mo) (java.net.URI/create)))]
    [system-message
     {:role    "user"
      :content (with-out-str (ttl/write-prefixes! *out* (dissoc (prefixes mo) "db")))}
     {:role    "user"
      :content (ttl/serialize id)}
     {:role    "assistant"
      :content (with-out-str (ttl/write-triples! *out* id (dissoc mo :db/ident :xsd/anyURI)))}]))

(defn examples
  [asami]
  (->> (asami/q '[:find ?ident
                  :where
                  [?e :db/ident ?ident]]
                asami)
       (pmap (fn [[ident]]
               (example ident)))
       (into [])))

(defn d3f
  [asami]
  (->> (asami/q '[:find ?ident
                  :where
                  [?e :db/ident ?ident]
                  [(namespace ?ident) ?ns]
                  [(= ?ns "d3f")]]
                asami)
       (pmap (fn [[ident]]
               (example ident)))
       (into [])))

(defmethod ttl/serialize java.math.BigDecimal [x] (str x))
(defmethod ttl/serialize clojure.lang.BigInt [x] (str x))

(defn chat
  "Chat with the OpenAI API."
  [& {:keys [model] :as params}]
  (api/create-chat-completion (assoc params :model (or model "gpt-3.5-turbo"))))

(defn test-example
  ([model messages md]
   (test-example model messages md 3 250)) ; Default limit set to 3, initial delay to 250ms
  ([model messages md limit delay]
   (when (pos? limit)
     (let [[system prefixes qname resource] messages
           {:keys [choices usage] :as response}   (chat :model model :messages messages)
           ttl                              (str (:content prefixes)
                                                 (:content (:message (nth choices 0))))]
       (try
         (assoc (dissoc response :choices)
                :output   (rdf/unroll-forms (rdf/parse (assoc md :rdf/value ttl :format :ttl)))
                :retries-left    limit
                :messages (conj messages (:message (nth choices 0))))
         (catch Throwable ex
           (Thread/sleep delay)
           (test-example model
                         (conj messages
                               (:message (nth choices 0))
                               {:role "user"
                                :content (str "Your response was not valid RDF Turtle. Please try again. The error message received was: " (.getMessage ex))})
                         md
                         (dec limit)
                         (* 2 delay)))))))) ; Double the delay for the next iteration

(defn write-jsonl
  [data file-path]
  (with-open [writer (io/writer file-path)]
    (doseq [messages data]
      (.write writer (json/write-str {:messages messages}))
      (.write writer "\n"))))

(comment
  (http/post "https://api.openai.com/v1/files"
             {:multipart   [{:name "purpose" :content "fine-tune"}
                            {:name "file" :content (io/file "resources/net/wikipunk/d3fend/d3fend.jsonl")}]
              :oauth-token (System/getenv "OPENAI_API_KEY")})
  (http/post "https://api.openai.com/v1/fine_tuning/jobs"
             {:form-params  {:training_file "USE THE FILE ID YOU GET FROM ABOVE"
                             :model         "gpt-3.5-turbo-0613"}
              :content-type :json
              :oauth-token  (System/getenv "OPENAI_API_KEY")}))

(defonce ^org.apache.commons.math3.distribution.NormalDistribution
  +normal-distribution+
  (org.apache.commons.math3.distribution.NormalDistribution.))

(defn z-value
  "Returns the z-value for a given confidence level."
  ^double [^double confidence]
  (.inverseCumulativeProbability +normal-distribution+ confidence))

(defn calculate-sample-size
  "Calculate the required sample size for a given confidence level, margin of error, 
   and estimated proportion (default is 0.5).
  
   Params:
   - confidence-level: A float representing the desired confidence level (e.g., 0.95 for 95%).
   - margin-of-error: A float representing the desired margin of error (e.g., 0.05 for 5%).
   - estimated-proportion: Optional, a float representing the estimated proportion of success. Defaults to 0.5.
   
   Returns:
   - An integer representing the required sample size."
  ([confidence-level margin-of-error]
   (calculate-sample-size confidence-level margin-of-error 0.5))
  ([confidence-level margin-of-error estimated-proportion]
   (let [z (z-value confidence-level)]
     (int (Math/ceil (* (Math/pow (/ z margin-of-error) 2) 
                       estimated-proportion 
                       (- 1 estimated-proportion)))))))

(def defensive-tactics
  (delay (mop/class-direct-subclasses :d3f/DefensiveTactic)))

(def defensive-techniques
  (delay (set (mapcat :d3f/enabled-by (map datafy @defensive-tactics)))))

(def d3fend-techniques
  (delay (set (mapcat descendants @defensive-techniques))))

(def offensive-techniques
  (delay (mop/class-direct-subclasses :d3f/OffensiveTechnique)))

(def attack-techniques
  (delay
    (->> (mapcat mop/class-direct-subclasses offensive-techniques)
         (random-sample 0.5)
         (take (- (calculate-sample-size 0.95 0.05 0.5)
                  (count defensive-tactics)
                  (count defensive-techniques)
                  (count d3fend-techniques)
                  (count offensive-techniques))))))

#_(def d3fend-eval-classes
  (concat defensive-tactics
          defensive-techniques
          d3fend-techniques
          offensive-techniques
          attack-techniques))
#_(spit "resources/net/wikipunk/d3fend/test/eval.edn" (pr-str d3fend-eval-classes))
(def d3fend-eval-classes
  (edn/read-string (slurp "resources/net/wikipunk/d3fend/eval.edn")))

;; This should add up to 271 samples for the evaluation

(defn read-qname
  "Returns a qualified keyword for qname."
  [qname]
  (apply keyword (str/split qname #":" 2)))

(def training-data
  "Returns the dataset used for fine-tuning."
  (delay
    (with-open [r (io/reader "resources/net/wikipunk/d3fend/d3fend.jsonl")]
      (update-vals (->> (repeatedly #(json/read r :eof-error? false :key-fn keyword))
                        (take-while (complement nil?))
                        (map :messages)
                        (group-by #(read-qname (:content (nth % 2)))))
                   first))))

(def d3fend-eval-prompt
  [system-message
   d3f-prefixes-message])

(def d3fend-eval0
  "zero shot"
  {:messages (->> d3fend-eval-classes
                  (mapv (fn [ident]
                          (conj d3fend-eval-prompt {:role "user" :content (ttl/serialize ident)}))))
   :md       {:rdfa/uri    "http://d3fend.mitre.org/ontologies/d3fend.owl#"
              :rdfa/prefix "d3f"}})

(def access-token-example
  "the example to add to each sample in the test"
  [{:role "user", :content "d3f:AccessToken"}
   {:role "assistant",
    :content
    "d3f:AccessToken rdf:type d3f:D3FENDThing, d3f:NetworkResource, d3f:DigitalArtifact, d3f:DigitalObject, d3f:Resource,\n                         d3f:Artifact, owl:NamedIndividual, d3f:RemoteResource, owl:Class;\n                skos:altLabel \"Ticket\", \"Token\";\n                rdfs:subClassOf d3f:Credential;\n                rdfs:seeAlso <http://dbpedia.org/resource/Access_token>;\n                d3f:used-by d3f:T1550.001;\n                d3f:may-be-created-by d3f:CopyToken, d3f:T1134.002, d3f:T1134.003, d3f:T1134.001;\n                mop:classPrecedenceList (d3f:AccessToken d3f:Credential d3f:NetworkResource d3f:CapabilityFeature\n                                         d3f:DigitalArtifact d3f:Resource d3f:DigitalObject d3f:OffensiveTechnique\n                                         d3f:D3FENDCatalogThing d3f:Artifact d3f:Technique d3f:RemoteResource\n                                         d3f:D3FENDThing d3f:ATTACKThing d3f:DefensiveTechnique\n                                         d3f:InformationContentEntity owl:Class rdfs:Class);\n                mop:classDirectSubclasses d3f:TicketGrantingTicket, d3f:KerberosTicket;\n                d3f:contained-by d3f:SecurityToken;\n                rdfs:label \"Access Token\";\n                d3f:associated-with d3f:SecurityToken, d3f:CopyToken, d3f:T1134.002, d3f:T1550.001, d3f:T1528,\n                                    d3f:T1134.003, d3f:T1134.001;\n                d3f:may-be-contained-by d3f:SecurityToken;\n                d3f:accessed-by d3f:T1528;\n                d3f:definition \"In computer systems, an access token contains the security credentials for a login session and identifies the user, the user's groups, the user's privileges, and, in some cases, a particular application. Typically one may be asked to enter the access token (e.g. 40 random characters) rather than the usual password (it therefore should be kept secret just like a password).\";\n                d3f:may-be-accessed-by d3f:T1528;\n                d3f:created-by d3f:CopyToken, d3f:T1134.002, d3f:T1134.003, d3f:T1134.001.\n\n"}])

(def d3fend-eval1
  "one example"
  {:messages (mapv (fn [ident]
                     (-> (into d3fend-eval-prompt access-token-example)
                         (conj {:role "user" :content (ttl/serialize ident)})))
                   d3fend-eval-classes)
   :md       {:rdfa/uri    "http://d3fend.mitre.org/ontologies/d3fend.owl#"
              :rdfa/prefix "d3f"}})

(def d3f-md
  {:rdfa/uri    "http://d3fend.mitre.org/ontologies/d3fend.owl#"
   :rdfa/prefix "d3f"
   :namespaces  {"d3f"     "http://d3fend.mitre.org/ontologies/d3fend.owl#",
                 "dcterms" "http://purl.org/dc/terms/",
                 "owl"     "http://www.w3.org/2002/07/owl#",
                 "rdf"     "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                 "rdfs"    "http://www.w3.org/2000/01/rdf-schema#",
                 "skos"    "http://www.w3.org/2004/02/skos/core#",
                 "xsd"     "http://www.w3.org/2001/XMLSchema#"}})

(def access-token-fixed
  "Oops! I included MOP properties in the example without declaring it in the prefixes. This may explain the weird results from providing an example. Costly mistake!"
  [{:role "user", :content "d3f:AccessToken"}
   {:role "assistant",
    :content
    "d3f:AccessToken rdf:type d3f:D3FENDThing, d3f:NetworkResource, d3f:DigitalArtifact, d3f:DigitalObject, d3f:Resource,\n                         d3f:Artifact, owl:NamedIndividual, d3f:RemoteResource, owl:Class;\n                skos:altLabel \"Ticket\", \"Token\";\n                rdfs:subClassOf d3f:Credential;\n                rdfs:seeAlso <http://dbpedia.org/resource/Access_token>;\n                d3f:used-by d3f:T1550.001;\n                d3f:may-be-created-by d3f:CopyToken, d3f:T1134.002, d3f:T1134.003, d3f:T1134.001;\n                d3f:contained-by d3f:SecurityToken;\n                rdfs:label \"Access Token\";\n                d3f:associated-with d3f:SecurityToken, d3f:CopyToken, d3f:T1134.002, d3f:T1550.001, d3f:T1528,\n                                    d3f:T1134.003, d3f:T1134.001;\n                d3f:may-be-contained-by d3f:SecurityToken;\n                d3f:accessed-by d3f:T1528;\n                d3f:definition \"In computer systems, an access token contains the security credentials for a login session and identifies the user, the user's groups, the user's privileges, and, in some cases, a particular application. Typically one may be asked to enter the access token (e.g. 40 random characters) rather than the usual password (it therefore should be kept secret just like a password).\";\n                d3f:may-be-accessed-by d3f:T1528;\n                d3f:created-by d3f:CopyToken, d3f:T1134.002, d3f:T1134.003, d3f:T1134.001.\n\n"}])

(def d3fend-eval2
  "one example fixed"
  {:messages (mapv (fn [ident]
                     (-> (into d3fend-eval-prompt access-token-fixed)
                         (conj {:role "user" :content (ttl/serialize ident)})))
                   d3fend-eval-classes)
   :md       {:rdfa/uri    "http://d3fend.mitre.org/ontologies/d3fend.owl#"
              :rdfa/prefix "d3f"}})

(defn evaluate-model
  "Evaluate a model on the same prompt. Return a map of data describing the results."
  [{:keys [model messages md]}]
  (pmap (fn f [model messages md limit]
          (Thread/sleep (rand-int 1000))
          (try
            (test-example model messages md)
            (catch clojure.lang.ExceptionInfo ex
              (case (:status (ex-data (ex-cause (ex-cause ex))))
                503 (if (pos? limit)
                      (do (Thread/sleep 2500)
                          (f model messages md (dec limit)))
                      (ex-data (ex-cause (ex-cause ex))))
                (ex-data (ex-cause (ex-cause ex)))))))
        (repeat model) messages (repeat md) (repeat 3)))

;; This model ID is unique to my organization on OpenAI but you should
;; be able to fine-tune your own using the jsonl dataset in
;; resources. I kept everything for reproducibility on my end.

(comment
  (def gpt35-evals (evaluate-model (assoc d3fend-eval0 :model "gpt-3.5-turbo")))
  (def ft-evals (evaluate-model (assoc d3fend-eval0 :model "ft:gpt-3.5-turbo-0613:wikipunk::86RYdAVy")))
  (def gpt4-evals (evaluate-model (assoc d3fend-eval0 :model "gpt-4")))

  (spit (str "resources/net/wikipunk/d3fend/test/gpt35/" (random-uuid) ".edn")
        (pr-str (vec gpt35-evals)))
  (spit (str "resources/net/wikipunk/d3fend/test/ft/" (random-uuid) ".edn")
        (pr-str (vec ft-evals)))
  (spit (str "resources/net/wikipunk/d3fend/test/gpt4/" (random-uuid) ".edn")
        (pr-str (vec gpt4-evals))))

(comment
  (def gpt35-evals (evaluate-model (assoc d3fend-eval1 :model "gpt-3.5-turbo")))
  (def ft-evals (evaluate-model (assoc d3fend-eval1 :model "ft:gpt-3.5-turbo-0613:wikipunk::86RYdAVy")))
  (def gpt4-evals (evaluate-model (assoc d3fend-eval1 :model "gpt-4")))

  (spit (str "resources/net/wikipunk/d3fend/test2/gpt35/" (random-uuid) ".edn")
        (pr-str (vec gpt35-evals)))
  (spit (str "resources/net/wikipunk/d3fend/test2/ft/" (random-uuid) ".edn")
        (pr-str (vec ft-evals)))
  (spit (str "resources/net/wikipunk/d3fend/test2/gpt4/" (random-uuid) ".edn")
        (pr-str (vec gpt4-evals))))

(comment
  (def gpt35-evals (evaluate-model (assoc d3fend-eval2 :model "gpt-3.5-turbo")))
  (def ft-evals (evaluate-model (assoc d3fend-eval2 :model "ft:gpt-3.5-turbo-0613:wikipunk::86RYdAVy")))
  (def gpt4-evals (evaluate-model (assoc d3fend-eval2 :model "gpt-4")))

  (spit (str "resources/net/wikipunk/d3fend/test3/gpt35/" (random-uuid) ".edn")
        (pr-str (vec gpt35-evals)))
  (spit (str "resources/net/wikipunk/d3fend/test3/ft/" (random-uuid) ".edn")
        (pr-str (vec ft-evals)))
  (spit (str "resources/net/wikipunk/d3fend/test3/gpt4/" (random-uuid) ".edn")
        (pr-str (vec gpt4-evals))))

(comment
  (test-example "ft:gpt-3.5-turbo-0613:wikipunk::86RYdAVy"
                [system-message
                 d3f-prefixes-message
                 {:role "user" :content "d3f:AccountLocking"}]
                {:rdfa/uri    "http://d3fend.mitre.org/ontologies/d3fend.owl#"
                 :rdfa/prefix "d3f"}))

(defn evals-report
  "returns a report of the evaluation on each model given a java
  resource on the classpath representing the evaluation using the
  above functions"
  ([]
   (evals-report ["net/wikipunk/d3fend/test/gpt35/b5a86931-6721-4df3-8237-ae859b9f6e4e.edn"
                  "net/wikipunk/d3fend/test/ft/d55f25e8-6183-4fc1-b761-ba23119f0db3.edn"
                  "net/wikipunk/d3fend/test/gpt4/2199e6cc-6fa1-496d-abd1-0bbe4b408423.edn"]
                 ["net/wikipunk/d3fend/test2/gpt35/9cf71804-aba8-44d9-a52a-67291a50c785.edn"
                  "net/wikipunk/d3fend/test2/ft/4277eebd-a51e-442b-b14a-71600ed9dac0.edn"
                  "net/wikipunk/d3fend/test2/gpt4/8b163dbd-85ca-4237-9d8c-d927742ef0cf.edn"]
                 ["net/wikipunk/d3fend/test3/gpt35/49927476-b62b-45f1-b007-28d89dbcc875.edn"
                  "net/wikipunk/d3fend/test3/ft/a2268e54-6dcb-4cf1-9435-f60876c006b2.edn"
                  "net/wikipunk/d3fend/test3/gpt4/2a8ef3c8-9024-442b-8cbc-10bf83e19f80.edn"]))
  ([[gpt35-evals1 ft-evals1 gpt4-evals1]
    [gpt35-evals2 ft-evals2 gpt4-evals2]
    [gpt35-evals3 ft-evals3 gpt4-evals3]]
   {:test1 {"gpt-3.5-turbo" (edn/read-string (slurp (io/resource gpt35-evals1)))
            "ft"            (edn/read-string (slurp (io/resource ft-evals1)))
            "gpt-4"         (edn/read-string (slurp (io/resource gpt4-evals1)))}
    :test2 {"gpt-3.5-turbo" (edn/read-string (slurp (io/resource gpt35-evals2)))
            "ft"            (edn/read-string (slurp (io/resource ft-evals2)))
            "gpt-4"         (edn/read-string (slurp (io/resource gpt4-evals2)))}
    :test3 {"gpt-3.5-turbo" (edn/read-string (slurp (io/resource gpt35-evals3)))
            "ft"            (edn/read-string (slurp (io/resource ft-evals3)))
            "gpt-4"         (edn/read-string (slurp (io/resource gpt4-evals3)))}}))

(def mem-datafy (memoize datafy))

(defn hallucinations
  "For each sample in the report that ended up returning parseable
  RDF, how many of the classes and properties used actually exist as
  far as we can tell?"
  [report]
  (let [ratios          (->> (filter some? report)
                             (map :output)
                             (map keywords)
                             (keep seq)
                             (map (fn [keyset]
                                    (double (/ (count (keep mem-datafy keyset))
                                               (count keyset))))))
        total-attempts  (count report)
        total-successes (count ratios)
        total-failures  (- total-attempts total-successes)
        avg-validity    (if (zero? total-successes)
                          0
                          (/ (reduce + ratios) total-successes))
        penalty         (/ (* avg-validity total-failures) total-attempts)
        score           (- (reduce + ratios) penalty)]
    (/ score total-attempts)))

(defn stats
  [report]
  (let [retries-left    (group-by :retries-left (filter some? report))
        total-attempts  (count report)
        total-successes (count (filter some? report))]
    {:success        (double (/ (count (filter some? report)) (count report)))
     :failure-rate   (double (/ (- total-attempts total-successes) total-attempts))
     :first-try      (count (get retries-left 3))
     :second-try     (count (get retries-left 2))
     :third-try      (count (get retries-left 1))
     :hallucinations (hallucinations report)}))

(defn report-graph
  "returns an apache jena graph of the report's Turtle"
  (^org.apache.jena.graph.Graph [report]
   (report-graph report d3f-md))
  (^org.apache.jena.graph.Graph [report md]
   (when report
     (rdf/graph (assoc md
                       :rdf/value (str (get-in report [:messages 1 :content])
                                       (:content (peek (:messages report))))
                       :format :ttl)))))

(defn print-jsonld
  [report]
  (org.apache.jena.riot.RDFDataMgr/write *out*
                                         (report-graph report)
                                         org.apache.jena.riot.Lang/JSONLD11))

(defn dataset-for-model
  [test model idents reports]
  (map (fn [ident report]
         (let [row {:qname                (ttl/serialize ident)
                    :retries-left         (:retries-left report 0)
                    :model                model
                    :num-triples          0
                    :test                 test
                    :num-predicates       0
                    :num-known-predicates 0}]
           (if-some [g (report-graph report)]
             (let [preds (into []
                               (comp
                                 (map #(g/data (.getPredicate %)))
                                 (filter qualified-keyword?)
                                 (map ttl/serialize)
                                 (distinct))
                               g)
                   known (into []
                               (comp
                                 (map #(g/data (.getPredicate %)))
                                 (filter qualified-keyword?)
                                 (filter datafy)
                                 (map ttl/serialize)
                                 (distinct))
                               g)]
               (assoc row
                      :num-triples  (.size g)
                      :num-predicates (count preds)
                      :num-known-predicates (count known)))
             row)))
       idents reports))

(defn all-evals
  []
  (reduce-kv (fn [dataset test models]
               (into dataset
                     (mapcat #(dataset-for-model test (key %) d3fend-eval-classes (val %)))
                     models))
             []
             (set/rename-keys (evals-report)
                              {:test1 "zero-shot"
                               :test2 "one-shot-mistake"
                               :test3 "one-shot"})))

(defn corrected-evals
  []
  (reduce-kv (fn [dataset test models]
               (into dataset
                     (mapcat #(dataset-for-model test (key %) d3fend-eval-classes (val %)))
                     models))
             []
             (set/rename-keys (dissoc (evals-report) :test2)
                              {:test1 "zero-shot"
                               :test3 "one-shot"})))

(defn faulty-evals
  []
  (reduce-kv (fn [dataset test models]
               (into dataset
                     (mapcat #(dataset-for-model test (key %) d3fend-eval-classes (val %)))
                     models))
             []
             (set/rename-keys (dissoc (evals-report) :test1 :test3)
                              {:test2 "one-shot-mistake"})))

(defn export-csv
  "Export the reports to a CSV file for a single test."
  ([file]
   (export-csv file (corrected-evals)))
  ([file evals]
   (with-open [w (io/writer file)]
     (csv/write-csv w (into [(map name (keys (first evals)))] (map vals) evals)))))
