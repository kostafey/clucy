(defproject com.otfrom/clucy "0.7.2.1"
  :description "A Clojure interface to the Lucene search engine"
  :url "http://github/kostafey/clucy"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.apache.lucene/lucene-core "7.2.1"]
                 [org.apache.lucene/lucene-queryparser "7.2.1"]
                 [org.apache.lucene/lucene-analyzers-common "7.2.1"]
                 [org.apache.lucene/lucene-highlighter "7.2.1"]
                 [me.raynes/fs "1.4.6"]]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cloverage "1.0.6"]])
