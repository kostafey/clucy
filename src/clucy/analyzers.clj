(ns clucy.analyzers
  (:use clucy.util)
  (:require [clojure.java.io :as io])
  (:import
   (java.io InputStream)
   (java.nio.charset StandardCharsets)
   (org.apache.lucene.analysis WordlistLoader
                               CharArraySet)
   (org.apache.lucene.util IOUtils)
   (org.apache.lucene.analysis.Analyzer$TokenStreamComponents)
   (org.apache.lucene.analysis Analyzer
                               TokenStream
                               Tokenizer
                               TokenFilter
                               CachingTokenFilter)
   (org.apache.lucene.analysis.standard StandardAnalyzer
                                        ClassicAnalyzer
                                        StandardFilter
                                        StandardTokenizer
                                        ClassicTokenizer
                                        ClassicFilter)
   (org.apache.lucene.analysis.snowball SnowballFilter)
   (org.apache.lucene.analysis.ar ArabicAnalyzer)
   (org.apache.lucene.analysis.bg BulgarianAnalyzer)
   (org.apache.lucene.analysis.de GermanAnalyzer
                                  GermanLightStemFilter)
   (org.apache.lucene.analysis.en EnglishAnalyzer
                                  EnglishMinimalStemFilter)
   (org.apache.lucene.analysis.fr FrenchAnalyzer
                                  FrenchLightStemFilter)
   (org.apache.lucene.analysis.ru RussianAnalyzer
                                  RussianLightStemFilter)
   (org.apache.lucene.analysis.core LowerCaseFilter
                                    StopFilter
                                    WhitespaceTokenizer
                                    LetterTokenizer
                                    KeywordTokenizer
                                    LowerCaseTokenizer)
   (org.apache.lucene.analysis.path PathHierarchyTokenizer)
   (org.apache.lucene.analysis.wikipedia WikipediaTokenizer)
   (org.apache.lucene.analysis.miscellaneous SetKeywordMarkerFilter
                                             LengthFilter)
   (org.tartarus.snowball.ext EnglishStemmer
                              FrenchStemmer
                              GermanStemmer
                              RussianStemmer)))

(def analysers-class-map
  {:basic    Analyzer
   :standard StandardAnalyzer
   :classic  ClassicAnalyzer
   :ar       ArabicAnalyzer
   :bg       BulgarianAnalyzer
   :fr       FrenchAnalyzer
   :de       GermanAnalyzer
   :en       EnglishAnalyzer
   :ru       RussianAnalyzer})

(def tokenizers-class-map
  {:standard       StandardTokenizer
   :whitespace     WhitespaceTokenizer
   :letter         LetterTokenizer
   :classic        ClassicTokenizer
   :keyword        KeywordTokenizer
   :lowercase      LowerCaseTokenizer
   :path-hierarchy PathHierarchyTokenizer
   :wikipedia      WikipediaTokenizer})

(def filters-class-map
  {:standard      StandardFilter
   :snowball      SnowballFilter
   :classic       ClassicFilter
   :caching-token CachingTokenFilter})

(def stemmers-class-map
  {:en EnglishStemmer
   :fr FrenchStemmer
   :de GermanStemmer
   :ru RussianStemmer
   :en-min EnglishMinimalStemFilter
   :fr-light FrenchLightStemFilter
   :de-light GermanLightStemFilter
   :ru-light RussianLightStemFilter})

(defn- build-analyzer
  ([analyzer-class]
   (.newInstance (analysers-class-map analyzer-class)))
  ([analyzer-class stop-words]
   (let [ctor (.getConstructor (analysers-class-map analyzer-class)
                               (into-array [CharArraySet]))]
     (.newInstance ctor (into-array [stop-words]))))
  ([analyzer-class stop-words stem-exclusion-words]
   (let [ctor (.getConstructor (analysers-class-map analyzer-class)
                               (into-array [CharArraySet
                                            CharArraySet]))]
     (.newInstance ctor (into-array [stop-words
                                     stem-exclusion-words])))))

(defn- get-analyzer [analyzer-class stop-words stem-exclusion-words]
  (assert (not (and (some #{analyzer-class} [:standard :classic])
                    (not (nil? stem-exclusion-words))))
          "Can't set stem-exclusion-words for Standard or Classic Analyzer.")
  (cond
    (and stop-words stem-exclusion-words) (build-analyzer
                                           analyzer-class
                                           stop-words
                                           stem-exclusion-words)
    (boolean stop-words) (build-analyzer analyzer-class stop-words)
    :else (build-analyzer analyzer-class)))


(defn- get-tokenizer [key-or-object]
  (if (instance? Tokenizer key-or-object)
    key-or-object
    (.newInstance (tokenizers-class-map key-or-object))))

(defn make-analyzer
  ([] (make-analyzer :class :standard))
  ([& {:keys [class
              version
              stop-words
              stem-exclusion-words
              tokenizer
              filter
              stemmer
              lower-case
              length-filter]
       :or {class :basic
            version org.apache.lucene.util.Version/LATEST
            stop-words nil
            stem-exclusion-words nil
            tokenizer :standard
            filter :standard
            stemmer nil
            lower-case true
            length-filter nil}}]
   (let [analyzer
         (if (not (= :basic class))
           ;; ------------------------------------------------------------
           ;; Use pre-defined analyzer class.
           ;; All params except stop-words, stem-exclusion-words and version
           ;; are ignored.
           (get-analyzer class stop-words stem-exclusion-words)
           ;; ------------------------------------------------------------
           ;; Custom analyser.
           (proxy [Analyzer] []
             (createComponents [fieldName]
               (let [^Tokenizer source (get-tokenizer tokenizer)
                     ^TokenStream result (.newInstance
                                          (.getConstructor
                                           (filters-class-map filter)
                                           (into-array [TokenStream]))
                                          (into-array [source]))
                     result (if lower-case (LowerCaseFilter. result) result)
                     result (if length-filter (LengthFilter.
                                               result
                                               (first length-filter)
                                               (second length-filter)) result)
                     result (if stop-words (StopFilter. result stop-words) result)
                     result (if stem-exclusion-words
                              (SetKeywordMarkerFilter. result stem-exclusion-words)
                              result)
                     result (if stemmer
                              ;; for light stemmers
                              (if (or (ends-with (name stemmer) "light")
                                      (ends-with (name stemmer) "min"))
                                (.newInstance
                                 (.getConstructor
                                  (stemmers-class-map stemmer)
                                  (into-array [TokenStream]))
                                 (into-array [result]))
                                ;; for snowball stemmers
                                (SnowballFilter.
                                 result
                                 (.newInstance (stemmers-class-map stemmer))))
                              result)]
                 (org.apache.lucene.analysis.Analyzer$TokenStreamComponents.
                  source result)))))]
     (.setVersion analyzer version)
     analyzer)))

(defn file->wordset ^CharArraySet [^String file-name]
  (WordlistLoader/getSnowballWordSet
   (IOUtils/getDecodingReader SnowballFilter
                              file-name
                              StandardCharsets/UTF_8)))

(defn resource->wordset ^CharArraySet [^String resource-file-name]
  (WordlistLoader/getSnowballWordSet
   (IOUtils/getDecodingReader
    (io/input-stream
     (io/resource resource-file-name))
    StandardCharsets/UTF_8)))

(defn stream->wordset ^CharArraySet [^InputStream istream]
  (WordlistLoader/getSnowballWordSet
   (IOUtils/getDecodingReader istream
                              StandardCharsets/UTF_8)))
