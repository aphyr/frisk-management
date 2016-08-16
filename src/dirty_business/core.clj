(ns dirty-business.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import [java.util Properties]
           (edu.stanford.nlp.simple Document
                                    Sentence)))

(def friendly-tags
  "Map of core-nlp classes to sane names.
  http://www.clips.ua.ac.be/pages/mbsp-tags"
  {"ADJP" :adjective-phrase
   "ADVP" :adverb-phrase
   "SBAR" :subordinating-conjunction
   "WHADVP" :wh-adverb-phrase
   "CC"   :coordinating-conjunction
   "CD"   :cardinal-number
   "DT"   :determiner
   "EX"   :existential-there
   "FW"   :foreign-word
   "IN"   :preposition-or-subordinating-conjunction
   "JJ"   :adjective
   "JJR"  :adjective-comparative
   "JJS"  :adjective-superlative
   "LS"   :list-item-marker
   "MD"   :modal
   "NN"   :noun-singular-or-mass
   "NNS"  :noun-plural
   "NP"   :proper-noun-singular
   "NNP"  :proper-noun-singular ; Renamed in Penn POS
   "NPS"  :proper-noun-plural
   "NNPS" :proper-noun-plural ; Renamed
   "PDT"  :predeterminer
   "POS"  :possessive-ending
   "PP"   :personal-pronoun
   "PRP"  :personal-pronoun ; renamed
   "PP$"  :possessive-pronoun
   "PRP$" :possessive-pronoun ; renamed
   "RB"   :adverb
   "RBR"  :adverb-comparative
   "RBS"  :adverb-superlative
   "ROOT" :root
   "RP"   :particle
   "SYM"  :symbol
   "S"    :s ; ???
   "INTJ" :interjection
   "TO"   :to
   "UH"   :interjection
   "VB"   :verb-base-form
   "VBD"  :verb-past-tense
   "VBG"  :verb-gerund-or-present-participle
   "VBN"  :verb-past-participle
   "VBP"  :verb-non-3rd-person-singular-present
   "VP"   :verb-phrase
   "VBZ"  :verb-3rd-person-singular-present
   "WDT"  :wh-determiner
   "WP"   :wh-pronoun
   "WP$"  :possessive-wh-pronoun
   "WRB"  :wh-adverb})

(defn friendly-tag
  [x]
  (friendly-tags x (keyword x)))

(defn ->sexp
  "Turns a core.nlp parse tree into a clojure data structure."
  [tree]
  (if (seq (.children tree))
    (cons (friendly-tag (.value (.label tree)))
          (map ->sexp (.children tree)))
    (.value (.label tree))))

(defn sexp->str
  "Turns a parse tree back into a string."
  [sexp]
  (str/join " " (remove keyword? (flatten sexp))))

(defn find-tree
  "Filters a tree to only nodes satisfying (f node)."
  [pred tree]
  (let [results (atom [])]
    (walk/prewalk (fn [t]
                    (prn :take t)
                    (when (pred t)
                      (prn "Found match" t)
                      (swap! results conj t))
                    t)
                  tree)
    @results))

(defn is?
  "Does the given tree begin with tag?"
  ([tag]
   (partial is? tag))
  ([tag tree]
   (and (sequential? tree)
        (= tag (first tree)))))

(defn adjectival-phrases
  "Find top-level adjectival phrases."
  [t]
  (find-tree (is? :adjective-phrase) t))

(defn go
  []
  (->> (Document. "Being young naïve, inexperienced, but curious
                  midshipman, my interest was duly noted by my fellow
                  compartment bunk mates.")
       .sentences
       (map #(->sexp (.parse %)))
       (map adjectival-phrases)
       pprint))
