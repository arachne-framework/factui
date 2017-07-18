(ns factui.specs.datalog
  (:require [#?(:clj clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.string :as str]
            [factui.specs.pull :as pull]))

(s/def ::query (s/or ::map-query ::map-query
                     ::vec-query ::vec-query))

(s/def ::map-query (s/keys :req-un [::find]
                           :opt-un [::with ::in ::where]))

(s/def ::vec-query
  (s/cat ::find-literal #{:find}
         ::find ::find
         ::with-clause (s/? (s/cat ::with-literal #{:with} ::with ::with))
         ::in-clause (s/? (s/cat ::in-literal #{:in} ::in ::in))
         ::where-clause (s/? (s/cat ::where-literal #{:where} ::where ::where))))

(s/def ::find (s/alt ::find-rel ::find-rel
                     ::find-coll ::find-coll
                     ::find-tuple ::find-tuple
                     ::find-scalar ::find-scalar))

(s/def ::with (s/+ ::variable))
(s/def ::in (s/+ (s/or ::src-var ::src-var
                       ::rules-var #{'%}
                       ::binding ::binding)))

(s/def ::where (s/+ ::clause))

(s/def ::find-rel (s/+ ::find-elem))
(s/def ::find-coll (s/tuple ::find-elem #{'...}))
(s/def ::find-tuple (s/coll-of ::find-elem :min-count 1))
(s/def ::find-scalar (s/cat ::find-elem ::find-elem ::dot #{'.}))

(s/def ::find-elem (s/or ::variable ::variable
                         ::pull ::pull-expression
                         ::aggregate ::aggregate))

(s/def ::variable (s/and simple-symbol?
                         #(str/starts-with? (name %) "?")))
(s/def ::src-var (s/and simple-symbol?
                        #(str/starts-with? (name %) "$")))

(s/def ::pull-expression (s/cat ::pull #{'pull}
                                ::variable ::variable
                                ::pattern ::pull/pattern))

(s/def ::aggregate (s/cat ::aggregate-fn-name ::fn-name
                          ::fn-args (s/+ ::fn-arg)))

(s/def ::fn-name (s/and symbol?
                   #(not (or (str/starts-with? (name %) "?")
                           (str/starts-with? (name %) "$")))))

(s/def ::fn-arg (s/or ::variable ::variable
                      ::src-var ::src-var
                      ::constant ::constant))

(s/def ::binding (s/or ::bind-scalar ::bind-scalar
                       ::bind-coll ::bind-coll
                       ::bind-tuple ::bind-tuple
                       ::bind-rel ::bind-rel))

(s/def ::bind-scalar ::variable)
(s/def ::bind-coll (s/tuple ::variable #{'...}))
(s/def ::bind-tuple (s/coll-of (s/or ::variable ::variable
                                     ::placeholder #{'_})
                      :min-count 1
                      :kind vector?))
(s/def ::bind-rel (s/tuple (s/coll-of (s/or ::variable ::variable
                                            ::placeholder #{'_})
                             :min-count 1
                             :kind vector?)))

(s/def ::clause (s/alt ::not-clause ::not-clause
                       ::not-join-clause ::not-join-clause
                       ::or-clause ::or-clause
                       ::or-join-clause ::or-join-clause
                       ::expression-clause ::expression-clause))

(s/def ::not-clause (s/cat ::src-var (s/? ::src-var)
                           ::not #{'not}
                           ::clauses (s/+ ::clause)))

(s/def ::not-join-clause (s/cat ::src-var (s/? ::src-var)
                                ::not-join #{'not-join}
                                ::variables (s/coll-of ::variable :min-count 1)
                                ::clauses (s/+ ::clause)))

(s/def ::or-clause (s/cat ::src-var (s/? ::src-var)
                          ::or #{'or}
                          ::clauses (s/+ (s/or :and-clause ::and-clause
                                              :clause ::clause))))

(s/def ::or-join-clause  (s/cat ::src-var (s/? ::src-var)
                                ::or-join #{'or-join}
                                ::variables (s/coll-of ::variable :min-count 1)
                                ::clauses (s/+ (s/or ::and-clause ::and-clause
                                                     ::clause ::clause))))

(s/def ::and-clause (s/cat ::and #{'and} ::clauses (s/+ ::clause)))

(s/def ::expression-clause (s/or ::fn-expr ::fn-expr
                                 ::pred-expr ::pred-expr
                                 ::data-pattern ::data-pattern
                                 ::rule-expr ::rule-expr))

(s/def ::data-pattern (s/cat ::src-var (s/? ::src-var)
                             ::terms (s/+ (s/or ::variable ::variable
                                                ::placeholder #{'_}
                                                ::constant ::constant))))

(s/def ::pred-expr (s/tuple (s/cat ::pred ::fn-name
                                   ::args (s/+ ::fn-arg))))

(s/def ::fn-expr (s/tuple (s/cat ::fn ::fn-name
                                 ::args (s/+ ::fn-arg))
                   ::binding))

(s/def ::rule-expr (s/cat ::src-var (s/? ::src-var)
                          ::rule-name ::fn-name
                          ::terms (s/+ (s/or ::variable ::variable
                                             ::placeholder #{'_}
                                             ::constant ::constant))))
(s/def ::constant #(not (and (symbol? %)
                          (or (str/starts-with? (name %) "?")
                              (str/starts-with? (name %) "$")))))