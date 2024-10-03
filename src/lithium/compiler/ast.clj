(ns lithium.compiler.ast
  "The AST is Lithium's first intermediate representation. As the name suggests,
  it's a uniform way to represent the Lithium language expressions as Clojure
  data structures – an abstract syntax tree.

  An AST node is a map containing the key `:type`, whose value is a keyword
  determining the expression type: `:if`, `:let`, etc. Depending on the type,
  there are additional members. A schema or spec will follow at some time, but
  for now, look at clojure->ast to see how it looks.

  This representation is inspired by edn-query-language's AST. The advantage
  is that it's more amenable to transformations than plain s-expressions, so
  it's easier to write code walkers or analysers that target it.

  This ns declares functions to convert Lithium code to AST and back, as well
  as helper macros and functions to access and transform the AST."
  (:require [lithium.compiler.primitives :as primitives]))

(declare clojure->ast)

(defn list->ast [[fst & lst :as expr]]
  (cond
    (= fst 'if) {:type :if
                 :condition (clojure->ast (first lst))
                 :then-expr (clojure->ast (second lst))
                 :else-expr (clojure->ast (last lst))}
    (= fst 'do) {:type :do
                 :exprs (mapv clojure->ast lst)}
    (= fst 'fn) {:type :fn
                 :args (first lst)
                 :body (mapv clojure->ast (next lst))}
    (= fst 'def) {:type :def
                  :symbol (first lst)
                  :definition (clojure->ast (second lst))}
    (contains? '#{let loop} fst) {:type (keyword (name fst))
                                  :bindings (vec (for [[k v] (partition 2 (first lst))]
                                                   {:symbol k, :expr (clojure->ast v)}))
                                  :body (mapv clojure->ast (next lst))}
    (contains? primitives/primitives fst) {:type :primitive-call
                                           :primitive fst
                                           :args (mapv clojure->ast lst)}
    :otherwise {:type :fn-call
                :fn-expr (clojure->ast fst)
                :args (mapv clojure->ast lst)}))

(defn clojure->ast [expr]
  (cond
    (list? expr) (list->ast expr)
    (int? expr) {:type :value, :value-type :int, :value expr}
    (boolean? expr) {:type :value, :value-type :boolean, :value expr}
    (char? expr) {:type :value, :value-type :char, :value expr}
    (symbol? expr) {:type :env-lookup, :symbol expr}
    (nil? expr) {:type :value, :value-type :nil, :value nil}
    :otherwise {:type :unrecognized
                :expr expr}))

(defn type-matches?
  "A helper for match. See below."
  [specifier type]
  (if (keyword? specifier)
    (= type specifier)
    (specifier type)))

(def let-like #{:let :loop})

(defmacro match
  "Evaluates one of the clauses depending on the type of the given AST node.
  Clauses are triplets, supplied sequentially (without additional parens),
  as in cond. Each clause has the form:

    `type-specifier bindings expr`

  where:

  - `type-specifier` is either a keyword (denoting AST of that specific type)
    or a predicate;
  - `bindings` is a vector of symbols to which their particular values
    in the AST will be bound;
  - `expr` is the expression to evaluate if type-specifier matches the AST.

  A final clause can be supplied as an expression for the `:otherwise` case.
  If not supplied and no clause matches, ast is returned unchanged."
  [ast & clauses]
  (let [ast-sym (gensym)
        transformed-clauses (for [[name bindings body] (partition 3 clauses)]
                              [name `(let [{:keys ~bindings} ~ast-sym] ~body)])]
    `(let [~ast-sym ~ast]
       (condp type-matches? (:type ~ast-sym)
         ~@(apply concat transformed-clauses)
         ~(or (when-not (zero? (mod (count clauses) 3))
                (last clauses))
              ast-sym)))))

(defn ast->clojure [ast]
  (match ast
         :value [value] value
         :env-lookup [symbol] symbol
         :primitive-call [primitive args] `(~primitive ~@(map ast->clojure args))
         :if [condition then-expr else-expr] `(~'if ~(ast->clojure condition) ~(ast->clojure then-expr) ~(ast->clojure else-expr))
         :do [exprs] `(~'do ~@(map ast->clojure exprs))
         :fn-call [fn-expr args] `(~(ast->clojure fn-expr) ~@(map ast->clojure args))
         :unrecognized [expr] expr
         :fn [args bound-vars body] `(~'fn ~@(when bound-vars [{:bound-vars bound-vars}]) ~args ~@(map ast->clojure body))
         :def [symbol definition] `(~'def ~symbol ~(ast->clojure definition))
         let-like [type bindings body] `(~(symbol (name type))
                                         ~(vec (apply concat (for [{:keys [symbol expr]} bindings]
                                                               [symbol (ast->clojure expr)])))
                                         ~@(map ast->clojure body))
         ;; after closure analysis:
         :closure [label vars] `(~'closure ~label ~@vars)
         :code [args bound-vars body] `(~'code ~args ~bound-vars ~@(map ast->clojure body))
         :labels [labels body] `(~'labels ~(vec (mapcat (fn [{:keys [label] :as code}]
                                                          [label (ast->clojure code)])
                                                        labels))
                                 ~(ast->clojure body))))

(defn expr-update
  "Applies f to every sub-expression of ast, returning the updated ast."
  ([ast f]
   (match ast
          let-like [] (-> ast
                          (update :bindings (partial mapv (fn [binding] (update binding :expr f))))
                          (update :body (partial mapv f)))
          :if [] (-> ast
                     (update :condition f)
                     (update :then-expr f)
                     (update :else-expr f))
          :do [] (update ast :exprs (partial mapv f))
          :def [] (update ast :definition f)
          :primitive-call [] (update ast :args (partial mapv f))
          :fn-call [] (-> ast
                          (update :fn-expr f)
                          (update :args (partial mapv f)))
          :fn [] (update ast :body (partial mapv f))))
  ([ast f arg & args] (expr-update ast #(apply f % arg args))))

(defn walk
  "Traverses and potentially modifies ast, applying inner recursively to each
  nested expression, and then outer to potentially transform the expression."
  [ast inner outer]
  (outer (expr-update (inner ast) walk inner outer)))

(comment
  (def ast1 (clojure->ast '(let [f (fn [] 42)
                                 g (fn [x] (+ (f) x))]
                             (g (f)))))
  (def ast2 (clojure->ast '(let [x 5]
                             (fn [y] (fn [] (+ x y)))))))
