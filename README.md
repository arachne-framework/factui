# FactUI

FactUI is a library for efficiently rendering user interfaces declaratively, based on a tuple-oriented data store.

## Rationale

Other ClojureScript/React frameworks have already broken important ground in establishing a pattern for data-driven interfaces: the visible UI is a pure function of data. This is a strong concept which has lead to powerful, simple UI programming idioms.

However, this pattern leaves room for different answers to the following questions:

1. What is the structure of the "application data" that ultimately drives the UI?
2. What is the pattern for making updates to the application data?

Each library answers these questions in its own way.

FactUI gives its own answers, answers which resolve a number of particularly difficult issues that often end up being pain points (especially in larger applications.)

In broad strokes:

1. Application state is stored in a single immutable value, in an atom.
2. Components register queries against the application state.
3. When the app-state atom is updated, only components with changed query results will re-render.

Query notification on updated facts is provided by the [RETE algorithm](https://en.wikipedia.org/wiki/Rete_algorithm), which is designed specifically to provide efficient activiation of rules in response to new facts. FactUI uses [Clara](http://www.clara-rules.org), an excellent RETE implementation for ClojureScript.

## Usage

Note: This section provides documentation using FactUI's integration with the [Rum](https://github.com/tonsky/rum) React wrapper. FactUI is not tightly coupled with Rum, it is entirely possible to build integration between FactUI and alternative wrappers (such as [Reagent](https://reagent-project.github.io) or [Om](https://github.com/omcljs/om) with relatively little effort. However, those have not yet been built.

### Initialization

Because queries and rules are built at compile time, FactUI requires a bit of setup.

#### Step 1: build a rulebase

First, you need to invoke the `factui.api/rulebase` macro at the top level of one of your source files, passing it a rulebase name and any number of namespace names. You must call `rulebase` *after* you `:require` the namespaces it mentions. Clara will search each of the specified namespaces for query and rule definitions and compile them into a rulebase *at macro-expansion time*, binding it to a var with the specified name.

```clojure
(ns example.core
  (:require [factui.api :as f :include-macros true]
            [example.ns-with-rules]
            [example.other-ns-with-rules]))

(f/rulebase my-rulebase
   example.ns-with-rules
   example-other-ns-with-rules)
```

#### Step 2: define some schema

FactUI requires that you define schema for all the attributes you want to insert. Although adherance to the schema is not rigorously validated (for performance reasons), some attributes are important to ensure correct update semantics, particularly `:db/cardinality`, `:db.type/ref` `:db/isComponent` and `:db/unique`.

Schema txdata is structured the same way as it is in Datomic. For example:

```clojure
(def schema
  [{:db/ident :tasklist/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :tasklist/tasks
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true}

    {:db/ident :task/title
     :db/valueType :db.type/string
     :db/cardinality :db.cardinality/one}
    {:db/ident :task/completed
     :db/valueType :db.type/boolean
     :db/cardinality :db.cardinality/one}])
```

#### Step 3: Define a component

Using the Rum wrapper, components are defined the same way as they always are in Rum. FactUI is introduced by calling the `factui.rum/q` mixin. `q` is a macro that defines a Clara query, and also resolves to a Rum mixin that subscribes to the defined query and causes the component to be re-rendered whenever the query results change.

```clojure
(ns example.ns-with-rules
  (:require [rum.core :as rum]
            [factui.api :as f :include-macros true]
            [factui.rum :as fr :refer [*results*] :include-macros true]))


(rum/defc TaskList < rum/static
                     (fr/q [:find ?list-title ?task-title
                            :in ?task-list
                            :where
                            [?task-list :tasklist/title ?list-title]
                            [?task-list :tasklist/tasks ?task]
                            [?task :task/title ?task-title])
  [app-state ?task-list]
  (let [list-title (ffirst *results*)
        task-titles (map second *results*)]
      [:div
       [:h1 list-title]
       [:ul
        (for [title task-titles]
            [:li.task {:key title} title])]]))
````

There are several elements here to unpack.

- The `rum/static` mixin is also used. You may use any other mixins in combination with the FactUI mixin, and using `rum/static` is reccomended to prevent the component from re-rendering when neither its arguments nor underlying query change.
- The body of the `q` macro is a Datomic-style query, which is compiled and defined as a Clara query.
- the `[app-state ?task-list]` vector defines the *arguments* to the component. Components with the FactUI mixin have special requirements about the arguments they are passed:
    - The first argument must be an atom containing the application state.
    - The next N arguments are passed as inputs to the query (those defined for the query's `:in` clause), where N is the number of arguments specified (this may be zero for queries with no `:in` clause).
    - Any additional arguments after the N query inputs are passed on the Rum component as usual and are not processed by the FactUI mixin.
    - By convention, arguments that map to query inputs use a symbol with a leading `?` to visually disambiguate them. This is not required.
- The `factui.rum/*results*` dynamic variable is bound within a component's body. It contains Datomic-style query results, in this case a set of `[list-title task-title]` tuples.
- The remainder of the body is used as the implementation for the Raact component's `render` lifecycle method and should return a single component or Virtual DOM node, as usual (or a Hiccup data structure representing a DOM node, as per Rum.)

#### Step 3: initialize rendering

Now you can initialize rendering of your application. FactUI's rum wrapper provides a single function to initialize an app state atom and start rendering it to the root component. The wrapper also handles ensuring that the application can successfully reload (preserving application state) after a recompile during development (such as from Figwheel.)

`factui.rum/initialize` takes 4 arguments:

1. A var identifying the rulebase to use.
2. The schema to use.
3. A var identifying the constructor of the root component. The root component constructor must take the application state atom as a single argument.
4. The root DOM element where the root component will be mounted using React.

The return value is an atom containing an initialized but empty application state value.

`factui.rum/initialize` is usually invoked from within your application's `main` function, like so:

```clojure

(defn ^:export main
    []
    (let [app-state (fr/initialize
                        #'rulebase
                        schema
                        #'Layout
                        (.getElementById js/document "root"))]
        (fr/transact! initial-data)))
```

Note again that the arguments for the rulebase and root component must be *vars*, not *values*. This ensures that they can be reloaded after being redefined by hiccup.

If you are using Figwheel, you should also set the `factui.rum/refresh` function as the value of Figwheel's `:on-jsload` config key to ensure that your application refreshes correctly after a Figwheel reload.

#### Step 4: transacting some data

You will have noticed the use of `factui.rum/transact!` in the above example. `transact!` is a tiny wrapper around FactUI's core API, which calls `swap!` on the application state atom, transacting the specified txdata and returning a map of tempid bindings that were resolved in the transaction.

An example value for `initial-data` might be:

```clojure
(def initial-data
  [{:db/id -42
    :tasklist/title "My Tasks"
    :tasklist/tasks [{:task/title "Write a readme"
                      :task/completed true}
                     {:task/title "Write an example"
                      :task/completed false}]}])
```

Calling `transact!` with this value would update the app-state atom and return `{-42 <new-eid>}` (or whatever the new entity ID is.)

Some applications will call `transact!` directly in event handlers in components. Others may wish to pass around a channel, use a global pub-sub system, or some other intermediary mechanism to give the system a chance to track or modify changes before they are transacted.

That's the basics! The tools described above are enough to get you started writing declarative UIs with FactUI.

### Other Features

#### Standalone queries

You can define named queries separately from a component using the `factui.api/defquery` macro. They have the same syntax as queries defined in `factui.rum/q`:

```clojure

(f/defquery tasks-for-list
    "Find the IDs of all tasks in the given task list"
    [:find [?task ...]
     :in ?tasklist
     :where
     [?tasklist :tasklist/tasks ?task]])
```

You can then get the results of the query using the `factui.api/query` function, passing it a *session* (not the application stat atom itself, but the value which is the contents of that atom), the query name, and any arguments:

```clojure
(f/query @app-state tasks-for-list 10042)
;; => #{10049 100029 100037#}
```

Note that queries performed in this manner are not reactive: they simply retrieve results, and do not set up any kind of notification or re-render process.

#### Standalone rules

You can also define standalone rules to perform logic whenever facts are inserted that cause a particular condition to become true using the `factui.api/defrule` macro.

```clojure
(f/defrule clean-brushes-rule
  "Add a task to clean the brushes after we finish a painting task"
  [?task :task/title "Paint"]
  [?task :task/complete true]
  [?tasklist :tasklist/tasks ?task]
  =>
  (f/transact! [{:db/id ?tasklist
                 :tasklist/tasks {:task/tittle "Clean Brushes"}}]))
```

When this rule is part of a rulebase, whenever facts are added which cause the conditions (or "left hand side") of the rule to become true, the consequence (or "right hand side") of the rule will be fired and new data will be added to session as part of the same transaction. The bindings established in the LHS of the rule are available in the RHS, with the same bindings.

Note that `factui.api/transact!` function is not the same as `factui.rum/transact!`. Wheras the version in the Rum API is a simple wrapper around an atom swap, the `transact!` in `factui.api` is a special function that may only be called in the right hand side of a rule, and adds new facts to the current pass over the rules.

Any new facts inserted will have the rules engine run on them as well, until no more new rules are activated. Rules may be recursive, adding facts which cause the same rule to fire again. Try not to write rules which never terminate, and be aware that recursive rules can be bad for performance.

Rules are guaranteed to fire only once for each unique set of bindings, meaning that it is safe to put side effects in the consequence of a rule.

In addition to `factui.api/transact!`, you can also use `factui.api/transact-logical!` to insert facts that will be automatically retracted if the condition for a rule ever becomes untrue. This feature is called "truth maintenance", please read Clara's documentation for a discussion of how it works and some of its limitations.

#### Transient facts

When defining your schema, you can declare individual attributes to be transient using the `:factui/isTransient` schema attribute.

```clojure
{:db/ident :task/select
 :db/valueType :db.type/boolean
 :db/cardinality :db.cardinality/one
 :factui/isTransient true}
```

Transient facts are never persisted in the session: they are removed at the end of every transaction. However, they *are* active during the rule activation phase, and can be used to perform conditional logic and (optionally) insert more long-lived facts.

Transient facts are often used to create reactions to temporary UI states, such as user clicks or hovers. Using transient facts, you can encode almost all of your application's interaction logic as declarative rules, if you so wish.

#### Reactive queries

If you want to create a "listener" that will be notified when ever a query's results change, you can do so directly using the `factui.api/register` and `factui.api/deregister` functions. These are what the Rum mixin uses under the hood to provide reactivity to its components.

The `factui.api/register` function takes a session, a query, an argument vector and a channel. It returns a new session, with a listener present, such that whenever a transaction causes the results of the query to change *for the provided arguments*, the new query results will be placed upon the provided channel.

For example, this code creates a listener on your primary app state atom, for the `tasks-for-list` rule defined above.

```clojure
(let [results-ch (chan)]
    (swap! f/register app-state tasks-for-list [42] results-ch)
    (go-loop []
        (when-let [results (<! results-ch)]
            (println "Got new results:" results))))
```

A registration can be removed using `factui.api/deregister`, which takes the same arguments and returns a new session, with the specified channel removed.

## Credits

Many thanks to [Precept](https://github.com/CoNarrative/precept) for being the immediate inspiration for this approach. Although I've been thinking about database-driven and rule-driven UIs for some time, Precept pulled a lot of things into focus and crystallized the kind of API I really wanted to see, which FactUI represents.

Also thanks to [Ryan Brush](https://github.com/rbrush) and the rest of the [Clara](https://github.com/cerner/clara-rules) team for building an excellent, well-maintained rules engine. FactUI would not be possible without it.

## F.A.Q.

**Q.** OMG it's full of macros. Why...!?

**A.** That's why it's fast. The ClojureScript version of Clara uses macros to build its RETE network at compile-time, so anything that defines Clara queries needs to use macros as well.

**Q**. Will I run into performance issues?

**A**. Maybe. It depends on what rules you write, and how complex the queries for your components are. FactUI easily supports tens of thousands of facts and thousands of simple rules and components. But it's certainly possible to write just a few complex rules, or rules that interact in unexpected ways, which will cause your performance to tank.

**Q**. The library seems a little bit memory-hungry...

**A**. Yup, it sure is. The RETE algorithm fundamentally works by trading of memory usage to increase speed. Know your limits, do some benchmarks.

**Q**. Can I dynamically generate rules!?!!

**A**. No. Calm yourself and embrace the fact that UI applications have to stop being abstract at some point.

## Future Improvments

### Datomic Functionality

Many useful features that Datomic provides are not currently supported. We will add support for these over time:

- Pull Expressions (both reactive and non-reactive)
- Aggregates in queries
- Predicates in queries
- "Transactor" functions

### Basic Implementation

Currently, FactUI works by putting a thin wrapper around Clara's external interfaces, enforcing Datom-style semantics (tempids, identity, upsert, cardinality-one) at the boundary. This works fine, but is a tiny bit hacky, as well as causing potential issues in a multi-threaded environment (i.e, not the browser.)

The "correct" way to do it is to implment new types to satisfy Clara's various Memory-related protocols. In theory, this would be both faster and safer. But it's also a lot of work, and a lot of work to get *right* since it integrates deeply with the internal engine.

The wrapper will do for now, but eventually it would be cool to do it the right way and see what performance looks like.