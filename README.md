# FactUI

FactUI is a library for efficiently rendering user interfaces declaratively from a flat, tuple-oriented data structure, as well as manipulating such structures.

## Rationale

Other ClojureScript/React frameworks have already broken important ground in establishing a pattern for data-driven interfaces: the visible UI is a pure function of data. This is a strong concept which has lead to powerful, simple UI programming idioms.

However, this pattern leaves room for different answers to the following questions:

1. What is the structure of the "application data" that ultimately drives the UI?
2. What is the pattern for making updates to the application data?

Each library answers these questions in its own way.

FactUI gives its own answers, answers which resolve a number of particularly difficult issues that often end up being pain points (especially in larger applications.)

## Usage

TODO

## Credits

Many thanks to [Precept](https://github.com/CoNarrative/precept) for being the immediate inspiration for this approach. Although I've been thinking about database-driven and rule-driven UIs for some time, Precept pulled a lot of things into focus and crystallized the kind of API I really wanted to see, which FactUI represents.

Also thanks to [Ryan Brush](https://github.com/rbrush) and the rest of the [Clara](https://github.com/cerner/clara-rules) team for building an excellent, well-maintained rules engine. FactUI would not be possible without it.

## F.A.Q.

Q. OMG it's full of macros. WHY!?
A. That's why it's fast. The ClojureScript version of Clara uses macros to build its RETE network at compile-time, so anything built on top of Clara needs to use macros as well.

Q. Will I run into performance issues?
A. Maybe. It depends on what rules you write, and how complex the queries for your components are. FactUI easily supports tens of thousands of facts and thousands of simple rules and components. But it's certainly possible to write just a few complex rules, or rules that interact in unexpected ways, which will cause your performance to tank.

Q. The library seems a little bit memory-hungry...
A. Yup, it sure is. Sorry. The RETE algorithm fundamentally works by trading of memory usage to increase speed. Know your limits, do some benchmarks.

Q. Can I dynamically generate rules!?!!
A. No. Put down the pipe and embrace the fact that UI applications have to stop being abstract at some point.

## Future Improvments

Currently, FactUI works by putting a thin wrapper around Clara's external interfaces, enforcing Datom-style semantics (tempids, identity, upsert, cardinality-one) at the boundary. This works fine, but is a tiny bit hacky, as well as causing potential issues in a multi-threaded environment.

The "correct" way to do it is to implment new types to satisfy Clara's various Memory-related protocols. In theory, this would be both faster and safer. But it's also a lot of work, and a lot of work to get *right* since it integrates deeply with the internal engine.

The wrapper will do for now, but eventually it would be cool to do it the right way and see what performance looks like. 