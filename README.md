kiama-source-to-source-translator
=================================
This project implements a purely functional source to source translator for a tiny Expression Language using Kiama - a Scala library for language processing applications. 

Simple, yet exhibiting all sorts of interesting architectural challenges when structuring a source to source translator.
It parses Expression Language programs into an immutable AST, decorates the tree with attributes (effectively solving the AST typing problem - see http://lambda-the-ultimate.org/node/4170) and uses these attributes to guide the consecutive optimization phases, leading to more and more optimized (idiomatic) target code. 

Each optimization phase rewrites the AST into a new, optimized yet semantically equivalent AST. These transformations are implemented using Kiama's rewrite rules/strategies, which allow for decomposing the translation into simple, composable rewrite rules, which can be applied to the AST using different traversal strategies.

Finally, the AST is pretty printed.
