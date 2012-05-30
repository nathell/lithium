Lithium is an attempt at several things at once:

 * An assembler for x86 CPUs, written in Clojure and using
   Clojure S-expressions as its input data. This part has been
   first announced in [this blog post][1].
 * A compiler for a toy Lisp-like language, using that assembler as a
   backend. The compiler is being written following the guidelines
   found in a paper "[An Incremental Approach to Compiler Construction][2]."

The purposes of Lithium are, first and foremost, to learn and to have fun. 

License: MIT.

 [1]: http://blog.danieljanus.pl/130656082
 [2]: http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf