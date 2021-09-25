# What is Lithium?

Lithium is an attempt at several things at once:

 * An assembler for x86 CPUs, written in Clojure and using
   Clojure S-expressions as its input data. This part has been
   first announced in [this blog post][1].
 * A compiler for a toy Lisp-like language, using that assembler as a
   backend. The compiler is being written following the guidelines
   found in a paper "[An Incremental Approach to Compiler Construction][2]."

The purposes of Lithium are, first and foremost, to learn and to have fun.

# Try it out

In the REPL:

```clojure
(require '[lithium.compiler :as compiler])
(compiler/compile-and-run! "examples/stripes-grey.clj")
```

# License

Unless otherwise noted, code in this repository is copyright by
Daniel Janus and released under the MIT license:

```
Copyright 2012–2021 Daniel Janus

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```

The file `resources/capture.com` comes from the DOS Utilities Collection as obtained
from http://www.pc-tools.net/dos/dosutils/, is copyright by Jem Berkes, and carries the
following copyright notice:

```
You may use and share the programs in this freeware package. However,
remember that YOU MAY NOT MODIFY THESE PROGRAMS IN ANY WAY. They are
NOT PUBLIC DOMAIN, but rather COPYRIGHTED FREEWARE.

I, the author, take NO RESPONSIBILITY for any damage that may result
from using any of these programs.

Jem Berkes <jberkes@pc-tools.net>
```

The assembly code in `register-dump` is a s-expression version of a snippet
that comes from http://www.fysnet.net/yourhelp.htm and is copyright by
Forever Young Software.

 [1]: https://blog.danieljanus.pl/2012/05/14/lithium/
 [2]: http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
