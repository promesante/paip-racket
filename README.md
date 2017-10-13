# Porting PAIP selected chapters from Common Lisp to Racket #

I am performing this port projects of the following chapters of Peter Norvig's classic text on AI, [Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp 1st Edition](https://www.amazon.com/Paradigms-Artificial-Intelligence-Programming-Studies/dp/1558601910/ref=sr_1_1?s=books&ie=UTF8&qid=1506786797&sr=1-1&keywords=paradigms+of+artificial+intelligence+programming) (also known as PAIP), an excellent entry point into Lisp and its dialects:

* **Chapter 11**: Logic Programming
* **Chapter 12**: Compiling Logic Programs
* **Chapter 22**: Scheme: An Uncommon Lisp
* **Chapter 23**: Compiling Lisp

Book's source code is available in the [author's Web site ](http://norvig.com/paip/README.html), and has been taken as this repo's initial version.

Common Lisp files to be ported will be:

1. Duplicated
2. Physically renamed, changing just its extension, from the original `lisp` to Racket's `rkt`
3. Each chapter and section will have its own git branch, in order to keep a tidy map between code here and book's contents

I will share the experience in [my blog](https://promesante.github.io/), in a section by section of PAIP fashion.

Comments there are more than welcome !
