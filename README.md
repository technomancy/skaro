# Skaro

A silly [robots](https://en.wikipedia.org/wiki/Robots_%28BSD_game%29)
game, implemented in a number of lisp dialects: Scheme (Chicken),
Racket, Clojure, and Emacs Lisp.

In each case I tried to stick with an idiomatic style for each
language. I also limited myself to using no third-party libraries,
(even though in some cases it would have resulted in neater code) in
order to better capture the spirit of the language.

The Scheme version is very imperative, but the Racket version is
functional because Racket has much better support for immutable data
structures. The Clojure version obviously is very functional and uses
destructuring heavily. The Emacs Lisp version is the most imperative
of all, but it includes a full textual buffer-based user interface
that is a lot more sophisticated than the read/print interface used by
the other versions.

No attempt is made to compare performance across these
implementations. Performance tuning and measurement is a very nuanced
task that is not possible to do well on a runtime you've spent merely
a matter of days on.
