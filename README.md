This project is some experimentation with CRDT's in Erlang, please be aware that it's an **experiment** meaning not suitable for production use, not optimised for either speed or space and probably broken in some places.

The code is written to look nice, be understandable not to make the most out of every operation.

Consider yourself warned, now go have some fun with it!

## Naming conventions

The first letter of the module name indicates what kind of CRDT the mudle implements:

* m* - implements CmRDT's (message based)
* v* - implements CvRDT's (state based)


## Resources
* [Call me maybe: Riak](http://aphyr.com/posts/285-call-me-maybe-riak)
* [Marc Shapiro](http://pagesperso-systeme.lip6.fr/Marc.Shapiro/pubs.html#SEC-SSS)
	* [CRDTs: Consistency without concurrency control](http://pagesperso-systeme.lip6.fr/Marc.Shapiro/papers/RR-6956.pdf)
	* [An Optimized Conﬂict-free Replicated Set](http://pagesperso-systeme.lip6.fr/Marc.Shapiro/papers/RR-8083.pdf)
	* [Conﬂict-free Replicated Data Types](http://hal.inria.fr/docs/00/61/73/41/PDF/RR-7687.pdf)
	* [A comprehensive study of
Convergent and Commutative Replicated Data Types](http://pagesperso-systeme.lip6.fr/Marc.Shapiro/papers/Comprehensive-CRDTs-RR7506-2011-01.pdf)
	* [Designing a commutative replicated data type](http://pagesperso-systeme.lip6.fr/Marc.Shapiro/papers/Commutative-Replicated-Data-Type-RR-6320_2007-10.pdf)
	* [A commutative replicated data type for cooperative editing](http://pagesperso-systeme.lip6.fr/Marc.Shapiro/papers/icdcs09-treedoc.pdf)