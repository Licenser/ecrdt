This project is some experimentation with CRDT's in Erlang, please be aware that it's an **experiment** meaning not suitable for production use, not optimised for either speed or space and probably broken in some places.

The code is written to look nice, be understandable not to make the most out of every operation.

Consider yourself warned, now go have some fun with it!

## Naming conventions

The first letter of the module name indicates what kind of CRDT the mudle implements:

* m* - implements CmRDT's (message based)
* v* - implements CvRDT's (state based)