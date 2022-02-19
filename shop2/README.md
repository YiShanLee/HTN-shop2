# shop2
### Manual for Package Use
- Please find your path on `local projects ` directory in windows files. 
	ex. 
	`C:\Users\Eve\quicklisp\local-projects`
- Copy `shop2` file into the `local projects` repository.
- Open Lisp from the directory `C:\Users\Eve\quicklisp\local-projects`.
- Type in `(ql:quickload :shop2)`. 
++ When errors show up in console, please choose the second option `<accept>` to let the filed be loaded ++
- Type in `(in-package shop2)` to use functions from shop2 file.
++ When there is a need to go back to default package, please type in `(in-package :cl-user)`.

[] progress
- all of the functions in readhtn.lisp is working, you can type in any function to try on yourself within shop2 package `(in-package shop2)`
- the functions in shop2.lisp still need to try its export. 

## References
* [Beginners from Youtube](https://youtu.be/SPgjgybGb5o)
* [Common Lisp book](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node117.html)
* [Tutorialspoint](https://www.tutorialspoint.com/lisp/lisp_packages.htm)
* [Some issue from stackoverflow](https://stackoverflow.com/questions/22524213/cant-access-cl-user-symbols-in-new-package-defined-with-make-package)
* [Small example to buildup](https://lispmethods.com/libraries.html#packages)
* [original shop2 files buildup](https://github.com/cl-axon/shop2)

## License

Specify license here

++ Attach me if some issues happend
