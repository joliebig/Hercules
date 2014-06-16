Hercules
========

Hercules is a source-code transformation tool.
It transforms compile-time variability (using `#ifdefs`) into run-time variability (using if statements of the programming language).
The transformation tool relies on the variability-aware parsing and analysis infrastructure [TypeChef](https://ckaestne.github.io/TypeChef/).


Installation and Usage
----------------------

Hercules requires a modified version of TypeChef. To install it simply run:

    git clone git://github.com/aJanker/TypeChef.git
	cd Typechef
    ./publish.sh
	cd ..

To install the last version of Hercules simply run:

	git clone git://github.com/joliebig/Hercules.git
	cd Hercules
	./mkrun.sh

Both projects folders `Hercules` and `TypeChef` should now be in your current working directory! In order to use Hercules simply run `./ifdeftoif.sh` in the Hercules directory.

If there are any problems with the initialization of the java VM change the VM options in the files `/TypeChef/sbt` and `/Hercules/mkrun.sh`.