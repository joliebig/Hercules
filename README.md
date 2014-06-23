Hercules
========

Hercules is a source-code transformation tool.
It transforms compile-time variability (using `#ifdefs`) into run-time variability (using if statements of the programming language).
The transformation tool relies on the variability-aware parsing and analysis infrastructure [TypeChef](https://ckaestne.github.io/TypeChef/).


Installation and Usage
----------------------

Hercules requires a modified version of TypeChef. To install it simply run:

    git clone https://github.com/aJanker/TypeChef.git
    ./TypeChef/publish.sh

To install the last version of Hercules simply run (from the same folder where the `TypeChef` directory lies):

	git clone https://github.com/joliebig/Hercules.git
	./Hercules/mkrun.sh

Both projects folders `Hercules` and `TypeChef` should now be in your current working directory! In order to use Hercules simply run `./ifdeftoif.sh` in the Hercules directory.

If there are problems creating the Java Virtual Machine tweak the `-Xmx`, `-Xss`, `-XX:PermSize=`, `-XX:MaxPermSize=` parameters in `Hercules/ifdeftoif.sh`, `Hercules/mkrun.sh`, `TypeChef/publish.sh`