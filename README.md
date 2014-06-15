Hercules
========

Hercules is a source-code transformation tool.
It transforms compile-time variability (using #ifdefs) into run-time variability (using if statements of the programming language).
The transformation tool relies on the variability-aware parsing and analysis infrastructure [TypeChef](https://ckaestne.github.io/TypeChef/).


Installation and Usage
----------------------

Hercules requires a modified version of TypeChef. To install it together with Hercules simply run:

    git clone git://github.com/aJanker/TypeChef.git
	git clone git://github.com/joliebig/Hercules.git
    cd TypeChef
    ./publish.sh
	cd ../Hercules
	./mkrun.sh
