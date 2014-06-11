Hercules
========

Hercules is a source-code transformation tool.
It transforms compile-time variability (using #ifdefs) into run-time variability (using if statements of the programming language).
The transformation tool relies on the variability-aware parsing and analysis infrastructure [TypeChef](https://ckaestne.github.io/TypeChef/).


Installation and Usage
----------------------

Hercules requires a modified version of TypeChef. To install it simply run:

    git clone git://github.com/aJanker/TypeChef.git
    cd TypeChef
    ./sbt clean update compile
    ./sbt mkrun
    ./sbt publish-local

To install the last version of Hercules simply run:

    git clone git://github.com/joliebig/Hercules.git
    cd Hercules
    ./sbt clean update compile
    ./sbt mkrun
