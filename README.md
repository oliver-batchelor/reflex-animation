reflex-animation [![Build Status](https://secure.travis-ci.org/Saulzar/reflex-animation.png?branch=master)](http://travis-ci.org/Saulzar/reflex-animation)
====================


This package provides a set of functions for creating and playing continuous animations of the form Time -> a.
Finite animations (with a length) and infinite animations complement one another, we chose a representation of 
finite animations which has only a length (and not a starting point) to keep things simple. If needed such animations
can be converted to infinite animations, combined, and clipped as required.