# RFS 0 (Ribbit Feature Standard)

## Status

This RFS is still ongoing and could change at any time

## Abstract

In all rvms, there will be a need to convert host values to scheme (ribbit) values. This document wants to establish a standard for doing so. For a RVM to implement the RFS-0, a set of functions and features must be defined, with a standard notation. For example :  
 - `bool2scm` do the conversion from a host boolean to a scheme (ribbit) boolean
 - `scm2host` does a conversion from a host value to the corresponding value in scheme

## Presentation

Any feature defined to do a conversion must follow this notation :

*For conversions from host to scheme* : `[host type]2scm`

*For conversions from scheme to host* : `scm2[host type]`

Where `[host type]` changes depending on the type being converted. If the langage is dynamic, `host` can be used instead to signify that the type will be checked and converted on-the-fly.

The function name must be the same and available at any `@@(location ...)@@` defined inside of the rvm. This way, user-defined primitives, as well as user-defined features can use value conversion. 

