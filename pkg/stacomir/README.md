---
title: StacomiR
author: Marion Legrand, Cédric Briand
output: github_document
toc: true
---  

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/stacomiR)](https://cran.r-project.org/package=stacomiR)



# stacomiR <img src="man/figures/logo.png" align="right" />




Installation
-------------------------

The package is available from CRAN, a development version is available from
R-Forge.


```r
install.packages("stacomiR", repos="http://R-Forge.R-project.org")
```

Usage
-------------------------

Launch the graphical interface


```r
stacomi(gr_interface = TRUE, login_window = TRUE, database_expected = TRUE)
```

The program can be launched to use from the command line


```r
stacomi(gr_interface = TRUE, login_window = TRUE, database_expected = FALSE)
```
Introduction
---------------------------
Link with open source database, 
objective evaluation and sharing stock indicators for migratory fishes. 
No R package
JAVA + postgres + R



Data structure
-------------------    
The structure of the data contained in the open source postgresql database
"bd_contmig_nat" is based on the following concepts.

### Station
Station is a fish migration monitoring and in general, it is considered to be a
section of a watercourse allowing the evaluation of fish upstream or downstream
migration.
Station may extend to the whole section of the migration axis to evaluate a
migratory flow on a section subdivided into several natural or artificial
channels. A station consists physically of as many weirs as hydrographic
sections monitored (river, channels, etc.).

### Weir
The concept of weir used in the context of fish migration monitoring database
refers to a system blocking or guiding the migratory flow like : (i) dam, (ii)
electric guide barrier, (iii) netting dam, etc.

### Crossing device   
A crossing device is a passageway that allows and concentrates the migratory
flow between upstream and downstream of a weir. We can have several types like
:(i) fishway, (ii) spillway, (iii) fish elevator, etc.

It is possible to have more than one crossing device on a same weir.

### Counting device
A couting device is a set of equipment installed on a crossing devise used to
monitor the migratory flow that passes through it.

### Operation

### Fishes


 Package structure
--------------------

Working examples
-------------------------

###        Interface

###        Command line

License
-------
Released under GPL-2.
