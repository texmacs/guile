# box module

## Overview

This directory includes an example program for extending Guile with a new (and even useful) data type.

The `box` program created by this example is nearly identical to the one produced in directory ../box, with one (important) difference: The interpreter in this directory will place all defined primitive procedures in a module called (box-module).  That means that this module must be used before the primitives can be accessed.

## Build Instructions

To build the example, simply type

```shell
  make box
```



in this directory.

The resulting `box` program is a Guile interpreter which has one additional data type called `box`.

## The Box Data Type

A box is simply an object for storing one other object in.  It can be used for passing parameters by reference, for example.  You simply store an object into a box, pass it to another procedure which can store a new object into it and thus return a value via the box.

### Usage

Box objects are created with `make-box`, set with `box-set!` and examined with `box-ref`.  Note that these procedures are placed in a module called (box-module) and can thus only be accessed after using this module.  See the following example session for usage details:

### Example Session

```shell
guile> (use-modules (box-module))
guile> (define b (make-box))
guile> b
#<box #f>
guile> (box-set! b '(list of values))
guile> b
#<box (list of values)>
guile> (box-ref b)
(list of values)
guile> (quit)
```

