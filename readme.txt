		TURBO PASCAL UNITS IN ADA


I Introduction

   Gnoga framework facilitate graphic programming in Ada.
And also ease translation of Pascal applications into Ada
language.

   For those who want to translate a Pascal language application
from Turbo Pascal, I provide some bindings of Turbo Pascal 7.0 units.

   This package is a very first version of TP7 units based on Gnoga.

In the package you will find some test examples.


II How to use this package

   GNAT GPL 2017 compiler (with Ada 2005) and Gnoga 1.3a library must be installed before,
see XNAdaLib-2017 on Blady web site.
   Your Ada program translated from Turbo Pascal (P2Ada is recommended)
must be referenced in main.adb source with TP7.Init subprogram.
   Executable is built and run with following instructions for example:

$ gnoga_make new main hello_world
$ cd main
$ cd js
$ git clone https://github.com/ajaxorg/ace-builds.git
$ cd ..
# Modify src/main.adb
# Add 'with "TP7";' to src/main.gpr
$ make
$ ./bin/main

  The program is composed of a control view : start and stop execution
of your program with a Debug option, a text view if TP7.CRT is used,
a graphic view if TP7.Graph is used.


III Use and licence

   This library is provide only for test, as it is.
It is not aimed to build any software other than for test.
All parts indicated belong to their copyright holder.
  This library is usable under CeCILL licence,
see Licence_CeCILL_V2-en.txt.

Pascal Pignard,
December 1998, June 1994, October 2002, September-December 2011, January-November 2012,
January 2014, June 2014, September 2015, September 2016, January 2018.
http://blady.pagesperso-orange.fr

