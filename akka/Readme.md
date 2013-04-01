
### Akka Implementation

This folder should eventually contain an implementation using Scala's
Akka library for distributed actors.

http://akka.io/

## Running

Running the code requires that one install "sbt", and then use the sbt
run command.

# sbt

This project is managed using "sbt" the "simple build tool". While it
is in fact far from simple it is easy to use. As long as you have a
working JVM setting up sbt is very easy:

http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html

There are no other dependencies, configure sbt and you are done.

# Actually Running

To actually run the code simply start sbt in the project directory
(this one!) with 'sbt'. This will launch sbt's shell. The 'run'
command will then run the project. 'test' will run the tests.

It will appear that sbt is downloading the *entire internet* when you
run it. This is normal.

DISCLAIMER: At the time of writing this Readme there are no actual
interesting files, so running stuff is going to be anti-climactic.

sbt also provides a console reachable by typing 'console' at the sbt
prompt. This console will be in the classpath of the project code, so
you can import the project package and run it from here.

If you just want to compile the code you can do that too with
'compile'.

## Current Status

The initial commit will be a very simple akka template project. It
will probably print something inane when run, possibly through
unreasonably convoluted means.
