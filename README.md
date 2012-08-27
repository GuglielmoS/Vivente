CLife - Conway's Game of Life
=============================

This is an attempt of implementation of the famous Conway's game of life.

At the moment it is really simple because I started studying Common Lisp recently, and I wrote this program in order to improve my programming skills.

Nevertheless I've planned to modify the output destination (now it prints on the terminal) by creating a graphical interface through the use of OpenGL.


Use
---

If you want to try it you should load the file and call the function 'evolve':

For instance, if you type this:
> (evolve)

It should starts the evolution of a random world which has the 
properties defined in the source.


Parameters
----------

The parameters which can be modified are:
1. the size of the world
2. the update interval between each generation
3. the characters used for representing alive and dead cells 
