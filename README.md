CLife - Conway's Game of Life
=============================

This is an attempt of implementation of the famous Conway's game of life.  
At the moment it is really simple because I started studying Common Lisp recently, and I wrote this program in order to improve my programming skills.  

Use
---

If you want to try it you should load the file and call the function 'run':

For instance, if you type this:
> (run)

It should starts the evolution of a random world which has the 
properties defined in the source.

Parameters
----------

The parameters which can be modified are:  
* the size of the world  
* the update interval between each generation  
* the characters used for representing alive and dead cells   

Dependencies
------------
* cl-opengl
* cl-glut
* cl-glu

