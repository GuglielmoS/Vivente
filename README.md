Vivente - Conway's Game of Life
=============================

This is an attempt of implementation of the famous Conway's game of life.  
At the moment it is really simple because I started studying Common Lisp recently, and I wrote this program in order to improve my programming skills.  

Use
---

If you want to try it you should load the file and call the function 'run'.  
Here are some examples of use:  

> (run)            ;; run the evolution of a world of predefined size  
> (run '(100 100)) ;; run the evolution of a 100x100 world  

In the case that you want to see the evolution to proceed slowly, you can
modify the update interval by changing the value of the respective parameter.  
For changing the size of the world, instead, you should modify the 
*default-world-size* parameter or call the 'run' function with a list of two
items as argoument, each representing the width and the height respectively.  
An example of the latest use can be seen in the examples presented over here.  

Parameters
----------

The parameters that can be easily modified in the source are the following:  
* size of the world  
* update interval between each generation  
* size of the window

Dependencies
------------
* asdf  
* cl-opengl  
* cl-glut  
* cl-glu  

Screenshot
----------
![Vivente image](http://postimage.org/image/7f7sskj4x/)
