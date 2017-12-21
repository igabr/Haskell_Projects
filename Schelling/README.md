In this folder, you will find all the files required to run a Schelling model.

The files themselves have lots of commentary and function headings. This will help give some insight into my logic and into what the functions do.

This was a 'beast' of a project, as such, there are LOTS of functions.

You can execute the program by doing the following:

Step 1: ghc Main.hs

Step 2: ./Main

You will be presented with a screen that has a default grid and some text that tells you how to alter the parameters.

I have noticed in my own testing, that the fastest way to "start seeing movement" is to keep the default parameters, but alter the grid size to 20.

I was only able to implement a full run of the model. This is equivalent to watching a movie in that all steps to convergence will be shown sequentially.

Furthermore, I have noticed that larger grid configurations or those that contain a 5 i.e. 15, 25 etc seem to take longer than those that are divisible by 10.

As such, it may take a while for the visualization to get started - again, a good test case is the default parameters with the grid size set to 20.
