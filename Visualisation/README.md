This project was to get myself comfortable with how Haskell handles IO in addition to getting exposure to the Gloss visualisation library.

The purple sub directory contains .txt files with voting results.

The purpose of this project is to visulaise different states/districts according to their voting pattern.

Red for Republican and Blue for Democrats.

Furthermore, there is an option to view the resulting maps in purple - this shows the distribution of R vs D votes in a given State/District.

You must use the ghc compiler to run this program.

Usage:

./Main region [-w | -rgb year | -p year]

if no flag specified, -w used by default.

-w: produces a wireframe map (i.e., an outline map) for a region.

-rgb year : produces a Red or Blue map for the region and year.

-p year : produces a purple map for the region and year.


All voting data files can be found in the `.zip` file.

Cheers!
