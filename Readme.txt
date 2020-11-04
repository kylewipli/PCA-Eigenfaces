Assignment 2: Eigenfaces & PCA  (Eigenfaces Wipfli)
____________________________________________________

Included:
EigenfacesWipfli.R
this README file
a link to a gethub with 200 photos for training data
and 3 photos for testing data

AUTHORS
----------------------------------------------------------
Kyle Wipfli


REQUIREMENTS
-----------------------------------------------------
This application uses the packages Shiny and EBImage.
These can be found at:

Shiny: (https://shiny.rstudio.com/)

EBImage: (https://www.bioconductor.org/packages/release/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html)



How it Use it
-----------------------------------------------------
When you run the application you will have two options
on two separate tabs. 

The first tab looks at using PCA on simple pre-loaded 
data where you can select the number of eigenvectors to
keep.

The second tab takes two inputs:
     The first requires a list of multiple black and white
images that are all the same size called "choose your data 
set".  This is the training data that is needed in order to 
preform eigenface recognition.

     The second requires a single black and white of the same
size as each image in your training data. This input is called
"choose your face to recognize". This will be your image to 
classify as a face or not. 

There is also a slider which allows you to configure the amount 
of eigenfaces to keep when running the application such that 
you may tune the application so that it can correctly identify
the face. When you are finished loading and adjusting, hit the
run button to see the amount of relative information each
eigenface keeps, the last test face projected into the 
eigenface-space, and the results of a standard eigenface
classifier.


Configuration
----------------------------------------------------------
If you wish to configure the data for which PCA is used on
you can replace the two vectors inside the cbind functions 
with other two other vectors on lines 89, 94, and/or 99. Where
the first is your vector is your x-axis data and the second
is your y-data.

The second tab is not configurable.



