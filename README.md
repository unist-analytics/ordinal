In this repository, there are are codes for classifying the imbalanced and ordinal data.
This R code is based on the knn method to classify data and improve the knn to adjust for imbalanced data set and ordianl classification.

This repository includes, 

1) data for test, which includes 6363 observation about disaster level and description of damage. 
2) function for classfying
3) main for showing the result by the graph

implementation

Before running the main program, you should make two files, which called "source" and "input", and 
install the data into "input" file and source function,"kknn.ordinal.R", into "source".
You must save files like this way to run main R program correctly. 
If you run "akknn_type.R", you cann see the graph for compairng the knn method, wknn and awknn.

To use this r program, we suggest you use the "cmd batch", which help you run R program without exeuting R or R studio.
To do this, first you have to set up path sutaible for R. 
https://www.java.com/ko/download/help/path.xml. This link let you know how to set up the path. 
Then, in cmd.exe, input R CMD BATCH "path about r file/akknn_type.R". After this, you can check result, akknn_type.Rout

