In this repository, there are codes for ordinal classification of the imbalanced data.
This R code is based on the knn method to classify data and improve the knn to adjust for imbalanced data set and ordianl classification.

This repository includes, 

1) data for test, which includes 6363 observation about disaster level, description of damage, date and type of disaster. 
2) source code for classfication
3) main for showing the result by the graph

implementation

Before running the main code, you should make two folders, which are named by "source" and "input", and 
store data file into the "input" folder and source code,"kknn.ordinal.R", into "source" folder.
You must save files like this way to run main R program correctly. 
If you run "akknn_type.R", you can see the figures for compairing the knn method, wknn and awknn.

To use this R program, we suggest you use the "cmd batch", which help you run R program without executing R or R studio.
To do this, first, you have to set up path sutaible for R. 
https://www.java.com/ko/download/help/path.xml. This link let you know how to set up the path. 
Then, in cmd.exe, input R CMD BATCH "path about r file/akknn_type.R". After this, you can check result, akknn_type.Rout

