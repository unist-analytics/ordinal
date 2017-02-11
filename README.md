In this repository, there are are codes for classifying the imbalanced and ordinal data.
This R code is based on the knn method to classify data and improves the knn method to adjust for imbalanced data set and ordianl classification.

This repository includes, 

1) data,dat.Rdate, for test, which includes 6363 observations about disaster level, description of damage, date and type of disaster. 
2) function for classfying, kknn.ordinal.R
3) main for showing the result by the graph, akknn_type.R

implementation

Before running the main program, aknn_type.R, you should make two files, which called "source" and "input", and 
install the data,dat.Rdate, into "input" file and source function,"kknn.ordinal.R", into "source".
You must save files like this way to run main R program correctly. If you don't do this, the akknn_tyep.R will make an error of source and load functions.
If you run "akknn_type.R", you can see the graph for compairng the knn method, wknn and awknn.

To use this r program, we suggest you use the "cmd batch", which helps you to run R program without exeuting R or R studio.
To do this, first you have to set up path sutaible for R. 
https://www.java.com/ko/download/help/path.xml. This link lets you know how to set up the path. 
If you follow the steps of that website, you can find the path setting panel. Then, you have to add the R path, "C:\Program Files\R\R-3.3.2\bin"
Then, in cmd.exe, input R CMD BATCH "path about r file/akknn_type.R". After this, you can check result, akknn_type.Rout

