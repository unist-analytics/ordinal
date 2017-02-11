In this repository, there are are codes for classifying the imbalanced and ordinal data.

This main R code, akknn_type.R, is based on the awknn method which improves the "wknn method" that condisders weights in stead of evenly weighting to different distances. Awknn considers the imbalanced data set like our test data set, disaster level, which doesn't have same number of observation in each classes, on the other hand, wkknn just assume that data set is balanced. Thus, when using the wknn method for imbalanced data, you have to apply other methods to make balaned data set. However, when using our awknn method, you doesn't have to do that.   

This repository includes, 

1) data,dat.Rdata, for test, which includes 6363 observations about disaster level, description of damage, date and type of disaster
2) function for classfying, kknn.ordinal.R
3) main for showing the result by the graph, akknn_type.R.

Implementation

Before running the main program, aknn_type.R, you should make two files, which called "source" and "input", and 
install the data,dat.Rdata, into "input" file and source function,"kknn.ordinal.R", into "source".
You must save files like this way to run main R program correctly. If you don't do this, the akknn_tyep.R will make an error of source and load functions.
If you run "akknn_type.R", you can see the graph for compairng the knn method, wknn and awknn. 

To use this r program, we suggest you use the "cmd batch", which helps you to run R program without exeuting R or R studio.
To do this, first you have to set up path sutaible for R. 
https://www.java.com/ko/download/help/path.xml. This link lets you know how to set up the path. 
If you follow the steps of that website, you can find the path setting panel. Then, you have to add the R path, "C:\Program Files\R\R-3.3.2\bin"
Then, in cmd.exe, input R CMD BATCH "path about r file/akknn_type.R". After this, you can check result, akknn_type.Rout

By seeing the result graph, accuracy, you can verify that the performance of classifying the classes is the best whne using the awknn.

Example-classifying the level of disaster.

The data set, dat.Rdata includes the level of disaster, description of damage, which is from newspaper, date and so on. The level is 0~5; the level from 1~5, which the number of each levels is imbalanced, is a case which is decided by institution based on some criteria like outcomes and strengths of the disaster, the level 0 means that the risk event is not classified to appropriate level because of shortage of accurate information. However, our R program can classify the undecided events based on description comparing the description of decided events. By running the akknn_type.R, the program makes the matrix showing the frequency of terms in description. Based on this measurment awknn is executed so we can classify the level 0 events into each appropriate level. 
