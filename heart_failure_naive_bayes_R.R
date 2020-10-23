#install.packages(“e1071”)
#Loading the library
library(e1071)
#Save into a data frame and view it
df=as.data.frame(read.csv('heart_failure_clinical_records_dataset.csv'))

#data exploration

#general summary of the data
str(df)

#exploring mean and percentiles
#install.packages('Hmisc')
library(Hmisc)
inf <- describe(df)
inf

#install.packages("tidyverse")
library(tidyverse)

#count of fatal events vs non fatal
dplyr::count(df, MORTALITY_EVENT, sort = TRUE)
#count of high bloodpressure vs not
dplyr::count(df, high_blood_pressure, sort = TRUE)

#ejecton fraction vs mortality even scatterplot
plot(df$ejection_fraction, df$MORTALITY_EVENT, main="high bloodpressure vs mortality even scatterplot",
     xlab="ejecton fraction ", ylab="mortality event ", pch=19)

#train test split
df_train <- df[1:219,]
df_test <- df[220:298, ]
df_train_labels <- df[1:219,]$MORTALITY_EVENT
df_test_labels <- df[220:298,]$MORTALITY_EVENT


#Model Training

#Loading the library
library(mlr)

#Create a classification task for learning
#and specify the target feature
task = makeClassifTask(data = df_train, target = "MORTALITY_EVENT")

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")

#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned
NB_mlr$learner.model

#Prediction

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = df_test[,1:12]))

#evaluation and accuracy

#install.packages("gmodels")
library(gmodels)

##Confusion matrix to check accuracy

CrossTable(predictions_mlr[,1], df_test$MORTALITY_EVENT, prop.chisq = FALSE, chisq = FALSE,
           prop.t = FALSE,
           dnn = c("Predicted", "Actual"))
