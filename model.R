####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Importing libraries
library(RCurl) # for downloading the iris CSV file
library(randomForest)
library(caret)
library(readr)
library(corrplot)
library(plyr)

setwd("C:/Users/admin/Downloads/Methods of DS/Project/diabetes/rshiny")

# Importing the diabetes data set
diabetes <- read_csv("diabetes_binary_5050split_health_indicators_BRFSS2015.csv", col_names = TRUE)
diabetes <- diabetes[sample(nrow(diabetes), 1000), ]
diabetes$Diabetes_binary <- factor(diabetes$Diabetes_binary)

diabetes$Diabetes_binary <- revalue(diabetes$Diabetes_binary, c("0" = "No diabetes", "1" = "diabetes"))
head(diabetes)
str(diabetes)
corrplot(cor(diabetes, method="spearman"), method="number")
# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(diabetes$Diabetes_binary, p=0.8, list = FALSE)
TrainingSet <- diabetes[TrainingIndex,] # Training Set
TestingSet <- diabetes[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
TrainSet$Diabetes_binary = factor(TrainSet$Diabetes_binary) 

head(TrainingSet)
str(TrainSet)

# Building Random forest model



#model <- randomForest(Diabetes_binary ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)
#varImp(model)
#varImpPlot(model,type=2)

model <- randomForest(Diabetes_binary ~ BMI + Age + HighBP + HighChol, data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)
summary(model)
# Save model to RDS file
saveRDS(model, "model.rds")
