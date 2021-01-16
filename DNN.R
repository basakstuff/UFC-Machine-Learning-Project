##libraries####
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
library(data.table)
library(ggplot2) #data visualization
library(plotly) #interactive data visualization
library(psych) #correlation visualization helping
library(rattle) #graphing decesiion trees
library(caret) # machine learning
library(tree)
library(e1071)
library(rpart)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(class)
library(formattable)
library(randomForest)
library(neuralnet)
library(caTools)
######


df2 <- read.csv("df2.csv")


################################
df2 <- subset(df2, select=-c(R_fighter,B_fighter, date))
df2$title_bout <- as.numeric(factor(df2$title_bout))
df2$weight_class <- as.numeric(factor(df2$weight_class))
df2 <- subset.data.frame(df2, subset= Winner!="Draw")
df2$Winner<-factor(df2$Winner)
#df2$Winner <- as.numeric(factor(df2$Winner))

###########################################################


prepare_data <- function(df, output_col_n, ratio, normalized = TRUE){
  # Randomisation with train to test ratio
  random <- sample(1:nrow(df), ratio * nrow(df))
  
  # Normalisation
  normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}
  if(normalized == TRUE){
    df_normalised <- as.data.frame(lapply(df[,-output_col_n], normalised))
  }
  else{
    df_normalised <- df[,-output_col_n]
  }
  
  # Extract training set
  df_train <- df_normalised[random,] 
  # Extract testing set
  df_test <- df_normalised[-random,]
  
  # Output category
  df_target_category <- df[random,output_col_n]
  df_test_category <- df[-random,output_col_n]
  
  toReturn <- list("training_set" = df_train, "testing_set" = df_test,"target_category" = df_target_category,"test_category"= df_test_category)
  
  return(toReturn)
}


# ************************************************
# main code goes below:
# ************************************************

UFC_DATA <- df2 # Normal data

#grep("Winner", colnames(UFC_DATA))

set.seed(123)
print("Running ... ")

UFC_DATA$Winner = as.character(UFC_DATA$Winner) # Factor to chr
UFC_DATA$Winner[UFC_DATA$Winner == "Blue"] <- 1 # encode blue as 1
UFC_DATA$Winner[UFC_DATA$Winner == 'Red']  <- 0 # encode red as 0
UFC_DATA$Winner = as.numeric(UFC_DATA$Winner) # as numeric

output_col_n = 1 # output target
UFC_DATA[-output_col_n]<-scale(UFC_DATA[-output_col_n]) # centre dataset

# Train/test split with 0.8 ratio with no normalisation   
prepared_dataset <- prepare_data(UFC_DATA, output_col_n = output_col_n,ratio = 0.1, normalized = FALSE)



# Concat X and y for NN training
nn_data <- data.frame(Winner=prepared_dataset$target_category, prepared_dataset$training_set)

# DNN Classifier
dnn_classifier <- neuralnet(Winner ~ . , data=nn_data, hidden=c(3,2),stepmax=1e6, linear.output=FALSE, threshold=0.01,lifesign = "full")
print("---------------------------------------------------------")

# Plot Neural Network
plot(dnn_classifier)


# DNN Predictions on unseen testing set
nnpred <- neuralnet::compute(dnn_classifier, prepared_dataset$testing_set)[["net.result"]]
nnpred[nnpred > 0.5] <- 1 # if probability > 0.5, classify as 1 (Blue) otherwise 0 (Red)
nnpred[nnpred <= 0.5] <- 0
nnpred <- as.factor(nnpred) # convert to factor


# Confusion Matrix
print("Normal DNN Consufion Matrix:")
cm <- confusionMatrix(nnpred, as.factor(prepared_dataset$test_category))
print(cm)

print("---------------------------------------------------------")


# Print accuracy
print(paste("DNN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))

#save(dnn_classifier, file="test.rda")
# Save DNN model
#saveRDS(dnn_classifier, file = "DNN_MODEL.rds"
#        ,ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)

print("~~ DNN ENDED:")

