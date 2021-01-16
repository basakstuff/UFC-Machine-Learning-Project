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
######

df2 <- read.csv("df2.csv")

################################
df2 <- subset(df2, select=-c(R_fighter,B_fighter, date))
df2$title_bout <- as.numeric(factor(df2$title_bout))
df2$weight_class <- as.numeric(factor(df2$weight_class))
df2$Winner<-factor(df2$Winner)
#df2$Winner <- as.numeric(factor(df2$Winner))

###########################################################


prepare_data <- function(df, output_col_n, ratio){
  # Randomisation with train to test ratio
  random <- sample(1:nrow(df), ratio * nrow(df))
  
  # Normalisation
  normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}
  df_normalised <- as.data.frame(lapply(df[,-output_col_n], normalised))
  
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


perform_knn <- function(df_train, df_test, df_target_category,
                        df_test_category, k){
  
  #Run the knn function
  df_KNN <- knn(train = df_train,test = df_test,cl = df_target_category,k = k)
  df_TAB <- table(df_KNN,df_test_category)
  
  #Out put the accuracy.
  accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  
  toReturn <- list("accuracy" = accuracy(df_TAB), "predicted" = df_KNN)
  return(toReturn)
  
}


# ************************************************
# main code goes below:
# ************************************************
k_value <- c()
avg_accuracy <- c()


UFC_DATA <- df2 # Normal data

#grep("Winner", colnames(UFC_DATA))

set.seed(123)

# Train/test split with 0.75 ratio
prepared_dataset <- prepare_data(df = UFC_DATA,output_col_n = 1, ratio = 0.8)


# KNN: Experiment tange of K values:    # note: initially, values 1 to 100 were tested as k.
for(k in 1:50){
  k_value <- c(k_value,k)
  
  # accuracy per run
  accuracies <- c()
  
  # number of runs per K    # note: initially, tested for 30 runs
  for(i in 1:5){
    accuracies<- c(accuracies,perform_knn(df_train = prepared_dataset$training_set, df_test = prepared_dataset$testing_set, df_target_category = prepared_dataset$target_category,
                                          df_test_category = prepared_dataset$test_category, k=k)$accuracy)
  }
  print(paste("KNN K=", k, "Average Accuracy in 30 run",mean(accuracies)))
  avg_accuracy <- c(avg_accuracy,mean(accuracies))
}

print("---------------------------------------------------------")


# Table of k-values & average accuracies
acc_df <- data.frame(k_value,avg_accuracy,pca_k_value,pca_avg_accuracy)

#write.csv(acc_df,"acc_df_2_70_e.csv", row.names = FALSE) #export df

# KNN and PCA-KNN Evaluation on K = 70
knn_pred <- perform_knn(df_train = prepared_dataset$training_set, df_test = prepared_dataset$testing_set, df_target_category = prepared_dataset$target_category,
                        df_test_category = prepared_dataset$test_category, k=70)$predicted

# Confusion Matrix
print("Normal KNN Consufion Matrix:")
cm <- confusionMatrix(knn_pred, prepared_dataset$test_category)
print(cm)

print("---------------------------------------------------------")


# Print accuracy
print(paste("KNN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))

print("~~ KNN ENDED:")

#
#********************Matrix Visualization*****************************
#

table <- data.frame(confusionMatrix(knn_pred, prepared_dataset$test_category)$table)

plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference)))
