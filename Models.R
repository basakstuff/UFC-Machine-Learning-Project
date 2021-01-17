load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
library(data.table)
library(ggplot2) #data visualization
library(plotly) #interactive data visualization
library(psych) #correlation visualization helping
library(rattle) #graphing decision trees
library(caret) # machine learning
library(tree)
library(e1071)
library(rpart)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(class)
library(formattable)
library(randomForest)

ufc_data <- read.csv("ufc_data.csv")
UFC_PCA <-read.csv("UFC_PCA.csv")
PCA_cols <- names(UFC_PCA)[2:length(names(UFC_PCA))] # PCA column names excluding Winner


######PREPARE DATA###
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
###Normalise

UFC_DATA <- subset(ufc_data, select=-c(R_fighter,B_fighter, date))
UFC_DATA$title_bout <- as.numeric(factor(ufc_data$title_bout))
UFC_DATA$weight_class <- as.numeric(factor(ufc_data$weight_class))
UFC_DATA$Winner<-factor(ufc_data$Winner)
UFC_PCA$Winner<-factor(UFC_PCA$Winner)

set.seed(123)

# Train/test split with 0.8 ratio
prepared_dataset <- prepare_data(df = UFC_DATA, output_col_n = 1, ratio = 0.8)

########Train/test split for PCA
pca_train <- prepared_dataset$training_set %>% select(PCA_cols)
pca_test <- prepared_dataset$testing_set %>% select(PCA_cols)

##################******************##################
##################******************##################
##################******MODELS******##################
##################******************##################
##################******************##################

##################******************##################
##################*******SVM********##################
##################*****************###################


# Concat X and y for SVM training
svm_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), prepared_dataset$training_set)
pca_svm_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), pca_train)

# k-fold cross validation (k=10)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10, 
                           repeats = 5)

# SVM Classifier
svm_classifier <- svm(Winner ~ ., 
                      data = svm_data, 
                      type = 'C-classification', 
                      kernel = 'radial',
                      trControl = fitControl,
                      cost=10,
                      scale=TRUE,
                      probability=TRUE)

# SVM Classifier With PCA
pca_svm_classifier <- svm(Winner ~ ., 
                          data = pca_svm_data, 
                          type = 'C-classification', 
                          kernel = 'radial',
                          trControl = fitControl,
                          cost=10,
                          scale=TRUE,
                          probability=TRUE)

svmpred <- predict(svm_classifier, newdata=prepared_dataset$testing_set, probability = TRUE)
pca_svmpred <- predict(pca_svm_classifier, newdata=pca_test, probability = TRUE)

# Confusion Matrix
print("Normal SVM Consufion Matrix:")
cm <- confusionMatrix(table(svmpred, prepared_dataset$test_category)) 
print(cm)
print(paste("SVM Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))
print("---------------------------------------------------------")

print("PCA SVM Consufion Matrix:")
pca_cm <- confusionMatrix(pca_svmpred, prepared_dataset$test_category)
print(pca_cm)
print(paste("PCA_SVM Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(pca_cm$overall[1], 4)*100,"%" ))
print("---------------------------------------------------------")


# Save SVM model
#saveRDS(svm_classifier, file = "SVM_MODEL.rds"
#        ,ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)


print("~~ SVM TABLE ~~")

table <- data.frame(confusionMatrix(svmpred, prepared_dataset$test_category)$table)

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


print("~~ SVM PCA TABLE ~~")

table <- data.frame(confusionMatrix(pca_svmpred, prepared_dataset$test_category)$table)

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


print("~~ SVM ENDED")

##################******************##################
##################***RANDOM FOREST***#################
##################*****************###################
set.seed(123)

# k-fold cross validation (k=10)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10, 
                           repeats = 5)


# Concat X and y for RF training
rf_data <- data.frame(Winner=prepared_dataset$target_category, prepared_dataset$training_set)
pca_rf_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), pca_train)


# RF Classifier
rf_classifier <- randomForest(Winner~., data = rf_data, norm.votes = TRUE, proximity = TRUE, trControl = fitControl)
pca_rf_classifier <- randomForest(Winner~., data = pca_rf_data, norm.votes = TRUE, proximity = TRUE, trControl = fitControl)

# RF Predictions on unseen testing set
rfpred <- predict(rf_classifier, prepared_dataset$testing_set)
pca_rfpred <- predict(pca_rf_classifier, pca_test)


# Confusion Matrix
print("Normal RF Consufion Matrix:")
cm <- confusionMatrix(rfpred, prepared_dataset$test_category)
print(cm)
# Print accuracy
print(paste("RF Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))
print("---------------------------------------------------------")

print("PCA RF Consufion Matrix:")
pca_cm <- confusionMatrix(pca_rfpred, prepared_dataset$test_category)
print(pca_cm)
print(paste("PCA_RF Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(pca_cm$overall[1], 4)*100,"%" ))
print("---------------------------------------------------------")




# Save RF model
#saveRDS(rf_classifier, file = "RF_MODEL-test.rds"
#        ,ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)




print("~~ RF TABLE ~~")

table <- data.frame(confusionMatrix(rfpred, prepared_dataset$test_category)$table)
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

print("~~ RF PCA TABLE ~~")

table <- data.frame(confusionMatrix(pca_rfpred, prepared_dataset$test_category)$table)

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


print("~~ RF ENDED")

##################******************##################
##################********KNN********#################
##################*****************###################

set.seed(123)


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

k_value <- c()
avg_accuracy <- c()
pca_k_value <- c()
pca_avg_accuracy <- c()


# KNN: Experiment tange of K values:    # note: initially, values 1 to 100 were tested as k.
for(k in 1:100){
  k_value <- c(k_value,k)
  
  # accuracy per run
  accuracies <- c()
  
  # number of runs per K    # note: initially, tested for 30 runs
  for(i in 1:30){
    accuracies<- c(accuracies,perform_knn(df_train = prepared_dataset$training_set, df_test = prepared_dataset$testing_set, df_target_category = prepared_dataset$target_category,
                                          df_test_category = prepared_dataset$test_category, k=k)$accuracy)
  }
  print(paste("KNN K=", k, "Average Accuracy in 30 run",mean(accuracies)))
  avg_accuracy <- c(avg_accuracy,mean(accuracies))
}

print("---------------------------------------------------------")

# PCA KNN: Experiment tange of K values:    # note: initially, values 1 to 100 were tested as k.
for(k in 1:100){
  pca_k_value <- c(pca_k_value,k)
  
  # accuracy per run
  accuracies <- c()
  
  # number of runs per k    # note: initially, tested for 30 runs
  for(i in 1:30){
    accuracies<- c(accuracies,perform_knn(df_train = pca_train, df_test = pca_test, df_target_category = prepared_dataset$target_category,
                                          df_test_category = prepared_dataset$test_category, k=k)$accuracy)
  }
  print(paste("PCA-KNN K=", k, "Average Accuracy in 30 run",mean(accuracies)))
  pca_avg_accuracy <- c(pca_avg_accuracy,mean(accuracies))
}
print("---------------------------------------------------------")

# Table of k-values & average accuracies
acc_df <- data.frame(k_value,avg_accuracy,pca_k_value,pca_avg_accuracy)

write.csv(acc_df,"acc_df.csv", row.names = FALSE) #export df

# KNN and PCA-KNN Evaluation on K = 54
knn_pred <- perform_knn(df_train = prepared_dataset$training_set, df_test = prepared_dataset$testing_set, df_target_category = prepared_dataset$target_category,
                        df_test_category = prepared_dataset$test_category, k=54)$predicted

pca_knn_pred <- perform_knn(df_train = pca_train, df_test = pca_test, df_target_category = prepared_dataset$target_category,
                            df_test_category = prepared_dataset$test_category, k=81)$predicted


# Confusion Matrix
print("Normal KNN Consufion Matrix:")
cm <- confusionMatrix(knn_pred, prepared_dataset$test_category)
print(cm)
# Print accuracy
print(paste("KNN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))
print("---------------------------------------------------------")


print("PCA KNN Consufion Matrix:")
pca_cm <- confusionMatrix(pca_knn_pred, prepared_dataset$test_category)
print(pca_cm)
print(paste("PCA_KNN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(pca_cm$overall[1], 4)*100,"%" ))
print("---------------------------------------------------------")


print("~~ KNN TABLE ~~")

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


print("~~ KNN PCA TABLE ~~")

table <- data.frame(confusionMatrix(pca_knn_pred, prepared_dataset$test_category)$table)

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



# *********************** #
#   KNN Accuracy Chart    #
# *********************** #

t <- list(family = "sans-serif",size = 14,color = 'black') # Text style
m <- list(l = 8,r = 8,b = 35,t = 35,pad =1) # Magins

acc_df<-read.csv("acc_df.csv",encoding="UTF-8",stringsAsFactors = FALSE) # accuracy table from knn.r
acc_df <- acc_df[,c(1,2,4)] 
names(acc_df) <- c("k_value","avg_accuracy","pca_avg_accuracy")
is.num <- sapply(acc_df, is.numeric) # Format to 3 Decimal Points
acc_df [is.num] <- lapply(acc_df [is.num], round, 3)

x <- acc_df$k_value
y1 <- acc_df$avg_accuracy
y2 <- acc_df$pca_avg_accuracy

t1 <- list(family = "sans-serif",size = 16,color = 'black') # Text style
m1 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
a<-plot_ly(acc_df,x=x, y=y1, type="scatter", mode="line", name="KNN") %>%
  add_trace(y = y2, name = 'PCA-KNN', mode = 'lines') %>%
  layout(
    title="KNN Average Accuracy on 30 runs per K",
    yaxis = list(
      title="Accuracy (%)",
      range=c(50,60)
    ),
    xaxis = list(
      title="K Value",
      range=c(0,105)
    ),
    font = t,
    margin = m
  )


print(a)

print("~~ KNN ENDED:")

##################******************##################
##################****NAIVE BAYES****#################
##################*****************###################


# Concat X and y for SVM training
NB_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), prepared_dataset$training_set)
pca_NB_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), pca_train)




# NB Classifier
NB_classifier <- train(Winner~., data = NB_data, norm.votes = TRUE, proximity = TRUE, 'nb', trControl = trainControl(method='cv', number = 10))
pca_NB_classifier <- train(Winner~., data = pca_NB_data, norm.votes = TRUE, proximity = TRUE, 'nb', trControl = trainControl(method='cv', number = 10))


# NB Predictions on unseen testing set
NBpred <- predict(NB_classifier, newdata=prepared_dataset$testing_set, probability = TRUE)
pca_NBpred <- predict(pca_NB_classifier, pca_test)


# Confusion Matrix
print("Normal RF Consufion Matrix:")
cm <- confusionMatrix(NBpred, prepared_dataset$test_category)
print(cm)
# Print accuracy
print(paste("NB Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))
print("---------------------------------------------------------")


print("PCA NB Consufion Matrix:")
pca_cm <- confusionMatrix(pca_NBpred, prepared_dataset$test_category)
print(pca_cm)
print(paste("PCA_NB Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(pca_cm$overall[1], 4)*100,"%" ))
print("---------------------------------------------------------")


print("~~ NB TABLE ~~")

table <- data.frame(confusionMatrix(NBpred, prepared_dataset$test_category)$table)

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

print("~~ NB PCA TABLE ~~")

table <- data.frame(confusionMatrix(pca_NBpred, prepared_dataset$test_category)$table)

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

##################******************##################
##################******************##################
##################******PLOTS******##################
##################******************##################
##################******************##################

# Models' Accuracy radar chart
h <- plot_ly(type = 'scatterpolar', fill = 'toself', mode="markers") %>%
  add_trace(
    r = c(58.89, 56.84, 59.63, 57.1),
    theta = c('KNN','NB','SVM', 'RF'),
    name = 'Normal Data'
  ) %>%
  add_trace(
    r = c(56.84, 58.17, 56.97, 56.04),
    theta = c('KNN','NB','SVM', 'RF'),
    name = 'PCA-Performed Data'
  ) %>%
  layout(
    margin=c(l=1,r=1,t=2,b=1),
    title = "Models Accuracy Performance",
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(55,62)
      )
    )
  )
print(h)

# Models' Accuracy bar chart
models <- c('KNN','NB','SVM', 'RF')
normalt <- c(0.5896, 0.5684, 0.5963, 0.5710)
pca_accu <- c(0.5684, 0.5817, 0.5697, 0.5604)
data <- data.frame(models, normalt, pca_accu)

i <- plot_ly(data, x = ~models, y = ~normalt, type = 'bar', name = 'Normal Data') %>%
  add_trace(y = ~pca_accu, name = 'PCA-Performed Data') %>%
  layout(yaxis = list(title = 'Accuracy (%)',range=c(0.5,0.6)), barmode = 'group')
print(i)

print("~~ VISUALISATION ENDED:")




