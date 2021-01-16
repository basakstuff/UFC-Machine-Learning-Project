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

# Train/test split with 0.8 ratio
prepared_dataset <- prepare_data(df = UFC_DATA, output_col_n = 1, ratio = 0.8,normalized = FALSE)

# Concat X and y for RF training
rf_data <- data.frame(Winner=prepared_dataset$target_category, prepared_dataset$training_set)

#str(rf_data)

# RF Classifier
rf_classifier <- randomForest(Winner~., data = rf_data, norm.votes = TRUE, proximity = TRUE)

# RF Predictions on unseen testing set
rfpred <- predict(rf_classifier, prepared_dataset$testing_set)


# Confusion Matrix
print("Normal RF Consufion Matrix:")
cm <- confusionMatrix(rfpred, prepared_dataset$test_category)
print(cm)

print("---------------------------------------------------------")

# Print accuracy
print(paste("RF Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))

# Save SVM model
#saveRDS(rf_classifier, file = "RF_MODEL-test.rds"
#        ,ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)




#
#********************Matrix Visualization*****************************
#

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
