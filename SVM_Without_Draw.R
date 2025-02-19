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
######

df2 <- read.csv("ufc_data.csv")

################################
df2 <- subset(df2, select=-c(R_fighter,B_fighter, date))
df2$title_bout <- as.numeric(factor(df2$title_bout))
df2$weight_class <- as.numeric(factor(df2$weight_class))
df2 <- subset.data.frame(df2, subset= Winner!="Draw")
df2$Winner<-factor(df2$Winner)
#df2$Winner <- as.numeric(factor(df2$Winner))
table(df2$Winner)

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


# ************************************************
# main code goes below:
# ************************************************

UFC_DATA <- df2 # Normal data

#grep("Winner", colnames(UFC_DATA))

set.seed(123)

# Train/test split with 0.8 ratio
prepared_dataset <- prepare_data(df = UFC_DATA, output_col_n = 1, ratio = 0.8)

# Concat X and y for SVM training
svm_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), prepared_dataset$training_set)


#str(svm_data)

# SVM Classifier
svm_classifier <- svm(Winner ~ ., 
                      data = svm_data, 
                      type = 'C-classification', 
                      kernel = 'radial',
                      cost=10,
                      scale=TRUE,
                      probability=TRUE)

svmpred <- predict(svm_classifier, newdata=prepared_dataset$testing_set, probability = TRUE)


# Confusion Matrix
print("Normal SVM Consufion Matrix:")
cm <- confusionMatrix(table(svmpred, prepared_dataset$test_category)) 
print(cm)
print("---------------------------------------------------------")

print(paste("SVM Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))

# Save SVM model
#saveRDS(svm_classifier, file = "SVM_MODEL.rds"
#        ,ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)


print("~~ SVM ENDED:")

#
#********************Matrix Visualization*****************************
#

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
