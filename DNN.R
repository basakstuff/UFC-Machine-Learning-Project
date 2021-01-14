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

data <- read.csv("data.csv")

########## 2010 sonrası ve secilmis features #############

df1 <- data %>%
  select(date, Winner, title_bout, weight_class,B_fighter, B_Height_cms, B_Reach_cms, B_age, B_current_lose_streak, B_current_win_streak,B_longest_win_streak, B_losses,B_wins,B_total_rounds_fought, B_total_title_bouts,B_win_by_KO.TKO,B_win_by_Submission, B_win_by_Decision_Majority,B_win_by_Decision_Split,B_win_by_Decision_Unanimous,B_win_by_TKO_Doctor_Stoppage,
         R_fighter, R_Height_cms, R_Reach_cms, R_age,
         R_current_lose_streak, R_current_win_streak,R_longest_win_streak, R_losses,R_wins,R_total_rounds_fought,
         R_total_title_bouts,R_win_by_KO.TKO,R_win_by_Submission,
         R_win_by_Decision_Majority,R_win_by_Decision_Split,R_win_by_Decision_Unanimous,R_win_by_TKO_Doctor_Stoppage)


df1 <- subset.data.frame(df1, subset= date >= "2010-01-01")

dim(df1)

# null iceren dataset degiskenleri

cat_var1 <- names(df1)[which(sapply(df1, is.character))] #kategorik
numeric_var1 <- names(df1)[which(sapply(df1, is.numeric))] #numeric

colSums(sapply(df1[,.SD, .SDcols = cat_var1], is.na))

colSums(sapply(df1[,.SD, .SDcols = numeric_var1], is.na)) #numericte null kontrolu

# null icermeyen  dataset degiskenleri

df2 <- na.omit(df1) ##null rowlari sildi

plot_Missing(df2[,colSums(is.na(df2)) >= 0, with = FALSE])


cat_var2 <- names(df2)[which(sapply(df2, is.character))] #kategorik
numeric_var2 <- names(df2)[which(sapply(df2, is.numeric))] #numeric

colSums(sapply(df2[,.SD, .SDcols = cat_var2], is.na)) #kategorikte null kontrolu
colSums(sapply(df2[,.SD, .SDcols = numeric_var2], is.na)) #numericte null kontrolu

dim(data)
dim(df1)
dim(df2)
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
prepared_dataset <- prepare_data(UFC_DATA, output_col_n = output_col_n,ratio = 0.8, normalized = FALSE)



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

