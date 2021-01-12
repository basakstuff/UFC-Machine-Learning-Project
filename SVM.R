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

data <- read.csv("data.csv")
setDT(data)


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

numeric_data <- select_if(df2, is.numeric)

prepare_data <- function(df, ratio){
  # Randomisation with train to test ratio
  random <- sample(1:nrow(df), ratio * nrow(df))
  
  # Normalisation
  df_normalised <- df[,-2]
  
  # Extract training set
  df_train <- df_normalised[random,] 
  # Extract testing set
  df_test <- df_normalised[-random,]
  
  # Output category
  df_target_category <- df[random,2]
  df_test_category <- df[-random,2]
  
  toReturn <- list("training_set" = df_train, "testing_set" = df_test,"target_category" = df_target_category,"test_category"= df_test_category)
  
  return(toReturn)
}

UFC_DATA <- UFC_DATA[, -2]


# ************************************************
# main code goes below:
# ************************************************

UFC_DATA <- df2 # Normal data

print("Running ...")


# Train/test split with 0.7 ratio
prepared_dataset <- prepare_data(df = UFC_DATA,ratio = 0.7)

svm_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), prepared_dataset$training_set)

# SVM Classifier
svm_classifier <- svm(Winner ~ ., 
                      data = svm_data, 
                      type = 'C-classification', 
                      kernel = 'radial',
                      cost=10,
                      scale=TRUE,
                      probability=TRUE)


