##import#####
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
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(datasets)  
library(caTools) 
library(party) 
library(dplyr) 
library(magrittr) 
library(rpart)
library(rpart.plot)
##

data <- read.csv("data.csv")
setDT(data)


dim(data)


#############################################################################
########## 2010 sonrası ve secilmis features #############

df1 <- data %>%
  select(date, Winner, title_bout, weight_class,B_fighter, B_Height_cms, B_Reach_cms, B_age, B_current_lose_streak, B_current_win_streak,B_longest_win_streak, B_losses,B_wins,B_total_rounds_fought, B_total_title_bouts,B_win_by_KO.TKO,B_win_by_Submission, B_win_by_Decision_Majority,B_win_by_Decision_Split,B_win_by_Decision_Unanimous,B_win_by_TKO_Doctor_Stoppage,
         R_fighter, R_Height_cms, R_Reach_cms, R_age,
         R_current_lose_streak, R_current_win_streak,R_longest_win_streak, R_losses,R_wins,R_total_rounds_fought,
         R_total_title_bouts,R_win_by_KO.TKO,R_win_by_Submission,
         R_win_by_Decision_Majority,R_win_by_Decision_Split,R_win_by_Decision_Unanimous,R_win_by_TKO_Doctor_Stoppage)


df1 <- subset.data.frame(df1, subset= date >= "2010-01-01")

## null iceren dataset degiskenleri############

cat_var1 <- names(data)[which(sapply(data, is.character))] #kategorik
numeric_var1 <- names(data)[which(sapply(data, is.numeric))] #numeric

colSums(sapply(data[,.SD, .SDcols = cat_var1], is.na))

colSums(sapply(data[,.SD, .SDcols = numeric_var1], is.na)) #numericte null kontrolu

## null icermeyen  dataset degiskenleri############

df2 <- na.omit(data) ##null rowlari sildi

cat_var2 <- names(df2)[which(sapply(df2, is.character))] #kategorik
numeric_var2 <- names(df2)[which(sapply(df2, is.numeric))] #numeric

colSums(sapply(df2[,.SD, .SDcols = cat_var2], is.na)) #kategorikte null kontrolu
colSums(sapply(df2[,.SD, .SDcols = numeric_var2], is.na)) #numericte null kontrolu
###########################################################################


numeric_data <- select_if(df2, is.numeric)

numeric_data <- numeric_data %>% select(-B_draw, -R_draw) #std is zero

summary(numeric_data)

cor_data <- cor(numeric_data)
corrplot(cor_data, method = "circle", order = "alphabet", type = "upper", tl.col = "black")
corrplot(cor_data, method = "color", type = "upper", tl.col = "black", order="hclust") ###en iyisi

#####################################
library(dplyr)
library(tidyr)
cor_mat <- cor(numeric_data)
cor_mat[!lower.tri(cor_mat)] <- NA # remove diagonal and redundant values
data.frame( cor_mat) %>%
  rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  filter(abs(correlation) > 0.9)

w = table(df2$Winner)

colnames(df2)

df2$no_of_rounds

str(df2$no_of_rounds)
df2$no_of_rounds <- as.factor(df2$no_of_rounds)



