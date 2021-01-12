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




##########plot############
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}
###########################

#############################################################################
########## 2010 sonrası ve secilmis features #############

df1 <- data %>%
  select(date, Winner, title_bout, weight_class,B_fighter, B_Height_cms, B_Reach_cms, B_age, B_current_lose_streak, B_current_win_streak,B_longest_win_streak, B_losses,B_wins,B_total_rounds_fought, B_total_title_bouts,B_win_by_KO.TKO,B_win_by_Submission, B_win_by_Decision_Majority,B_win_by_Decision_Split,B_win_by_Decision_Unanimous,B_win_by_TKO_Doctor_Stoppage,
         R_fighter, R_Height_cms, R_Reach_cms, R_age,
         R_current_lose_streak, R_current_win_streak,R_longest_win_streak, R_losses,R_wins,R_total_rounds_fought,
         R_total_title_bouts,R_win_by_KO.TKO,R_win_by_Submission,
         R_win_by_Decision_Majority,R_win_by_Decision_Split,R_win_by_Decision_Unanimous,R_win_by_TKO_Doctor_Stoppage)


df1 <- subset.data.frame(df1, subset= date >= "2010-01-01")

dim(df1)

## null iceren dataset degiskenleri############

cat_var1 <- names(df1)[which(sapply(df1, is.character))] #kategorik
numeric_var1 <- names(df1)[which(sapply(df1, is.numeric))] #numeric

colSums(sapply(df1[,.SD, .SDcols = cat_var1], is.na))

colSums(sapply(df1[,.SD, .SDcols = numeric_var1], is.na)) #numericte null kontrolu

## null icermeyen  dataset degiskenleri############

df2 <- na.omit(df1) ##null rowlari sildi

plot_Missing(df2[,colSums(is.na(df2)) >= 0, with = FALSE])

dim(df2)

cat_var2 <- names(df2)[which(sapply(df2, is.character))] #kategorik
numeric_var2 <- names(df2)[which(sapply(df2, is.numeric))] #numeric


colSums(sapply(df2[,.SD, .SDcols = cat_var2], is.na)) #kategorikte null kontrolu
colSums(sapply(df2[,.SD, .SDcols = numeric_var2], is.na)) #numericte null kontrolu







###################DT###################

#df2$Winner <- revalue(df2$Winner, c("0"=1))
#b_win <- df2 %>% filter(Winner == "Blue")

set.seed(1023)
samp <- sample(nrow(df2), nrow(df2)*0.7)
df2.train <- data.frame(df2[samp,])
df2.valid <- data.frame(df2[-samp,])


fit <- rpart(B_age ~ Winner  + B_total_title_bouts + B_losses, data = df2.train, method = 'class')
rpart.plot(fit, box.palette="red")

###############

tree <- rpart(Winner ~ ., data = df2.train)

res <- predict(tree, test)
######################################
summary(df2)
model <- rpart(
  Winner ~ weight_class + B_age + R_age,
  data = df2.train, 
  control = rpart.control(minsplit = 2))

rpart.plot(model, box.palette="blue")

rpart <- rpart(B_wins ~ Winner  + weight_class  + B_losses,
               data=df2.train,
               method="class",
               parms=list(split="information"),
               control=rpart.control(minsplit=2,
                                     usesurrogate=0, 
                                     maxsurrogate=0))

# Generate a textual view of the Decision Tree model.
print(rpart)
rpart.plot(rpart)

par(xpd = NA, mar = rep(0.7, 4)) 
plot(model, compress = TRUE)
text(model, cex = 0.7, use.n = TRUE, fancy = FALSE, all = TRUE)


prp(rpart)
########KNN############




