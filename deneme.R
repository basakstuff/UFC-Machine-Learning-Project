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


#getwd() workspace ogrenme
#setwd("/Users/.../") workspace ayarlama



data <- read.csv("data.csv")
setDT(data)


cat_var <- names(data)[which(sapply(data, is.character))] #kategorik
numeric_var <- names(data)[which(sapply(data, is.numeric))] #numeric

dim(data)

str(data)

head(data)

colSums(sapply(data, is.na)) #kac null var

colSums(sapply(data[,.SD, .SDcols = cat_var], is.na)) #kategorikte null kontrolu

colSums(sapply(data[,.SD, .SDcols = numeric_var], is.na)) #numericte null kontrolu

################################ null value gorsellestirme ###############

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

plot_Missing(data[,colSums(is.na(data)) >= 0, with = FALSE])

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

####################################################################################
dataNorm <- df2

library(class)
set.seed(1234)

ind <- sample(2, nrow(dataNorm), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dataNorm[ind==1,]
testData <- dataNorm[ind==2,]

KnnTestPrediction_k1 <- knn(trainData, testData, trainData$Winner, k=5, prob=TRUE)

#######################################
library(tree)
library(e1071)
library(rpart)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

n <- nrow(df2)
idx <- sample(n, n * .70)


train <- df2[idx, ]
test <- df2[-idx, ]

winner <- df2$Winner
tree <- tree(Winner ~ ., df2, train)

ir.tr2 <- tree(Winner ~ ., data = df2, subset = train)

##################################









##################################################################################

#butun degerleri null olan kolonlari siliyor (bizde yok)
data <- data %>% 
  select(where(~!all(is.na(.))))

dim(data)

#en az bir degeri null olan kolonlari siliyor.
data <- data %>% 
  select(where(~!any(is.na(.))))

#null value icin gorsellestirmeyi yeniden run edersek missing deger kalmadigini goruruz

plot_Missing(data[,colSums(is.na(data)) >= 0, with = FALSE])

dim(data)

cat('Data has', dim(data)[1], 'rows and', dim(data)[2], 'columns.')

names(data)

data <- data %>% select(-Referee, -date, -location, -B_win_by_Decision_Majority, -R_win_by_Decision_Majority, -B_win_by_Decision_Split, 
                        -R_win_by_Decision_Split, -B_win_by_Decision_Unanimous, -R_win_by_Decision_Unanimous, -B_win_by_TKO_Doctor_Stoppage, 
                        -R_win_by_TKO_Doctor_Stoppage, -B_win_by_Submission, -R_win_by_Submission)  #düşürülecek kolonlar 


names(data)

##korelasyon matrisi cizdirme

cat_var <- names(data)[which(sapply(data, is.character))] #kategorik
numeric_var <- names(data)[which(sapply(data, is.numeric))] #numeric

numeric_data <- select_if(data, is.numeric)

numeric_data <- numeric_data %>% select(-B_draw, -R_draw) #std is zero
summary(numeric_data)


cor_data <- cor(numeric_data)
corrplot(cor_data, method = "circle", order = "alphabet")
corrplot(cor_data, method = "number")

colnames(data)
