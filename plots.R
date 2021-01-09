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







########TESTS#################
colnames(df2)

ggplot(df2, aes(x=(R_Reach_cms - B_Reach_cms), y =..density.., fill=..count..)) + geom_histogram(binwidth = 5, ) + labs(x = "Reach Difference (cm)", title = "Histogram for Red Fighter Reach Difference") + scale_fill_gradient("Count", low="green", high="red")

# make a bar plot
ggplot(df2, aes(x = B_age)) + geom_bar() #B_age

ggplot(df2, aes(x = R_age)) + geom_bar() #R_age


temp <- df2 %>% select(B_fighter,B_wins)
temp <- temp %>% group_by(B_fighter) %>% summarise(avg=mean(B_wins))
temp <- temp %>% arrange(desc(avg))
temp <- temp[1:20,]
temp %>%
  formattable(list(avg = color_bar("orange")), align = 'l')


df2 %>% filter(Winner == "Blue") %>% count(weight_class) #weight_class'a göre kazanan blue
df2 %>% filter(Winner == "Red") %>% count(weight_class) #weight_class'a göre kazanan red

b_win <- df2 %>% filter(Winner == "Blue")

#####correlation#########
numeric_data <- select_if(df2, is.numeric)

summary(numeric_data)

cor_data <- cor(numeric_data)
corrplot(cor_data, method = "circle", order = "alphabet")
corrplot(cor_data, method = "number")

colnames(data)
#############################

year <- format(as.Date(df2$date, format="%Y-%m-%d"),"%Y")

#########Pie Chart##############

custom_col <- c("blue", "green", "red") 

fig(12,8)                   
ggplot(df2, aes(x = "", y="", fill = factor(Winner))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5,size=22)) + 
  labs(fill="Winner", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Winners") + coord_polar(theta = "y", start=0)+
  scale_fill_manual(values=custom_col)

####

