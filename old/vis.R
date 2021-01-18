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
library(countrycode)
library(ggthemes)
library(gridExtra)


######
print("~~ VISUALISATION STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "df2.csv"



t <- list(family = "sans-serif",size = 14,color = 'black') # Text style
m <- list(l = 8,r = 8,b = 35,t = 35,pad =1) # Magins
# *********************** #
# 1- KNN Accuracy Chart
acc_df<-read.csv("acc_df.csv",encoding="UTF-8",stringsAsFactors = FALSE) # accuracy table from knn.r
acc_df <- acc_df[,c(1,2)] 
names(acc_df) <- c("k_value","avg_accuracy")
is.num <- sapply(acc_df, is.numeric) # Format to 3 Decimal Points
acc_df [is.num] <- lapply(acc_df [is.num], round, 3)

x <- acc_df$k_value
y1 <- acc_df$avg_accuracy

t1 <- list(family = "sans-serif",size = 16,color = 'black') # Text style
m1 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
a<-plot_ly(acc_df,x=x, y=y1, type="scatter", mode="line", name="KNN") %>%
  layout(
    title="KNN Average Accuracy on 30 runs per K",
    yaxis = list(
      title="Accuracy (%)",
      range=c(45,60)
    ),
    xaxis = list(
      title="K Value",
      range=c(0,105)
    ),
    font = t,
    margin = m
  )


print(a)


# *********************** #
# 2- Weight Class Donut
ufc_data <- read.csv("df2.csv",encoding="UTF-8",stringsAsFactors = TRUE)
is.num <- sapply(ufc_data, is.numeric) # Format to 3 Decimal Points
ufc_data [is.num] <- lapply(ufc_data [is.num], round, 3)

weight_class <- ufc_data$weight_class
weight_class <- na.omit(weight_class) # drop na
weight_class <- as.data.frame(table(ufc_data$weight_class)) # frequency

t2 <- list(family = "sans-serif",size = 16,color = 'black') # Text style
m2 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
b <- plot_ly(weight_class, labels = ~Var1, values = ~Freq)%>%add_pie(hole = 0.6) %>%
  layout(title = "UFC Weight Class 1993 - 2019",
         showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font = t,
         margin = m
  )

print(b)




# *********************** #
# 3- Events vs Years BarChart
ufc_data$date <- na.omit(ufc_data$date) # drop na
# Extract Year
yearsList <- c()
for(date in ufc_data[,]$date){
  date <- strsplit(date,"-") # split by -
  date <- date[[1]][1] # get date
  yearsList <- c(yearsList,date)
  
}
yearsDF <- data.frame(yearsList)
yearsDF <- as.data.frame(table(yearsDF)) # frequency
names(yearsDF) <- c("year", "count")
x4 = yearsDF$year
y4 = yearsDF$count

t4 <- list(family = "sans-serif",size = 14,color = 'Black') # Text style
m4 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
bar_color <- rep("#3caef2",27)
bar_color[22] <- '#07466c'
d <- plot_ly(yearsDF, x = ~x4, y = ~y4, type = 'bar',text=y4, textposition="auto",
             marker = list(color = bar_color)) %>%
  layout(title = "Number of UFC matches Over Years",
         xaxis = list(title = "Year"),
         yaxis = list(title = "No. Of matches"),
         font = t,
         margin = m)


print(d)
colnames(ufc_data)

# *********************** #
# 4- Density plots

fighter_measures <- data.frame(
  "height"  = c(ufc_data$B_Height_cms, ufc_data$B_Height_cms),
  "reach"   = c(ufc_data$B_Reach_cms, ufc_data$R_Reach_cms),
  "age"     = c(ufc_data$B_age, ufc_data$R_age))
fighter_measures <- na.omit(fighter_measures)

p1 <- ggplot(fighter_measures, aes(x=age))+
  geom_density(color="darkblue", fill="lightblue")

p2 <- ggplot(fighter_measures, aes(x=height))+
  geom_density(color="darkblue", fill="lightblue")

p3 <- ggplot(fighter_measures, aes(x=reach))+
  geom_density(color="darkblue", fill="lightblue")

# Subcharts
grid.arrange(p1, p2, p3,  ncol=2, nrow=2)


# 5- Models' Accuracy radar chart
h <- plot_ly(type = 'scatterpolar', fill = 'toself', mode="markers") %>%
  add_trace(
    r = c(58.3, 59.63, 55.95, 56.44),
    theta = c('KNN','SVM','DNN', 'RF'),
    name = 'Normal Data'
  ) %>%
  layout(
    margin=c(l=1,r=1,t=2,b=1),
    title = "Models Accuracy Performance",
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(50,65)
      )
    )
  )
print(h)

# 9- Models' Accuracy bar chart
models <- c("KNN", "SVM", "DNN", "RF")
normalt <- c(0.58, 0.59, 0.55, 0.56)
data <- data.frame(models, normalt)

i <- plot_ly(data, x = ~models, y = ~normalt, type = 'bar', name = 'Normal Data') %>%
  layout(yaxis = list(title = 'Accuracy (%)',range=c(0.5,0.6)), barmode = 'group')
print(i)

print("~~ VISUALISATION ENDED:")




