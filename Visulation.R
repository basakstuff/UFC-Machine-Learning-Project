load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 
                    'ggplot2', 'e1071', 'dplyr', 'Hmisc', 'tidyverse', 'funModeling',
                    'plotly','psych','rattle','caret','tree', 'rpart','magrittr',
                    'class','formattable','randomForest')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)



data <- read.csv("data.csv")
setDT(data)

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

cat('Data has', dim(data)[1], 'rows and', dim(data)[2], 'columns.')

str(data)

cat_var <- names(data)[which(sapply(data, is.character))] #kategorik
numeric_var <- names(data)[which(sapply(data, is.numeric))] #numeric

colSums(sapply(data, is.na))

colSums(sapply(data[,.SD, .SDcols = cat_var], is.na)) #kategorikte null kontrolu

colSums(sapply(data[,.SD, .SDcols = numeric_var], is.na)) #numericte null kontrolu

plot_Missing(data)
#############################

ufc_data <- data %>%
  select(date, Winner, title_bout, weight_class,B_fighter, B_Height_cms, B_Reach_cms, B_age, B_current_lose_streak, B_current_win_streak,B_longest_win_streak, B_losses,B_wins,B_total_rounds_fought, B_total_title_bouts,B_win_by_KO.TKO,B_win_by_Submission, B_win_by_Decision_Majority,B_win_by_Decision_Split,B_win_by_Decision_Unanimous,B_win_by_TKO_Doctor_Stoppage,
         R_fighter, R_Height_cms, R_Reach_cms, R_age,
         R_current_lose_streak, R_current_win_streak,R_longest_win_streak, R_losses,R_wins,R_total_rounds_fought,
         R_total_title_bouts,R_win_by_KO.TKO,R_win_by_Submission,
         R_win_by_Decision_Majority,R_win_by_Decision_Split,R_win_by_Decision_Unanimous,R_win_by_TKO_Doctor_Stoppage)


ufc_data <- subset.data.frame(ufc_data, subset= date >= "2010-01-01")

dim(ufc_data)
############################
cat_ufc <- names(ufc_data)[which(sapply(ufc_data, is.character))] #kategorik
numeric_ufc <- names(ufc_data)[which(sapply(ufc_data, is.numeric))] #numeric

colSums(sapply(ufc_data, is.na))

plot_Missing(ufc_data)
################################ delete missing

ufc_data <- na.omit(ufc_data)
plot_Missing(ufc_data)
dim(ufc_data)

colnames(ufc_data)
write.csv(ufc_data,"ufc_data.csv", row.names = FALSE)

# ************************************************
# perform_PCA()
# ************************************************
perform_PCA <- function(df) {
  
  # PCA and Plotting Component Variance
  df.prc = prcomp(df, center = TRUE,scale = TRUE)
  
  # Variance
  variance = df.prc$sdev ^ 2
  
  # Kaiser Criterion
  pca_vars = variance[variance >= 1] 
  number_of_PCAs = length(pca_vars)
  
  #Scree Plot
  screeplot(df.prc,type = "line",main = "PCA: Scree Plot")
  
  #Varimax Rotation
  df.varimax_rotation = varimax(df.prc$rotation)
  
  test = data.frame(unclass(df.varimax_rotation$loadings))
  test = cbind(rownames(test),test)
  row.names(test)<-1:nrow(test)
  
  colnames_test = names(test)
  colnames_test = colnames_test[2:number_of_PCAs]
  selected_variables = c()
  for(i in colnames_test){
    for (k in 1:nrow(test)){
      if (test[k,i]>0.2 | test[k,i]<=-0.2){
        selected_variables <- c(selected_variables,paste(test[k,1]))
      }
    }
  }
  return(selected_variables)
}

df <- read.csv("ufc_data.csv")
df <- subset(df, select=-c(R_fighter,B_fighter, date))
df$title_bout <- as.numeric(factor(df$title_bout))
df$weight_class <- as.numeric(factor(df$weight_class))
df$Winner<-factor(df$Winner)
PCA_cols <- perform_PCA(df[,-1])

normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}

UFC_PCA<-data.frame(df[1],df[,PCA_cols]) # concat 1st column
cat("[1] Principle Components: \n", PCA_cols,"\n",sep =" | ")


write.csv(UFC_PCA,"UFC_PCA.csv", row.names = FALSE)


### Visualization of numeric column information
plot_num(ufc_data)


### Histogram for Blue Fighter Reach Difference
ggplot(ufc_data, aes(x=(B_Reach_cms - R_Reach_cms), y =..density.., fill=..count..)) + geom_histogram(binwidth = 5, ) + labs(x = "Reach Difference (cm)", title = "Histogram for Blue Fighter Reach Difference") + scale_fill_gradient("Count", low="#87CEEB", high="#00008B")


### Histogram for Red Fighter Reach Difference
ggplot(ufc_data, aes(x=(R_Reach_cms - B_Reach_cms), y =..density.., fill=..count..)) + geom_histogram(binwidth = 5, ) + labs(x = "Reach Difference (cm)", title = "Histogram for Red Fighter Reach Difference") + scale_fill_gradient("Count", low="#FFA07A", high="#FF0000")


### Barplot of Blue Fighter Age
ggplot(ufc_data, aes(x = B_age)) + geom_bar(fill = "#0000FF") #B_age

### Barplot of Blue Fighter Age
ggplot(ufc_data, aes(x = R_age)) + geom_bar(fill = "#FF0000") #R_age

### List of Blue fighter's winning average
temp <- ufc_data %>% select(B_fighter,B_wins)
temp <- temp %>% group_by(B_fighter) %>% summarise(avg=mean(B_wins))
temp <- temp %>% arrange(desc(avg))
temp <- temp[1:10,]
temp %>%
  formattable(list(avg = color_bar("#85C1E9")), align = 'l')

### List of Red fighter's winning average
temp <- ufc_data %>% select(R_fighter,R_wins)
temp <- temp %>% group_by(R_fighter) %>% summarise(avg=mean(R_wins))
temp <- temp %>% arrange(desc(avg))
temp <- temp[1:10,]
temp %>%
  formattable(list(avg = color_bar("#FF0000")), align = 'l')

##The winning Blue fighter according to weight_class
ufc_data %>% filter(Winner == "Blue") %>% count(weight_class) #weight_class'a göre kazanan blue


##The winning Red fighter according to weight_class
ufc_data %>% filter(Winner == "Red") %>% count(weight_class) #weight_class'a göre kazanan red

### Splitting columns containing numeric data
numeric_data <- select_if(ufc_data, is.numeric)

### Correlation Matrix
cor_data <- cor(numeric_data)
corrplot(cor_data, method = "color", type = "upper", tl.col = "black", tl.cex = 0.75, order="hclust", tl.col.cex=0.30)


### Pie chart showing the winning fighter
custom_col <- c("blue", "green", "red") 
ggplot(ufc_data, aes(x = "", y="", fill = factor(Winner))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5,size=22)) + 
  labs(fill="Winner", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Winners") + coord_polar(theta = "y", start=0)+
  scale_fill_manual(values=custom_col)

################
t <- list(family = "sans-serif",size = 14,color = 'black') # Text style
m <- list(l = 8,r = 8,b = 35,t = 35,pad =1) # Magins
################

### Weight Class Donut
is.num <- sapply(ufc_data, is.numeric) # Format to 3 Decimal Points
ufc_data [is.num] <- lapply(ufc_data [is.num], round, 3)

weight_class <- ufc_data$weight_class
weight_class <- as.data.frame(table(ufc_data$weight_class)) # frequency

t2 <- list(family = "sans-serif",size = 16,color = 'black') # Text style
m2 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
b <- plot_ly(weight_class, labels = ~Var1, values = ~Freq)%>%add_pie(hole = 0.6) %>%
  layout(title = "UFC Weight Class 2010 - 2019",
         showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font = t,
         margin = m
  )

print(b)


# *********************** #
# Events vs Years BarChart

# Extract Year
yearsList <- c()
for(date in data[,]$date){
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
  layout(title = "Number of UFC Matches Over Years",
         xaxis = list(title = "Year"),
         yaxis = list(title = "No. Of matches"),
         font = t,
         margin = m)


print(d)

# *********************** #
# Density plots

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

grid.arrange(p1, p2, p3, nrow=3)



print("~~ VISUALISATION ENDED:")


