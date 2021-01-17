
# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "formattable",
  "PerformanceAnalytics",
  "dplyr",
  "ggplot2",
  "reshape2",
  "caret",
  "corrplot",
  "sqldf",
  "lattice"
) 


library(pacman) 
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE) 

NreadDataset <- function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}


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
str(df)
PCA_cols <- perform_PCA(df[,-1])

normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}

UFC_PCA<-data.frame(df[1],df[,PCA_cols]) # concat 1st column
cat("[1] Principle Components: \n", PCA_cols,"\n",sep =" | ")


write.csv(UFC_PCA,"UFC_PCA.csv", row.names = FALSE)

print("~~ DATA PRE-PROCESSING ENDED")
dim(UFC_PCA)

str(UFC_PCA)

