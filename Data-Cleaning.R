load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
library(data.table)


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

#butun degerleri null olan kolonlari siliyor (bizde yok)
data <- data %>% 
  select(where(~!all(is.na(.))))

dim(data)

#en az bir degeri null olan kolonlari siliyor.
data <- data %>% 
  select(where(~!any(is.na(.))))

#null value icin gorsellestirmeyi yeniden run edersek missing deger kalmadigini goruruz

dim(data)

cat('Data has', dim(data)[1], 'rows and', dim(data)[2], 'columns.')

names(data)

data <- data %>% select(-Referee, -date, -location, -B_win_by_Decision_Majority, -R_win_by_Decision_Majority, -B_win_by_Decision_Split, 
                        -R_win_by_Decision_Split, -B_win_by_Decision_Unanimous, -R_win_by_Decision_Unanimous, -B_win_by_TKO_Doctor_Stoppage, 
                        -R_win_by_TKO_Doctor_Stoppage, -B_win_by_Submission, -R_win_by_Submission)  #düşürülecek kolonlar 


names(data)

##korelasyon matrisi cizdirme

