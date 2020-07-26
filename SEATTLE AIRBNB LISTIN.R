#library's
library(dplyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(tidyr)
library( gapminder)
library(MASS)
library(data.table)
library(onehot)
library(mltools)
library(EnvStats)
library(VIM)
library(Amelia)
library(utils)
library(graphics)
library(mlbench)
library(generics)
library(caret)
library(DataExplorer)
library(funModelling)
library(MLmetrics)
library(xgboost)
library(earth)
library(glmnet)
library(randomForest)
library(Metrics) 
library(ROCR)
library(classifierplots)
library(mapview)
library(Hmisc)
library(ggmap)
library(ggplot2)
library(lattice)
library(Seurat)
library(geohashTools)
library(forcats)
library(lubridate)
library(caret)
library(naniar)
library(mice)

# To read the train dataset without any blanks 
PData = read.csv("/Users/saisripotluru/Desktop/Uni\ of\ ok/IDA/project/seattle_01.csv",stringsAsFactors=TRUE)

View(PData)
summary(PData)
str(PData)
setnames(PData,old=c("overall_satisfaction"),new=c("cust_sat"))
#The location plot for the areas in which the Airbnb rooms are located
locations_sf <- st_as_sf(PData, coords = c("longitude", "latitude"), crs = 4326)

mapview(locations_sf)

# plot for area location rep
ggplot(data=PData)+geom_point(aes(x=latitude,y=longitude,colour=address))
#plot for cust satisfaction 
PData$cust_sat<-as.factor(PData$cust_sat)
ggplot(data=PData)+geom_point(aes(x=latitude,y=longitude,colour=cust_sat))


#Renaming address column names to a correct nameing format

PData$address<-as.character(PData$address)

PData$address[PData$address == "西雅图, WA, United States"] <- "Seattle , WA, United States"
PData$address[PData$address == "Kirkland , Wa, United States"] <- "Kirkland , WA, United States"
PData$address[PData$address == "Seattle, United States"] <- "Seattle , WA, United States"
PData$address[PData$address == "Seattle, WA, United States"] <- "Seattle , WA, United States"
PData$address[PData$address == "Seattle, WA"] <- "Seattle , WA, United States"
PData$address[PData$address == "Kirkland , WA, United States"] <- "Kirkland, WA, United States"
PData$address[PData$address == "Seattle, Washington, US, WA, United States"] <- "Seattle , WA, United States"
PData$address[PData$address == "Ballard Seattle, WA, United States"] <- "Ballard, Seattle, WA, United States"
#to know the unique addresses
f<-unique(PData$address)
View(f)
View(PData)
# to know the percentage of missingness in all the variables
PData %>% mutate_all(is.na) %>% summarise_all(mean)

#missing values imputation
summary(PData$cust_sat)
PData$cust_sat<-as.character(PData$cust_sat)
PData$cust_sat[is.na(PData$cust_sat)]<-'6'
PData$cust_sat<-as.numeric(PData$cust_sat)
PData$bathrooms<-as.factor(PData$bathrooms)
summary(PData$bathrooms)
PData$bathrooms[is.na(PData$bathrooms)]<-'1'


# Horizontal box plotfor detecting outliers

ggplot(PData, aes(room_type,price))  +geom_boxplot(outlier.colour = "red", outlier.shape = 1)       
ggplot(PData, aes(address,price))  +geom_boxplot(outlier.colour = "red", outlier.shape = 1,main="Outliers") + coord_flip()     
ggplot(PData, aes(accommodates,price))  +geom_boxplot(outlier.colour = "red", outlier.shape = 1)       
ggplot(PData, aes(bathrooms,price))  +geom_boxplot(outlier.colour = "red", outlier.shape = 1)       
ggplot(PData, aes(bedrooms,price))  +geom_boxplot(outlier.colour = "red", outlier.shape = 1)       

# plot for the price distribution
ggplot(PData, aes(price)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Bin width is $10")


#outliers detection and graph representation
#outlierKD <- function(dt,var) {
#  var_name <- eval(substitute(price),eval(dt))
#na1 <- sum(is.na(var_name))
#  m1 <- mean(var_name, na.rm = T)
#  par(mfrow=c(2, 2), oma=c(0,0,3,0))
#  boxplot(var_name, main="With outliers")
#  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
#  outlier <- boxplot.stats(var_name)$out
#  mo <- mean(outlier)
#  var_name <- ifelse(var_name %in% outlier, NA, var_name)
#  boxplot(var_name, main="Without outliers")
#  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
#  title("Outlier Check", outer=TRUE)
#  na2 <- sum(is.na(var_name))
#  cat("Outliers identified:", na2 - na1, "n")
#  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
#  cat("Mean of the outliers:", round(mo, 2), "n")
#  m2 <- mean(var_name, na.rm = T)
#  cat("Mean without removing outliers:", round(m1, 2), "n")
#  cat("Mean if we remove outliers:", round(m2, 2), "n")
#  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
#  if(response == "y" | response == "yes"){
#    dt[as.character(substitute(var))] <- invisible(var_name)
#    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
#    cat("Outliers successfully removed", "n")
#    return(invisible(dt))
#  } else{
#    cat("Nothing changed", "n")
#    return(invisible(var_name))
#  }
#}
#outlierKD(PData,Price)
#removal of outliers 
PData<- subset(PData, price!="5900")
PData<- subset(PData, price!="3000")
PData<- subset(PData, price!="2800")
PData$cust_sat<-as.factor(PData$cust_sat)

#correlation matrix
x<-PData[,c(7,8,11)]
res <- cor(x)
res

#plot representation for room type (to understande the correlation)
PData$accommodates<-as.numeric(PData$accommodates)
PData$bedrooms<-as.numeric(PData$bedrooms)
PData$bathrooms<-as.numeric(PData$bathrooms)
summary(PData)

ab<-ggplot(PData, aes(accommodates, bedrooms))+
  stat_bin2d(aes(fill = stat(count)), binwidth = c(1,1))+
  geom_smooth(method = "lm", fill = "red3", color = "brown4", size = 0.45)+
  annotate("text", x = 24, y = 2, label = "italic(r) == 0.802", parse = T, size = 5)+
  scale_x_continuous(name = "accommodates", breaks = seq(0,30,2))+
  scale_y_continuous(name = "bedrooms", breaks = seq(0,8,1))+
  scale_fill_distiller(palette = "Purples", direction = 1)

aba <- ggplot(PData, aes(accommodates, bathrooms)) + 
  stat_bin2d(aes(fill = stat(count)), binwidth = c(1,1))+
  geom_smooth(method = "lm", fill = "red3", color = "brown4", size = 0.45)+
  annotate("text", x = 24, y = 4.5, label = "italic(r) == 0.545", parse = T, size = 5)+
  scale_x_continuous(name = "accommodates", breaks = seq(0,30,2))+
  scale_y_continuous(name = "bathrooms", breaks = seq(0,8,1))+
  scale_fill_distiller(palette = "Reds", direction = 1)

bb <- ggplot(PData, aes(bedrooms, bathrooms)) + 
  stat_bin2d(aes(fill = stat(count)), binwidth = c(1,1))+
  geom_smooth(method = "lm", fill = "red3", color = "brown4", size = 0.45)+
  annotate("text", x = 7.5, y = 4.5, label = "italic(r) == 0.324", parse = T, size = 5)+
  scale_x_continuous(name = "bedrooms", breaks = seq(0,8,1))+
  scale_y_continuous(name = "bathrooms", breaks = seq(0,8,1))+
  scale_fill_distiller(palette = "Blues", direction = 1)

grid.arrange(ab, aba, bb, nrow=3)


#the count of the address variable in the dataset

PData %>% 
  group_by(PData$address) %>%
  summarise(n=n())

#to remove the unique factor level in the address variable
subset(PData, PData$address=="Capitol Hill, Seattle, WA, United States")
PData <- PData[-c(3740),]

subset(PData, PData$address=="Hunts Point, WA, United States")
PData <- PData[-c(4621),] 

subset(PData, PData$address=="Seattle, DC, United States")
PData <- PData[-c(6069),]

subset(PData, PData$address=="Tukwila, WA, United States")
PData <- PData[-c(3352),]

subset(PData, PData$address=="Vashon, WA, United States")
PData <- PData[-c(6518),]

subset(PData, PData$address=="Washington, WA, United States")
PData <- PData[-c(7575),]

subset(PData, PData$address=="West Seattle, WA, United States")
PData <- PData[-c(571),]

PData<- subset(PData, address!="Hunts Point, WA, United States")
PData<- subset(PData, address!="Seattle, DC, United States")
PData<- subset(PData, address!="Vashon, WA, United States")
PData<- subset(PData, address!="Washington, WA, United States")
PData<- subset(PData, address!="Ballard, Seattle, WA, United States")
PData<- subset(PData, address!="Bothell, WA, United States")
PData<- subset(PData, address!="Clyde Hill, WA, United States")
PData<- subset(PData, address!="Medina, WA, United States")
PData<- subset(PData, address!="Yarrow Point, WA, United States")

#price along with area
PData %>%
  mutate(price = as.numeric(gsub("\\$", "", price))) %>%
  ggplot() +
  geom_histogram(mapping = aes(price, fill=address), binwidth=10) +
  labs(fill = NULL)


#plots for most popular types of accomodations (room_type) by area(address)

ggplot(data = PData, aes(x = address)) +
  geom_bar(aes(fill = room_type), position = "dodge")+ coord_flip()


#plot for overall satisfaction(cus_sat) for accomodation 

ggplot(data = PData, aes(x = room_type)) +
  geom_bar(aes(fill = cust_sat), position = "dodge")+ coord_flip()



#dividing data to train and test
#set the seed 
set.seed(123)
ind<-sample.int(2,nrow(PData),replace=TRUE,prob=c(0.70,0.30))
# the training dataset which has the 65% of observations
training <- PData[ind==1,]
# the testing dataset which has the 35% of the observations
testing <- PData[ind==2,]
View(training)
View(testing)
summary(training)
summary(testing)
summary(PData$price)


#price graph

ggplot(PData) +
  geom_histogram(aes(x = log(price)),
                 binwidth = 0.05, fill = "grey", color = "black")


#Modelling
#linear model
LinearModel <- lm(data = training,log(price) ~ room_type+address+cust_sat+log(accommodates+1)+bedrooms+bathrooms)
summary(LinearModel)
# to predict the linear model with original one
predictM<-predict(LinearModel,training)
# to calculate the rmse value for the train data
rmse(log(training$price),predictM)
# to predict the linear model with test data 
predictM8<-predict(LinearModel,testing)
# to calculate the rmse value for the test data 
rmse(log(testing$price),predictM8)


plot(LinearModel)
#CV 
model_lm_cv <- train(log(price) ~ room_type+address+cust_sat+log(accommodates+1)+bedrooms+bathrooms, 
                     data = training,                  
                     trControl = ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.omit, metric = 'RMSE')
model_lm_cv


#plot with residuals and fitted values

ggplot(training, aes(x = X)) +
  geom_point(aes(y = LinearModel$residuals)) + 
  geom_hline(yintercept = 0, color = 'red', linetype = "dashed") + 
  labs(x = "X", y = "Residuals")

xyplot(values ~ x, data = testing, group = ind, auto.key = TRUE)


#lasso
ctrl <- trainControl(method = "cv", number = 7)
lasso<-caret::train(log(price) ~ room_type+address+cust_sat+accommodates+bedrooms+bathrooms, 
                    training, method='glmnet', tuneGrid=expand.grid(alpha=1,
                                                                    lambda=seq(0.0001,0.2, length=5)), trControl= ctrl)
# to predict the  model with original one
predictM<-predict(lasso,training)
# to calculate the rmse value for the train data
rmse(log(training$price),predictM)
# to predict the linear model with test data
predictM8<-predict(lasso,testing)
# to calculate the rmse value for the test data 
rmse(log(testing$price),predictM8)



# ridge
ridge<-caret::train(log(price) ~ room_type+address+cust_sat+accommodates+bedrooms+bathrooms, 
                    training, method='glmnet', tuneGrid=expand.grid(alpha=0,
                                                              lambda=seq(0.0001,1, length=10)), trControl= ctrl)

ridge
# to predict the  model with original one
predictR<-predict(ridge,training)
# to calculate the rmse value for the train data
rmse(log(training$price),predictR)
# to predict the linear model with test data
predictRT<-predict(ridge,testing)
# to calculate the rmse value for the test data 
rmse(log(testing$price),predictRT)



#MARS


#model for mars
mars1 <- earth(
  log(price) ~ room_type+address+cust_sat+accommodates+bedrooms+bathrooms,  
  data = training   
)

mars1
plot(mars1)
hyper_grid<-expand.grid(degree=1:3,nprune=seq(2,100,length.out = 10)%>%floor())
tuned_mars<- train(log(price) ~ room_type+address+cust_sat+accommodates+bedrooms+bathrooms, 
                   training,
                   method="earth",
                   metric ="RMSE",
                   trControl = trainControl(method = "cv",number=3),
                   Use.beta.cache = TRUE,
                   tuneGrid = hyper_grid)

tuned_mars
# to predict the  model with original one
predictMA<-predict(tuned_mars,training)
# to calculate the rmse value for the train data
rmse(log(training$price),predictMA)
# to predict the linear model with test data
predictMAT<-predict(tuned_mars,testing)
# to calculate the rmse value for the test data 
rmse(log(testing$price),predictMAT)

#plot with residuals and fitted values

ggplot(training, aes(x = X)) +
  geom_point(aes(y = tuned_mars$residuals)) + 
  geom_hline(yintercept = 0, color = 'red', linetype = "dashed") + 
  labs(x = "X", y = "Residuals")














