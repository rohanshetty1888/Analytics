# To determine which physiochemical properties make a wine 'good'!

#Load dataset
setwd('D:\\EPBA\\GradedAssignments\\Assignment3')

wine<-read.csv("wineQualityReds.csv")

#Load libraries
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)

head(wine)
str(wine)
wine <-wine[,2:13]
View(wine)

##Do exploratory analysis##

summary(wine)

# Missing Values - None


#Transforming Quality from an Integer to a Factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new Factored Variable called 'Rating'

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))

## Structure and summary of the Dataframe
str(wine)
summary(wine)
nrow(wine)
ncol(wine)

## Univariate Plots

#I am going to plot the distribution of each of the variable as I would like to get a feel of the variables first.
#Based on the distribution shape, i.e. Normal, Positive Skew or Negative Skew, 
#this willhelp me to get some sense what to expect when I plot different variables against each other.

ggplot(data = wine, aes(x = quality)) +
  geom_bar(width = 1, color = 'black',fill = I('orange'))


ggplot(data = wine, aes(x = rating)) +
  geom_bar(width = 1, color = 'black',fill = I('blue'))

#One thing I am seeing from the above two plots is most of the wines in the dataset are average quality wines.
#As the good quality and the poor quality wines are almost like outliers here,
#it might be difficult to get an accurate model of the Wine Quality.
#Let's look at the other plots.

#fixed acidity
hist(wine$fixed.acidity,  col = "orange")

#volatile acidity
hist(wine$volatile.acidity, col = "orange")

#citric.acid
hist(wine$citric.acid, col = "orange")

#residual.sugar
hist(wine$residual.sugar, col = "orange")

# Chlorides
hist(wine$chlorides, col = "orange")

# free.sulfur.dioxide
hist(wine$free.sulfur.dioxide, col = "orange")

# total.sulfur.dioxide
hist(wine$total.sulfur.dioxide, col = "orange")

# density
hist(wine$density, col = "orange")

# pH 
hist(wine$pH, col = "orange")

#sulphates
hist(wine$sulphates, col = "orange")

#alcohol
hist(wine$alcohol, col = "orange")

##Distribution and Outliers
#Density and pH seems normally distributed with few outliers.
#Residual sugar and Chloride seems to have extreme outliers.
#Fixed and volatile acidity, total and free sulfur dioxides, alcohol and sulphates seem to be long-tailed for the outliers present.
#Citric acid has many zero values. I wonder if this is due to incomplete data entry.


#Bivariate Plots

#Transforming quality to numeric
wine$quality<-as.numeric(as.character(wine$quality))
str(wine)

#Removing rating column as it is not required
#wine%>%select(-rating)->wine


# Checking co-relation
cor(wine[,1:12],wine$quality)

#fixed.acidity         0.12405165
#volatile.acidity     -0.39055778
#citric.acid           0.22637251
#residual.sugar        0.01373164
#chlorides            -0.12890656
#free.sulfur.dioxide  -0.05065606
#total.sulfur.dioxide -0.18510029
#density              -0.17491923
#pH                   -0.05773139
#sulphates             0.25139708
#alcohol               0.47616632

#Volatile acidity, and alcohol have strong influence on quality
#Even Sulphates and Citric acid has some good amount of co-relation with quality of red wine.

#Now let us create some Box plots between these variables to see if anything is missed from the correlation table.

boxplot(fixed.acidity~quality,data=wine,xlab="quality", ylab="fixed.acidity", col="orange")
means <- tapply(wine$fixed.acidity,wine$quality,mean)
points(means,col="blue",pch=8)
#Fixed Acidity has almost no effect on the Quality.

boxplot(volatile.acidity~quality,data=wine,xlab="quality", ylab="volatile.acidity", col="orange")
means <- tapply(wine$volatile.acidity,wine$quality,mean)
points(means,col="blue",pch=8)
#Volatile acid seems to have a negative impact on the quality of the wine.
#As volatile acid level goes up, the quality of the wine degrades.

boxplot(citric.acid~quality,data=wine,xlab="quality", ylab="citric.acid",col="orange")
means <- tapply(wine$citric.acid,wine$quality,mean)
points(means,col="blue",pch=8)
#Citric acid seems to have a positive correlation with Wine Quality. Better wines have higher Citric Acid.

boxplot(residual.sugar~quality,data=wine,xlab="quality", ylab="residual sugar",col="orange")
means <- tapply(wine$residual.sugar,wine$quality,mean)
points(means,col="blue",pch=8)
#Residual Sugar almost has no effect on the Quality of the Wine. 

boxplot(chlorides~quality,data=wine,xlab="quality", ylab="chlorides", col="orange")
means <- tapply(wine$chlorides,wine$quality,mean)
points(means,col="blue",pch=8)
#Even though weakly correlated,it seems that lower percent of Chloride seems to produce better wines.

boxplot(free.sulfur.dioxide~quality,data=wine,xlab="quality", ylab="free.sulfur.dioxide",col="orange")
means <- tapply(wine$free.sulfur.dioxide,wine$quality,mean)
points(means,col="blue",pch=8)
#We cannot see much significant information here.However concentration of free sulfur dioxide is more in average wines.

boxplot(total.sulfur.dioxide~quality,data=wine,xlab="quality", ylab="total.sulfur.dioxide",col="orange")
means <- tapply(wine$total.sulfur.dioxide,wine$quality,mean)
points(means,col="blue",pch=8)
#As this is a Subset of Free Sulphur Dioxide, we see a similar pattern here.

boxplot(density~quality,data=wine,xlab="quality", ylab="density",col="orange")
means <- tapply(wine$density,wine$quality,mean)
points(means,col="blue",pch=8)
# Better wines seems to have lower densities.

boxplot(pH~quality,data=wine,xlab="quality", ylab="pH",col="orange")
means <- tapply(wine$pH,wine$quality,mean)
points(means,col="blue",pch=8)
# Better wines seems to have lower pH level i.e. more acidic.

boxplot(sulphates~quality,data=wine,xlab="quality", ylab="sulphates", col="orange")
means <- tapply(wine$sulphates,wine$quality,mean)
points(means,col="blue",pch=8)
# Even though we see many outliers in the 'Average' quality wine, it seems that better red wines have a stronger concentration of Sulphates.

boxplot(alcohol~quality,data=wine,xlab="quality", ylab="alcohol",col="orange")
means <- tapply(wine$alcohol,wine$quality,mean)
points(means,col="blue",pch=8)
#It is pretty evident that better red wines have higher Alcohol content in it.

#Let's make a simple linear model and try to get the statistics here.

alcoholLinearModel <- lm(quality~alcohol,data = wine)
summary(alcoholLinearModel)

#Based on the value of R squared, we see that Alcohol alone contributes to only about 22% of the Wine quality. 

#Splitting data set into test and training samples
set.seed(200)
index<-sample(nrow(wine),0.70*nrow(wine),replace=F)
train<-wine[index,]
test<-wine[-index,]

nrow(train)
nrow(test)
nrow(wine)

## Multivariate Plots

# We saw above alcohol is one of the strong variable in influencing the quality of wine.
#Now, keeping alcohol constant, let see if other variables influence quality of wine. 

ggplot(data = wine, aes(x = alcohol,y = sulphates, col = rating)) +
         geom_point(alpha = 0.8, size = 1) + geom_smooth(method = "lm", se = F) 
#High alcohol with considerable high concentration of sulphate seem to produce better wine.
       
ggplot(data = wine, aes(x = alcohol,y = volatile.acidity, col = rating)) +
  geom_point(alpha = 0.8, size = 1) + geom_smooth(method = "lm", se = F) 
#High alcohol with considerable low concentration of volatile acidity seem to produce better wine.

#Now I am going to take the variables with strong correlation with the quality of the wine and generate a linear model with them.

## Applying Linear Regression
#Model 1
mod1<-lm(quality~alcohol,data=train)
summary(mod1)

hist(mod1$residuals)
qqPlot(mod1$residuals)

# Adjusted R-squared:  0.2476, overall p-value: < 2.2e-16

#Model 2 

mod2<-lm(quality~sulphates+alcohol,data=train)
summary(mod2)

hist(mod2$residuals)
qqPlot(mod2$residuals)
plot(mod2$fitted.values,mod2$residuals)
vif(mod2)

# Adjusted R-squared:  0.2908, p-value: < 2.2e-16. Better but not good enough

# Model 3

mod3<-lm(quality~volatile.acidity+sulphates+alcohol,data=train)
summary(mod3)

hist(mod3$residuals)
qqPlot(mod3$residuals) ##little better than previous models
plot(mod3$fitted.values,mod3$residuals)
vif(mod3)

# Adjusted R-squared:  0.3474, p-value: < 2.2e-16. Better but not good enough

#Model 4
mod4<-lm(quality~volatile.acidity+citric.acid+sulphates+alcohol,data=train)
summary(mod4)

hist(mod4$residuals)
qqPlot(mod4$residuals)
plot(mod3$fitted.values,mod3$residuals)
vif(mod4)

# Adjusted R-squared:  0.3469, p-value: < 2.2e-16. Almost same as previous model

#Model 5
mod5<-lm(quality~alcohol+volatile.acidity+citric.acid+sulphates+fixed.acidity,data=train)
summary(mod5)

hist(mod5$residuals)
qqPlot(mod5$residuals)
plot(mod5$fitted.values,mod5$residuals)
vif(mod5)

# Adjusted R-squared:  0.3543, p-value: < 2.2e-16. Better than previous model
#VIF
#alcohol    volatile.acidity citric.acid  sulphates    fixed.acidity 
#1.084415   1.574973         2.676747     1.134811     1.927460 

# Prediction : Fitted Values of Model VS Actual Value
predicted<-mod5$fitted.values
actual<-train$quality

dat<-data.frame(predicted,actual)
head(dat)

# head(dat)
#      predicted     actual
#854   6.057541      6
#933   5.389957      6
#942   6.522246      7
#1103  5.813115      6
#1065  5.823891      6
#1338  5.198089      5

#Graph Plot: Actual Values Vs Error (Predicted - Actual)
df <- data.frame(dat$actual,dat$predicted-dat$actual)
names(df) <- c("quality", "error")
head(df)

ggplot(data=df, aes(x=quality,y=error)) +
  geom_jitter(alpha = 0.3) +
  ggtitle("Linear model errors vs expected quality")


#Here, We see that the error is much more dense in the 'Average' quality section than the 'Good' and the 'Bad' quality wines. 
#This is evident from the fact that most of our dataset contains 'Average' quality wines and there is not too many data in the extreme ranges. 
#The R squared value for 5th model (mod5) could only explain around 35% change in quality. 
#However, due to the lack of information, it is not the best of models to predict both 'Good' and 'Bad' quality wines

##Validation

# Applying the fifth model (mod5) on Test data

finalmod<-lm(quality~alcohol+volatile.acidity+citric.acid+sulphates+fixed.acidity,data=test)
summary(finalmod)

finalmod<-lm(quality~alcohol+volatile.acidity+citric.acid+sulphates+fixed.acidity,data=test)
summary(finalmod)

hist(finalmod$residuals)
qqPlot(finalmod$residuals)
plot(finalmod$fitted.values,finalmod$residuals)
vif(finalmod)


# Adjusted R-squared:  0.308, p-value: < 2.2e-16.
#vif
#alcohol    volatile.acidity  citric.acid   sulphates   fixed.acidity 
#1.073685   1.499089          2.618850      1.118005    1.966821 


predicted1<-finalmod$fitted.values
actual1<-test$quality

dat1<-data.frame(predicted1,actual1)
dat1$error<-dat1$predicted1-dat1$actual1
head(dat1)

#     predicted1    actual1      error
#6    5.155359       5           0.1553588
#7    5.188282       5           0.1882817
#11   5.123794       5           0.1237940
#14   5.582424       5           0.5824239
#15   5.331526       5           0.3315263
#20   5.676508       6           -0.3234922

df1 <- data.frame(dat1$actual1,dat1$predicted1-dat1$actual1)
names(df1) <- c("quality", "error")
head(df1)

ggplot(data=df1, aes(x=quality,y=error)) +
  geom_jitter(alpha = 0.3) +
  ggtitle("Linear model errors vs expected quality")



##Summary / Reflection

#Observations
#High Alcohol and Sulaphate content seems to produce better wines.
#Citric Acid, even though weakly correlated plays a part in improving the wine quality.
#However, as the data was very centralized towards the 'Average' quality, 
#my training set did not have enough data to accurately build a model which can predict the quality of wine.
#So in future if I can get a dataset about Red Wines with more complete information then
#I can build my models more effectively.


