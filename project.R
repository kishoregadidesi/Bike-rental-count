rm(list=ls())
setwd("C:/Users/Kishore/Desktop/project1")
df= read.csv("day.csv",header= T)
head(df,10)

# To check weather the variable is normallydistrubuted
qqnorm(df$cnt)
hist(df$cnt)

#...............................  misising value analysis............................#
 
missingval= data.frame(apply(df,2,function(x){sum(is.na(x))}))
missingval$var_name= row.names(missingval)
row.names(missingval)= NULL
names(missingval)[1]= "value"
missingval= missingval[,c(2,1)]

# there are no missing values in the data

#...............................detect and remove outliers..............................#


df$mnth= as.factor(df$mnth)
df$season= as.factor(df$season)
df$yr= as.factor(df$yr)
df$holiday= as.factor(df$holiday)
df$weekday= as.factor(df$weekday)
df$weathersit= as.factor(df$weathersit)
df$workingday= as.factor(df$workingday)
df$dteday= as.numeric(df$dteday)

#extracting numerical data
numeric_var= sapply(df,is.numeric)
numeric_data= df[,numeric_var]
cnames= colnames(numeric_data)

library(ggplot2)
library(scales)
library(psych)
library(gplots)

#boxplot graph for indepandent variables with respect to depandent variable(registered)
ggplot(df, aes(x =cnt, y = registered)) + 
  geom_boxplot(fill="cornsilk",outlier.color = "red",outlier.size = 3)+
  guides(fill=FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x =cnt, y = casual)) + 
  geom_boxplot(fill="cornsilk",outlier.color = "red",outlier.size = 3)+
  guides(fill=FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x =cnt, y = windspeed)) + 
  geom_boxplot(fill="cornsilk",outlier.color = "red",outlier.size = 3)+
  guides(fill=FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x =cnt, y = hum)) + 
  geom_boxplot(fill="cornsilk",outlier.color = "red",outlier.size = 3)+
  guides(fill=FALSE)+theme_bw()+theme(text=element_text(size=10))


ggplot(df, aes(x =cnt, y = temp)) + 
  geom_boxplot(fill="cornsilk",outlier.color = "red",outlier.size = 3)+
  guides(fill=FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x =cnt, y = atemp)) + 
  geom_boxplot(fill="cornsilk",outlier.color = "red",outlier.size = 3)+
  guides(fill=FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x =cnt, y = dteday)) + 
  geom_boxplot(fill="cornsilk",outlier.color = "red",outlier.size = 3)+
  guides(fill=FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x =cnt, y = instant)) + 
  geom_boxplot(fill="cornsilk",outlier.color = "red",outlier.size = 3)+
  guides(fill=FALSE)+theme_bw()+theme(text=element_text(size=10))

#outliers are present in 'casual','windspeed','hum'
#to remove outliers in one variable

outdata= df$casual[df$casual%in% boxplot.stats(df$casual)$out] 
df= df[which(!df$casual %in% outdata),]

outdata= df$windspeed[df$windspeed%in% boxplot.stats(df$windspeed)$out] 
df= df[which(!df$windspeed %in% outdata),]

outdata= df$hum[df$hum%in% boxplot.stats(df$hum)$out] 
df= df[which(!df$hum %in% outdata),]


#...................................feauture selection...........................#

#corelation plot

numeric_new= sapply(df,is.numeric)

library(corrgram)
corrgram(df[,numeric_var],order = F, upper.panel=panel.pie,text.panel = panel.txt,main="correlation plot")

# atemp variable is highly corelated with temp and windspeed
# hum is very low correltaed with the target variable

factor_var= sapply(df,is.factor)
factor_data= df[,factor_var]

for (i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$weathersit,factor_data[,i])))
}

df= subset(df,select=-c(atemp,hum))

#...............................feature scaling...................................#

#to check weather the data is normally distributed in graphical form
qqnorm(df_del$cnt)
hist(df_del$cnt)

#data is not normally distributed so we can go for normalisation
#normalisation

cnames= c("instant","casual","registered")

for(i in cnames){
  print(i)
  df[,i]= (df[,i]-min(df[,i]))/ (max(df[,i]-min(df[,i])))
}


#.....................................developing the model..........................#

#....................................decision treeregression.........................#

library(rpart)
library(MASS)
#dteday carrying the same information of instant
df$dteday= NULL
set.seed(123)
day_index= sample(1:nrow(df),0.8*nrow(df))
train= df[day_index,]
test=  df[-day_index,]

fit= rpart(cnt~., data= train, method= "anova")
predict_dt= predict(fit,test[,-13])

y= df_del["cnt"]
yhat= predict_dt

mape= function(y,yhat){
  mean(abs((y-yhat)/y)*100)
}

mape(test[,13],predict_dt)

# error= 10.08858
rmse(test[,13],predict_dt)

#....................................random forest.....................................#

library(randomForest)
set.seed(232)
rf_mod= randomForest(cnt~.,train,importance= TRUE)
predict_rf=predict(rf_mod,test[,-13])

y=df_del["cnt"]
yhat= predict_rf

mape= function(y,yhat){
  mean(abs((y-yhat)/y)*100)
}


mape(test[,13],predict_rf)

#error= 6.787741

rmse= function(y, ypred){
  sqrt(mean((y-ypred)^2))
}

rmse(test[,13],predict_rf)



#...................................linear regression.......................................#

#to check multi colinearity
numeric_nvar= sapply(df,is.numeric)
numeric_ndata= df[,numeric_var]

library(usdm)
vifcor(numeric_ndata[,-13],th=0.9)



set.seed(235)
linear_r= lm(cnt~., data= train)
summary(linear_r)

predict_lr= predict(linear_r,test[,-13])

y=df["cnt"]
yhat= predict_lr

mape= function(y,yhat){
  mean(abs((y-yhat)/y)*100)
}

mape(test[,13],predict_lr)

# error=1.196801e-13

#comapring all methods linear regression accuracy is high
# conclusion is linear regression is best suited for this dataset

