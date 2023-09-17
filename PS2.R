## Problem 2
### a
data<-read.csv('/Users/cathy/Desktop/cars.csv')
colnames(data) <- c("height", "length", "width", "driveline", 
                    "eng_type", "hybrid", "gears", "transmission","city_mpg", "fuel_type", "highway_mpg",
                    "classification","ID","make","model_year","year","horsepower","torque")
print(head(data))
### b
newdata<-data[which(data$fuel_type=='Gasoline'),]
print(head(newdata))
### c
mod<-lm(highway_mpg~horsepower+torque+width+length+height+as.factor(year),data=newdata)
summary(mod)
### d
library(emmeans)
mod_interaction<-lm(highway_mpg~horsepower*torque+width+length+height+as.factor(year),data=newdata)
summary(mod_interaction)
em<-emmeans(mod_interaction,~horsepower*torque,at=list(horsepower=c(150,300,450),torque=c(100,250,400)))
emmip(em, horsepower~torque,at=list(wt=1:5))
### e
data2<-newdata
data2$y1<-ifelse(data2$year=="2010",1,0)
data2$y2<-ifelse(data2$year=="2011",1,0)
data2$y3<-ifelse(data2$year=="2012",1,0)
X<-as.matrix(data2[, c("horsepower","torque","width","length","height","y1","y2","y3")])
X<-cbind(1,X)
Y<-data2$highway_mpg
beta_hat<-solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_hat)
