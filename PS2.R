## Problem 1
## Problem 1
### version 1: using loop
cost_per_roll<-2
play_dice_1<-function(num_dice){
  win<-0
  for (i in 1:num_dice) {
    roll<-sample(1:6,1,replace=TRUE)
    if (roll %in% c(2,4,6)) {
      win<-win+roll
    } else {
      win<-win-cost_per_roll
    }
  }
  return(win)
}
play_dice_1(5)
play_dice_1(10)
play_dice_1(3)
play_dice_1(3000)
### version 2: using built-in R vectorized function
play_dice_2<-function(num_dice) {
  roll<-sample(1:6, num_dice,replace=TRUE)
  win<-sum(ifelse(roll %in% c(2,4,6),roll,-cost_per_roll))
  return(win)
}
play_dice_2(3)
play_dice_2(5)
play_dice_2(10)
play_dice_2(3000)
### version 3: collapsing the die rolls into a single table()


### version 4: using one of the “apply” functions.
play_dice_4<-function(num_dice){
roll1<-sample(1:6,num_dice,replace=TRUE)
win_total<-function(roll){
  if (roll1 %in% c(2,4,6)){
    return(roll1)
  }else{
    return(-cost_per_play)
  }
}
win<-sum(sapply(roll1,win_total))
return(win)
}
play_dice_4(3)
play_dice_4(5)
play_dice_4(6)
play_dice_4(3000)

### d)
library(microbenchmark)
results_low<-microbenchmark(
  V1=play_dice_1(100),
  V2=play_dice_2(100),
  V3=play_dice_3(100),
  V4=play_dice_4(100))

results_high<-microbenchmark(
  V1=play_dice_1(1000),
  V2=play_dice_2(1000),
  V3=play_dice_3(1000),
  V4=play_dice_4(1000))
print(results_low)
print(results_high)
### e)

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
coefficients(mod)

X_int<-cbind(1,data2$horsepower,data2$torque,data2$width,data2$length,data2$height,data2$y1,data2$y2,data2$y3,data2$horsepower*data2$torque)
beta_hat2<-solve(t(X_int) %*% X_int) %*% t(X_int) %*% Y
print(beta_hat2)
coefficients(mod_interaction)