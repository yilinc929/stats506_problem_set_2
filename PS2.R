
## Problem 1
### version 1: using loop
cost_per_roll<-2
play_dice_1<-function(num_rolls){
  win<-0
  for (i in 1:num_rolls) {
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
play_dice_2<-function(num_rolls) {
  roll<-sample(1:6, num_rolls,replace=TRUE)
  win<-sum(ifelse(roll %in% c(2,4,6),roll,-cost_per_roll))
  return(win)
}
play_dice_2(3)
play_dice_2(5)
play_dice_2(10)
play_dice_2(3000)
### version 3: collapsing the die rolls into a single table()
play_dice_3<-function(num_rolls) {
  rolls<-sample(1:6,num_rolls,replace=TRUE)
  roll_table<-table(rolls)
  cols<-as.integer(names(roll_table))
  cols<-cols[cols %in% c(2,4,6)]
  
  win<-sum(cols*roll_table[as.character(cols)])
  total_cost<-cost_per_roll*num_rolls
  total_win<-win-total_cost
  return(total_win)
}
play_dice_3(3)
play_dice_3(5)
play_dice_3(10)
play_dice_3(3000)

### version 4: using one of the “apply” functions.
play_dice_4<-function(num_rolls){
roll1<-sample(1:6,num_rolls,replace=TRUE)
win_total<-function(roll){
  if (any(roll) %in% c(2, 4, 6)) {
    return(roll)
  } else {
    return(-cost_per_roll)
  }
}
win<-sum(sapply(roll1,win_total))
return(win)
}
play_dice_4(3)
play_dice_4(5)
play_dice_4(10)
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
MC_dice<-function(num_simulation,num_plays_per_simulation){
  results<-numeric(num_simulation)
  
  for (i in 1:num_simulation){
    win<-sum(sample(c(1:6),num_plays_per_simulation,replace=TRUE))
    results[i]<-win-(num_plays_per_simulation*cost_per_roll)
  }
  mean_win<-mean(results)
  return(mean_win)
}
MC_dice(10000,100)
#To determine if the dice game is fair, we conduct a Monte Carlo simulation by simulating a large number of games.
#I choose this simulation to repeat 10000 times and 100 plays for each simulation.
#In a fair game, the expected winnings over a large number of plays should be close to zero. 
#However, the result of simulation demonstrated a large expected winning amount (mean) of 149.939, it is not a fair game.

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