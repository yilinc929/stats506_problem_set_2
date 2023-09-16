## Problem 2
### a
data<-read.csv('/Users/cathy/Desktop/cars.csv')
colnames(data) <- c("height", "length", "width", "driveline", 
                    "eng_type", "hybrid", "gears", "transmission","city_mpg", "fuel_type", "highway_mpg",
                    "classification","ID","make","model_year","year","horsepower","torque")
print(head(data))
newdata<-data[which(data$fuel_type=='Gasoline'),]
print(head(newdata))
