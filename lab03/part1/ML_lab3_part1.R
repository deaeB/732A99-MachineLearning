set.seed(1234567890)
library(geosphere)
library(ggplot2)
stations <- read.csv("C:/Users/yj313/Desktop/LAB/732A99-MachineLearning/lab03/part1/stations.csv")
temps <- read.csv("C:/Users/yj313/Desktop/LAB/732A99-MachineLearning/lab03/part1/temps50k.csv")
st <- merge(stations,temps,by="station_number")

# create three functions to calculate three kinds of distances between the point
# of interest and the training point.

# distance_1 <- function(point1,point2){
#   return(distHaversine(point1,as.matrix(point2,ncol = 2)))
# }
distance_1 <- function(point1,point2){
  return(distHaversine(point1,point2))
}

distance_2 <- function(date1,date2){
  days <- difftime(as.POSIXct(date1,format= "%Y-%m-%d"), as.POSIXct(date2,
                                                                    format = "%Y-%m-%d"),units = "days")
  return(as.numeric(days))
}


distance_3 <- function(time1,time2){
  hours <- difftime(as.POSIXct(time1,format = "%H:%M:%S"), as.POSIXct(time2,
                                                                      format = "%H:%M:%S"),units = "hours")
  return(as.numeric(hours))
}


# using plots to show widths chosen are appropriate

new_index <- sample(nrow(st))
new_data <- st[new_index,]
data_shown <- new_data[1:10,]

# for the first width

new_data_1 <-  c(" 58.4274", "14.826", "2013-11-04","08:00:00")
h_distance <- 600000
distance <- NULL
for(i in 1:10){
  distance_temp <- distance_1(as.numeric(new_data_1[1:2]),data_shown[i,4:5])
  distance <- c(distance,distance_temp)
}

order_ascend <- order(distance)
data_distance_1 <- data_shown[order_ascend,]

kernel_distance_1 <- NULL

for (i in 1:10) {
  kernel_distance_1_temp <- exp(-distance_1(as.numeric(new_data_1[1:2]),
                                            data_distance_1[i,4:5])^2 / (2 * h_distance^2))
  kernel_distance_1 <- c(kernel_distance_1,kernel_distance_1_temp)
}

plot(1:10,kernel_distance_1,main = "the plot for h_distance = 600000")


# for second width

new_data_2 <-  c("58.4274", "14.826", "2013-11-04","08:00:00")
h_date <- 300
distance <- NULL
for(i in 1:10){
  distance_temp <- distance_2(new_data_2[3],data_shown[i,9])
  distance <- c(distance,distance_temp)
}

order_ascend <- order(distance)
data_distance_2 <- data_shown[order_ascend,]

kernel_distance_2 <- NULL

for (i in 1:10) {
  kernel_distance_2_temp <- exp(-distance_2(new_data_2[3],
                                            data_distance_2[i,9])^2 / (2 * h_date^2))
  kernel_distance_2 <- c(kernel_distance_2,kernel_distance_2_temp)
}

plot(1:10,kernel_distance_2,main = "the plot for h_date = 300")


# for thrid width

new_data_3 <-  c("58.4274", "14.826", "2013-11-04","08:00:00")
h_time <- 3
distance <- NULL
for(i in 1:10){
  distance_temp <- distance_3(new_data_2[4],data_shown[i,10])
  distance <- c(distance,distance_temp)
}

order_ascend <- order(abs(distance))
data_distance_3 <- data_shown[order_ascend,]

kernel_distance_3 <- NULL

for (i in 1:10) {
  kernel_distance_3_temp <- exp(-distance_3(new_data_3[4],
                                            data_distance_3[i,10])^2 / (2 * h_time^2))
  kernel_distance_3 <- c(kernel_distance_3,kernel_distance_3_temp)
}

plot(1:10,kernel_distance_3,main = "the plot for h_time = 3")





predicted_temp <- function(data1,data2,h_distance = 600000,h_date = 300, h_time = 3){
  n <- nrow(data2)
  nominator<- NULL
  denominator <- NULL

  nominator_2 <- NULL
  denominator_2 <- NULL

  for(i in 1:n){
    kernel_1 <- exp(-distance_1(as.numeric(data1[1:2]),
                                data2[i,4:5])^2 / (2 * h_distance^2))
    kernel_2 <- exp(-distance_2(data1[3],data2[i,9])^2 / (2 * h_date^2))
    kernel_3 <- exp(-distance_3(data1[4],data2[i,10])^2 / (2 * h_time^2))

    kernel_real <- kernel_1 * kernel_2 * kernel_3
    nominator_temp <- kernel_real * data2[i,11]
    denominator_temp <- kernel_real
    nominator <- c(nominator,nominator_temp)
    denominator <- c(denominator,denominator_temp)

    kernel_real_2 <- kernel_1 +  kernel_2 + kernel_3
    nominator_temp_2 <- kernel_real_2 * data2[i,11]
    denominator_temp_2 <- kernel_real_2
    nominator_2 <- c(nominator_2,nominator_temp_2)
    denominator_2 <- c(denominator_2,denominator_temp_2)
  }
  y_hat <- sum(nominator) /sum(denominator)
  y_hat_2 <- sum(nominator_2) /sum(denominator_2)
  return(c(y_hat,y_hat_2))

}


day_times <- c("04:00:00","06:00:00","08:00:00","10:00:00","12:00:00","14:00:00"
               ,"16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")

temperature_one <- NULL
temperature_two <- NULL

for(i in day_times){
  data_interest <- c(" 58.4274", "14.826", "2013-11-04",i)
  # the training data consist of two parts, one part is data created before
  # the date we want to predict for. Another part is that data we collected
  # during that day but not at the same time as that
  # we want to predict for.

  data_needed_part1 <- st[which(st$date < "2013-11-04"),]
  data_needed_part2 <- st[which(st$date == "2013-11-04" & distance_3(st$time,i) < 0),]
  data_needed <- rbind(data_needed_part1, data_needed_part2)

  temperature_combination <- predicted_temp(data_interest,data_needed)

  temperature_one <-c(temperature_one,temperature_combination[2])
  temperature_two <-c(temperature_two,temperature_combination[1])
}


data_plot1 <- data.frame(day_times, temperature_one)
data_plot2 <- data.frame(day_times, temperature_two)

plot1 <- ggplot(data_plot1, aes(x = day_times, y = temperature_one)) + geom_point() +
  xlab("time") + ylab("temperature") +labs(title = "sum of kernels")
plot2 <- ggplot(data_plot2,aes(x = day_times, y = temperature_two)) + geom_point() +
  xlab("time") + ylab("temperature")+labs(title = "the mutiplication of kernels")

plot1
plot2


