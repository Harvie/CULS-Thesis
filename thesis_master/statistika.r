#!/bin/env Rscript

data = read.table("713d498cc2f06d5e83d03c443691fb18.csv", header=T)
mean(abs(data$Diff));
hist(data$Diff);


angle_diff <- function(a, b){
        diff <- a - b;
        while (diff < -180) { diff = diff + 360; }
        while (diff > 180) { diff = diff - 360; }
        diff 
}
data$new <- mapply(angle_diff, data$Clicked, data$Audio)
data$new <- mapply(angle_diff, -data$Clicked, data$Audio) #pro otoceny sluchatka

setwd("rdata");
files = list.files(pattern="*.csv")
for (i in 1:length(files)) data = rbind(data, read.csv(files[i], header=T)$Diff)

