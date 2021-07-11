#!/bin/env Rscript

#write.table(data, "mama2.csv", sep="\t", quote = F, row.names = F)
data = read.table("713d498cc2f06d5e83d03c443691fb18.csv", header=T)
mean(abs(data$Diff));
hist(data$Diff);
boxplot(data$Diff)
plot(density(data$Diff))
sort(-table(abs(data$Diff)))[1] #mode

plot(ecdf(abs(data$Diff)))
ecdf(abs(data$Diff))(90)

quantile(abs(data$Diff), c(.50, .60, .75, .85, .90, .95, .97))

data2 <- data[order(data$Audio),]
plot(data2$Audio, abs(data2$Diff), type="l")
plot(aggregate(abs(data$Diff), by=list(data$Audio), FUN=mean), type="b")


#install.packages("plotrix")
install.packages("ggplot2")
library(ggplot2)


sd(abs(data$Diff))
shapiro.test(rnorm(100, mean = 17.775, sd = 17.05517))
wilcox.test(abs(data$Diff), conf.int = TRUE, conf.level = 0.95)

angle_diff <- function(a, b){
	diff = a - b;
	while (diff < -180) { diff = diff + 360; }
	while (diff > 180) { diff = diff - 360; }
	diff
}
data$new <- mapply(angle_diff, data$Clicked, data$Audio)
data$new <- mapply(angle_diff, -data$Clicked, data$Audio) #pro otoceny sluchatka



setwd("rdata");
files = list.files(pattern="*.csv")
data = NULL
for (i in 1:length(files)) data = rbind(data, read.table(files[i], header=T))

stat = NULL
for (i in 1:length(files)) { data = read.table(files[i], header=T); stat = rbind(stat, cbind(files[i], mean(abs(data$Diff)))); }

t.test(abs(data$Diff))
t.test(abs(data$Diff), conf.level=0.97)


#vykreslit
svg("out.svg")
hist(data$new, breaks=15)
dev.off()




#Directional
https://cran.r-project.org/web/packages/circular/circular.pdf
https://cran.r-project.org/web/packages/Directional/Directional.pdf

install.packages("circular")
install.packages("Directional")
circ.cor1(theta, phi, rads = FALSE)
spml.reg(y, x, rads = FALSE, seb = TRUE)


Directional::circ.cor1(data$Audio, data$Click, rads = FALSE)




#Polar plot
datapolar = aggregate(abs(data$Diff), by=list(data$Audio), FUN=mean)

library(plotrix)
polar.plot(datapolar$x,datapolar$Group.1,main="Polar Plot",radial.lim=c(0,45),start=90,clockwise=TRUE,lwd=3,line.col="blue",rp.type="polygon")
polar.plot(smooth.spline(datapolar$x)$y,datapolar$Group.1,main="Polar Plot",radial.lim=c(0,45),start=90,clockwise=TRUE,lwd=3,line.col="blue",rp.type="polygon")


#Vyhlad
plot(smooth.spline(datapolar$x)$y)
plot(smooth(datapolar$x))

#Moving average
http://www.markhneedham.com/blog/2014/09/13/r-calculating-rolling-or-moving-averages/
mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
mav(c(4,5,4,6), 3)

library(zoo)
rollmean(c(4,5,4,6), 3)


#Vyhlazeny polar plot
polar.plot(datapolar$x,datapolar$Group.1,radial.lim=c(0,45),start=90,clockwise=TRUE,lwd=3,line.col="blue",rp.type="s")
polar.plot(datapolar$x,datapolar$Group.1,main="Polar Plot",radial.lim=c(0,45),start=90,clockwise=TRUE,lwd=2,line.col="blue",rp.type="polygon", add=T)
polar.plot(smooth.spline(datapolar$x)$y,datapolar$Group.1,main="Polar Plot",radial.lim=c(0,45),start=90,clockwise=TRUE,lwd=2,line.col="red",rp.type="polygon",add=T)

