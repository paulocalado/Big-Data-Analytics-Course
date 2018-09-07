install.packages("mlbench")
library(mlbench)
library(ggplot2)

data("BostonHousing2")

ggplot(BostonHousing2,aes(x=rm,y=medv))+
  geom_point()+
  labs(x="number of rooms",y="median price",title="Price x Rooms")

small.xy <- data.frame(X=c(8,5,4,5,6,8,9,7,5,7),Y=c(2,3,4,2,5,6,4,4,6,5))
small.xy

estimatedB1<- cov(small.xy$X,small.xy$Y)/var(small.xy$X)
estimatedB0<- mean(small.xy$Y) - (estimatedB1*mean(small.xy$X))

install.packages("tidyverse")
library(tidyverse)
data("diamonds")
diamonds

diamonds.lm  <- lm(log(price) ~ log(carat), data = diamonds)

#finding values for B0 and B1 
summary(diamonds.lm)

#predicting value in dollars of diamnond with 0.5 carat
exp(8.448661 + 1.675817 * 0.5)

#checking linearity
plot(diamonds.lm,which = 1)

#checking constant spread
plot(diamonds.lm, which = 3)

#checking normality
plot(diamonds.lm, which = 2)

#quiz 2
-4.128 + (-1.420*0.694)