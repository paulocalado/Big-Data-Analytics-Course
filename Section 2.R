diamonds.lm2  <- lm(log(price) ~ log(carat) + x, data = diamonds)
summary(diamonds.lm2)

exp(7.97082 + 1.53579*0.5 + 0.07373 * 4)

#quiz 1.1-1.2
mtcars.lm<- lm(mpg ~ wt, data = mtcars)
summary(mtcars.lm)

#quiz 1.3-1.5
mtcars.lm2<- lm(mpg ~ wt+disp, data = mtcars)
summary(mtcars.lm2)

#quiz 1.6
34.96055 + (-3.35082*2.700) + (-0.01773*198)

str(mtcars)

##########################

library(tidyverse)
AdX_team  <- tribble(
  ~Person, ~Pet, 
  "Lewis", "Cat",
  "Sash", "Cat",
  "David", "Dog",
  "Jono", "Rabbit",
  "Dee", "Rabbit",
  "Alex", "Dog"
)
AdX_team

AdX_team  <- AdX_team %>% 
              mutate(Pet = factor(Pet))
AdX_team

library(modelr)
model_matrix(AdX_team,~Pet)

data("chickwts")
chickwts  <- as_tibble(chickwts)
chickwts

ggplot(chickwts,aes(feed, weight)) + geom_boxplot()

chickwts.lm  <- lm(weight ~ feed, data = chickwts)

summary(chickwts.lm)

anova(chickwts.lm)

levels(factor(mpg$class))


library(mlbench)
data("BostonHousing2")
BostonHousing2 <- as_tibble(BostonHousing2)
BostonHousing2

BostonHousing2.lm  <- lm(medv ~ rm + chas, data = BostonHousing2)
summary(BostonHousing2.lm)

ggplot(BostonHousing2,mapping = aes(rm,medv)) + 
  geom_point(aes(color = chas)) + 
  geom_smooth(method = 'lm', mapping = aes(color=chas))

summary(lm(medv ~ rm + chas + rm*chas, data = BostonHousing2))

#Quiz
diamonds.lm3<- lm(formula = price ~ carat + factor(cut, ordered = FALSE), data = diamonds)
anova(diamonds.lm3)
levels(factor(diamonds$cut))

-3875.47 + (7871.08*1.261)+1120.33
-1839.07+(5924.50*0.433)+ (-540.83)+1883.26*0.433

