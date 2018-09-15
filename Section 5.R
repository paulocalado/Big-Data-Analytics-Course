head(mtcars)

mt.lm<- lm(mpg ~ disp,data=mtcars)
summary(mt.lm)
broom::glance(mt.lm)

mt.lm<- lm(mpg ~ disp + wt, data=mtcars)
broom::glance(mt.lm)

mt.lm<- lm(mpg ~ disp + wt + hp,data = mtcars)
broom::glance(mt.lm)

mt.lm<- lm(mpg ~ disp + wt + hp + cyl, data = mtcars)
broom::glance(mt.lm)

mt.lm<- lm(mpg ~ disp + wt + hp + cyl + gear, data = mtcars)
broom::glance(mt.lm)

broom::glance(lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data=mtcars))

#forward selection
lm.null<- lm(mpg ~ 1,data = mtcars)
AIC(lm.null)
BIC(lm.null)

step(lm.null, scope = list(upper=lm(mpg ~ .,data = mtcars)),direction = "forward")

#backward elimination
lm.full<- lm(mpg ~ .,data = mtcars)
summary(lm.full)
AIC(lm.full)
BIC(lm.full)

step(lm.full, scope = list(lower=lm.null), direction = "backward")

#bidirection elimination
lm.mtcars <- lm(mpg ~ disp + cyl + qsec, data=mtcars)
summary(lm.mtcars)

step(lm.mtcars,scope = list(upper=lm.full,lower=lm.null), direction = "both")


#cross validation
data(mpg)
mpg

library(modelr)

cv<-crossv_kfold(mpg,k=5)
cv

models1  <- map(cv$train, ~lm(hwy ~ displ, data = .))
models2  <- map(cv$train, ~lm(hwy ~ displ + drv, data = .))

get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}
pred1  <- map2_df(models1, cv$test, get_pred, .id = "Run")
pred2  <- map2_df(models2, cv$test, get_pred, .id = "Run")

MSE1  <- pred1 %>% group_by(Run) %>% 
  summarise(MSE = mean( (hwy - pred)^2))
MSE1

MSE2  <- pred2 %>% group_by(Run) %>% 
  summarise(MSE = mean( (hwy - pred)^2))
MSE2

mean(MSE1$MSE)
mean(MSE2$MSE)

#Activity
set.seed(64)

heights$income <- heights$income/1000
heights <- heights[-c(721,973,1611,1641,1866,2828,3122,6178,6319,6794),]

incomeCV<- crossv_kfold(heights,k=6)

modelh1<- map(incomeCV$train, ~lm(income ~ height, data=.))
modelh2<- map(incomeCV$train, ~lm(income ~ sex, data=.))
modelh3<- map(incomeCV$train, ~lm(income ~ education, data=.))

get_pred <- function(model, test_data) {
  data <- as.data.frame(test_data)
  pred <- add_predictions(data, model)
  return(pred)
}

predh1<- map2_df(modelh1, incomeCV$test, get_pred, .id="Run")
predh2<- map2_df(modelh2, incomeCV$test, get_pred, .id="Run")
predh3<- map2_df(modelh3, incomeCV$test, get_pred, .id="Run")

MSEh1<- predh1 %>% group_by(Run) %>%
  summarise(MSE = mean( (income - pred ) ^2 ))

MSEh2<- predh2 %>% group_by(Run) %>%
  summarise(MSE = mean((income - pred)^2))

MSEh3<- predh3 %>% group_by(Run) %>%
  summarise(MSE = mean((income - pred)^2))

mean(MSEh1$MSE)
mean(MSEh2$MSE)
mean(MSEh3$MSE)



