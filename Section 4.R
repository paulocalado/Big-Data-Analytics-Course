install.packages("glmx")
library(glmx)
library(ggplot2)
data("BeetleMortality")
BeetleMortality

BeetleMortality$propDied<- BeetleMortality$died/BeetleMortality$n
ggplot(BeetleMortality,aes(dose,propDied)) + geom_point()

install.packages("titanic")
library(titanic)

titanic.glm<- glm(Survived ~ Age + Sex, family = binomial(),titanic_train)
summary(titanic.glm)

titanic.glm<- glm(Survived ~ factor(Pclass) + Sex, family = binomial(),titanic_train)
summary(titanic.glm)


library(modelr)
summary(heights$sex)

library(MASS)
data("iris")
iris

ggplot(iris,aes(Sepal.Length,Sepal.Width, col= Species))+geom_point()

iris.lda<- lda(Species ~ Sepal.Length + Sepal.Width,data=iris)
iris.lda

sepal.grid  <- expand.grid(Sepal.Length = seq(4,8,length = 100), 
                           Sepal.Width = seq(2,4.5,length = 100))

sepal.grid$Species  <- predict(iris.lda,sepal.grid)$class
sepal.grid

ggplot(sepal.grid, aes(Sepal.Length, Sepal.Width, fill = Species)) + 
  geom_tile(alpha = 0.2) + 
  geom_point(data = iris, aes(col = Species, fill = NA))

#Activity
library(dplyr)
cancer <- tibble(case = factor(c(rep(1,3),rep(0,3)),
                               levels = c(0,1),
                               labels = c("control","case")),
                 x1 = c(1,2,3,4,2,3),
                 x2 = c(4,8,6,0,2,4))
cancer

cancer %>% ggplot(aes(x1,x2)) + geom_point(aes(color=case))

#question 1
u1<- cancer%>%
  filter(case == "case")%>%
  summarise(x1 = mean(x1),
            x2 = mean(x2))
u1<- as.matrix(as.numeric(u1))

#question 2
X <- matrix(c(-1,0,1,1,-1,0,-2,2,0,-2,0,2), nrow=6, byrow=FALSE)
X

#question 3
E<- (t(X)%*%X)/4
solve(E)

#question 4
A1<- solve(E) %*% u1
B1<- t(u1)%*%solve(E)%*%u1
u2<- cancer%>%
  filter(case == "control")%>%
  summarise(x1 = mean(x1),
            x2 = mean(x2))
u2<- as.matrix(as.numeric(u2))
A2<- solve(E)%*% u2
B2<- t(u2)%*%solve(E)%*%u2

#question 5
xNew<- matrix(c(1,4),nrow = 1)

xNew%*%(A1-A2)+(1/2)*(B1-B2)