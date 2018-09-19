install.packages("sparklyr")
library(sparklyr)

spark_install(version = "2.1.0")
sc <- spark_connect(master = "local")

install.packages("nycflights13")
library(nycflights13)
library(dplyr)
library(ggplot2)

flights_tbl<- copy_to(sc,nycflights13::flights,"flights")

src_tbls(sc)

flights_tbl %>% filter(year==2013)
flights_tbl %>% select(year, origin)

flights_tbl %>% 
  group_by(origin)%>%
  summarise(mean_delay= mean(dep_delay))
  
delay <- flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>% 
  collect

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

flights_tbl

flights_tbl%>%
  ft_binarizer("distance","long_flight",threshold =1500)%>%
  select(distance,long_flight)%>%
  collect()%>%
  mutate(long_flight2 = ifelse(long_flight== 0,"short","long"))

flights_tbl%>%
  ft_bucketizer("distance","distance_cat",
                splits = c(0,500,1500,Inf))%>%
  select(distance, distance_cat)

###PCA
iris_tbl<- copy_to(sc, iris, "iris",overwrite = TRUE)

pca_model<- tbl(sc,"iris") %>%
  select(-Species)%>%
  ml_pca()

print(pca_model)

D <- as.matrix(iris[1:4])
E <- as.matrix(pca_model$pc)
P <-  D %*% E

PCs <- as.data.frame(P)
PCs$Species <- iris$Species

ggplot(PCs,aes(PC1,PC2)) + geom_point(aes(colour = iris$Species))

###K-MEANS
kmeans_model <- iris_tbl %>% 
  ml_kmeans(formula= ~ Petal_Width + Petal_Length, centers = 3)

print(kmeans_model)

sdf_predict(kmeans_model) %>%
  collect() %>%
  ggplot(aes(Petal_Length, Petal_Width)) +
  geom_point(aes(Petal_Width, Petal_Length, col = factor(prediction+1)),
             size = 2, alpha = 0.5) + 
  geom_point(data = kmeans_model$centers, aes(Petal_Width, Petal_Length),
             pch = 'x', size = 12) +
  scale_color_discrete(name = "Predicted cluster")

##ACTIVITY
install.packages("rattle.data")
wine <- rattle.data::wine

wine[-1] <- scale(wine[-1])

wine_tbl<- copy_to(sc, wine, "wine",overwrite = TRUE)
wine_pca_model<- tbl(sc,"wine")%>%
  select(-Type)%>%
  ml_pca()

print(wine_pca_model)

kmeans_model_wine<- wine_tbl%>%
  ml_kmeans(formula= ~Nonflavanoids + Hue, centers = 3)

print(kmeans_model_wine)