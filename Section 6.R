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
