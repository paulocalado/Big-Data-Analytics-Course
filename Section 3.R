library(ggplot2)
library(tidyverse)
install.packages("gapminder")
library(gapminder)
library(dplyr)

ggplot(gapminder, aes(year, lifeExp, group = country)) +
  geom_line(alpha = 0.25)

aus<- gapminder%>%
    filter(country=="Australia")
ggplot(aus,aes(year, lifeExp)) + geom_line()

aus.lm<- lm(lifeExp ~ year,data = aus)
summary(aus.lm)

data("gapminder")
gapminder %>%
  filter(country=="Australia")%>%
  nrow()

gapminder.nested<- gapminder%>%
                    group_by(country,continent)%>%
                    nest()
gapminder.nested

gapminder.nested$data[gapminder.nested$country=="Brazil"]

country_model<- function(df){
  lm(lifeExp ~ year, data=df)
}

gapminder_model<- gapminder.nested%>%
  mutate(model = map(data,country_model),
         coef = map(model, broom::tidy))
#create a new column called model, containing the output of the country_model function applied (mapped) to the column data, and
#create another new column called coef which contains a tidied version of model with just the coefficients.

gapminder_model$model[gapminder_model$country=="Brazil"]
gapminder_model$coef[gapminder_model$country=="Brazil"]

gapminder.coef<-gapminder_model %>% unnest(coef)

install.packages("forcats")
library(forcats)

gapminder.coef %>% 
  mutate(term = fct_recode(term, 
                           Intercept = "(Intercept)", 
                           Slope = "year")) %>% 
  ggplot(aes(estimate, fill = term)) + 
  geom_density(show.legend = FALSE, alpha = 0.5) + 
  geom_histogram(col = "black", fill = "lightgrey",
                 alpha = 0.5,
                 aes(y = ..density..))+
  facet_wrap(~term, scales = "free") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() + 
  labs(y = NULL, x = "Estimate")

#Activity
nested.gapminder<- gapminder%>%
  group_by(country,continent)%>%
  nest()

gdpPercap.model<-function(df){
  lm(gdpPercap ~ year, data=df)
}

model.gapminder<- nested.gapminder%>%
  mutate(model = map(data,gdpPercap.model),
         coef = map(model,broom::tidy))

glance_df <- model.gapminder %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE)

#question 1
glance_df<- glance_df %>% arrange(r.squared)

#question 2
countries<- c("Afghanistan","Gambia","Serbia","Venezuela","New Zeland")
countries <- glance_df%>%
  filter(r.squared<0.25 & match(country,countries))

#question 3
continents<- glance_df%>%
  filter(r.squared<0.25)
continents$continent

#question 4
glance_df%>%
  filter(country == "Australia")%>%
  summarise(value = r.squared)

#question 5-6
glance_df %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(aes(color=continent))
