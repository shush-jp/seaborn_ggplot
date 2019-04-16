library(tidyverse)
theme_set(theme_bw())

df_mpg = read_csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/mpg.csv')

df_mpg %>% 
  ggplot(aes(x=weight, y=mpg, color=origin, shape=origin)) +
  geom_point()

df_mpg %>% 
  ggplot(aes(x=weight, y=mpg, size=cylinders)) +
  geom_point()

df_mpg %>% 
  ggplot(aes(x=weight, y=mpg)) +
  geom_point() +
  facet_grid(.~origin)

df_mpg %>% 
  ggplot(aes(x=weight, y=mpg, color=origin)) +
  geom_point() +
  geom_smooth(method=lm)
