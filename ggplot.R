library(tidyverse)
theme_set(theme_bw())

#df_mpg = read_csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/mpg.csv')
df_mpg = read_csv('mpg.csv')

#df_tips <- read_csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv')
df_tips <- read_csv('tips.csv')


# scatter -----------------------------------------------------------------

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



# bar ---------------------------------------------------------------------

df_tips %>% 
  mutate(day = fct_relevel(day, c('Thur', 'Fri', 'Sat', 'Sun')),
         sex = fct_relevel(sex, c('Male', 'Female')),
         smoker = fct_relevel(smoker, c('Yes', 'No'))) %>%
  group_by(day, sex, smoker) %>% 
  summarise(total_bill=mean(total_bill)) %>% 
  ggplot(aes(x=day, y=total_bill, fill=sex)) +
  geom_col(position="dodge") +
  facet_grid(.~smoker)
