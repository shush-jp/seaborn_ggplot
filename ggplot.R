library(tidyverse)
theme_set(theme_bw())

df_mpg <-  read_csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/mpg.csv')
#df_mpg <-  read_csv('mpg.csv')


df_tips <- read_csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv')
#df_tips <- read_csv('tips.csv') 

df_fmr <- read_csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/fmri.csv')
##df_tips <- read_csv('fmri.csv') 

df_flight <- read_csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/flights.csv')


df_tips <- df_tips%>% 
  mutate(day = fct_relevel(day, c('Thur', 'Fri', 'Sat', 'Sun')),
         sex = fct_relevel(sex, c('Male', 'Female')),
         smoker = fct_relevel(smoker, c('Yes', 'No')),
         time = fct_relevel(time, c('Lunch', 'Dinner'))) 

df_fmr <- df_fmr %>% 
  mutate(region = fct_relevel(region, c('parietal', 'frontal')))

df_flight <- df_flight %>% 
  mutate(month = fct_inorder(month))
         
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

# line
df_fmr %>% 
  ggplot(aes(x=timepoint, y=signal, color=event, linetype=event)) +
  geom_line(stat='summary') +
  facet_grid(.~region)

# bar ---------------------------------------------------------------------

df_tips %>% 
  ggplot(aes(x=day, y=total_bill, fill=sex)) +
  geom_bar(position="dodge", stat='summary') +
  facet_grid(.~smoker)

df_tips %>% 
  group_by(day, sex, smoker) %>% 
  summarise(total_bill=mean(total_bill)) %>% 
  ggplot(aes(x=day, y=total_bill, fill=sex)) +
  geom_col(position="dodge") +
  facet_grid(.~smoker)

# box ---------------------------------------------------------------------

df_tips %>% 
  ggplot(aes(x=day, y=total_bill, fill=sex)) +
  geom_boxplot() +
  facet_grid(.~smoker)

# violin ------------------------------------------------------------------

df_tips %>% 
  ggplot(aes(x=day, y=total_bill, fill=sex)) +
  geom_violin(trim=FALSE) +
  facet_grid(.~smoker)

# histogram

df_tips %>% 
  ggplot(aes(x=total_bill, fill=sex)) +
  geom_histogram(alpha=0.5, position='identity') +
  facet_grid(.~time)


df_tips %>% 
  ggplot(aes(x=total_bill, color=sex)) +
  geom_density() +
  facet_grid(.~time)

df_tips %>% 
  ggplot(aes(x=total_bill)) +
  geom_histogram(aes(y=..density.., fill=sex), alpha=0.5, position='identity') +
  geom_density(aes(color=sex))+
  facet_grid(.~time)


# bar ---------------------------------------------------------------------

df_tips %>% 
  ggplot(aes(x=smoker, fill=sex)) +
  geom_bar(position='dodge') +
  facet_grid(.~time)


# heatmap
df_flight %>% 
  ggplot(aes(x=year, y=month, fill=passengers)) +
  geom_raster()

df_flight %>% 
  ggplot(aes(x=year, y=month, fill=passengers)) +
  geom_tile()

df_flight %>% 
  ggplot(aes(x=year, y=month, z=passengers)) +
  geom_contour()

# bivariate kernel density 
df_tips %>% 
  ggplot(aes(x=total_bill, y=tip)) +
  geom_density2d() +
  facet_grid(.~time)
