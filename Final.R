library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(quantreg)
dailyActivity <- read_csv("C:/Users/mg/Documents/DataA/Final/FitabaseData/dailyActivity_merged.csv")
View(dailyActivity)
colnames(dailyActivity)
str(dailyActivity)
skim_without_charts(dailyActivity)
glimpse(dailyActivity)
head(dailyActivity)
dailyActivity %>%
  select(Id, ActivityDate, TotalSteps)
dailyActivity <- dailyActivity %>%
  rename(date = activity_date)
dailyActivity <- clean_names(dailyActivity)
# ctrl + shift + M
dailyActivity %>%
  arrange(id)
intensity_total <- 
  dailyActivity %>% 
    group_by(id) %>% 
    drop_na() %>% 
    summarise(total_very_active_distance = sum(very_active_distance), 
              total_moderately_active_distance = sum(moderately_active_distance),
              total_light_active_distance = sum(light_active_distance),
              total_calories = sum(calories)) %>% 
  arrange(-total_calories)
View(intensity_total)
print(intensity_total)
ggplot(data = dailyActivity_mean_data) +
  geom_point(mapping = aes(x=mean_total_steps, y=mean_calories))
ggplot(data = dailyActivity_mean_data) +
  geom_point(mapping = aes(x=mean_total_distance, y=mean_calories))
ggplot(data = dailyActivity_total) +
  geom_point(mapping = aes(x=sum_total_steps, y=sum_calories))
ggplot(data = dailyActivity_total) +
  geom_point(mapping = aes(x=sum_total_distance, y=sum_calories))


ggplot(dailyActivity_total, aes(x=sum_total_distance, y=sum_calories)) +
  geom_smooth(method = "lm", se= FALSE) +
  geom_point() +
  stat_poly_eq(use_label(c("eq", "R2")))

ggplot(intensity_total, aes(x=total_light_active_distance, y=total_calories)) +
  geom_smooth(method = "lm", se= FALSE) +
  geom_point() +
  stat_poly_eq(use_label(c("eq", "R2")))

ggplot(intensity_minutes_total, aes(x=total_very_active_minutes, y=total_calories)) +
  geom_smooth(method = "lm", se= FALSE) +
  geom_point() +
  stat_poly_eq(use_label(c("eq", "R2")))

colnames(dailyActivity)
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = sum(total_distance), y = sum(calories)))
