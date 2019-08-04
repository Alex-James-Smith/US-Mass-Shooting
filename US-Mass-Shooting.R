library(readr)
library(tidyverse)
library(magrittr)
library(lubridate)

## Import data
Mass_Shooting13 <- read_csv("MST Data 2013 - 2015.csv", 
                              +     col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                                     +         killed = col_integer(), wounded = col_integer()))
Mass_Shooting14 <- read_csv("MST Data 2014 - 2015.csv", 
                              +     col_types = cols(date = col_date(format = "%m/%d/%Y")))
Mass_Shooting15 <- read_csv("MST Data 2015 - 2015.csv", 
                            +     col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                                   +         killed = col_integer(), wounded = col_integer()))
Mass_Shooting19 <- read_csv("data.csv", col_types = cols(X10 = col_skip(), 
                                                         +     X11 = col_skip(), X12 = col_skip(), X13 = col_skip(), 
                                                         +     X14 = col_skip(), X15 = col_skip(), X16 = col_skip(), 
                                                         +     X17 = col_skip(), X18 = col_skip(), X19 = col_skip(), 
                                                         +     X20 = col_skip(), X21 = col_skip(), X22 = col_skip(), 
                                                         +     X23 = col_skip(), X8 = col_skip(), X9 = col_skip(), 
                                                         +     date = col_date(format = "%m/%d/%y"), 
                                                         +     killed = col_integer(), wounded = col_integer()))

## Bind DFs to single DF
Mass_Shooting13_19 <- bind_rows(list(Mass_Shooting13, 
               Mass_Shooting14, 
               Mass_Shooting15, 
               Mass_Shooting16, 
               Mass_Shooting17, 
               Mass_Shooting18, 
               Mass_Shooting19))

## Remove extra DFs
rm(Mass_Shooting13, Mass_Shooting14, Mass_Shooting15,Mass_Shooting16, Mass_Shooting17, Mass_Shooting18, Mass_Shooting19)

## Export full DF
write_csv(Mass_Shooting13_19, "Mass_Shooting13_19.csv")

## Today's date 2019-08-04

## Shootings per day by year
Mass_Shooting13_19 %>% mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(shootings = n())  %>%
  mutate(days_in_year = case_when(year == 2019 ~ 215,
                                  leap_year(year) == TRUE ~ 366,
                                  year != 2019 |leap_year(year) == FALSE ~ 365)) %>%
  group_by(year) %>%
  summarise(shootings_per_day = shootings/days_in_year) %>% ggplot() +
  geom_col(aes(year, shootings_per_day)) +
  geom_text(aes(year, shootings_per_day, 
                label = round(shootings_per_day, digits = 3)),
            colour = "white", 
            nudge_y = -0.02)

## fct_count(Mass_Shooting13_19$state)

## Shootings in each year and month
Mass_Shooting13_19  %>% 
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>% 
           ggplot(aes(month)) +
  geom_bar() +
  facet_grid(rows = vars(year(date)))

## Create data frame for shootings per day in each month and year
monthly <- Mass_Shooting13_19 %>% mutate(year = year(date),
                              month = month(date)) %>%
  select(year, month) %>% group_by(year, month) %>%
  summarise(shootings = n()) %>%
  mutate(days_in_month = case_when(
              year == 2019 & month == 8 ~ 4L,
              year == 2019 & month < 8 ~ days_in_month(month),
              year != 2019 ~ days_in_month(month)),
         shootings_per_day = shootings/days_in_month)
monthly <- as.data.frame(monthly)

## Chart for shootinngs per day in each month and year
monthly %>% ggplot(aes(x = month(month, label = TRUE, abbr= TRUE), y = shootings_per_day, fill = shootings_per_day)) + 
  geom_col() +
  geom_text(aes(label = round(shootings_per_day, digits = 3)), colour = "white", nudge_y = -0.16) +
  labs(x = NULL) +
  facet_grid(rows = vars(year)) + 
  scale_fill_viridis_c() +
  theme(panel.grid.major.x = element_blank())
