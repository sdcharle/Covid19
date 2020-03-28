# covid stuff
# https://talkingpointsmemo.com/edblog/key-source-of-covid-19-testing-infection-data

library(tidyverse)
library(lubridate)
library(readxl)

x <- read_csv("http://covidtracking.com/api/states/daily.csv") %>% 
  filter(state %in% c('IN', 'OH', 'NC', 'PA', 'NY', 'WA'))

x$goodday <- ymd(x$date)

df <- x %>% 
  select(goodday, negative, death, state, totalTestResults, total, positive, hospitalized) %>% 
  rename(Date = goodday) %>% 
  pivot_longer(cols = c('negative',
                                   'positive',
                                   'hospitalized',
                                   'death',
                                   'totalTestResults'), 
               values_to = 'Count',
               names_to = 'Variable')

ggplot(df, aes(x = Date, y = Count, color = Variable))  +  
  geom_line() + 
  ggtitle("Testing Volumes and Results (source: covidtracking.com)") + 
  facet_wrap(~state) +
  scale_y_log10()

# population data

census_data <- read_excel("local_data/nst-est2019-01respopstates.xlsx", 
                                         skip = 3)
names(census_data)[1] <- 'Region'

census_data$Region <-
  stringr::str_remove_all(census_data$Region, regex('\\.'))
state_lookup <- tibble(name = state.name, abbr = state.abb)
census_data <- census_data %>% left_join(state_lookup, by = c("Region" = "name"))
# doesn't have D.C. by the way


