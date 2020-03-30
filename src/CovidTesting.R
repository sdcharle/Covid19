# covid stuff
# https://talkingpointsmemo.com/edblog/key-source-of-covid-19-testing-infection-data

"

Can grab census data directly

https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv

https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx

"

library(tidyverse)
library(lubridate)
library(readxl)

x <- read_csv("http://covidtracking.com/api/states/daily.csv") %>% 
  filter(state %in% c('IN', 'OH', 'NC', 'PA', 'NY', 'WA'))

x$goodday <- ymd(x$date)

# population data
download.file("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx", 
              "local_data/nst-est2019-01respopstates.xlsx")

census_data <- read_excel("local_data/nst-est2019-01respopstates.xlsx", 
                          skip = 3)
names(census_data)[1] <- 'Region'

census_data$Region <-
  stringr::str_remove_all(census_data$Region, regex('\\.'))
state_lookup <- tibble(name = state.name, abbr = state.abb)
census_data <- census_data %>% left_join(state_lookup, by = c("Region" = "name"))
# doesn't have D.C. by the way

x <- x %>% 
  left_join((census_data %>% select(abbr, `2019`)), by = c("state" = "abbr")) %>% 
  rename(population2019 = `2019`)

df <- x %>% 
  select(goodday, negative, death, state, totalTestResults, total, positive, hospitalized,
         population2019) %>% 
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

ggplot(df, aes(x = Date, y = Count/population2019, color = Variable))  +  
  geom_line() + 
  ggtitle("Testing Volumes and Results per capita (source: covidtracking.com)") + 
  facet_wrap(~state) +
  scale_y_log10()
