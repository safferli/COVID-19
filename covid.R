library(tidyverse)


# data from Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)
# https://github.com/CSSEGISandData/COVID-19
base.url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
ll <- c("confirmed", "recovered", "deaths")

## get data

map(ll, function(nn){
    assign(nn, read_csv(paste0(base.url, "time_series_covid19_", nn, "_global.csv")), envir = .GlobalEnv)
  })

# country ISO lookup table
country.lookup <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")

## start analysis

f.pick.and.munch <- function(data, countries){
  data %>% 
  rename(
    province = `Province/State`, 
    country = `Country/Region`
  ) 
}


dta <- confirmed %>% 
  rename(
    province = `Province/State`, 
    country = `Country/Region`
  ) %>% 
  select(-Lat, -Long) %>% 
  group_by(country, province) %>% 
  filter(country %in% c("Germany", "Italy", "France")) %>% 
  pivot_longer(
    cols = matches("\\d", perl = TRUE),
    names_to = "day",
    values_to = "cases"
  ) %>% 
  mutate(
    # what the *bleep* happened on the 9th of November, anyway? 
    day = as.Date(day, "%m/%d/%y"),
    delta = cases-lag(cases)
  )


dta %>% 
  filter(country == "Germany") %>% 
  ggplot()+
    geom_line(aes(x=day, y=cases, colour = country))
  

