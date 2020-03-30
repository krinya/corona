library(data.table)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(plotly)
library(stringi)

fromwidetolong <- function(link, coltoname) {
  data_wide <- fread(link)
  long_number <- match("Long", names(data_wide))
  last <- length(colnames(data_wide))
  
  data <- gather(data_wide, date, number, (long_number + 1):last)
  
  setDT(data)
  setnames(data, old = "number", new = coltoname)
  
  data[, date := mdy(date)]
  
  return(data)
}


combinetreetable <- function() {
  
  baseURL <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
  
  cases_t <- fromwidetolong(paste0(baseURL, "time_series_covid19_confirmed_global.csv"), "cases")
  death_t <- fromwidetolong(paste0(baseURL, "time_series_covid19_deaths_global.csv"), "death")
  recovered_t <- fromwidetolong(paste0(baseURL, "time_series_covid19_recovered_global.csv"), coltoname = "recovered")
  
  population <- fread("D:/corona/countries of the world.csv")
  
  population[Country == "United States ", Country:= "US "]
  
  population <- population[, .(Country, Population)]
  
  population[, Country:= stri_sub(Country, from = 1, to = -2) ]
  
  data <- merge(cases_t, death_t[, .(`Province/State`, `Country/Region`, date, death)],
                by = c("Province/State", "Country/Region", "date"),
                all.x = T
  )
  
  data <- merge(data, recovered_t[, .(`Province/State`, `Country/Region`, date, recovered)],
                by = c("Province/State", "Country/Region", "date"),
                all.x = T
  )
  
 
  long_number <- match("Long", names(data))
  last <- length(colnames(data))
  
  data <- gather(data, type, val, (long_number + 1):last)
  
  setDT(data)
  
  setnames(data, old = "Province/State", new = "Province_State")
  setnames(data, old = "Country/Region", new = "Country_Region")
  
  data <- merge(data, population,
                by.x = c("Country_Region"),
                by.y = c("Country"),
                all.x = T
  )
  
  return(data)
}

####

data <- combinetreetable()
data_agg <- data[, .(val = sum(val, na.rm = T)), by = .(type, date)]

####

ggplot(data_agg, aes(date, val)) +
  geom_point() +
  geom_line() +
  facet_grid(type ~ ., scales = "free_y") +
  theme_bw()

####
data_agg <- data_agg[order(type, date)]
data_agg[,new_cases:= val - shift(val), by = type]
data_cases <- data_agg[type == "cases"]

ggplot(data_cases, aes(x = date, y = new_cases)) +
  geom_bar(stat = "identity") +
  theme_bw()

####
data_recent <- data_recent[order(-date), head(.SD, 1),by= .(Country_Region, Province_State)]

####
selectedCountries <- c("Iran",
                       "Italy",
                       "Spain",
                       "Hungary",
                       "Korea, South",
                       "Germany",
                       "France", 
                       "Switzerland", 
                       "United Kingdom",
                       "Netherlands",
                       "US")

dataCountries <- data[Country_Region %in% selectedCountries & Province_State == ""]

if(FALSE){
  dataToAdd <- data[Province_State %in% c("Hubei")]
  dataCountries <- rbindlist(list(dataCountries, dataToAdd))
}


dataCountries <- dataCountries[type == "death"]
dataCountries[,new_cases:= val - shift(val), by = type]
dataCountries <- dataCountries[val >= 50]
dataCountries[,log_val:= log(val)]
dataCountries[, location:= paste0(Country_Region, "-", Province_State)]
dataCountries[, daysfrom:= 1:.N, by = location]

dataCountries[, pop_val:= val/Population]




ggplot(dataCountries[daysfrom < 40], aes(x = daysfrom, y = val, color = location)) +
  geom_point() +
  geom_line() +
  theme_bw() -> plot1

plot1

ggplotly(plot1)

ggplot(dataCountries[daysfrom < 40], aes(x = daysfrom, y = pop_val, color = location)) +
  geom_point() +
  geom_line() +
  theme_bw() -> plot1Pop

plot1Pop

ggplotly(plot1Pop)




ggplot(dataCountries[daysfrom < 40], aes(x = daysfrom, y = log_val, color = location)) +
  geom_point() +
  geom_line() +
  theme_bw() -> plot2

plot2

ggplotly(plot2)


ggplot(dataCountries[daysfrom < 30], aes(x = daysfrom, y = new_cases, color = location)) +
  #geom_point() +
  geom_smooth(se = F) +
  #geom_line() +
  theme_bw() -> plot3

ggplotly(plot3)




cases_death_recovered <- function(count, setScales = "fixed") {
  data_agg <- data[, .(val = sum(val, na.rm = T)), by = .(type, date, Country_Region)]
  
  countries <- count
  
  ggplot(data_agg[Country_Region %in% countries & type == "cases"], aes(date, val)) +
    geom_point() +
    geom_line() +
    facet_grid(Country_Region ~ ., scales = setScales) +
    labs(title = "Cases") +
    theme_bw() -> plot1
  
  
  
  ggplot(data_agg[Country_Region %in% countries & type == "death"], aes(date, val)) +
    geom_point() +
    geom_line() +
    facet_grid(Country_Region ~ ., scales = setScales) +
    labs(title = "Death") +
    theme_bw() -> plot2
  
  
  
  ggplot(data_agg[Country_Region %in% countries & type == "recovered"], aes(date, val)) +
    geom_point() +
    geom_line() +
    facet_grid(Country_Region ~ ., scales = setScales) +
    labs(title = "Recovered") +
    theme_bw() -> plot3
  
  figure <- ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)
  return(figure)
}

countries <- c("Mainland China", "Iran", "France", "Japan", "Germany", "Italy", "Hungary", "Netherlands", "India")

cases_death_recovered(count = countries)

cases_death_recovered(count = countries, setScales = "free_y")
