library(data.table)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggpubr)

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

  cases_t <- fromwidetolong(paste0(baseURL, "time_series_19-covid-Confirmed.csv"), "cases")
  death_t <- fromwidetolong(paste0(baseURL, "time_series_19-covid-Deaths.csv"), "death")
  recovered_t <- fromwidetolong(paste0(baseURL, "time_series_19-covid-Recovered.csv"), coltoname = "recovered")

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

  return(data)
}

####

data <- combinetreetable()
data_agg <- data[, .(val = sum(val, na.rm = T)), by = .(type, date)]

ggplot(data_agg, aes(date, val)) +
  geom_point() +
  geom_line() +
  facet_grid(type ~ ., scales = "free_y") +
  theme_bw()


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
