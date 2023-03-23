library(dplyr)
library(lubridate)

file <- "sample.csv"
year_init <- 2011
year_final <- 2020
max_order <- 100

flows <- read.csv(file, header = TRUE, sep = ",")

flows <- flows %>%
  mutate(Year = year(as.Date(Fecha, "%d/%m/%Y")),
         Media = as.numeric(sub(",", ".", flows$Media))) %>%
  select(Year, Media)

min_value <- function(year, order, year_days, flows) {
  means_order <- NULL
  last_day <- year_days - order + 1
  for (Day in 1:last_day) {
    end_day <- Day + order - 1
    means_order[Day] <- mean(flows[flows$Year ==
                                    year, ]$Media[Day:end_day])
  }
  min_value <- min(means_order)
  return(min_value)
}

Qb_year <- function(min_values) {
  ratio <- NULL
  for (order in 1:(max_order - 1)) {
    if (min_values[order] != 0) {
      ratio[order] <- (min_values[order + 1] -
                        min_values[order]) / min_values[order]
    }
  }
  Qb_year <- min_values[which((ratio == max(ratio[!is.na(ratio)]))) +
                         1]
  return(Qb_year)
}

Qb <- function(flows, year_init, year_final) {
  Qb_years <- NULL
  for (year in year_init:year_final) {
    min_values <- NULL
    year_days <- sum(flows$Year == year)
    for (order in 1:max_order) {
      min_values[order] <- min_value(year, order,
                                    year_days, flows)
    }
    Qb_years[year - (year_init) + 1] <- Qb_year(min_values)
  }
  Qb <- mean(Qb_years)
  return(Qb)
}

Qb_value <- Qb(flows, year_init, year_final)
print(paste("Base flow is ", Qb_value, "m3/s"))
