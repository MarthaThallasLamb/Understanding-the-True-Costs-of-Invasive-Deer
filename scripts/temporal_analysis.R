#load packages
library(readr)
library(invacost)
library(dplyr)

#read data
data <- read_csv("Cleaned Additional cervidae data points.csv")

#turn into dataframe
data_df <- as.data.frame(data)

#manually view the dataframe
View(data_df)

#expand data using invacost package
db.over.time <- expandYearlyCosts(
  data_df,
  startcolumn = "Probable_starting_year_adjusted",
  endcolumn = "Probable_ending_year_adjusted"
)

#USD 2017 to AUD 2025 conversion factor
cpi_2017 <- 245.121
cpi_2025 <- 321.962

aud_usd_2017 <- 0.7669  # 1 AUD = 0.7669 USD

conv_factor <- (cpi_2025 / cpi_2017) / aud_usd_2017

# Add converted costs to dataset
db.over.time <- db.over.time %>%
  mutate(
    AUD_2025 = Cost_estimate_per_year_2017_USD_exchange_rate * conv_factor
  )

#plot publication lag
db.over.time$Publication_lag <- db.over.time$Publication_year - db.over.time$Impact_year
#create quantiles
quantiles <- quantile(db.over.time$Publication_lag, probs = c(.25, .5, .75))
#print the quantile
quantiles

#Creating the vector of weights
year_weights <- rep(1, length(1990:2026))
names(year_weights) <- 1990:2026

#Assigning weights
#Below 25% the weight does not matter because years will be removed
year_weights[names(year_weights) >= (2026 - quantiles["25%"])] <- 0
#Between 25 and 50%, assigning 0.25 weight
year_weights[names(year_weights) >= (2026 - quantiles["50%"]) &
               names(year_weights) < (2026 - quantiles["25%"])] <- .25
#Between 50 and 75%, assigning 0.5 weight
year_weights[names(year_weights) >= (2026 - quantiles["75%"]) &
               names(year_weights) < (2026 - quantiles["50%"])] <- .5

#print the year weights
year_weights

#plot global costs in specified timeframe
global.trend <- modelCosts(
  db.over.time, # The EXPANDED database
  cost.column = "AUD_2025",
  minimum.year = 1990, 
  maximum.year = 2026,
  incomplete.year.threshold = 2025)

#Let's see the results in the console
global.trend
#plot these results with a changed title
plot(global.trend) + labs(y = "Average annual cost (2025 AUD$, millions)")


#Australian exclusive data
#find all country names
unique(data_df$Official_country)
#choose only australian costs
dataAUS <- data_df[which(data_df$Official_country == "Australia"), ]

#check the number of rows 
nrow(dataAUS)

#expand the australian costs
db.over.timeAUS <- expandYearlyCosts(
  dataAUS,
  startcolumn = "Probable_starting_year_adjusted",
  endcolumn = "Probable_ending_year_adjusted"
)

# Add converted costs to dataset
db.over.timeAUS <- db.over.timeAUS %>%
  mutate(
    AUD_2025 = Cost_estimate_per_year_2017_USD_exchange_rate * conv_factor
  )

#check that the conversion worked and has been added to aus data
db.over.timeAUS %>%
  filter(Cost_ID == "FD61") %>%
  mutate(
    expected = Cost_estimate_per_year_2017_USD_exchange_rate *
      conv_factor
  ) %>%
  select(
    Impact_year,
    AUD_2025,
    expected
  )

#plot timelag
db.over.timeAUS$Publication_lag <- db.over.timeAUS$Publication_year - db.over.timeAUS$Impact_year

#calculate quantiles
quantiles <- quantile(db.over.timeAUS$Publication_lag, probs = c(.25, .5, .75))
#print the quantiles
quantiles

# Creating the vector of weights
year_weights <- rep(1, length(2010:2024))
names(year_weights) <- 2010:2024

#Assigning weights
#Below 25% the weight does not matter because years will be removed
year_weights[names(year_weights) >= (2024 - quantiles["25%"])] <- 0
#Between 25 and 50%, assigning 0.25 weight
year_weights[names(year_weights) >= (2024 - quantiles["50%"]) &
               names(year_weights) < (2024 - quantiles["25%"])] <- .25
#Between 50 and 75%, assigning 0.5 weight
year_weights[names(year_weights) >= (2024 - quantiles["75%"]) &
               names(year_weights) < (2024 - quantiles["50%"])] <- .5

#Let's look at it
year_weights

#plot regressions models 
global.trendAUS <- modelCosts(
  db.over.timeAUS, # The EXPANDED database
  cost.column = "AUD_2025",
  minimum.year = 2010, 
  maximum.year = 2026,
  incomplete.year.threshold = 2024)

#Let's see the results in the console
global.trendAUS

#plot with changed title
plot(global.trendAUS) + labs(y = "Average annual cost (2025 AUD$, millions)")
