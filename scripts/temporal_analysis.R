#load packages
library(readr)
library(invacost)
library(dplyr)

#Read data
data <- read_csv("Cleaned Additional cervidae data points.csv")

#Turn into dataframe
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
cpi_2017 <- 115.6868
cpi_2025 <- 148.4573

aud_usd_2017 <- 0.7669  # 1 AUD = 0.7669 USD

conv_factor <- (1 / aud_usd_2017) * (cpi_2025 / cpi_2017)

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
year_weights <- rep(1, length(2000:2026))
names(year_weights) <- 2000:2026

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
  minimum.year = 2000, 
  maximum.year = 2026,
  final.year = 2026,
  incomplete.year.threshold = 2026 - quantiles["25%"], 
  incomplete.year.weights = year_weights)

#Let's see the results in the console
global.trend

summary(global.trend)
global.trend$RMSE

#see which models produced a AIC value
sapply(global.trend$fitted.models, function(x)
  tryCatch(AIC(x), error = function(e) NA))

#Obtain AIC values for suitable models
AIC(global.trend$fitted.models$ols.linear)
AIC(global.trend$fitted.models$ols.quadratic)
AIC(global.trend$fitted.models$gam)


AICs_global <- c(
  ols.linear = AIC(global.trend$fitted.models$ols.linear),
  ols.quadratic = AIC(global.trend$fitted.models$ols.quadratic),
  gam = AIC(global.trend$fitted.models$gam)
)

#view AIC values
AICs_global

#calculate ΔAIC
deltaG <- AICs_global - min(AICs_global)

#print 
deltaG

#Calculate Akike weights
AICw <- exp(-0.5 * deltaG) /
  sum(exp(-0.5 * deltaG))

#print
AICw

#calculate model-averaged costs
weights_df <- data.frame(
  model = c("OLS regression", "OLS regression", "GAM"),
  Details = c("Linear", "Quadratic", ""),
  weight = c(
    AICw["ols.linear"],
    AICw["ols.quadratic"],
    AICw["gam"]
  )
)

# Model-averaged annual costs
model_avg_global <- global.trend$estimated.annual.costs %>%
  inner_join(weights_df, by = c("model", "Details")) %>%
  mutate(weighted_fit = fit * weight) %>%
  group_by(Year) %>%
  summarise(
    model_avg_cost = sum(weighted_fit),
    .groups = "drop"
  )

model_avg_global

#just for 2026 
model_avg_global %>%
  filter(Year == 2026)

#average across all years
mean(model_avg_global$model_avg_cost)

#plot these results with a changed title
plot(global.trend, plot.type = "single") +
  labs(
    y = "Average annual cost (2025 AUD$, millions)",
  ) +
  geom_line(
    data = model_avg_global,
    aes(x = Year, y = model_avg_cost),
    colour = "red",
    linewidth = 2
  )

#Australian exclusive data
#find all country names
unique(data_df$Official_country)
#choose only Australian costs
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

#plot timelag
db.over.timeAUS$Publication_lag <- db.over.timeAUS$Publication_year - db.over.timeAUS$Impact_year

#calculate quantiles
quantiles <- quantile(db.over.timeAUS$Publication_lag, probs = c(.25, .5, .75))
#print the quantiles
quantiles

# Creating the vector of weights
year_weightsAUS <- rep(1, length(2012:2026))
names(year_weightsAUS) <- 2012:2026

#Assigning weights
#Below 25% the weight does not matter because years will be removed
year_weightsAUS[names(year_weightsAUS) >= (2026 - quantiles["25%"])] <- 0
#Between 25 and 50%, assigning 0.25 weight
year_weightsAUS[names(year_weightsAUS) >= (2026 - quantiles["50%"]) &
               names(year_weightsAUS) < (2026 - quantiles["25%"])] <- .25
#Between 50 and 75%, assigning 0.5 weight
year_weightsAUS[names(year_weightsAUS) >= (2026 - quantiles["75%"]) &
               names(year_weightsAUS) < (2026 - quantiles["50%"])] <- .5

#Let's look at it
year_weightsAUS

#plot regressions models 
global.trendAUS <- modelCosts(
  db.over.timeAUS, # The EXPANDED database
  cost.column = "AUD_2025",
  minimum.year = 2012, 
  maximum.year = 2026,
  final.year = 2026,
  incomplete.year.threshold = 2026 - quantiles["25%"], 
  incomplete.year.weights = year_weightsAUS)

#Let's see the results in the console
global.trendAUS
summary(global.trendAUS)
global.trendAUS$RMSE

#see which models produced a AIC value
sapply(global.trendAUS$fitted.models, function(x)
  tryCatch(AIC(x), error = function(e) NA))

#Obtain AIC values for suitable models
AIC(global.trendAUS$fitted.models$ols.linear)
AIC(global.trendAUS$fitted.models$ols.quadratic)
AIC(global.trendAUS$fitted.models$gam)


AICs_AUS <- c(
  ols.linear = AIC(global.trendAUS$fitted.models$ols.linear),
  ols.quadratic = AIC(global.trendAUS$fitted.models$ols.quadratic),
  gam = AIC(global.trendAUS$fitted.models$gam)
)

#view AIC values
AICs_AUS

#calculate ΔAIC
deltaA <- AICs_AUS - min(AICs_AUS)

#print 
deltaA

#Calculate Akike weights
AICa <- exp(-0.5 * deltaA) /
  sum(exp(-0.5 * deltaA))

#print
AICa

#calculate model-averaged costs
weights_df_AUS <- data.frame(
  model = c("OLS regression", "OLS regression", "GAM"),
  Details = c("Linear", "Quadratic", ""),
  weight = c(
    AICa["ols.linear"],
    AICa["ols.quadratic"],
    AICa["gam"]
  )
)

# Model-averaged annual costs
model_avg_AUS <- global.trendAUS$estimated.annual.costs %>%
  inner_join(weights_df_AUS, by = c("model", "Details")) %>%
  mutate(weighted_fit = fit * weight) %>%
  group_by(Year) %>%
  summarise(
    model_avg_cost = sum(weighted_fit),
    .groups = "drop"
  )

model_avg_AUS

#just for 2026 
model_avg_AUS %>%
  filter(Year == 2026)

#average across all years
mean(model_avg_AUS$model_avg_cost)

#plot these results with a changed title
plot(global.trendAUS, plot.type = "single") +
  labs(
    y = "Average annual cost (2025 AUD$, millions)",
  ) +
  geom_line(
    data = model_avg_AUS,
    aes(x = Year, y = model_avg_cost),
    colour = "red",
    linewidth = 2
  )
