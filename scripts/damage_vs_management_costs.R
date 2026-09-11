#load packages 
library(readr)
library(dplyr)
library(purrr)
library(invacost)
library(maps)
library(tidyr)

##read data
data <- read_csv("Cleaned Additional cervidae data points.csv")

##turn to dataframe
data_df <- as.data.frame(data)

View(data_df)

##expand data
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

##check row and column 
nrow(db.over.time)
ncol(db.over.time)

##use plyr::count to count the number of occurrences of unique values in a dataframe or vector in this case (damage or management)
plyr::count(data_df$Type_of_cost_merged)
costtype <- list(
  Damage = db.over.time[db.over.time$Type_of_cost_merged %in% "Damage", ],
  Management = db.over.time[db.over.time$Type_of_cost_merged %in% "Management", ]
)

##Summarise cost function used
obs.costtype <- purrr::map(costtype, 
                           summarizeCosts,
                           cost.column = "AUD_2025",
                           minimum.year = 1990,
                           maximum.year = 2026)

##combine raw cost data and period data 
cost.data <- rbind(
  data.frame(obs.costtype$Damage$cost.data,
             type = "Damage"),
  data.frame(obs.costtype$Management$cost.data,
             type = "Management")
)


costperiod <- rbind(
  data.frame(obs.costtype$Damage$average.cost.per.period,
             type = "Damage"),
  data.frame(obs.costtype$Management$average.cost.per.period,
             type = "Management")
)

#calculate middle years for plotting
costperiod$middle.years <- costperiod$initial_year +
  (costperiod$final_year - 
     costperiod$initial_year) / 2 
plot.breaks = 10^(-15:15)

##extract column names dynamic mapping
yeargroups.damage <- dplyr::group_by(obs.costtype$Damage$cost.data,
                                     get(obs.costtype$Damage$parameters$year.column)) 

yeargroups.management <- dplyr::group_by(obs.costtype$Management$cost.data,
                                         get(obs.costtype$Management$parameters$year.column)) 

##dynamic grouping and summarising using tidy evaluation 
yearly.cost <- rbind.data.frame(
  setNames(data.frame(dplyr::summarise(yeargroups.damage, 
                                       Annual.cost = sum(get(obs.costtype$Damage$parameters$cost.column))),
                      "Damage"), c("Year", "Annual.cost", "type")),
  setNames(data.frame(dplyr::summarise(yeargroups.management, 
                                       Annual.cost = sum(get(obs.costtype$Management$parameters$cost.column))),
                      "Management"), c("Year", "Annual.cost", "type"))
)


#generate the visualisation
ggplot(costperiod) +
  ylab("Average annual cost per period in 2025 AU$ millions") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1990, 2026, 10)) +
  theme_bw(base_size = 10) +
  scale_y_log10(
    breaks = plot.breaks,
    labels = scales::comma
  ) +
  annotation_logticks() +
  geom_point(
    aes(x = middle.years,
        y = annual_cost,
        col = type),
    size = 2
  ) +
  geom_line(
    aes(x = middle.years,
        y = annual_cost,
        col = type),
    linetype = 2,
    linewidth = 0.6
  ) +
  geom_segment(
    aes(x = initial_year,
        xend = final_year,
        y = annual_cost,
        yend = annual_cost,
        col = type),
    linewidth = 0.6
  ) +
  geom_point(
    data = yearly.cost,
    aes(x = Year,
        y = Annual.cost,
        col = type),
    size = 1,
    alpha = 0.8
  ) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.position = "bottom"
  )

##publication lag analysis 
db.over.time$Publication_lag <- db.over.time$Publication_year - db.over.time$Impact_year

quantiles <- quantile(db.over.time$Publication_lag, probs = c(.25, .5, .75))
quantiles

# Creating the vector of weights
year_weights <- rep(1, length(1990:2026))
names(year_weights) <- 1990:2026

# Assigning weights
# Below 25% the weight does not matter because years will be removed
year_weights[names(year_weights) >= (2024 - quantiles["25%"])] <- 0
# Between 25 and 50%, assigning 0.25 weight
year_weights[names(year_weights) >= (2024 - quantiles["50%"]) &
               names(year_weights) < (2024- quantiles["25%"])] <- .25
# Between 50 and 75%, assigning 0.5 weight
year_weights[names(year_weights) >= (2024 - quantiles["75%"]) &
               names(year_weights) < (2024 - quantiles["50%"])] <- .5

# Let's look at it
year_weights

#fit cost trend models separately for each cost type 
pred.costtype.25 <- purrr::map(costtype, 
                               modelCosts,
                               cost.column = "AUD_2025",
                               minimum.year = 1990, 
                               maximum.year = 2026,
                               final.year = 2024,
                               
                               # Some years are so incomplete that we eliminate with our 25% threshold (see above)
                               incomplete.year.threshold = 2024 - quantiles["25%"], 
                               
                               # For the other incomplete years we apply the vector of weights that we defined above
                               incomplete.year.weights = year_weights)

#combine observed damage and management cost data into a single dataframe
cost.data <- rbind(
  data.frame(pred.costtype.25$Damage$cost.data,
             type = "Damage"),
  data.frame(pred.costtype.25$Management$cost.data,
             type = "Management")
)

#combine predicted annual cosst from both cost types
cost.data$Calibration <- factor(cost.data$Calibration, 
                                levels = c("Included", "Excluded"))

model.preds <- rbind(
  data.frame(pred.costtype.25$Damage$estimated.annual.costs,
             type = "Damage",
             calib = 2026 - quantiles["25%"]),
  data.frame(pred.costtype.25$Management$estimated.annual.costs,
             type = "Management",
             calib = 2026 - quantiles["25%"])
)

#convert calibration threshold to a factor for plotting/grouping 
model.preds$calib <- as.factor(model.preds$calib)
#keep only robust regression models with a linear trend
model.preds <- model.preds[which(model.preds$model == "Robust regression" &
                                   model.preds$Details == "Linear"), ]



#extract only robust linear regression predictions 
robust_preds <- model.preds %>%
  filter(
    model == "Robust regression",
    Details == "Linear"
  )

#reshape predictions so damage and management are separate columns 
dm_ratio <- robust_preds %>%
  select(Year, type, fit) %>%
  pivot_wider(
    names_from = type,
    values_from = fit
  ) %>%
  mutate(
    DM_ratio = Damage / Management
  )

#plot temporal trend in damage:management ratio
ggplot(dm_ratio,
       aes(x = Year, y = DM_ratio)) +
  geom_line(colour = "firebrick", linewidth = 1) +
  geom_point(size = 2) +
  theme_classic() +
  labs(
    x = "Year",
    y = "Damage : Management ratio",
    title = "Temporal trend in the D:M ratio"
  )


#create figure comparing annual damage and management costs 
fig2 <- ggplot() + 
  
  #axis labels
  ylab("Annual cost in 2025 AUD$ millions") +
  xlab("Year") +
  
  #base theme
  theme_bw() +
  
  #log-scaled y-axis
  scale_y_log10(breaks = plot.breaks,
                labels = scales::comma) +
  
  #log tick marks
  annotation_logticks() +
  
  #observed annual costs
  geom_point(data = cost.data, 
             aes_string(x = "Year",
                        y = "Annual.cost",
                        col = "type",
                        shape = "Calibration"),
             size = 2, alpha = .8) +
  
  #robust linear regression trend lines 
  geom_line(data = model.preds[which(model.preds$model == "Robust regression" &
                                       model.preds$Details == "Linear"), ], 
            aes_string(x = "Year",
                       y = "fit",
                       col = "type"),
            size = 1.1,
            alpha = .8) +
  
  #95% confidence intervals around trend lines 
  geom_ribbon(data = model.preds[which(model.preds$model == "Robust regression" &
                                         model.preds$Details == "Linear"), ], 
              aes_string(x = "Year",
                         ymin = "lwr",
                         ymax = "upr",
                         fill = "type"),
              alpha = .1,
              linetype = 0) +
  
  #legend formatting
  scale_color_discrete(name = "Type of cost") +
  theme() +
  guides(col = guide_legend(title = "Category of cost"),
         fill = FALSE) +
  
  #theme customisation
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12,
                                   margin = margin(t = 5, b = 5, unit = "pt")),
        legend.title = element_text(size = 12),
        legend.position = c(.2, .75))


print(fig2)

##australian
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

# Calculating time lag
db.over.timeAUS$Publication_lag <- db.over.timeAUS$Publication_year - db.over.timeAUS$Impact_year

#inspect frequency of cost types
plyr::count(dataAUS$Type_of_cost_merged)
costtype <- list(
  Damage = db.over.timeAUS[db.over.timeAUS$Type_of_cost_merged %in% "Damage", ],
  Management = db.over.timeAUS[db.over.timeAUS$Type_of_cost_merged %in% "Management", ]
)

#summarise annual and period costs
obs.costtypeAUS <- purrr::map(costtype, 
                              summarizeCosts,
                              cost.column = "AUD_2025",
                              minimum.year = 2010,
                              maximum.year = 2026,
                              year.breaks = seq(2010, 2026, by = 4))

#combine average costs by time period 
cost.data <- rbind(
  data.frame(obs.costtypeAUS$Damage$cost.data,
             type = "Damage"),
  data.frame(obs.costtypeAUS$Management$cost.data,
             type = "Management")
)
costperiod <- rbind(
  data.frame(obs.costtypeAUS$Damage$average.cost.per.period,
             type = "Damage"),
  data.frame(obs.costtypeAUS$Management$average.cost.per.period,
             type = "Management")
)

#calculate midpoint year for each period 
costperiod$middle.years <- costperiod$initial_year +
  (costperiod$final_year - 
     costperiod$initial_year) / 2 

#log-scale axis breaks
plot.breaks = 10^(-15:15)

#group annual costs by year
yeargroups.damage <- dplyr::group_by(obs.costtypeAUS$Damage$cost.data,
                                     get(obs.costtypeAUS$Damage$parameters$year.column)) 

yeargroups.management <- dplyr::group_by(obs.costtypeAUS$Management$cost.data,
                                         get(obs.costtypeAUS$Management$parameters$year.column)) 

#calculate observed annual costs
yearly.cost <- rbind.data.frame(
  setNames(data.frame(dplyr::summarise(yeargroups.damage, 
                                       Annual.cost = sum(get(obs.costtypeAUS$Damage$parameters$cost.column))),
                      "Damage"), c("Year", "Annual.cost", "type")),
  setNames(data.frame(dplyr::summarise(yeargroups.management, 
                                       Annual.cost = sum(get(obs.costtypeAUS$Management$parameters$cost.column))),
                      "Management"), c("Year", "Annual.cost", "type"))
)

#plot decadal average costs and annual observations
ggplot(costperiod) +
  ylab("Average annual cost per period in 2025 AU$ millions") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2010, 2026, 4)) +
  theme_bw(base_size = 4) +
  scale_y_log10(
    breaks = plot.breaks,
    labels = scales::comma
  ) +
  annotation_logticks() +
  geom_point(
    aes(x = middle.years,
        y = annual_cost,
        col = type),
    size = 2
  ) +
  geom_line(
    aes(x = middle.years,
        y = annual_cost,
        col = type),
    linetype = 2,
    linewidth = 0.6
  ) +
  geom_segment(
    aes(x = initial_year,
        xend = final_year,
        y = annual_cost,
        yend = annual_cost,
        col = type),
    linewidth = 0.6
  ) +
  geom_point(
    data = yearly.cost,
    aes(x = Year,
        y = Annual.cost,
        col = type),
    size = 1,
    alpha = 0.8
  ) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.position = "bottom"
  )

#recalculate publication lag 
db.over.timeAUS$Publication_lag <- db.over.timeAUS$Publication_year - db.over.timeAUS$Impact_year

#calculate lag quantiles 
quantiles <- quantile(db.over.timeAUS$Publication_lag, probs = c(.25, .5, .75))
quantiles

# Creating the vector of weights
year_weights <- rep(1, length(2010:2026))
names(year_weights) <- 2010:2026

# Assigning weights
# Below 25% the weight does not matter because years will be removed
year_weights[names(year_weights) >= (2026 - quantiles["25%"])] <- 0
# Between 25 and 50%, assigning 0.25 weight
year_weights[names(year_weights) >= (2026 - quantiles["50%"]) &
               names(year_weights) < (2026 - quantiles["25%"])] <- .25
# Between 50 and 75%, assigning 0.5 weight
year_weights[names(year_weights) >= (2026 - quantiles["75%"]) &
               names(year_weights) < (2026 - quantiles["50%"])] <- .5

# Let's look at it
year_weights

#fit robust regression models to damage and management costs 
pred.costtype.25 <- purrr::map(costtype, 
                               modelCosts,
                               cost.column = "AUD_2025",
                               minimum.year = 2010, 
                               maximum.year = 2026,
                               final.year = 2026,
                               # Some years are so incomplete that we eliminate with our 25% threshold (see above)
                               incomplete.year.threshold = 2024 - quantiles["25%"], 
                               # For the other incomplete years we apply the vector of weights that we defined above
                               incomplete.year.weights = year_weights,
                               gam.k = 4)

#combine observed model input data
cost.data <- rbind(
  data.frame(pred.costtype.25$Damage$cost.data,
             type = "Damage"),
  data.frame(pred.costtype.25$Management$cost.data,
             type = "Management")
)

#flag included/excluded calibration years
cost.data$Calibration <- factor(cost.data$Calibration, 
                                levels = c("Included", "Excluded"))

#combine annual model predictions 
model.preds <- rbind(
  data.frame(pred.costtype.25$Damage$estimated.annual.costs,
             type = "Damage",
             calib = 2026 - quantiles["25%"]),
  data.frame(pred.costtype.25$Management$estimated.annual.costs,
             type = "Management",
             calib = 2026 - quantiles["25%"])
)

#store calibration threshold 
model.preds$calib <- as.factor(model.preds$calib)
#retain only robust linear regression results 
model.preds <- model.preds[which(model.preds$model == "Robust regression" &
                                   model.preds$Details == "Linear"), ]

#plot observed costs, trends, and confidence intervals 
fig3 <- ggplot() + 
  ylab("Annual cost in 2025 AU$ millions") +
  xlab("Year") +
  theme_bw() +
  scale_y_log10(breaks = plot.breaks,
                labels = scales::comma) +
  annotation_logticks() +
  geom_point(data = cost.data, 
             aes_string(x = "Year",
                        y = "Annual.cost",
                        col = "type",
                        shape = "Calibration"),
             size = 2, alpha = .8) +
  geom_line(data = model.preds[which(model.preds$model == "Robust regression" &
                                       model.preds$Details == "Linear"), ], 
            aes_string(x = "Year",
                       y = "fit",
                       col = "type"),
            size = 1.1,
            alpha = .8) +
  geom_ribbon(data = model.preds[which(model.preds$model == "Robust regression" &
                                         model.preds$Details == "Linear"), ], 
              aes_string(x = "Year",
                         ymin = "lwr",
                         ymax = "upr",
                         fill = "type"),
              alpha = .1,
              linetype = 0) +
  scale_color_discrete(name = "Type of cost") +
  theme() +
  guides(col = guide_legend(title = "Category of cost"),
         fill = FALSE) +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12,
                                   margin = margin(t = 5, b = 5, unit = "pt")),
        legend.title = element_text(size = 12),
        legend.position = c(.2, .75))

print(fig3)

#calculate damage:management ratio from model predictions
robust_predsAUS <- model.preds %>%
  filter(
    model == "Robust regression",
    Details == "Linear"
  )
dm_ratioAUS <- robust_predsAUS %>%
  select(Year, type, fit) %>%
  pivot_wider(
    names_from = type,
    values_from = fit
  ) %>%
  mutate(
    dm_ratioAUS = Damage / Management
  )

#Plot temporal trend in Damage:Management ratio
ggplot(dm_ratioAUS,
       aes(x = Year, y = DM_ratio)) +
  geom_line(colour = "firebrick", linewidth = 1) +
  geom_point(size = 2) +
  theme_classic() +
  labs(
    x = "Year",
    y = "Damage : Management ratio",
    title = "Temporal trend in the D:M ratio"
  )
