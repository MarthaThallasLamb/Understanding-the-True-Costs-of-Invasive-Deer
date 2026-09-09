#Load data and packages needed
library(readr)
library(invacost)
library(dplyr)
library(ggplot2)
library(scales)

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

#manually inspect conversions
head(db.over.time$Cost_estimate_per_year_2017_USD_exchange_rate)

#create histogram to visualise the data
hist(db.over.time$AUD_2025)

#check the number of row and column 
nrow(db.over.time)
ncol(db.over.time)

#Calculating time lag
db.over.time$Publication_lag <- db.over.time$Publication_year - db.over.time$Impact_year

#Make a boxplot of the time lag
ggplot(db.over.time,
       aes(y = Publication_lag)) +
  geom_boxplot(outlier.alpha = .2) +
  ylab("Publication lag (in years)") + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = c(-25, 0, 25, 50, 75, 100),
                     labels = c(-25, 0, 25, 50, 75, 100)) +
  xlab("") +
  coord_flip()

#check the quantiles
quantiles <- quantile(db.over.time$Publication_lag, probs = c(.25, .5, .75))
#print quantiles
quantiles

#summarise the expanded costs using the 2025 aud conversion
obs.costs <- summarizeCosts(
  db.over.time,
  cost.column = "AUD_2025",
  maximum.year = 2026
)

#print the summarised costs
obs.costs
#plot the summarised costs
plot(obs.costs)

#store the plot in object p1 to customize it afterwards
p1 <- plot(obs.costs,
           graphical.parameters = "manual")

#Show the graph in its initial state
p1

#Customize p1 now
p1 <- p1 +
  xlab("Year") + 
  ylab("Average annual cost of invasive deer in 2025 AUD$ millions") +
  scale_x_continuous(breaks = obs.costs$year.breaks) + # X axis breaks
  theme_bw() + # Minimal theme
  scale_y_log10(breaks = 10^(-15:15), # y axis in log 10 with pretty labels
                labels = scales::comma) +
  annotation_logticks(sides = "l") # log10 tick marks on y axis

#print the customised plot
p1

#plot the expanded and summarised data looking at specified timeframe
obs.costs2 <- summarizeCosts(db.over.time,
                             cost.column = "AUD_2025",
                             minimum.year = 1990,
                             maximum.year = 2026,
                             year.breaks = seq(1990, 2026, by = 9))

#print the summarised costs
obs.costs2
#plot the summarised costs
plot(obs.costs2)

#Store the plot in object p2 to customize it afterwards
p2 <- plot(obs.costs2,
           graphical.parameters = "manual")

#Show the graph in its initial state
p2

#Customize p2 now
p2 <- p2 +
  labs(
    x = "Year",
    y = "Average annual cost (2025 AUD$, millions)"
  ) +
  scale_x_continuous(
    breaks = obs.costs$year.breaks
  ) +
  scale_y_log10(
    labels = comma
  ) +
  annotation_logticks(sides = "l") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#display figure
p2

#look at the publication dates (look at the number of estimates made over time)
ggplot(obs.costs2$cost.per.year,
       aes(x = year, y = number_estimates)) +
  geom_point() +
  ylab("Number of estimates") +
  xlab("Year") +
  theme_minimal()

#refine the graph
ggplot(obs.costs2$cost.per.year,
       aes(x = year, y = number_estimates,
           size = cost)) +
  geom_point() +
  ylab("Number of estimates") +
  xlab("Year") +
  theme_minimal()

#print the customised plot
p2

#now do for just the australian costs 
#exclude non aus costs 
unique(data_df$Official_country)
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
  summarise(
    years = n(),
    annual_aud = first(AUD_2025),
    total_aud = sum(AUD_2025)
  )

db.over.time %>%
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

#Calculating time lag
db.over.timeAUS$Publication_lag <- db.over.timeAUS$Publication_year - db.over.timeAUS$Impact_year

#Make a boxplot of the time lag
ggplot(db.over.timeAUS,
       aes(y = Publication_lag)) +
  geom_boxplot(outlier.alpha = .2) +
  ylab("Publication lag (in years)") + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = c(-25, 0, 25, 50, 75, 100),
                     labels = c(-25, 0, 25, 50, 75, 100)) +
  xlab("") +
  coord_flip()

#check quantiles
quantiles <- quantile(db.over.timeAUS$Publication_lag, probs = c(.25, .5, .75))
#print the quantiles 
quantiles

#summarise the expanded cost
obs.costsAUS <- summarizeCosts(db.over.timeAUS,
                               cost.column = "AUD_2025",
                               maximum.year = 2026)

#print the expanded and summarised aus costs
obs.costsAUS
#plot the expanded and summarised aus costs overtime
plot(obs.costsAUS)

#plot costs for specified timeframe
obs.costsAUS2 <- summarizeCosts(db.over.timeAUS,
                                cost.column = "AUD_2025",
                                minimum.year = 2010,
                                maximum.year = 2026,
                                year.breaks = seq(2010, 2026, by = 4))
#print the expanded and summarised aus costs
obs.costsAUS2
#plot the expanded and summarised aus costs overtime
plot(obs.costsAUS2)

#Store the plot in object p3 to customize it afterwards
p3 <- plot(obs.costsAUS2,
           graphical.parameters = "manual")

#Show the graph in its initial state
p3

#Customize p3 now
p3 <- p3 +
  labs(
    x = "Year",
    y = "Average annual cost (2025 AUD$, millions)"
  ) +
  scale_x_continuous(
    breaks = seq(2010, 2026, by = 4)
  ) +
  scale_y_log10(
    labels = comma
  ) +
  annotation_logticks(sides = "l") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#print the changed plot
p3

#look at publication dates (look at when the number of estimates were made over time)
ggplot(obs.costsAUS2$cost.per.year,
       aes(x = year, y = number_estimates)) +
  geom_point() +
  ylab("Number of estimates") +
  xlab("Year") +
  theme_minimal()

#refine the publication date graph (look at when the number of estimates were made over time)
ggplot(obs.costsAUS2$cost.per.year,
       aes(x = year, y = number_estimates,
           size = cost)) +
  geom_point() +
  ylab("Number of estimates") +
  xlab("Year") +
  theme_minimal()
