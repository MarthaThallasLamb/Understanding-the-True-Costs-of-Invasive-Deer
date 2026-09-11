#Load data and packages needed
library(readr)
library(invacost)
library(plyr)
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
cpi_2017 <- 115.6868
cpi_2025 <- 148.4573

aud_usd_2017 <- 0.7669  # 1 AUD = 0.7669 USD

conv_factor <- (1 / aud_usd_2017) * (cpi_2025 / cpi_2017)

# Add converted costs to dataset
db.over.time <- db.over.time %>%
  mutate(
    AUD_2025 = Cost_estimate_per_year_2017_USD_exchange_rate * conv_factor
  )

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

#create annual costs so to be put against the yearly GDP
annual.costs <- db.over.timeAUS %>%
  #group the data by the impact year and rename the grouping column to year in the output
  dplyr::group_by(Year = Impact_year) %>%
  #calculate the total sum of the column for each year  
  dplyr::summarise(
    AnnualCostAUD = sum(
      AUD_2025,
      #ensure that missing values are ignored instead of causing the sum to return NA      
      na.rm = TRUE
    ),
    #removes the grouping structure from the resulting data frame, leaving you with a clean standard tibble.
    .groups = "drop"
  )

#read Australian GDP data
gdp <- read_csv("data/GDPAUS.csv")

#creates a new dataframe to store the results of this operation
gdp.analysis <- 
  #takes your existing data and passess it into the next function using the pipe operator
  annual.costs %>%
  #merges the datasets by matching rows that share the exact same year (keeps only the years that exist in both dataframes)
  inner_join(gdp, by = "Year") %>%
  #Add a new column names CostPercentGDP  
  mutate(
    #calculates the percentage by dividing the annual cost by the GDP value and multiplying by 100.    
    CostPercentGDP = AnnualCostAUD / Australian_2025_conversion * 100
  )

#costs per billions
gdp.analysis <- gdp.analysis %>%
  #calculates a new metric using the mutate function  
  mutate(
    #divide the total annual cost by Australia's GDP scaled to billions
    #represents how many dollars invasive deer cost for every 1 billion dollars of Australia's total GDP
    CostPerBillionGDP =
      AnnualCostAUD / (Australian_2025_conversion / 1e9)
  )

#Chart A (tracks cost per billion dollars of GDP)
ggplot(gdp.analysis,
       aes(Year, CostPerBillionGDP)) +
  geom_area(fill = "#D55E00", alpha = .7) +
  geom_line(colour = "#B2182B")

#Chart B (tracks the cost as a direct percentage of GDP)
ggplot(gdp.analysis,
       aes(Year, CostPercentGDP)) +
  geom_area(fill = "#D55E00", alpha = .7) +
  geom_line(colour = "#B2182B")

#find the averages across mean annual gdp, mean percent of gdp and mean cost per billion
gdp.analysis %>%
  summarise(
    MeanAnnualCostAUD = mean(AnnualCostAUD, na.rm = TRUE),
    MeanPercentGDP = mean(CostPercentGDP, na.rm = TRUE),
    MeanCostPerBillionGDP = mean(CostPerBillionGDP, na.rm = TRUE)
  )
