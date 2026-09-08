#Load data and packages needed
library(readr)
library(invacost)
library(dplyr)
library(plyr)

#read data 
data <- read_csv("Cleaned Additional data points 1-09.csv")
#turn to dataframe
data_df <- as.data.frame(data)
#print data
View(data_df)

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

#convert from 2017 usd to aus 2026 costs
conv_factor <- 1.3047 * (101.3 / 111.175)
#add that converted column to the expanded data
db.over.timeAUS <- db.over.timeAUS %>%
  mutate(
    AUD.2026.dollar.conversions =
      Cost_estimate_per_year_2017_USD_exchange_rate * conv_factor
  )

#check that the conversion worked by checking a single datapoint
db.over.timeAUS %>%
  filter(Cost_ID == "FD61") %>%
  summarise(
    years = n(),
    annual_aud = first(AUD.2026.dollar.conversions),
    total_aud = sum(AUD.2026.dollar.conversions)
  )

db.over.timeAUS %>%
  filter(Cost_ID == "FD61") %>%
  mutate(
    expected = Cost_estimate_per_year_2017_USD_exchange_rate *
      conv_factor
  ) %>%
  select(
    Impact_year,
    AUD.2026.dollar.conversions,
    expected
  )

#create annual costs so to be put against the yearly GDP
annual.costs <- db.over.timeAUS %>%
#group the data by the impact year and rename the grouping column to year in the output
  dplyr::group_by(Year = Impact_year) %>%
#calculate the total sum of the column for each year  
dplyr::summarise(
    AnnualCostAUD = sum(
      AUD.2026.dollar.conversions,
#ensure that missing values are ignored instead of causing the sum to return NA      
na.rm = TRUE
    ),
#removes the grouping structure from the resulting data frame, leaving you with a clean standard tibble.
    .groups = "drop"
  )

#read Australian GDP data
gdp <- read_csv("GDPAUS.csv")

#creates a new dataframe to store the results of this operation
gdp.analysis <- 
#takes your existing data and passess it into the next function using the pipe operator
annual.costs %>%
#merges the datasets by matching rows that share the exact same year (keeps only the years that exist in both dataframes)
  inner_join(gdp, by = "Year") %>%
#Add a new column names CostPercentGDP  
mutate(
#calculates the percentage by dividing the annual cost by the GDP value and multiplying by 100.    
CostPercentGDP = AnnualCostAUD / Australia * 100
  )

#get range of data
ggplot(gdp.analysis,
       aes(Year, CostPercentGDP)) +
#draw the grey "sticks" of the lollipop running from the baseline  
geom_segment(
    aes(xend = Year, y = 0, yend = CostPercentGDP),
    colour = "grey70"
  ) +
#draws the large dots at the top of each stick
  geom_point(
    size = 4,
    colour = "#D55E00"
  ) +
#cleans up the plot by removing the defult grey background and grid, also set text size
  theme_classic(base_size = 12) +
  labs(
    x = "Year",
    y = "Invasive deer costs (% GDP)"
  )

#costs per billions
gdp.analysis <- gdp.analysis %>%
#calculates a new metric using the mutate function  
mutate(
#divide the total annual cost by Australia's GDP scaled to billions
#represents how many dollars invasive deer cost for every 1 billion dollars of Australia's total GDP
    CostPerBillionGDP =
      AnnualCostAUD / (Australia / 1e9)
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
