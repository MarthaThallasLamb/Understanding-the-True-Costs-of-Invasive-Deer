#load packages
library(dplyr)
library(gt)

#Eradication costs with 95% UI
#random number generator to produce a repeatable, identical sequence of random numbers
set.seed(123)
#set the number of simulation you want run
n_sims <- 10000
#state the median cost of eradication which is based on the Australian national deer program
total_cost_median <- 14000000
#As we are basing this off of one value a 95% UI assumed to be ±20% around median
sdlog <- log(1.20) / 1.96
# Simulate total eradication costs
#the variable name storing the resulting vector of simulation values
eradication_cost_sim <- rlnorm(
#number of simulations to generate  
n_sims,
#the mean of the distribution on the log scale  
meanlog = log(total_cost_median),
#the standard deviation of the distirbution on the log scale this controls the spread or uncertainty of the simulated costs   
sdlog = sdlog
)

# Spread costs evenly across program years
cost_years <- 2022:2032

#calculates the average annual cost of an eradication program by dividing the simulated total cost by the number of years the program lasts
annual_cost_sim <- eradication_cost_sim / length(cost_years)

#create function to discounted present values
#stores the final output into new vector
PV_cost_4_sim <- 
#loops through each element of the annual_cost_sim list
sapply(
#the input data  
annual_cost_sim,
#defines an anonymous function applied to each simulation run  
function(x)
    sum(x / (1.04)^(cost_years - 2025))
)
#repeat for 7% discount rate
PV_cost_7_sim <- sapply(
  annual_cost_sim,
  function(x)
    sum(x / (1.07)^(cost_years - 2025))
)

#Create a dataframe of summary table
cost_summary <- data.frame(
  DiscountRate = c("4%", "7%"),
#add lower interval costs 
  Lower95 = c(
    quantile(PV_cost_4_sim, 0.025),
    quantile(PV_cost_7_sim, 0.025)
  ) / 1e6,
#add median interval costs
  Median = c(
    median(PV_cost_4_sim),
    median(PV_cost_7_sim)
  ) / 1e6,
#add upper interval costs  
  Upper95 = c(
    quantile(PV_cost_4_sim, 0.975),
    quantile(PV_cost_7_sim, 0.975)
  ) / 1e6
)
#round decimal places to 2 decimal places
round(cost_summary[, 2:4], 2)

#print summary table
print(cost_summary)

#check data 
#function used to plot columns of a matrix againts a vector 
matplot(
#the x-axis variable  
years,
#the y-axis variables (takes first 100 simulated trajectories of biomass from a larger dataset)  
biomass_sims[,1:100],
#tells R to draw these as lines instead of points  
type = "l",
#uses a solid line type for all 100 simulations
  lty = 1,
#colours the lines black with 90% transparency
  col = rgb(0,0,0,0.1),
#labels the axes  
xlab = "Year",
  ylab = "Biomass"
)
#adding the trend line
lines(
#plots the calculated median biomass for each year over time  
years,
  biomass_median,
#thicken the 3 times  
lwd = 3,
#colour the line
  col = "red"
)

#plot the costs of eradication
#set the constant cash outflow for each year
annual_cost <- 1272727.27
#years of program
cost_years <- 2022:2032
#evaluate the total present value of program costs at 4% discount rate
PV_cost_4 <- 
#adds all 11 adjusted annual values together to get a single project lifetime cost estimates
sum(
  annual_cost /
#applies the time-value-of-money multiplier for each year and calculate the timeline distance relative to the 2025 base year
    (1.04)^(cost_years - 2025)
)
#evaluate the total present value of program costs at 7% discount rate
PV_cost_7 <- sum(
  annual_cost /
    (1.07)^(cost_years - 2025)
)

#Calculate the total projected damages for each discount rate
total_pv_4 <- grazing_pv_4 +
  management_pv_4 +
  forestry_pv_4 +
  collision_pv_4

total_pv_7 <- grazing_pv_7 +
  management_pv_7 +
  forestry_pv_7 +
  collision_pv_7


#Monte Carlo simulation eradication costs with uncertainty
#set the random seed (makes the simulation reproducible everytime code is run same random results produced)
set.seed(123)

#define median cost
total_cost_median <- 14000000

#define uncertainty 95% UI assumed to be ±20% around median
sdlog <- log(1.20) / 1.96

#simulate 1000 pissible total program costs 
eradication_cost_sim <- rlnorm(
  n_sim,
  meanlog = log(total_cost_median),
  sdlog = sdlog
)
#define program years (national deer management program)
cost_years <- 2022:2032

#spread evenly across years (assumes spending is pread evenly across the program)
annual_cost_sim <-
  eradication_cost_sim /
  length(cost_years)

#calculates 4% present value (for each simulated annual cost)
PV_cost_4_sim <- sapply(
  annual_cost_sim,
  function(x)
    sum(
      x /
        (1.04)^(cost_years - 2025)
    )
)

#calculates 4% present value (for each simulated annual cost)
PV_cost_7_sim <- sapply(
  annual_cost_sim,
  function(x)
    sum(
      x /
        (1.07)^(cost_years - 2025)
    )
)


#Table 1 projected damage vs eradication
#create variable name 
econ_table <- data.frame(
  #create first column of table and name it estimates (the three text rows will also be filled with what you have written below)
  Estimate = c(
    "Lower 95% UI",
    "Median",
    "Upper 95% UI"
  ),
  #damages divided by 1 billion and to two decimal places
  PV_Damages_4 = round(
    c(
      quantile(total_pv_4, 0.025),
      median(total_pv_4),
      quantile(total_pv_4, 0.975)
    ) / 1e9,
    2
  ),
   #damages divided by 1 billion and to two decimal places
  PV_Damages_7 = round(
    c(
      quantile(total_pv_7, 0.025),
      median(total_pv_7),
      quantile(total_pv_7, 0.975)
    ) / 1e9,
    2
  ),
  #eradication costs divided by 1 million and to two decimal places 
  Eradication_Cost_4 = round(
    c(
      quantile(PV_cost_4_sim, 0.025),
      median(PV_cost_4_sim),
      quantile(PV_cost_4_sim, 0.975)
    ) / 1e6,
    2
  ),
  #eradication costs divided by 1 million and to two decimal places 
  Eradication_Cost_7 = round(
    c(
      quantile(PV_cost_7_sim, 0.025),
      median(PV_cost_7_sim),
      quantile(PV_cost_7_sim, 0.975)
    ) / 1e6,
    2
  ),
  #ratios calculated using raw vectors so units cancel out properly
  Damage_Cost_Ratio_4 = round(
    quantile(
      total_pv_4 / PV_cost_4_sim,
      c(0.025, 0.50, 0.975)
    ),
    0
  ),
  
  Damage_Cost_Ratio_7 = round(
    quantile(
      total_pv_7 / PV_cost_7_sim,
      c(0.025, 0.50, 0.975)
    ),
    0
  ) 
)

#print the table 
print(econ_table)

#Table 2. cost benefit analyses
#state the levels of effectivness
effectiveness <- c(
  0.25,
  0.50,
  0.75,
  1.00
)
#calculates the median present value of avoided damages from a baseline simulation.
PV_Damages_Median <- median(total_pv_4)
#combines the scenario into a structured table containing
cba_table <- data.frame(
#labels for the scenarios
  Effectiveness = c(
    "25%",
    "50%",
    "75%",
    "100%"
  ),
#calculated as the total median potential damages multiplied by each effectivness level
  Benefits = PV_Damages_Median * effectiveness,
#taken from the median present value of costs (this does remain constant across all effectiveness levels)
  Costs = median(PV_cost_4_sim)
)
#Calculating the economic metrics
#net present value
cba_table$NPV <-
  cba_table$Benefits -
  cba_table$Costs
#benefit-cost ratio
cba_table$BCR <-
  cba_table$Benefits /
  cba_table$Costs
#return on investment
cba_table$ROI <-
  ((cba_table$Benefits -
      cba_table$Costs) /
     cba_table$Costs) * 100

#Formatting the final output
#scales and round final numbers
cba_table <- cba_table |>
  mutate(
#divide the raw values by 1 billion and rounds them to 2 decimal places to display them as billions    
Benefits_B = round(
      Benefits / 1e9,
      2
    ),
#divide the cost by 1 million and rounds to 2 decimal places to display it in millions    
Costs_M = round(
      Costs / 1e6,
      2
    ),
    NPV_B = round(
      NPV / 1e9,
      2
    ),
#rounds the benefit-cost ratio to 1 decimal place and the ROI percentage to the nearest whole number
    BCR = round(
      BCR,
      1
    ),
    ROI = round(
      ROI,
      0
    )
  )

#print table 
print(cba_table)
