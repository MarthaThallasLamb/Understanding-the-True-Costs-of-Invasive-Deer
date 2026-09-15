#load packages
library(dplyr)
library(gt)

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

# Figure 1
# damage and program costs
econ_plot <- econ_table %>%
  pivot_longer(
    cols = -Estimate,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Estimate,
    values_from = Value
  ) %>%
  mutate(
    Type = case_when(
      grepl("PV_Damages", Metric) ~ "PV Damages",
      grepl("Eradication_Cost", Metric) ~ "Program Cost",
      grepl("Damage_Cost_Ratio", Metric) ~ "Damage:Cost Ratio"
    ),
    Discount = case_when(
      grepl("_4$", Metric) ~ "4%",
      grepl("_7$", Metric) ~ "7%"
    )
  )

econ_plot <- econ_table %>%
  pivot_longer(
    cols = -Estimate,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Estimate,
    values_from = Value
  ) %>%
  mutate(
    Group = case_when(
      grepl("PV_Damages", Metric) ~ "PV Damages 2025 AUD Billions",
      grepl("Eradication_Cost", Metric) ~ "Program Cost 2025 AUD Millions",
      grepl("Damage_Cost_Ratio", Metric) ~ "Damage:Cost Ratio"
    ),
    Discount = case_when(
      grepl("_4$", Metric) ~ "4%",
      grepl("_7$", Metric) ~ "7%"
    )
  )
ggplot(
  econ_plot,
  aes(
    x = Discount,
    ymin = `Lower 95% UI`,
    ymax = `Upper 95% UI`,
    colour = Discount
  )
) +
  geom_linerange(
    linewidth = 8,
    alpha = 0.6
  ) +
  geom_point(
    aes(y = Median),
    size = 4,
    colour = "black"
  ) +
  facet_wrap(
    ~ Group,
    scales = "free_y"
  ) +
   scale_colour_manual(
    values = c(
      "4%" = "olivedrab4",
      "7%" = "red4"
    )
  )+
  labs(
    x = "Discount rate",
    y = NULL
  ) +
  theme_bw()

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

# Figure 2
# Eradication effectiveness
plot_data <- cba_table %>%
  select(
    Effectiveness,
    NPV_B,
    BCR,
    ROI,
    Benefits_B
  ) %>%
  pivot_longer(
    cols = c(NPV_B, BCR, ROI, Benefits_B),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = recode(
      Metric,
      NPV_B = "Net Present Value (2025 AUD Billion)",
      BCR = "Benefit-Cost Ratio",
      ROI = "Return on Investment (%)",
      Benefits_B = "Avoided damages (2025 AUD Billion)"
    ),
    Effectiveness = factor(
      Effectiveness,
      levels = c("25%", "50%", "75%", "100%")
    )
  )

ggplot(
  plot_data,
  aes(
    x = Effectiveness,
    y = Value,
    fill = Metric
  )
) +
  geom_col(
    width = 0.7
  ) +
  facet_wrap(
    ~Metric,
    scales = "free_y",
    ncol = 2
  ) +
  scale_fill_manual(
    values = c(
      "Net Present Value (2025 AUD Billion)" = "grey28",
      "Benefit-Cost Ratio" = "olivedrab4",
      "Return on Investment (%)" = "#2a9d8f",
      "Avoided damages (2025 AUD Billion)" = "plum4"
    )
  ) +
  labs(
    x = "Eradication effectiveness",
    y = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )
