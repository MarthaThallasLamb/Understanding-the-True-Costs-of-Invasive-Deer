library(ggplot2)
library(gt)
library(tidyr)
library(dplyr)

# ==========================================
# PARAMETERS
# ==========================================

#starting biomass tonnes
Btlow  <- 123404
Btmed  <- 185106
Bthigh <- 246808

#annual biomass growth rate
r_low  <- 0.115
r_med  <- 0.1325
r_high <- 0.15

#biomass (t) carrying capacity per hectare 
K_low <- 0.03697 * 10951900

K_med <- (0.03697 * 10951900) +
  (0.018485 * 8317350)

K_high <- 0.03697 * 45808675

years <- 2025:2055

# ==========================================
# BIOMASS MODEL
# ==========================================

##using the rickers model (eulers rule)
project_biomass <- function(B0, r, K, years = 30){
  
  biomass <- numeric(years + 1)
  biomass[1] <- B0
  
  for(t in 1:years){
    
    biomass[t + 1] <-
      biomass[t] *
      exp(
        r * (1 - biomass[t] / K)
      )
    
  }
  
  biomass
  
}

# ==========================================
# MONTE CARLO SIMULATION for 95% UI
# ==========================================

set.seed(123)

n_sim <- 1000

biomass_sims <- matrix(
  NA,
  nrow = length(years),
  ncol = n_sim
)

for(i in seq_len(n_sim)){
  
  B0 <- runif(
    1,
    Btlow,
    Bthigh
  )
  
  r <- runif(
    1,
    r_low,
    r_high
  )
  
  K <- runif(
    1,
    K_low,
    K_high
  )
  
  biomass_sims[,i] <-
    project_biomass(
      B0,
      r,
      K,
      years = 30
    )
  
}

# ==========================================
# BIOMASS UNCERTAINTY INTERVALS
# ==========================================

#quantile 0.025
biomass_lower <- apply(
  biomass_sims,
  1,
  quantile,
  probs = 0.025
)

#median
biomass_median <- apply(
  biomass_sims,
  1,
  median
)

#quantile 0.975
biomass_upper <- apply(
  biomass_sims,
  1,
  quantile,
  probs = 0.975
)

# ==========================================
# BIOMASS PLOT
# ==========================================

##creating uncertainty interval data frame
ci_df <- data.frame(
  Year = years,
  Lower = biomass_lower / 1e6,
  Median = biomass_median / 1e6,
  Upper = biomass_upper / 1e6
)

##plotting projected biomass
ggplot(ci_df, aes(Year)) +
  
  geom_ribbon(
    aes(
      ymin = Lower,
      ymax = Upper
    ),
    fill = "#A6DBA0",
    alpha = 0.4
  ) +
  
  geom_line(
    aes(y = Median),
    colour = "#1B7837",
    linewidth = 1.5
  ) +
  
  theme_classic(base_size = 14) +
  
  labs(
    x = "Year",
    y = "Biomass (million tonnes)"
  )


###biomass summary table to show projected biomass growth every 15 years
biomass_table <- data.frame(
  
  Year = c(
    2025,
    2040,
    2055
  )
  
)

rows <- match(
  biomass_table$Year,
  years
)

biomass_table$Lower95 <- round(
  biomass_lower[rows] / 1e6,
  3
)

biomass_table$Median <- round(
  biomass_median[rows] / 1e6,
  3
)

biomass_table$Upper95 <- round(
  biomass_upper[rows] / 1e6,
  3
)

biomass_table


##create publishable table
biomass_table |>
  gt() |>
  tab_header(
    title = md("**Projected Deer Biomass in Australia**"),
    subtitle = "Million tonnes, median estimates with 95% uncertainty intervals"
  ) |>
  fmt_number(
    columns = c(
      Lower95,
      Median,
      Upper95
    ),
    decimals = 3
  ) |>
  cols_label(
    Year = "Year",
    Lower95 = "Lower 95% UI",
    Median = "Median",
    Upper95 = "Upper 95% UI"
  )

# ==========================================
# COST FUNCTIONS
# ==========================================

##calculation of grazing loss
grazing_cost <- function(
    biomass,
    farmland_prop,
    dse_per_tonne,
    gross_margin_dse
){
  
  biomass *
    farmland_prop *
    dse_per_tonne *
    gross_margin_dse
  
}

##calculation of farmour labour costs
management_cost <- function(
    n_farms,
    prop_affected,
    days,
    hours_day,
    wage
){
  
  affected <- n_farms * prop_affected
  
  affected *
    days *
    hours_day *
    wage
  
}

##calculation of lost forestry yield costs
forestry_cost <- function(
    production_m3,
    prop_affected,
    yield_loss,
    price_m3
){
  
  production_m3 *
    prop_affected *
    yield_loss *
    price_m3
  
}

# ==========================================
# CONSTANTS
# ==========================================

##farmour labour assumptions
management_annual <- management_cost(
  n_farms = 65000,
  prop_affected = 0.10,
  days = 20,
  hours_day = 8,
  wage = 40
)

##lost forestry yield costs assumptions 
forestry_annual <- forestry_cost(
  production_m3 = 22864000,
  prop_affected = 0.40,
  yield_loss = 0.055,
  price_m3 = 100.5
)

crash_rate <- 300 / 1001385

cost_per_crash <- 20500

# ==========================================
# PRESENT VALUE FUNCTION
# ==========================================

pv <- function(
    costs,
    years,
    discount_rate
){
  
  sum(
    costs /
      (1 + discount_rate)^(
        years - min(years)
      )
  )
}

# ==========================================
# STORAGE OBJECTS
# ==========================================

grazing_pv_4 <- numeric(n_sim)
grazing_pv_7 <- numeric(n_sim)

management_pv_4 <- numeric(n_sim)
management_pv_7 <- numeric(n_sim)

forestry_pv_4 <- numeric(n_sim)
forestry_pv_7 <- numeric(n_sim)

collision_pv_4 <- numeric(n_sim)
collision_pv_7 <- numeric(n_sim)

# ==========================================
# ECONOMIC MONTE CARLO based on upper median and lower biomass estimates
# ==========================================

for(i in seq_len(n_sim)){
  
  biomass <- biomass_sims[,i]

#weighted calculation for abundance 
  abundance <-
    (biomass * 1000) / 184.850

## grazing cost assumptions
  grazing <- grazing_cost(
    biomass = biomass,
    farmland_prop = 0.10,
    dse_per_tonne = 16.23,
    gross_margin_dse = 23.5
  )

#projection of future costs using projected biomass

  #farmour labour management
  management <-
    management_annual *
    (biomass / biomass[1])
  
  #loss forestry yield costs
  forestry <-
    forestry_annual *
    (biomass / biomass[1])
  
  #deer-vehicle collision costs
  collisions <-
    abundance *
    crash_rate *
    cost_per_crash
  
  ##calculation 4% and 7% discount rate 
  grazing_pv_4[i] <- pv(
    grazing,
    years,
    0.04
  )
  
  grazing_pv_7[i] <- pv(
    grazing,
    years,
    0.07
  )
  
  management_pv_4[i] <- pv(
    management,
    years,
    0.04
  )
  
  management_pv_7[i] <- pv(
    management,
    years,
    0.07
  )
  
  forestry_pv_4[i] <- pv(
    forestry,
    years,
    0.04
  )
  
  forestry_pv_7[i] <- pv(
    forestry,
    years,
    0.07
  )
  
  collision_pv_4[i] <- pv(
    collisions,
    years,
    0.04
  )
  
  collision_pv_7[i] <- pv(
    collisions,
    years,
    0.07
  )
  
}

# ==========================================
# SUMMARY TABLE
# ==========================================

category_summary <- data.frame(
  
  Category = rep(
    c(
      "Grazing losses",
      "Farmer labour",
      "Forestry losses",
      "Deer-vehicle collisions"
    ),
    2
  ),
  
  DiscountRate = c(
    rep("4%",4),
    rep("7%",4)
  ),

  #calculate 95% uncertainty interval and median for each cost category based on monte carlo simulation
  Lower95 = c(
    quantile(grazing_pv_4,0.025),
    quantile(management_pv_4,0.025),
    quantile(forestry_pv_4,0.025),
    quantile(collision_pv_4,0.025),
    
    quantile(grazing_pv_7,0.025),
    quantile(management_pv_7,0.025),
    quantile(forestry_pv_7,0.025),
    quantile(collision_pv_7,0.025)
  ) / 1e9,
  
  Median = c(
    median(grazing_pv_4),
    median(management_pv_4),
    median(forestry_pv_4),
    median(collision_pv_4),
    
    median(grazing_pv_7),
    median(management_pv_7),
    median(forestry_pv_7),
    median(collision_pv_7)
  ) / 1e9,
  
  Upper95 = c(
    quantile(grazing_pv_4,0.975),
    quantile(management_pv_4,0.975),
    quantile(forestry_pv_4,0.975),
    quantile(collision_pv_4,0.975),
    
    quantile(grazing_pv_7,0.975),
    quantile(management_pv_7,0.975),
    quantile(forestry_pv_7,0.975),
    quantile(collision_pv_7,0.975)
  ) / 1e9
  
)

print(category_summary)

# ==========================================
# PRESENT VALUE PLOT of total projected cost
# ==========================================

ggplot(
  category_summary,
  aes(
    x = Category,
    y = Median,
    fill = DiscountRate
  )
) +
  
  geom_col(
    position = position_dodge(0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = Lower95,
      ymax = Upper95
    ),
    width = 0.2,
    position = position_dodge(0.8)
  ) +
  
  scale_fill_manual(
    values = c(
      "4%" = "olivedrab4",
      "7%" = "magenta4"
    )
  ) +
  
  theme_classic(base_size = 14) +
  
  labs(
    x = NULL,
    y = "Present value (AU$ billion)",
    fill = "Discount rate"
  ) +
  
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold"
    )
  )


## ==========================================
## STORAGE MATRICES create empty containers to store results from every Monte Carlo simulation
## ==========================================

grazing_cum_4 <- matrix(NA, length(years), n_sim)
grazing_cum_7 <- matrix(NA, length(years), n_sim)

management_cum_4 <- matrix(NA, length(years), n_sim)
management_cum_7 <- matrix(NA, length(years), n_sim)

forestry_cum_4 <- matrix(NA, length(years), n_sim)
forestry_cum_7 <- matrix(NA, length(years), n_sim)

collision_cum_4 <- matrix(NA, length(years), n_sim)
collision_cum_7 <- matrix(NA, length(years), n_sim)

## ==========================================
## CUMULATIVE PV FOR EACH SIMULATION 
## ==========================================

#(runs through each of the 1000 biomass simulations )
for(i in seq_len(n_sim)){
  
#extracts biomass trajectory 
  biomass <- biomass_sims[, i]
  
#calculate abundnace (convert biomass into number of deer)
  abundance <-
    (biomass * 1000) / 184.85

  #calcuate annual grazing cost (must be here as it uses biomass as part of the baseline equation)
  grazing <- grazing_cost(
    biomass = biomass,
    farmland_prop = 0.10,
    dse_per_tonne = 16.23,
    gross_margin_dse = 23.5
  )

  #calculate annual labour costs (scale baseline labour costs accourding to biomass growth)
  management <-
    management_annual *
    (biomass / biomass[1])
  
  #calculate annual forestry losses (same logic as labour costs) 
  forestry <-
    forestry_annual *
    (biomass / biomass[1])
  
  #calculate collision costs 
  collisions <-
    abundance *
    crash_rate *
    cost_per_crash
  
  #conver annual costs inmto cummulative present value (4% and 7% discount rate)
  grazing_cum_4[, i] <- cumsum(
    grazing /
      (1 + 0.04)^(years - min(years))
  )
  
  grazing_cum_7[, i] <- cumsum(
    grazing /
      (1 + 0.07)^(years - min(years))
  )
  
  management_cum_4[, i] <- cumsum(
    management /
      (1 + 0.04)^(years - min(years))
  )
  
  management_cum_7[, i] <- cumsum(
    management /
      (1 + 0.07)^(years - min(years))
  )
  
  forestry_cum_4[, i] <- cumsum(
    forestry /
      (1 + 0.04)^(years - min(years))
  )
  
  forestry_cum_7[, i] <- cumsum(
    forestry /
      (1 + 0.07)^(years - min(years))
  )
  
  collision_cum_4[, i] <- cumsum(
    collisions /
      (1 + 0.04)^(years - min(years))
  )
  
  collision_cum_7[, i] <- cumsum(
    collisions /
      (1 + 0.07)^(years - min(years))
  )
  
}

## ==========================================
## FUNCTION TO BUILD PLOTTING DATA
## ==========================================

#build plotting function
build_df <- function(mat, category, rate){
  
#lower estimate /1e9 converts $ to billions
  data.frame(
    Year = years,
    Lower = apply(
      mat, 1, quantile,
      probs = 0.025
    ) / 1e9,
#median     
    Median = apply(
      mat, 1, median
    ) / 1e9,
#upper    
    Upper = apply(
      mat, 1, quantile,
      probs = 0.975
    ) / 1e9,
    
    Category = category,
    DiscountRate = rate
  )
  
}

## ==========================================
## COMBINE EVERYTHING all results 
## ==========================================

plot_df <- rbind(
  
#stack results together
  build_df(
    grazing_cum_4,
    "Grazing losses",
    "4%"
  ),
  
  build_df(
    grazing_cum_7,
    "Grazing losses",
    "7%"
  ),
  
  build_df(
    management_cum_4,
    "Farmer labour",
    "4%"
  ),
  
  build_df(
    management_cum_7,
    "Farmer labour",
    "7%"
  ),
  
  build_df(
    forestry_cum_4,
    "Forestry losses",
    "4%"
  ),
  
  build_df(
    forestry_cum_7,
    "Forestry losses",
    "7%"
  ),
  
  build_df(
    collision_cum_4,
    "Deer-vehicle collisions",
    "4%"
  ),
  
  build_df(
    collision_cum_7,
    "Deer-vehicle collisions",
    "7%"
  )
  
)

## ==========================================
## FACET PLOT (multi panel figure)
## ==========================================

#generate figure 
ggplot(
  plot_df,
  aes(
    x = Year,
    y = Median,
    colour = DiscountRate,
    fill = DiscountRate
  )
) +
  
  geom_ribbon(
    aes(
      ymin = Lower,
      ymax = Upper
    ),
    alpha = 0.15,
    colour = NA
  ) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  facet_wrap(
    ~ Category,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_colour_manual(
    values = c(
      "4%" = "olivedrab4",
      "7%" = "maroon4"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "4%" = "olivedrab4",
      "7%" = "maroon4"
    )
  ) +
  
  labs(
    x = "Year",
    y = "Cumulative PV (AU$ billion)",
    colour = "Discount rate",
    fill = "Discount rate"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )


#check data (red line should run through the centre)

matplot(
  years,
  biomass_sims[,1:100],
  type = "l",
  lty = 1,
  col = rgb(0,0,0,0.1),
  xlab = "Year",
  ylab = "Biomass"
)

lines(
  years,
  biomass_median,
  lwd = 3,
  col = "red"
)
