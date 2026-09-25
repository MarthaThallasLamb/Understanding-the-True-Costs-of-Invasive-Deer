#load packages
library(ggplot2)
library(gt)
library(tidyr)
library(dplyr)
library(patchwork)

# ==========================================
# Total invasive deer costs
# ==========================================
total_pv_4 <- grazing_pv_4 +
  management_pv_4 +
  forestry_pv_4 +
  collision_pv_4

total_pv_7 <- grazing_pv_7 +
  management_pv_7 +
  forestry_pv_7 +
  collision_pv_7

# ========================================================================================================================
# Estimated Australian invasive deer eradication costs based on assumptions of cost per deer and and average deer weight
# ========================================================================================================================

# Input average deer wight of 184 kg based on weighted calculation
mean_deer_mass <- 184

# Convert biomass to abundance (184 kg based on weighted calculation)
abundance_sims <- (biomass_sims * 1000) / mean_deer_mass

# 2025 abundance only
abundance_2025 <- abundance_sims[1, ]

# Cost per deer based on (Bengsen et al. 2023) aierel culling effectivness analysis. 
cost_per_deer <- 309

# Eradication cost simulations
eradication_cost_sim <- abundance_2025 * cost_per_deer

# No discounting required as we are looking at 2025 costs
PV_cost_4_sim <- eradication_cost_sim
PV_cost_7_sim <- eradication_cost_sim

#print summary of estimated eradication costs
summary_costs <- c(
  Mean = mean(eradication_cost_sim),
  Median = median(eradication_cost_sim),
  Lower95 = quantile(eradication_cost_sim, 0.025),
  Upper95 = quantile(eradication_cost_sim, 0.975)
)

#print summary costs in the millions
summary_costs / 1e6

# ==========================================
# TABLE 1
# PROJECTED DAMAGES VS ERADICATION COSTS
# ==========================================
econ_table <- data.frame(
  
  Estimate = c(
    "Lower 95% UI",
    "Median",
    "Upper 95% UI"
  ),
  
  PV_Damages_4 = round(
    c(
      quantile(total_pv_4, 0.025),
      median(total_pv_4),
      quantile(total_pv_4, 0.975)
    ) / 1e9,
    2
  ),
  
  PV_Damages_7 = round(
    c(
      quantile(total_pv_7, 0.025),
      median(total_pv_7),
      quantile(total_pv_7, 0.975)
    ) / 1e9,
    2
  ),
  
  Eradication_Cost = round(
    c(
      quantile(eradication_cost_sim, 0.025),
      median(eradication_cost_sim),
      quantile(eradication_cost_sim, 0.975)
    ) / 1e6,
    2
  ),
  
  Damage_Cost_Ratio_4 = round(
    quantile(
      total_pv_4 / eradication_cost_sim,
      c(0.025, 0.50, 0.975)
    ),
    0
  ),
  
  Damage_Cost_Ratio_7 = round(
    quantile(
      total_pv_7 / eradication_cost_sim,
      c(0.025, 0.50, 0.975)
    ),
    0
  )
  
)

#plot table
econ_table |>
  gt() |>
  cols_label(
    Estimate = "Estimate",
    PV_Damages_4 = "PV damages (4%, A$B)",
    PV_Damages_7 = "PV damages (7%, A$B)",
    Eradication_Cost = "Eradication cost (A$M)",
    Damage_Cost_Ratio_4 = "Damage:cost ratio (4%)",
    Damage_Cost_Ratio_7 = "Damage:cost ratio (7%)"
  )

# =========================
# DAMAGES
# =========================

damage_plot <- data.frame(
  Scenario = c("4%", "7%"),
  Lower = c(
    econ_table$PV_Damages_4[1],
    econ_table$PV_Damages_7[1]
  ),
  Median = c(
    econ_table$PV_Damages_4[2],
    econ_table$PV_Damages_7[2]
  ),
  Upper = c(
    econ_table$PV_Damages_4[3],
    econ_table$PV_Damages_7[3]
  )
)

p1 <- ggplot(
  damage_plot,
  aes(
    x = Scenario,
    y = Median
  )
) +
  geom_col(
    fill = "grey40",
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = Lower,
      ymax = Upper
    ),
    width = 0.15
  ) +
  labs(
    x = "Discount rate",
    y = "AUD 2025 Billions"
  ) +
  theme_bw()

# =========================
# ERADICATION COST
# =========================

cost_plot <- data.frame(
  Scenario = "2025",
  Lower = econ_table$Eradication_Cost[1],
  Median = econ_table$Eradication_Cost[2],
  Upper = econ_table$Eradication_Cost[3]
)

p2 <- ggplot(
  cost_plot,
  aes(
    x = Scenario,
    y = Median
  )
) +
  geom_col(
    fill = "olivedrab4",
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = Lower,
      ymax = Upper
    ),
    width = 0.15
  ) +
  labs(
    x = NULL,
    y = "AUD 2025 Millions"
  ) +
  theme_bw()

# =========================
# DAMAGE:COST RATIO
# =========================

ratio_plot <- data.frame(
  Scenario = c("4%", "7%"),
  Lower = c(
    econ_table$Damage_Cost_Ratio_4[1],
    econ_table$Damage_Cost_Ratio_7[1]
  ),
  Median = c(
    econ_table$Damage_Cost_Ratio_4[2],
    econ_table$Damage_Cost_Ratio_7[2]
  ),
  Upper = c(
    econ_table$Damage_Cost_Ratio_4[3],
    econ_table$Damage_Cost_Ratio_7[3]
  )
)

p3 <- ggplot(
  ratio_plot,
  aes(
    x = Scenario,
    y = Median
  )
) +
  geom_col(
    fill = "#2a9d8f",
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = Lower,
      ymax = Upper
    ),
    width = 0.15
  ) +
  labs(
    x = "Discount rate",
    y = "Damage:Cost Ratio"
  ) +
  theme_bw()

(p1 | p2 | p3)

# ===========================================================
# Figure 2
# Avoided damages based on eradication effectiveness 
# ===========================================================

effectiveness <- c(
  0.25,
  0.50,
  0.75,
  1.00
)

PV_Damages_Median <- median(total_pv_4)

cba_table <- data.frame(
  
  Effectiveness = c(
    "25%",
    "50%",
    "75%",
    "100%"
  ),
  
  Benefits = PV_Damages_Median * effectiveness,
  
  Costs = median(PV_cost_4_sim)
  
)

cba_table$NPV <-
  cba_table$Benefits -
  cba_table$Costs

cba_table$BCR <-
  cba_table$Benefits /
  cba_table$Costs

cba_table$ROI <-
  ((cba_table$Benefits -
      cba_table$Costs) /
     cba_table$Costs) * 100

cba_table <- cba_table |>
  mutate(
    Benefits_B = round(
      Benefits / 1e9,
      2
    ),
    Costs_M = round(
      Costs / 1e6,
      2
    ),
    NPV_B = round(
      NPV / 1e9,
      2
    ),
    BCR = round(
      BCR,
      1
    ),
    ROI = round(
      ROI,
      0
    )
  )

#plot figure 2 
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
    ~ Metric,
    scales = "free_y",
    ncol = 2
  ) +
  theme_bw()
