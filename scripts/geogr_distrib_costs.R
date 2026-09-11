#load packages
library(maps)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(tmap)
library(purrr)
library(sp)
library(readr)
library(invacost)
library(tidyverse)
library(ozmaps)
library(tools)
library(patchwork)
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
cpi_2017 <- 115.6868
cpi_2025 <- 148.4573

aud_usd_2017 <- 0.7669  # 1 AUD = 0.7669 USD

conv_factor <- (1 / aud_usd_2017) * (cpi_2025 / cpi_2017)

# Add converted costs to dataset
db.over.time <- db.over.time %>%
  mutate(
    AUD_2025 = Cost_estimate_per_year_2017_USD_exchange_rate * conv_factor
  )

#check row and column 
nrow(db.over.time)
ncol(db.over.time)

#aggregate the cummulative regional costs
regional_costs <- aggregate(db.over.time$AUD_2025 ~ db.over.time$Geographic_region, FUN = sum)
print(regional_costs)

#aggregate the cummulative country costs
country_costs <- aggregate(db.over.time$AUD_2025 ~ db.over.time$Official_country, FUN = sum)
print(country_costs)

#create time summary of costs for regions
Regions <- split(
  db.over.time,
  db.over.time$Geographic_region
)

obs.costsregional <- purrr::map(
  Regions,
  ~ summarizeCosts(
    .x,
    cost.column = "AUD_2025",
    minimum.year = 1990,
    maximum.year = 2026,
    year.breaks = seq(1990, 2026, by = 6)
  )
)

#print the regional costs overtime 
obs.costsregional

#create summary plots of countries
Countries <- split(
  db.over.time,
  db.over.time$Official_country
)

obs.costscountry <- purrr::map(
  Countries,
  ~ summarizeCosts(
    .x,
    cost.column = "AUD_2025",
    minimum.year = 1990,
    maximum.year = 2026,
    year.breaks = seq(1990, 2026, by = 6)
  )
)

#print the country costs overtime 
obs.costscountry

#find the geographic regions in data
unique(db.over.time$Geographic_region)

#specify names of regions
db.over.time$Geographic_region <- dplyr::recode(
  db.over.time$Geographic_region,
  "Antarctic-Subantarctic" = "Antarctic_Subantarctic",
  "Oceania/Pacific Islands" = "Oceania_Pacific_Islands"
)

#create region list
Regions <- list(
  Asia = db.over.time[db.over.time$Geographic_region %in% "Asia", ],
  Europe = db.over.time[db.over.time$Geographic_region %in% "Europe", ],
  `Oceania` = db.over.time[db.over.time$Geographic_region %in% "Oceania", ],
  `Antarctic_Subantarctic` = db.over.time[db.over.time$Geographic_region %in% "Antarctic_Subantarctic", ],
  `Oceania_Pacific_Islands` = db.over.time[db.over.time$Geographic_region %in% "Oceania_Pacific_Islands", ]
)

#summerise regional costs overtime
obs.regions <- map(Regions, 
                   summarizeCosts,
                   cost.column = "AUD_2025",
                   minimum.year = 1990,
                   maximum.year = 2026)

# Download the data here from natural earth data, 10m resolution
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_map_units.zip
# Map units, admin 0: https://www.naturalearthdata.com/downloads/10m-cultural-vectors/

#disable the S2 spherical geometry engine for unprojected geographic (longitude/latitude) coordinates
sf_use_s2(FALSE)

#Read Natural Earth shapefile
invacost.world <- read_sf("ne_10m_admin_0_countries.shp") 
  
  #Create region field
  invacost.world$Region <- as.character(invacost.world$CONTINENT)
  
  #Match names used in cost database
  invacost.world$Region <- dplyr::recode(
    invacost.world$Region,
    "Antarctica" = "Antarctic_Subantarctic"
  )
  
  #Dissolve countries into regions
  invacost.world <- invacost.world |>
    dplyr::select(Region) |>
    st_make_valid() |>
    dplyr::group_by(Region) |>
    dplyr::summarise()
  
  #Check result
  print(names(invacost.world))
  print(invacost.world$Region)
  
  #Create region summary table
  #create dataframe
  region_summary <- data.frame()
  
  for(region in unique(db.over.time$Geographic_region))
  {
    subdb <- db.over.time[
      db.over.time$Geographic_region == region,
    ] 
    if(nrow(subdb) > 0)
    { 
      curestimate <- summarizeCosts(
        subdb,
        cost.column = "AUD_2025",
        minimum.year = 1990,
        maximum.year = 2026
      ) 
      
      region_summary <- rbind(
        region_summary,
        data.frame(
          Region = region,
          nb.estimates =
            curestimate$average.total.cost$number_estimates,
          cumulated.cost =
            curestimate$average.total.cost$total_cost,
          average.annual.cost =
            curestimate$average.total.cost$annual_cost,
          average.annual.cost.1990.2026 =
            curestimate$average.cost.per.period$annual_cost[
              curestimate$average.cost.per.period$initial_year == 1990
            ]
        )
      )
    }
  }
  
  # Join summaries to map
  invacost.world <- dplyr::left_join(
    invacost.world,
    region_summary,
    by = "Region"
  )
  
  # Check results
  summary(invacost.world$cumulated.cost)
  summary(invacost.world$nb.estimates)
  
  # Cost map
  tm_shape(
    invacost.world,
    projection = "+proj=eck4"
  ) +
    tm_polygons(
      "cumulated.cost",
      palette = "OrRd",
      style = "pretty",
      title = "Cost in 2025 AUD"
    ) +
    tm_layout(
      legend.bg.color = "white",
      frame = FALSE
    )
  
  
#Australian distribution
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
  
  #change variable name
  names(db.over.timeAUS)[names(db.over.timeAUS) == "State|Province|Administrative_area"] <- "State"
  
  #create table of state names
  table(db.over.timeAUS$State)
  
  #Let's see some columns to see if worked 
  print(db.over.timeAUS$State)
  
  ##change state names to fit analysis / map
  db.over.timeAUS$State <- revalue(db.over.timeAUS$State, c("Tas" = "Tasmania",
                                            "SA" = "South Australia"))
  
  db.over.timeAUS$State[is.na(db.over.timeAUS$State)] <- "Diverse/Unspecified"
  
  
  print(db.over.timeAUS$State)
  
  
  ##plot costs overtime
  obs.costsAUS <- summarizeCosts(db.over.timeAUS,
                                      cost.column = "AUD_2025",
                                      minimum.year = 2010,
                                      maximum.year = 2026,
                                      year.breaks = seq(2010, 2026, by = 9))
  
  obs.costsAUS
  
  #load Australian map into R
  ozmap_states
  
  # Sum costs by state
  names(ozmap_states)[names(ozmap_states) == "NAME"] <- "State"
  
  #calculate total deer-related costs for each Australian state
  state_costs <- db.over.timeAUS %>%
    dplyr::group_by(State) %>%
    dplyr::summarise(
      TotalCost = sum(
        AUD_2025,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  #inspect structure and values 
  str(state_costs)
  print(state_costs)
  
  #rename unspecified records to match map layer category 
  state_costs$State[state_costs$State == "Diverse/Unspecified"] <-
    "Other Territories"
  
  print(state_costs)
  
  #Join cost data to Australian state polygons
  map_data <- left_join(
    ozmap_states,
    state_costs,
    by = "State"
  )
  
  #plot spatial distribution of total costs by state
  ggplot(map_data) +
    geom_sf(aes(fill = TotalCost), colour = "white") +
    scale_fill_viridis_c(
      trans = "log10",
      labels = label_comma(),
      na.value = "grey90"
    ) +
    coord_sf() +
    labs(
      x = "Longitude",
      y = "Latitude",
      fill = "Total Cost (AUD 2025)"
    ) +
    theme_minimal()
  
#calculate number of cost estimates reported for each state 
  state_counts <- db.over.timeAUS %>%
    group_by(State) %>%
    summarise(
      n_estimates = n_distinct(Cost_ID),
      .groups = "drop"
    )
  
  names(db.over.timeAUS)[
    names(db.over.timeAUS) == "State|Province|Administrative_area"
  ] <- "State"
    

  #replace missing state names and combine counts
  state_counts <- db.over.timeAUS %>%
    dplyr::group_by(State) %>%
    dplyr::summarise(
      n_estimates = dplyr::n_distinct(Cost_ID),
      .groups = "drop"
    )
  
  #inspect estimate counts
  str(state_counts)
  print(state_counts)
  
  # Create cost classes
  map_data <- map_data %>%
    mutate(
      CostClass = cut(
        TotalCost,
        breaks = c(-Inf, 5e6, 10e6, 20e6, Inf),
        labels = c(
          "< $5M",
          "$5-10M",
          "$10-20M",
          "> $20M"
        )
      )
    )
  

  #Map of australian cost with the number of estimates alongside
  p_map <- ggplot(map_data) +
    geom_sf(
      aes(fill = CostClass),
      colour = "white",
      linewidth = 0.8
    ) +
    scale_fill_manual(
      values = c(
        "< $5M"   = "#41AB5D",
        "$5-10M"  = "#238B45",
        "$10-20M" = "#006D2C",
        "> $20M"  = "#00441B"
      )
    ) +
    coord_sf(expand = FALSE) +
    labs(
      title = "Documented invasive deer costs",
      fill = "Total cost\n(2025 AU$)"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16,
        hjust = 0
      ),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(5, 5, 5, 5)
    )
  

  #Bar chart of the number of estimates
  p_bar <- state_counts %>%
    arrange(n_estimates) %>%
    ggplot(
      aes(
        y = reorder(State, n_estimates),
        x = n_estimates
      )
    ) +
    geom_col(
      fill = "#D73027",
      width = 0.7
    ) +
    geom_text(
      aes(label = n_estimates),
      hjust = -0.3,
      size = 4
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = "Number of cost estimates",
      x = "Estimates",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10),
      plot.margin = margin(5, 5, 5, 5)
    )
  

  # Combine panels (map and number of estimates)
  final_plot <- p_map + p_bar +
    plot_layout(widths = c(1.25, 1))
  
  final_plot
