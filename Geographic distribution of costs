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

#load data
data <- read_csv("Cleaned Additional data points 1-09.csv")
#turn to dataframe
data_df <- as.data.frame(data)
#print the data
View(data_df)

#check for duplicates
data_df$Cost_ID[duplicated(data$Cost_ID)]

#expand data
db.over.time <- expandYearlyCosts(
  data_df,
  startcolumn = "Probable_starting_year_adjusted",
  endcolumn = "Probable_ending_year_adjusted"
)

#convert from 2017 USD to aus 2026 costs
conv_factor <- 1.3047 * (101.3 / 111.175)

#add the converted column to whole dataset 
db.over.time <- db.over.time %>%
  mutate(
    AUD.2026.dollar.conversions =
      Cost_estimate_per_year_2017_USD_exchange_rate * conv_factor
  )

#check row and column 
nrow(db.over.time)
ncol(db.over.time)

#aggregate the cummulative regional costs
regional_costs <- aggregate(db.over.time$AUD.2026.dollar.conversions ~ db.over.time$Geographic_region, FUN = sum)
print(regional_costs)

#aggregate the cummulative country costs
country_costs <- aggregate(db.over.time$AUD.2026.dollar.conversions ~ db.over.time$Official_country, FUN = sum)
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
    cost.column = "AUD.2026.dollar.conversions",
    minimum.year = 2004,
    maximum.year = 2026,
    year.breaks = seq(2004, 2026, by = 6)
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
    cost.column = "AUD.2026.dollar.conversions",
    minimum.year = 2004,
    maximum.year = 2026,
    year.breaks = seq(2004, 2026, by = 6)
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
                   cost.column = "AUD.2026.dollar.conversions",
                   minimum.year = 2004,
                   maximum.year = 2026)

# Download the data here from natural earth data, 10m resolution
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_map_units.zip
# Map units, admin 0: https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
# World region map
#disable the S2 spherical geometry engine for unprojected geographic (longitude/latitude) coordinates
sf_use_s2(FALSE)

#Read Natural Earth shapefile
invacost.world <- read_sf(

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
      cost.column = "AUD.2026.dollar.conversions",
      minimum.year = 2004,
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
        average.annual.cost.2004.2026 =
          curestimate$average.cost.per.period$annual_cost[
            curestimate$average.cost.per.period$initial_year == 2004
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
    title = "Cost in 2026 AUD"
  ) +
  tm_layout(
    legend.bg.color = "white",
    frame = FALSE
  )

# Cost + estimates map
tm_shape(
  invacost.world,
  crs = "+proj=eck4"
) +
  tm_polygons(
    "cumulated.cost",
    palette = "OrRd",
    style = "pretty",
    title = "Cost in 2026 AUD"
  ) +
  tm_symbols(
    size = "nb.estimates",
    col = "steelblue",
    alpha = 0.4,
    scale = 1.5
  ) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE
  )

#Australian distribution
unique(data_df$Official_country)
dataAUS <- data_df[which(data_df$Official_country == "Australia"), ]
#check rows 
nrow(dataAUS)
#create table of state names
table(dataAUS$State)

#change variable name
names(dataAUS)[names(dataAUS) == "State|Province|Administrative_area"] <- "State"
library(plyr)
#Let's see some columns to see if worked 
print(dataAUS$State)

##change state names to fit analysis / map
dataAUS$State <- revalue(dataAUS$State, c("Tas" = "Tasmania",
                                          "SA" = "South Australia"))

dataAUS$State[is.na(dataAUS$State)] <- "Diverse/Unspecified"


print(dataAUS$State)

##expand aus costs overtimr
db.over.timeAUS <- expandYearlyCosts(
  dataAUS,
  startcolumn = "Probable_starting_year_adjusted",
  endcolumn = "Probable_ending_year_adjusted")

##aud 2026 conversion calc
conv_factor <- 1.3047 * (101.3 / 111.175)

##add conversion to results
db.over.timeAUS <- db.over.timeAUS %>%
  mutate(
    AUD.2026.dollar.conversions =
      Cost_estimate_per_year_2017_USD_exchange_rate * conv_factor
  )

#check conversions worked
db.over.timeAUS %>%
  filter(Cost_ID == "FD61") %>%
  summarise(
    years = n(),
    annual_aud = first(AUD.2026.dollar.conversions),
    total_aud = sum(AUD.2026.dollar.conversions)
  )

##plot costs overtime
obs.costsAUSstate <- summarizeCosts(db.over.timeAUS,
                                    cost.column = "AUD.2026.dollar.conversions",
                                    minimum.year = 2010,
                                    maximum.year = 2026,
                                    year.breaks = seq(2010, 2026, by = 9))

obs.costsAUSstate
ozmap_states

# Sum costs by state
names(ozmap_states)[names(ozmap_states) == "NAME"] <- "State"

#calculate total deer-related costs for each Australian state
state_costs <- db.over.timeAUS %>%
  dplyr::group_by(State) %>%
  dplyr::summarise(
    TotalCost = sum(
      AUD.2026.dollar.conversions,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

#replace missing state names and combine any duplicated entires
state_costs <- state_costs %>%
  dplyr::mutate(
    State = dplyr::if_else(
      is.na(State),
      "Diverse/Unspecified",
      State
    )
  ) %>%
  dplyr::group_by(State) %>%
  dplyr::summarise(
    TotalCost = sum(TotalCost),
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
    fill = "Total Cost (USD)"
  ) +
  theme_minimal()

#calculate number of cost estimates reported for each state 
state_counts <- db.over.timeAUS %>%
  dplyr::group_by(State) %>%
  dplyr::summarise(n_estimates = dplyr::n())

#replace missing state names and combine counts
state_counts <- state_counts %>%
  dplyr::mutate(
    State = dplyr::if_else(
      is.na(State),
      "Diverse/Unspecified",
      State
    )
  ) %>%
  dplyr::group_by(State) %>%
  dplyr::summarise(
    n_estimates = sum(n_estimates),
    .groups = "drop"
  )

#inspect estimate counts
str(state_counts)
print(state_counts)

#rename category to match map layer 
state_counts$State[state_counts$State == "Diverse/Unspecified"] <-
  "Other Territories"

#Join total costs and numbers of estimates to state polygons
map_data <- ozmap_states %>%
  left_join(state_costs, by = "State") %>%
  left_join(state_counts, by = "State")

#check attribute names
names(map_data)

#create centroid points for eacg state ploygon
centroids <- st_centroid(map_data)
#check centroid attributes
names(centroids)

#plot state costs and numbers of estimates simultaneiously 
ggplot(map_data) +
  #state polygons colpured by total cost
  geom_sf(aes(fill = TotalCost), colour = "white") +
  #centroid bubbles scaled by number of estimates
  geom_sf(
    data = centroids,
    aes(size = n_estimates),
    shape = 21,
    fill = "red",
    colour = "black",
    alpha = 0.8
  ) +
  #labels showing exact estimates counts
  geom_sf_text(
    data = centroids,
    aes(label = n_estimates),
    size = 3
  ) +
  #colour scale for total costs
  scale_fill_viridis_c(
    labels = function(x) format(x, scientific = FALSE, big.mark = ","),
    na.value = "grey90"
  ) +
  #bubble size legend
  scale_size_area(
    name = "Number of estimates",
    max_size = 15
  ) +
  coord_sf() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Total Cost (AUD 2026)"
  ) +
  theme_minimal()

#compare state names between datasets
sort(unique(state_counts$State))
sort(unique(map_data$State))

#--------------------------------------------------
# Create cost classes
#--------------------------------------------------

map_data <- map_data %>%
  mutate(
    CostClass = cut(
      TotalCost,
      breaks = c(0, 5e6, 10e6, 20e6, Inf),
      labels = c(
        "< $5M",
        "$5-10M",
        "$10-20M",
        "> $20M"
      )
    )
  )

#----------------------------------------------------------------
# Map of australian cosst with the number of estimates alongside
#---------------------------------------------------------------

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
    fill = "Total cost\n(2026 AU$)"
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

#--------------------------------------------------
# Bar chart of the number of estimates
#--------------------------------------------------

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

#--------------------------------------------------
# Combine panels (map and number of estimates)
#--------------------------------------------------

final_plot <- p_map + p_bar +
  plot_layout(widths = c(1.25, 1))

final_plot
