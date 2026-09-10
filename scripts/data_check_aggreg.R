#Load packages 
library(invacost)
library(readr)
library(dplyr)

#load data
data(invacost)

#check number of current estimates
nrow(invacost)
ncol(invacost)

##exclude all data that is not part of the cervidae family
unique(invacost$Family)
invacost.cervid <- invacost[which(invacost$Family == "Cervidae"), ]

#check the number of rows
nrow(invacost.cervid)

#print data and save as csv to manually check datapoints
write.csv(invacost.cervid, "invacostv4.1 cervidae datapoints only.csv", row.names = FALSE)
#open csv
file.show("invacostv4.1 cervidae datapoints only.csv")

##filter out points with missing information
if(any(is.na(invacost.cervid$Cost_estimate_per_year_2017_USD_exchange_rate)))
{
  invacost.cervid <- invacost.cervid[-which(is.na(invacost.cervid$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
}

#check the number of rows (cost estimates)
nrow(invacost.cervid)

#check for points with Uncertain starting periods (without beginning or end date)
uncertain.starts <- invacost.cervid[which(invacost.cervid$Time_range == "Period" &
                                            is.na(invacost.cervid$Probable_starting_year)), ]

#print the number of estimates without adequate information about starting year
nrow(uncertain.starts)

#print any estimated with no info about whether cost was annual or over a period
unknown.periods <- invacost.cervid[which(is.na(invacost.cervid$Time_range)), ]
# Number of estimates without adequate information about period
nrow(unknown.periods) 

#check methods and reliability of cost estimates by printing of table containing information
table(invacost.cervid$Acquisition_method, invacost.cervid$Implementation)

#remove cost estimates with low reliability and potential
invacost.cervid <- invacost.cervid[which(invacost.cervid$Method_reliability == "High"), ]
invacost.cervid <- invacost.cervid[which(invacost.cervid$Implementation == "Observed"), ]

#check the number of rows after filtering
nrow(invacost.cervid)

#data check for additional cost estimates found
#load data
Additional<- read_csv("data/Additional Cost estimates for invasive cervidae 9-09.csv")

#check number of current cost estimates within dataset
nrow(Additional)
ncol(Additional)

#filter out points with missing information
if(any(is.na(Additional$Cost_estimate_per_year_2017_USD_exchange_rate)))
{
  Additional <- Additional[-which(is.na(Additional$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
}

#check the number of rows (cost estimates)
nrow(Additional)

#check for points with Uncertain starting periods
uncertain.startsadditional <- Additional[which(Additional$Time_range == "Period" &
                                                 is.na(Additional$Probable_starting_year)), ]
# Number of estimates without adequate information about starting year
nrow(uncertain.startsadditional)

#remove any points that include no info about whether cost was annual or over a period
unknown.periodsadditional <- Additional[which(is.na(Additional$Time_range)), ]
# Number of estimates without adequate information about period
nrow(unknown.periodsadditional) 

#check methods and reliability of cost estimates
table(Additional$Acquisition_method, Additional$Implementation)

#remove cost estimates with low reliability and potential
Additional <- Additional[which(Additional$Method_reliability == "High"), ]
Additional <- Additional[which(Additional$Implementation == "Observed"), ]

#find the number of rows after filtering
nrow(Additional)

#print data to manually check datapoints
write.csv(Additional, "Additional cervidae data points filtered.csv", row.names = FALSE)
#open csv file
file.show("Additional cervidae data points filtered.csv")

#aggregate both datasets 
#check variable names are the same 
setdiff(names(Additional), names(invacost.cervid))
setdiff(names(invacost.cervid), names(Additional))

#change data set names to match
Additional <- Additional %>%
  rename(
    HabitatVerbatim = Habitat_verbatim,
    protectedArea = ProtectedArea,
    Raw_cost_estimate_original_currency =
      Raw_cost_estimate_local_currency,
    Min_Raw_cost_estimate_original_currency =
      Min_Raw_cost_estimate_local_currency,
    Max_Raw_cost_estimate_original_currency =
      Max_Raw_cost_estimate_local_currency,
    Cost_estimate_per_year_original_currency =
      Cost_estimate_per_year_local_currency,
    Method_reliability_Explanation =
      Method_reliability_refined_Explanation,
    Method_reliability_Expert_Name =
      Method_reliability_refined_Expert_Name,
    Overlap = Overlaps,
    `Initial contributors_names` =
      Initial_contributors_names
  )

#check again if variable names now match
setdiff(names(Additional), names(invacost.cervid))
setdiff(names(invacost.cervid), names(Additional))

#remove variables that are not included in both dataset 
invacost.cervid <- invacost.cervid %>%
  select(-InvaCost_ID)

Additional <- Additional %>%
  select(-Initial_contributors_emails)

#check again if all variable names are matching
setdiff(names(Additional), names(invacost.cervid))
setdiff(names(invacost.cervid), names(Additional))

#check again if all variable names are matching
class(Additional$Max_Raw_cost_estimate_original_currency)
class(invacost.cervid$Max_Raw_cost_estimate_original_currency)

#fix numeric column to be correctly labled as numerical
invacost.cervid$Max_Raw_cost_estimate_original_currency <-
  as.numeric(invacost.cervid$Max_Raw_cost_estimate_original_currency)

#check that the numeric column is now correctly labeled
class(invacost.cervid$Max_Raw_cost_estimate_original_currency)

#combine the original invacost dataset that has been filtered and the additional cervidae cost dataset that was also filtered 
invacost.aggcervidae <- bind_rows(Additional, invacost.cervid)

##print data to manually check datapoints
write.csv(invacost.aggcervidae, "Aggregated cervidae data filtered.csv", row.names = FALSE)
file.show("Aggregated cervidae data filtered.csv")

##remove data points after manual check 117 (sc3299 duplicate) 113 (sc3091 horse/donkey)
cleaned_df <- subset(
  invacost.aggcervidae,
  !(Cost_ID %in% c("SC3299", "SC3091"))
)

##check rows and columns
nrow(invacost.aggcervidae)
ncol(invacost.aggcervidae)

##check rows and columns
nrow(cleaned_df)
ncol(cleaned_df)

#check for duplicates
cleaned_df$Cost_ID[duplicated(cleaned_df$Cost_ID)]

#print data again to manually check data points
write.csv(cleaned_df, "Cleaned Additional cervidae data points.csv", row.names = FALSE)
file.show("Cleaned Additional cervidae data points.csv")
