################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
  bigrquery,
  lubridate,
  tidyverse
)

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery -------------------------------------------
mex_fisheries <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "mex_fisheries",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

## PROCESSING ##################################################################

# Build two tables with info we need -------------------------------------------
# A table of vessel characteristics
vessel_info <- tbl(mex_fisheries, "vessel_info_v_20230803") %>%
  filter(sql("engine_power_hp IS NOT NULL"),
         !owner_name == "INSTITUTO NACIONAL DE PESCA",
         fleet == "large scale") %>%
  select(vessel_rnpa, finfish, sardine, shark, shrimp, tuna, others, state, contains("vesse"), engine_power_kw)

# A table of vessel activity
mex_vms <- tbl(mex_fisheries, "mex_vms_processed_v_20230419") %>%
  filter(speed > 0,
         between(year, 2018, 2022))  %>%
  mutate(date = sql("EXTRACT(DATE FROM datetime)")) %>%
  group_by(date, vessel_rnpa) %>%
  summarize(hours = sum(hours, na.rm = T))

# Collect the data -------------------------------------------------------------
vessel_info_local <- collect(vessel_info)

# Generate additional variables in-memory
mex_vms_local <- mex_vms %>%
  collect() %>%
  mutate(yday = yday(date),
         month = month(date),
         year = year(date),
         week = week(date)) %>%
  ungroup()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = mex_vms_local,
        file = here("data", "raw", "daily_activity_by_vessel.rds"))
saveRDS(object = vessel_info_local,
        file = here("data", "raw", "mex_vessel_info.rds"))
