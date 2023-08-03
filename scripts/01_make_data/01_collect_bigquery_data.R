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
vessel_info <- tbl(mex_fisheries, "vessel_info_v_20221104") %>%
  filter(sql("engine_power_hp IS NOT NULL"),
         sql("sardine IS NOT NULL AND shrimp IS NOT NULL AND tuna IS NOT NULL AND others IS NOT NULL")) %>%
  select(vessel_rnpa, tuna, sardine, shrimp, others, state, contains("vesse"), engine_power_hp)

# A table of vessel activity
mex_vms <- tbl(mex_fisheries, "mex_vms_processed_v_20230419") %>%
  filter(speed > 0,
         between(year, 2019, 2022))  %>%
  mutate(date = sql("EXTRACT(DATE FROM datetime)")) %>%
  group_by(date, vessel_rnpa) %>%
  summarize(hours = sum(hours, na.rm = T))

# Collect the data -------------------------------------------------------------
combined <- inner_join(mex_vms, vessel_info, by = "vessel_rnpa") %>%
  collect()

# Generate additional variables in-memory
mex_vms_local <- combined %>%
  mutate(yday = yday(date),
         month = month(date),
         year = year(date),
         week = week(date)) %>%
  ungroup()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = mex_vms_local,
        file = here("data", "raw", "daily_activity_by_vessel.rds"))
