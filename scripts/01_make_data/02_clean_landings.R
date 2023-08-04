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

# Load data --------------------------------------------------------------------
mex_vms_local <- readRDS(file = here("data", "raw", "daily_activity_by_vessel.rds"))
raw_land <- readRDS(file = here("data", "raw", "mex_annual_landings_by_vessel.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
# Vessels for which we have tracking data
tracking_eus <- sort(unique(mex_vms_local$vessel_rnpa))

landings <- raw_land %>%
  filter(between(year, 2018, 2022),
         vessel_rnpa %in% tracking_eus) %>%
  mutate(species_group = case_when(main_species_group == "ATUN" ~ "tuna",
                                   main_species_group == "CAMARON" ~ "shrimp",
                                   main_species_group == "CAZON" ~ "shark",
                                   main_species_group == "TIBURON" ~ "shark",
                                   main_species_group == "SARDINA" ~ "sardine",
                                   main_species_group == "PULPO" ~ "cephalopods",
                                   main_species_group == "CALAMAR" ~ "cephalopods",
                                   main_species_group == "OTRAS" ~ "others",
                                   T ~ "finfish")) %>%
  group_by(year, vessel_rnpa, species_group) %>%
  summarise(landed_weight = sum(landed_weight, na.rm = T),
            live_weight = sum(live_weight, na.rm = T),
            value = sum(value, na.rm = T)) %>%
  ungroup()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = landings,
        file = here("data", "processed", "clean_landings_annual_vessel_species_group.rds"))
