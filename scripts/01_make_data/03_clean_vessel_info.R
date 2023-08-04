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
  tidyverse
)

# Load data --------------------------------------------------------------------
landings <- readRDS(file = here("data", "processed", "landings_annual_vessel_species_group.rds"))
vessel_info <- readRDS(file = here("data", "raw", "mex_vessel_info.rds"))

## PROCESSING ##################################################################

# ------------------------------------------------------------------------------
landing_eus <- sort(unique(landings$vessel_rnpa))

group_land <- landings %>%
  select(vessel_rnpa, species_group, value) %>%
  group_by(vessel_rnpa, species_group) %>%
  summarize(value = sum(value)) %>%
  slice_max(value) %>%
  ungroup()

vessel_info %>%
  select(vessel_rnpa, finfish, sardine, shark, shrimp, tuna, others) %>%
  distinct() %>%
  count(finfish, sardine, shark, shrimp, tuna, others) %>%
  mutate(tot = finfish + shark + shrimp + sardine + tuna + others) %>%
  arrange(desc(tot)) %>%
  janitor::adorn_totals()

# Build an aggregate data set --------------------------------------------------
inter <- vessel_info %>%
  filter(vessel_rnpa %in% landing_eus) %>%
  mutate(others = ifelse((sardine == 1 & tuna == 1) | (shrimp == 1 & tuna == 1), 1, others),
         tuna = ifelse(others == 1, 0, tuna),
         sardine = ifelse(others == 1, 0, sardine),
         shrimp = ifelse(others == 1, 0, shrimp)) %>%
  mutate(tot = finfish + shark + shrimp + sardine + tuna + others)

new_groups <- inter %>%
  filter(tot > 1) %>%
  left_join(group_land, by = "vessel_rnpa") %>%
  select(vessel_rnpa, species_group) %>%
  distinct()

singles <- inter %>%
  filter(tot == 1) %>%
  pivot_longer(cols = c(finfish, sardine, shark, shrimp, tuna, others),
               names_to = "species_group",
               values_to = "value") %>%
  filter(value == 1) %>%
  select(-value)

multiples <- inter %>%
  filter(tot > 1) %>%
  select(-c(finfish, sardine, shark, shrimp, tuna, others)) %>%
  left_join(new_groups, by = "vessel_rnpa")

final <- bind_rows(singles, multiples) %>%
  select(vessel_rnpa, vessel_name, state, contains("vessel"), engine_power_kw, species_group)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = final,
        file = here("data", "processed", "clean_vessel_info.rds"))
