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
# Mexico timeline available here: https://www.capital21.cdmx.gob.mx/noticias/?p=12574#:~:text=La%20'L%C3%ADnea%20del%20tiempo%20COVID%2D19'%20est%C3%A1%20dividida%20en,de%20febrero%20de%20hoy%2C%202021.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
mex_vms_local <- readRDS(file = here("data", "raw", "daily_activity_by_vessel.rds"))

## PROCESSING ##################################################################

# Uncoment to check number of vessels with more than one permit
# mex_vms_local %>%
#   select(vessel_rnpa, sardine, shrimp, tuna, others) %>%
#   distinct() %>%
#   count(sardine, shrimp, tuna, others)

# Build an aggregate data set --------------------------------------------------
mex_vms_local_fleets <- mex_vms_local %>%
  mutate(others = ifelse((sardine == 1 & tuna == 1) | (shrimp == 1 & tuna == 1), 1, others),
         tuna = ifelse(others == 1, 0, tuna),
         sardine = ifelse(others == 1, 0, sardine),
         shrimp = ifelse(others == 1, 0, shrimp)) %>%
  pivot_longer(cols = c(sardine, shrimp, tuna, others), names_to = "fleet", values_to = "in_fleet") %>%
  filter(in_fleet == 1) %>%
  select(-in_fleet)

# Chet to make sure that the pivoting didn't fuck things up and we still have same number of rows
# identical(dim(mex_vms_local)[1], dim(mex_vms_local_fleets)[1])

# Uncomment to check number of vessels per group
# mex_vms_local_fleets %>%
#   select(vessel_rnpa, sardine, shrimp, tuna, others) %>%
#   distinct() %>%
#   count(sardine, shrimp, tuna, others)

total_fleet_local <- mex_vms_local_fleets %>%
  group_by(year) %>%
  mutate(n_vessels = n_distinct(vessel_rnpa)) %>%
  ungroup() %>%
  group_by(fleet, date, yday, year, month, n_vessels) %>%
  summarize(hours = sum(hours, na.rm = T),
            hours_hp = sum(hours * engine_power_hp)) %>%
  ungroup() %>%
  mutate(norm_hours = hours / n_vessels,
         norm_hours_hp = hours_hp / n_vessels)

# Data for the "treated" (2020) and "control" (all others)
tc_reg_data_fleet <- total_fleet_local %>%
  filter(between(yday, 0, 213)) %>% # Keep Jan1 to Jul 31
  mutate(event = as.numeric(yday - lubridate::yday("2020-03-23")),
         lockdown = ifelse(year == 2020, "Yes", "No"),
         lockdown = fct_relevel(lockdown, "No", "Yes"),
         prepost = ifelse(event < 0, "pre", "post"),
         prepost = fct_relevel(prepost, "pre", "post"),
         grp = "TC") %>%
  group_by(fleet, event, yday, lockdown, prepost, grp) %>%
  summarize(hours_sd = sd(hours, na.rm = T),
            hours = mean(hours, na.rm = T),
            hours_hp_sd = sd(hours_hp, na.rm = T),
            hours_hp = mean(hours_hp, na.tm = T)) %>%
  ungroup()

dif_reg_data_fleet <- tc_reg_data_fleet %>%
  select(-contains("sd")) %>%
  pivot_wider(names_from = lockdown, values_from = c(hours, hours_hp)) %>%
  mutate(hours = hours_Yes - hours_No,
         hours_hp = hours_hp_Yes - hours_hp_No,
         lockdown = "Difference",
         grp = "Diff") %>%
  select(fleet, event, yday, lockdown, prepost, hours, hours_hp, grp)

total_reg_data_fleet <- bind_rows(tc_reg_data_fleet, dif_reg_data_fleet) %>%
  mutate(grp = fct_relevel(grp, "TC", "Diff"),
         lockdown = fct_relevel(lockdown, "No", "Yes", "Difference"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = total_reg_data_fleet,
        file = here("data", "processed", "total_hours_regression_panel_fleet.rds"))
