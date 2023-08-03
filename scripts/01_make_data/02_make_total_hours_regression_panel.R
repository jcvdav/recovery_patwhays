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

# Build an aggregate data set --------------------------------------------------
total_local <- mex_vms_local %>%
  filter(between(year, 2019, 2022))  %>%
  group_by(year) %>%
  mutate(n_vessels = n_distinct(vessel_rnpa)) %>%
  ungroup() %>%
  group_by(date, yday, year, month, n_vessels) %>%
  summarize(hours = sum(hours, na.rm = T),
            hours_hp = sum(hours * engine_power_hp)) %>%
  ungroup() %>%
  mutate(norm_hours = hours / n_vessels,
         norm_hours_hp = hours_hp / n_vessels)

# Data for the "treated" (2020) and "control" (all others)
tc_reg_data <- total_local %>%
  filter(between(yday, 0, 213)) %>% # Keep Jan1 to Jul 31
  mutate(event = as.numeric(yday - lubridate::yday("2020-03-23")),
         lockdown = ifelse(year == 2020, "Yes", "No"),
         lockdown = fct_relevel(lockdown, "No", "Yes"),
         prepost = ifelse(event < 0, "pre", "post"),
         prepost = fct_relevel(prepost, "pre", "post"),
         grp = "TC") %>%
  group_by(event, yday, lockdown, prepost, grp) %>%
  summarize(hours_sd = sd(hours, na.rm = T),
            hours = mean(hours, na.rm = T)) %>%
  ungroup()

dif_reg_data <- tc_reg_data %>%
  select(-hours_sd) %>%
  pivot_wider(names_from = lockdown, values_from = hours) %>%
  mutate(hours = Yes - No,
         lockdown = "Difference",
         grp = "Diff") %>%
  select(event, yday, lockdown, prepost, hours, grp)

total_reg_data <- bind_rows(tc_reg_data, dif_reg_data) %>%
  mutate(grp = fct_relevel(grp, "TC", "Diff"),
         lockdown = fct_relevel(lockdown, "No", "Yes", "Difference"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = total_reg_data,
        file = here("data", "processed", "total_hours_regression_panel.rds"))
