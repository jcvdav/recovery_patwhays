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
  lubridate,
  fixest,
  rms,
  tidyverse
)

# Load data --------------------------------------------------------------------
reg_panel_species_group <- readRDS(file = here("data", "processed", "total_hours_regression_panel_species_group.rds")) %>%
  filter(!between(event, -3, 4))

## PROCESSING ##################################################################

mods_species_group <- reg_panel_species_group %>%
  group_by(species_group, prepost) %>%
  nest() %>%
  expand_grid(knots = c(3:8)) %>%
  mutate(#knots = ifelse(prepost == "pre", pre_knot, post_knot),
    mod = map2(data, knots, ~feols(kwh ~ 0 + lockdown*rcs(event, .y),
                                   data = .x,
                                   vcov = "NW",
                                   panel.id = ~lockdown + event))) %>%
  ungroup()

mods_species_group %>%
  mutate(summary = map(mod, broom::glance)) %>%
  select(-c(data, mod)) %>%
  unnest(summary) %>%
  group_by(species_group, prepost) %>%
  mutate(AIC_ref = min(AIC)) %>%
  ungroup() %>%
  mutate(deltaAIC = AIC_ref - AIC,
         AIC_w = exp((deltaAIC) / 2)) %>%
  select(species_group, prepost, knots, adj.r.squared, sigma, nobs, contains("AIC"))

# X ----------------------------------------------------------------------------
best_mod_species_group <- mods_species_group %>%
  mutate(summary = map(mod, broom::glance)) %>%
  unnest(summary) %>%
  group_by(species_group, prepost) %>%
  filter(AIC == min(AIC)) %>%
  ungroup() %>%
  mutate(fit = map(mod, predict),
         # v_hac = map(mod, ~NeweyWest(.x, prewhite = FALSE, lag = 60)),
         v_hac = map(mod, vcov),
         X_mat = map(mod, model.matrix),
         se_fit_hac = map2(X_mat, v_hac, ~sqrt(rowSums((.x %*% .y) * .x))))

pred_data_species_group <- best_mod_species_group %>%
  select(species_group, prepost, data, fit, se_fit_hac) %>%
  unnest(cols = c(data, fit, se_fit_hac)) %>%
  mutate(lwr_hac = fit - se_fit_hac,
         upr_hac = fit + se_fit_hac)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(obj = pred_data_species_group,
        file = here("data", "output", "restricted_cubic_spline_fitted_to_total_effort_species_group.rds"))
