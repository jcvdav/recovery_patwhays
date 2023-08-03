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
reg_panel <- readRDS(here("data", "processed", "total_hours_regression_panel.rds"))

## PROCESSING ##################################################################

mods <- reg_panel %>%
  group_by(prepost) %>%
  nest() %>%
  expand_grid(knots = c(3:9)) %>%
  mutate(#knots = ifelse(prepost == "pre", pre_knot, post_knot),
         mod = map2(data, knots, ~feols(hours ~ lockdown*rcs(event, .y),
                                        data = .x,
                                        vcov = "NW",
                                        panel.id = ~lockdown + event))) %>%
  ungroup()

mods %>%
  mutate(summary = map(mod, broom::glance)) %>%
  select(-c(data, mod)) %>%
  unnest(summary) %>%
  group_by(prepost) %>%
  mutate(AIC_ref = min(AIC)) %>%
  ungroup() %>%
  mutate(deltaAIC = AIC_ref - AIC,
         AIC_w = exp((deltaAIC) / 2)) %>%
  select(prepost, knots, adj.r.squared, sigma, nobs, contains("AIC"))

# X ----------------------------------------------------------------------------
best_mod <- mods %>%
  mutate(knots = ifelse(prepost == "pre", 8, 9),
         mod = map2(data, knots, ~feols(hours ~ lockdown*rcs(event, .y),
                                        data = .x,
                                        vcov = "NW",
                                        panel.id = ~lockdown + event)),
         fit = map(mod, predict),
         # v_hac = map(mod, ~NeweyWest(.x, prewhite = FALSE, lag = 60)),
         v_hac = map(mod, vcov),
         X_mat = map(mod, model.matrix),
         se_fit_hac = map2(X_mat, v_hac, ~sqrt(rowSums((.x %*% .y) * .x)))) %>%
  ungroup()

pred_data <- best_mod %>%
  select(prepost, data, fit, se_fit_hac) %>%
  unnest(cols = c(data, fit, se_fit_hac)) %>%
  mutate(lwr_hac = fit - se_fit_hac,
         upr_hac = fit + se_fit_hac)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(obj = pred_data,
        file = here("data", "output", "restricted_cubic_splite_fitted_to_total_effort.rds"))
