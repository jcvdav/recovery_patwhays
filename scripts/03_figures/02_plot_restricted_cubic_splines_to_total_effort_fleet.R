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
pred_data <- readRDS(file = here("data", "output", "restricted_cubic_spline_fitted_to_total_effort_species_group.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
return_date <- pred_data %>%
  filter(grp == "Diff",
         prepost == "post",
         fit >= 0) %>%
  group_by(species_group) %>%
  summarize(return_date = min(yday)) %>%
  ungroup() %>%
  mutate(duration = return_date - 82)

missing_effort <- pred_data %>%
  filter(grp == "Diff",
         prepost == "post") %>%
  mutate(fit = -1 * fit) %>%
  group_by(species_group) %>%
  summarize(missing_effort = sum(fit, na.rm = T))

combined <- return_date %>%
  full_join(missing_effort, by = "species_group") %>%
  mutate(mean_missing = missing_effort / duration)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ggplot(data = pred_data,
       aes(x = yday,
           y = fit,
           group = paste(lockdown, prepost),
           color = lockdown,
           fill = lockdown)) +
  geom_ribbon(
    data = . %>%
      left_join(combined, by = "species_group") %>%
      group_by(species_group) %>%
      filter(grp == "Diff",
             yday >= 82,
             fit <= 0,
             yday <= min(return_date, 213, na.rm = T)) %>%
      ungroup(),
    aes(ymin=0, ymax=fit),
    fill="gray",
    color = "transparent",
    alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 82, linetype = "dashed") +
  geom_vline(data = combined, aes(xintercept = return_date), linetype = "dashed") +
  geom_errorbar(aes(ymin = kwh - kwh_sd,
                    ymax = kwh + kwh_sd),
                linewidth = 0.1,
                color = "black",
                width = 0) +
  geom_ribbon(aes(ymin = lwr_hac, ymax = upr_hac), alpha = 0.25) +
  geom_point(aes(y = kwh),
             size = 1,
             alpha = 0.5) +
  geom_line() +
  theme_bw() +
  facet_wrap(species_group ~ grp, nrow = 4, scales = "free_y") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 213, by = 14),
                     labels = seq(1, 213, by = 14))

## EXPORT ######################################################################


# X ----------------------------------------------------------------------------
