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
pred_data <- readRDS(file = here("data", "output", "restricted_cubic_splite_fitted_to_total_effort.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
return_date <- pred_data %>%
  filter(grp == "Diff",
         prepost == "post",
         event > 7,
         fit >= 0) %>%
  group_by(fleet) %>%
  summarize(return_date = min(yday)) %>%
  ungroup() %>%
  mutate(duration = return_date - 82)

missing_effort <- pred_data %>%
  filter(grp == "Diff",
         prepost == "post") %>%
  mutate(fit = -1 * fit) %>%
  group_by(fleet) %>%
  summarize(missing_effort = sum(fit))

combined <- return_date %>%
  left_join(missing_effort, by = "fleet") %>%
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
      left_join(combined, by = "fleet") %>%
      filter(grp == "Diff",
             yday >= 82,
             yday <= return_date),
    aes(ymin=0, ymax=fit),
    fill="gray",
    color = "transparent",
    alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 82, linetype = "dashed") +
  geom_vline(data = combined, aes(xintercept = return_date), linetype = "dashed") +
  geom_errorbar(aes(ymin = hours_hp - hours_hp_sd,
                    ymax = hours_hp + hours_hp_sd),
                linewidth = 0.1,
                color = "black",
                width = 0) +
  geom_ribbon(aes(ymin = lwr_hac, ymax = upr_hac), alpha = 0.25) +
  geom_point(aes(y = hours_hp),
             size = 1,
             alpha = 0.5) +
  geom_line() +
  theme_bw() +
  facet_wrap(fleet ~ grp, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 213, by = 14),
                     labels = seq(1, 213, by = 14))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
