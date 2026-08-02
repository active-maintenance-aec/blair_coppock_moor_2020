# blair_coppock_moor_2020/maintained/figure_3b_diff_in_sensitivity_power.R
# Output: output/figure_3b_diff_in_sensitivity_power.pdf, .png
#   output/figure_3b_power_curve.csv
# Depends on: helpers.R, original_extracted/code/2_simulations/theory_list_power_function.R
# Description: Figure 3b. The difference in sensitivity bias between two groups that a
#   list experiment has 80% power to detect, as a function of total sample size.

source(here::here("maintained", "helpers.R"))
source(path_original("code", "2_simulations", "theory_list_power_function.R"))

Ns <- seq(100, 10000, 50)
sensitivity_bias_B <- seq(0, 0.6, 0.001)
dat <- expand.grid(Ns = Ns, sensitivity_bias_B = sensitivity_bias_B)

# diff_in_sensitivity_power is vectorized; see the note in figure_3a.
sim_df <-
  diff_in_sensitivity_power(
    N_A = dat$Ns / 2,
    N_B = dat$Ns / 2,
    prevalence_rate_A = 0.6,
    prevalence_rate_B = 0.6,
    sensitivity_bias_A = 0,
    sensitivity_bias_B = dat$sensitivity_bias_B,
    variance_control_items_A = 0.75,
    variance_control_items_B = 0.75,
    covariance_control_items_prevalance_A = 0.025,
    covariance_control_items_prevalance_B = 0.025
  ) |>
  mutate(
    difference_in_sensitivity_bias = sensitivity_bias_B - sensitivity_bias_A,
    N = N_A + N_B
  )

gg_df <-
  sim_df |>
  group_by(N) |>
  summarize(
    difference_in_sensitivity_bias = difference_in_sensitivity_bias[which.min(abs(power - 0.8))]
  )

write_csv(gg_df, path_output("figure_3b_power_curve.csv"))

text_df <-
  tibble(
    difference_in_sensitivity_bias = c(.08, .45),
    N = c(1950, 3100),
    text = c(
      "Below the line, the power to detect \n the difference-in-sensitivity bias \n is less than 80%",
      "Above the line, the power to detect \n the difference-in-sensitivity bias \n is greater than 80%"
    )
  )

g_group_dis <-
  ggplot(gg_df, aes(N, difference_in_sensitivity_bias * 100)) +
  geom_smooth(method = "loess", span = 0.1, linewidth = 1, colour = "black", se = FALSE) +
  geom_text(data = text_df, aes(label = text), size = 3) +
  coord_cartesian(ylim = c(0, 50), xlim = c(0, 5000)) +
  theme_bw() +
  ylab("Difference in sensitivity bias") +
  xlab("Total Sample Size")

save_figure(g_group_dis, "figure_3b_diff_in_sensitivity_power", width = 3.25, height = 3)

print(filter(gg_df, N %in% c(2000, 3000)))
