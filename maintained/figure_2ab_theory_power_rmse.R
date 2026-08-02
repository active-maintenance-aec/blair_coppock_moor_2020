# blair_coppock_moor_2020/maintained/figure_2ab_theory_power_rmse.R
# Output: output/figure_2ab_theory_power_rmse.pdf, .png
#   output/figure_2ab_indifference_curves.csv
# Depends on: helpers.R, original_extracted/code/2_simulations/theory_list_power_function.R
# Description: Figure 2, panels a and b. Panel a traces the sensitivity bias at which the
#   list experiment and the direct question have equal RMSE; panel b traces the bias at
#   which a study has 80% power to detect sensitivity bias. Both as functions of N.

source(here::here("maintained", "helpers.R"))
source(path_original("code", "2_simulations", "theory_list_power_function.R"))

biases <- seq(0, 1, 0.0001)
ns <- c(seq(10, 500, by = 10), seq(500, 15000, by = 50))
dat <- expand.grid(biases = biases, ns = ns)

# The archive walks this 3,410,341 row grid one row at a time with map2_df, which takes
# about a quarter of an hour. list_power is already fully vectorized, so a single call
# over the whole grid returns the identical frame in under half a second. The full grid is
# not written to disk: it is a 245 MB frame and is only an intermediate, and the two
# indifference curves summarized from it are what Figure 2 and Figure 5 actually use.
sim_df <-
  list_power(
    N = dat$ns,
    prevalence_rate = 0.5,
    sensitivity_bias = dat$biases,
    variance_control_items = 0.75,
    covariance_control_items_prevalance = 0.025
  )

curves_df <-
  sim_df |>
  group_by(N) |>
  summarize(
    power_target = sensitivity_bias[which.min(abs(power - 0.8))],
    rmse_target = sensitivity_bias[which.min(abs(list_rmse - direct_rmse))]
  )

write_csv(curves_df, path_output("figure_2ab_indifference_curves.csv"))

gg_df <-
  curves_df |>
  rename(
    `Power to detect sensitivity bias` = power_target,
    `Difference in RMSE` = rmse_target
  ) |>
  pivot_longer(-N, names_to = "key", values_to = "value")

text_df <-
  tibble(
    value = c(0, .1, .05, .2),
    N = c(2400, 3000, 1800, 3000),
    text = c(
      "Below the line, RMSE (direct) < RMSE (list)",
      "Above the line, \n RMSE (direct) > RMSE (list)",
      "Below the line, \n the power to detect sensitivity bias \n is less than 80%",
      "Above the line, \n the power to detect sensitivity bias \n is greater than 80%"
    ),
    key = c(
      "Difference in RMSE", "Difference in RMSE",
      "Power to detect sensitivity bias", "Power to detect sensitivity bias"
    )
  )

g <-
  ggplot(gg_df, aes(N, value * 100)) +
  geom_line(linewidth = 1) +
  geom_text(data = text_df, aes(label = text), size = 3) +
  coord_cartesian(ylim = c(0, 30), xlim = c(0, 5000)) +
  facet_wrap(~key) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  ylab("Sensitivity Bias in Percentage Points") +
  xlab("Sample Size")

save_figure(g, "figure_2ab_theory_power_rmse", width = 6.5, height = 3)

print(filter(curves_df, N %in% c(1000, 2000)))
