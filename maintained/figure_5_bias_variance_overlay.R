# blair_coppock_moor_2020/maintained/figure_5_bias_variance_overlay.R
# Output: output/figure_5_bias_variance_overlay.pdf, .png
#   output/figure_5_region_counts.csv
# Depends on: helpers.R, output/figure_2ab_indifference_curves.csv (from
#   figure_2ab_theory_power_rmse.R), original_extracted/data/estimates_df.RDS
# Description: Figure 5. Each of the 264 list experiments is placed on the sample size by
#   sensitivity bias plane, with the RMSE and power indifference curves from Figure 2
#   drawn over them. The region counts under, between and above the curves are the
#   in-text figures for how many studies are large enough for each purpose.

source(here::here("maintained", "helpers.R"))

curves_df <- read_csv(path_output("figure_2ab_indifference_curves.csv"), show_col_types = FALSE)
wide_df <- read_rds(path_original("data", "estimates_df.RDS"))

gg_curves_df <-
  curves_df |>
  rename(
    `Above the line, the power to detect sensitivity bias is greater than 80%` = power_target,
    `Above the line, the RMSE of the list experiment is smaller than that of the direct question` = rmse_target
  ) |>
  pivot_longer(-N, names_to = "key", values_to = "biases")

# Each study is matched to the curve at its sample size rounded up to the next 50, which
# is the grid the indifference curves are evaluated on.
gg_df <-
  wide_df |>
  filter(polarity != "Prediction: Nonsensitive") |>
  mutate(
    is_pos = difference > 0,
    plot_weights = 1 / difference_se^2,
    biases = if_else(is_pos, difference, -1 * difference),
    N = list_n,
    round_50_N = 50 * ceiling(N / 50)
  ) |>
  left_join(rename(curves_df, round_50_N = N), by = "round_50_N") |>
  mutate(
    region = case_when(
      biases < rmse_target ~ "small",
      biases >= rmse_target & biases < power_target ~ "medium",
      biases >= power_target ~ "big"
    )
  )

region_counts <-
  gg_df |>
  count(region) |>
  mutate(proportion = n / sum(n))

write_csv(region_counts, path_output("figure_5_region_counts.csv"))

g <-
  ggplot(gg_df, aes(x = N, y = biases * 100)) +
  geom_point(aes(size = plot_weights), alpha = 0.5, stroke = 0) +
  geom_line(data = gg_curves_df, aes(group = key, linetype = key)) +
  coord_cartesian(ylim = c(0, 50), xlim = c(0, 8000)) +
  theme_bw() +
  annotate("text", x = 3500, y = 4.75, hjust = 0, label = "RMSE of list less than RMSE of direct question", size = 3) +
  annotate("text", x = 4000, y = 11, hjust = 0, label = "Power to detect sensitivity bias above 80 percent", size = 3) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  ylab("Sensitivity Bias in Percentage Points") +
  xlab("Sample Size")

save_figure(g, "figure_5_bias_variance_overlay", width = 6.5, height = 3)

print(region_counts)
