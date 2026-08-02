# blair_coppock_moor_2020/maintained/text_power_thresholds.R
# Output: output/text_power_thresholds.csv
# Depends on: helpers.R, output/figure_2ab_indifference_curves.csv,
#   output/figure_3a_power_curve.csv, output/figure_3b_power_curve.csv
# Description: The sample size thresholds quoted in the design section, read off the same
#   curves the figures plot rather than recomputed: the bias at which a study of a given
#   size should prefer a list experiment on RMSE grounds, the bias it has 80% power to
#   detect, and the between-group differences detectable at 80% power.

source(here::here("maintained", "helpers.R"))

curves_2ab <- read_csv(path_output("figure_2ab_indifference_curves.csv"), show_col_types = FALSE)
curve_3a <- read_csv(path_output("figure_3a_power_curve.csv"), show_col_types = FALSE)
curve_3b <- read_csv(path_output("figure_3b_power_curve.csv"), show_col_types = FALSE)

claims_df <-
  bind_rows(
    curves_2ab |>
      filter(N %in% c(1000, 2000)) |>
      transmute(
        claim = str_glue("Figure 2a: bias at which RMSE of list equals RMSE of direct, N = {N}"),
        value_points = rmse_target * 100
      ),
    curves_2ab |>
      filter(N %in% c(1000, 2000)) |>
      transmute(
        claim = str_glue("Figure 2b: bias detectable with 80% power, N = {N}"),
        value_points = power_target * 100
      ),
    curve_3a |>
      filter(N %in% c(2000, 3000)) |>
      transmute(
        claim = str_glue("Figure 3a: difference in prevalence detectable with 80% power, N = {N}"),
        value_points = difference_in_prevalence * 100
      ),
    curve_3b |>
      filter(N %in% c(2000, 3000)) |>
      transmute(
        claim = str_glue("Figure 3b: difference in sensitivity bias detectable with 80% power, N = {N}"),
        value_points = difference_in_sensitivity_bias * 100
      )
  )

write_csv(claims_df, path_output("text_power_thresholds.csv"))

print(claims_df, n = nrow(claims_df))
