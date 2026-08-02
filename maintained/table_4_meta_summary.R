# blair_coppock_moor_2020/maintained/table_4_meta_summary.R
# Output: output/table_4_meta_summary_cat.csv, output/table_4_meta_summary_overall.csv
# Depends on: helpers.R, output/meta_analysis_summaries.rds (from
#   figure_4_meta_analysis_coefplots.R)
# Description: Table 4, the meta-analysis estimates of sensitivity bias by domain and by
#   predicted polarity, with the 50% and 95% predictive intervals and the number of
#   studies. Reads the posterior summaries Figure 4 plots, so the table and the figures
#   cannot report different draws.

source(here::here("maintained", "helpers.R"))

meta_fits <- read_rds(path_output("meta_analysis_summaries.rds"))

meta_bind <- bind_rows(meta_fits, .id = "subset")

n_df <-
  meta_bind |>
  filter(!grepl("Random|Predictive", sensitive_list_item_unique)) |>
  group_by(subset) |>
  summarize(n = n())

re_df <-
  meta_bind |>
  filter(
    grepl("Random|Predictive", sensitive_list_item_unique),
    !is.na(overreporting)
  ) |>
  transmute(
    subset,
    mu_est_se = make_se_entry(overreporting, difference_se),
    mu_ci = make_interval_entry(difference_ci_lower, difference_ci_upper)
  )

ppi_df <-
  meta_bind |>
  filter(
    is.na(overreporting),
    grepl("Random|Predictive", sensitive_list_item_unique)
  ) |>
  transmute(
    subset,
    ppi_1 = make_interval_entry(ppi1.low, ppi1.high),
    ppi_2 = make_interval_entry(ppi2.low, ppi2.high)
  )

labels_df <- tibble(
  subset = paste0("g", c(1, 2, 4, 5, 7, 6, 9, 8, 10, 11)),
  final_category = c(
    "Vote buying", "Turnout", "Racial prejudice", "Religious prejudice",
    "Sexual orientation prejudice", "", "Support for authoritarian regimes", "",
    "All results", ""
  ),
  polarity = c(
    "Underreporting", "Overreporting", "Underreporting", "Underreporting",
    "Underreporting", "Overreporting", "Underreporting", "Overreporting",
    "Underreporting", "Overreporting"
  )
)

table_df <-
  labels_df |>
  left_join(re_df, by = "subset") |>
  left_join(ppi_df, by = "subset") |>
  left_join(n_df, by = "subset") |>
  select(-subset)

write_csv(slice(table_df, 1:8), path_output("table_4_meta_summary_cat.csv"))
write_csv(slice(table_df, 9:10), path_output("table_4_meta_summary_overall.csv"))

print(table_df)
