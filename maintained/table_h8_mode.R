# blair_coppock_moor_2020/maintained/table_h8_mode.R
# Output: output/table_h8_mode.csv
# Depends on: helpers.R, original_extracted/data/estimates_df.RDS
# Description: Table H.8, the number of studies in each domain by survey mode.

source(here::here("maintained", "helpers.R"))

wide_df <- read_rds(path_original("data", "estimates_df.RDS"))

meta_df <-
  wide_df |>
  filter(
    final_category %in% c(
      "vote_buying", "turnout", "race", "religion",
      "sexual_orientation", "authoritarian"
    )
  ) |>
  filter(!(final_category == "race" & polarity == "Prediction: Overreporting")) |>
  mutate(
    category_polarity = paste0(final_category, " ", polarity),
    category_polarity = factor(
      category_polarity,
      levels = c(
        "vote_buying Prediction: Underreporting",
        "turnout Prediction: Overreporting",
        "race Prediction: Underreporting",
        "religion Prediction: Underreporting",
        "sexual_orientation Prediction: Underreporting",
        "sexual_orientation Prediction: Overreporting",
        "authoritarian Prediction: Underreporting",
        "authoritarian Prediction: Overreporting"
      )
    )
  )

mode_df <-
  meta_df |>
  tabyl(category_polarity, mode) |>
  adorn_totals("row")

output_df <-
  mode_df |>
  slice(-n()) |>
  bind_cols(tibble(
    final_category = c(
      "Vote buying", "Turnout", "Racial prejudice", "Religious prejudice",
      "Sexual orientation prejudice", "", "Support for authoritarian regimes", ""
    ),
    polarity = c(
      "Underreporting", "Overreporting", "Underreporting", "Underreporting",
      "Underreporting", "Overreporting", "Underreporting", "Overreporting"
    )
  )) |>
  select(final_category, polarity, online, person, `self-report`, telephone)

total_row <-
  tibble(
    final_category = "Total",
    polarity = "",
    online = mode_df$online[nrow(mode_df)],
    person = mode_df$person[nrow(mode_df)],
    `self-report` = mode_df$`self-report`[nrow(mode_df)],
    telephone = mode_df$telephone[nrow(mode_df)]
  )

full_table <- bind_rows(output_df, total_row)

write_csv(full_table, path_output("table_h8_mode.csv"))

print(full_table)
