# blair_coppock_moor_2020/maintained/text_summary_stats.R
# Output: output/text_summary_stats.csv
# Depends on: helpers.R, original_extracted/data/estimates_df.RDS
# Description: The study level quantities quoted in the results section: how many list
#   experiments and how many papers the meta-analysis covers, the range of the vote
#   buying estimates, the single vote buying study the text walks through, and the
#   largest over- and underreporting estimates in the authoritarian regimes literature.

source(here::here("maintained", "helpers.R"))

wide_df <- read_rds(path_original("data", "estimates_df.RDS"))

vote_buying_df <- wide_df |>
  filter(final_category == "vote_buying", polarity == "Prediction: Underreporting")

vote_buying_top <- slice_max(vote_buying_df, overreporting, n = 1)

authoritarian_df <- wide_df |> filter(final_category == "authoritarian")
authoritarian_max <- slice_max(authoritarian_df, overreporting, n = 1)
authoritarian_min <- slice_min(authoritarian_df, overreporting, n = 1)

claims_df <- tibble(
  claim = c(
    "list experiments in the meta-analysis data",
    "papers in the meta-analysis data",
    "studies predicted to show underreporting",
    "studies predicted to show overreporting",
    "vote buying: most negative study estimate, points",
    "vote buying: most positive study estimate, points",
    str_glue("vote buying: top row of Figure 4a ({vote_buying_top$cite_key}), estimate in points"),
    str_glue("vote buying: top row of Figure 4a ({vote_buying_top$cite_key}), 95% CI lower in points"),
    str_glue("vote buying: top row of Figure 4a ({vote_buying_top$cite_key}), 95% CI upper in points"),
    str_glue("authoritarian: largest overreporting estimate ({authoritarian_max$cite_key}), points"),
    str_glue("authoritarian: largest underreporting estimate ({authoritarian_min$cite_key}), points")
  ),
  value = c(
    nrow(wide_df),
    n_distinct(wide_df$paper_id),
    sum(wide_df$polarity == "Prediction: Underreporting", na.rm = TRUE),
    sum(wide_df$polarity == "Prediction: Overreporting", na.rm = TRUE),
    min(vote_buying_df$overreporting) * 100,
    max(vote_buying_df$overreporting) * 100,
    vote_buying_top$overreporting * 100,
    vote_buying_top$difference_ci_lower * 100,
    vote_buying_top$difference_ci_upper * 100,
    authoritarian_max$overreporting * 100,
    authoritarian_min$overreporting * 100
  )
)

write_csv(claims_df, path_output("text_summary_stats.csv"))

print(claims_df, n = nrow(claims_df))
