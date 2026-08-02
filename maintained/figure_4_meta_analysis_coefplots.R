# blair_coppock_moor_2020/maintained/figure_4_meta_analysis_coefplots.R
# Output: output/figure_4[a-h]_*.pdf, .png and the _sm small multiples versions (32 files)
#   output/figure_4_meta_estimates.csv, output/meta_analysis_summaries.rds
# Depends on: helpers.R, original_extracted/data/estimates_df.RDS,
#   original_extracted/code/1_meta_analysis/random_effects.stan
# Description: Bayesian random effects meta-analysis of sensitivity bias, one fit per
#   domain and predicted polarity, plotted as the panels of Figure 4 in the main text and
#   Figures E.3 to E.9 in the supplementary materials. The fitted summaries are saved so
#   that Table 4 reports the same posterior draws these figures plot.

source(here::here("maintained", "helpers.R"))

set.seed(500)

# One core per chain, pinned rather than taken from detectCores(), so the committed output
# does not depend on how many cores the reader has. rstan never uses more than the four
# chains ask for, so this is the archive's setting made explicit.
options(mc.cores = 4)

wide_df <- read_rds(path_original("data", "estimates_df.RDS"))
meta_model <- stan_model(file = path_original("code", "1_meta_analysis", "random_effects.stan"))

# Domain fits ----
# Named for the subsets the archive calls g1 through g11; g3 is absent there too, because
# racial prejudice has only two overreporting studies and the model needs at least three.

fit_subset <- function(category, prediction) {
  wide_df |>
    filter(final_category == category, polarity == prediction) |>
    make_meta_df(stan_model = meta_model)
}

meta_fits <- list(
  g1 = fit_subset("vote_buying", "Prediction: Underreporting"),
  g2 = fit_subset("turnout", "Prediction: Overreporting"),
  g4 = fit_subset("race", "Prediction: Underreporting"),
  g5 = fit_subset("religion", "Prediction: Underreporting"),
  g6 = fit_subset("sexual_orientation", "Prediction: Overreporting"),
  g7 = fit_subset("sexual_orientation", "Prediction: Underreporting"),
  g8 = fit_subset("authoritarian", "Prediction: Overreporting"),
  g9 = fit_subset("authoritarian", "Prediction: Underreporting"),
  g10 = wide_df |> filter(polarity == "Prediction: Underreporting") |> make_meta_df(meta_model),
  g11 = wide_df |> filter(polarity == "Prediction: Overreporting") |> make_meta_df(meta_model)
)

write_rds(meta_fits, path_output("meta_analysis_summaries.rds"))

# Panels ----

panel_stems <- c(
  g1 = "figure_4a_vote_buying_underreporting",
  g2 = "figure_4b_turnout_overreporting",
  g4 = "figure_4c_racial_prejudice_underreporting",
  g5 = "figure_4d_religious_prejudice_underreporting",
  g7 = "figure_4e_sexual_orient_underreporting",
  g6 = "figure_4f_sexual_orient_overreporting",
  g9 = "figure_4g_authoritarian_underreporting",
  g8 = "figure_4h_authoritarian_overreporting"
)

# Panel height follows the archive: a fixed inch of margin plus 0.285 inches for every row
# the panel draws, counting the two meta-analysis rows as well as the study estimates, so
# that a row is the same size across panels of different length.
walk2(names(panel_stems), panel_stems, function(subset, stem) {
  gg_df <- meta_fits[[subset]]
  height <- 1 + 0.285 * nrow(gg_df)
  save_figure(overreporting_plot(gg_df), stem, width = 6.5, height = height)
})

walk2(names(panel_stems), panel_stems, function(subset, stem) {
  gg_df <- meta_fits[[subset]]
  labelled <- subset == "g1"
  save_figure(
    overreporting_plot_small_multiples(gg_df, labels = labelled),
    paste0(stem, "_sm"),
    width = 6, height = 3.25
  )
})

# Plotted values ----
# A PDF differs on every run because it records the time it was written, so the figure
# files cannot be diffed against a fresh run. Every value the panels draw is written here
# instead, and that is what a reader compares.

estimates_df <-
  meta_fits |>
  bind_rows(.id = "subset") |>
  mutate(figure = unname(panel_stems[subset])) |>
  transmute(
    subset,
    figure,
    row = str_trim(as.character(sensitive_list_item_unique)),
    polarity = as.character(polarity),
    estimate = overreporting,
    std_error = difference_se,
    ci_lower = difference_ci_lower,
    ci_upper = difference_ci_upper,
    ppi_50_lower = ppi1.low,
    ppi_50_upper = ppi1.high,
    ppi_95_lower = ppi2.low,
    ppi_95_upper = ppi2.high,
    point_weight = re_weight
  ) |>
  arrange(subset, row)

write_csv(estimates_df, path_output("figure_4_meta_estimates.csv"))

print(count(estimates_df, subset, figure))
