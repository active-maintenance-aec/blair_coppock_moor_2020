# blair_coppock_moor_2020/ground_truth/build_ground_truth.R
# Output: ground_truth/blair_coppock_moor_2020_ground_truth.csv
# Depends on: maintained/output/ (run run_all.R first), ground_truth/published_claims.csv,
#   ground_truth/published_g_table_cells.csv, ground_truth/published_e_figure_counts.csv,
#   ground_truth/archive_*.csv, maintained/in_text_claims.R
# Description: Assemble the ground truth table. Every published number enters through
#   ground_truth/published_claims.csv, the exhaustive extraction of the numeric claims in
#   the article and its supplementary materials, and is used only as a comparison target;
#   no published number is an input to any computation here or anywhere in maintained/.
#   Every value_rewrite entry is read back out of maintained/output/, so the table cannot
#   drift from the pipeline. value_script is filled only where a script in the deposited
#   archive computes the quantity itself.
#
#   The last section is the coverage gate. Every claim the extraction classifies as
#   pipeline or descriptive must be checked twice: by a row here, and by a block in
#   maintained/in_text_claims.R that reaches the same number from the same outputs by its
#   own path. The gate runs that file non-interactively and counts what it printed, because
#   a block that errors, or that ends in a bare expression and so prints nothing under
#   source(), satisfies a scan for markers completely while checking nothing.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

options(width = 200)

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

# What the deposited scripts produce, written by ground_truth/extract_archive_values.R,
# which runs the archive's own meta_analysis.R in a scratch copy. That script is not part
# of run_all.R because it answers a question about the archive rather than about the
# rewrite, so its output is committed and read here.
archive <- read_csv(here::here("ground_truth", "archive_meta_values.csv"), show_col_types = FALSE)

meta <- out("figure_4_meta_estimates.csv")
h8 <- out("table_h8_mode.csv")
tg <- out("table_g_study_estimates.csv")
thresholds <- out("text_power_thresholds.csv")
theory <- out("text_theory_variance.csv")
summary_stats <- out("text_summary_stats.csv")
regions <- out("figure_5_region_counts.csv")
deming <- out("figure_f10_deming_coefficients.csv")

# The published side ----
# published_claims.csv is the extraction: every numeric claim the article and the
# supplementary materials make, carrying the string the page prints. It is the only place
# a published number is written down, and paper() is the only way one enters this file.

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character())
)

stopifnot(!anyDuplicated(published_claims$claim_id))

paper <- function(id) {
  value <- published_claims$value_paper[published_claims$claim_id == id]
  stopifnot(length(value) == 1)
  value
}

# Appendix G and E are read off the published pages positionally by
# ground_truth/extract_published_values.R, because 239 study rows carrying 717 numbers is
# more than anyone will type correctly, and because a figure that prints nothing on its
# face still states how many estimates it draws.
published_g <- read_csv(here::here("ground_truth", "published_g_table_cells.csv"),
                        show_col_types = FALSE)
published_e <- read_csv(here::here("ground_truth", "published_e_figure_counts.csv"),
                        show_col_types = FALSE)

pick_claim <- function(data, pattern, column = "value") {
  rows <- str_detect(data$claim, fixed(pattern))
  stopifnot(sum(rows) == 1)
  data[[column]][rows]
}

# Table 4 ----
# The rewrite's nine printed quantities per subset, read out of the plotted estimates.

rewrite_t4 <-
  meta |>
  summarize(
    mu_estimate = estimate[row == "Random effects meta-analysis"],
    mu_se = std_error[row == "Random effects meta-analysis"],
    ci_lower = ci_lower[row == "Random effects meta-analysis"],
    ci_upper = ci_upper[row == "Random effects meta-analysis"],
    ppi50_lower = ppi_50_lower[row == "Predictive Intervals (50% and 95%)"],
    ppi50_upper = ppi_50_upper[row == "Predictive Intervals (50% and 95%)"],
    ppi95_lower = ppi_95_lower[row == "Predictive Intervals (50% and 95%)"],
    ppi95_upper = ppi_95_upper[row == "Predictive Intervals (50% and 95%)"],
    n_studies = sum(!is.na(estimate)) - 1,
    .by = subset
  )

archive_t4 <-
  archive |>
  summarize(
    mu_estimate = estimate[row == "Random effects meta-analysis"],
    mu_se = std_error[row == "Random effects meta-analysis"],
    ci_lower = ci_lower[row == "Random effects meta-analysis"],
    ci_upper = ci_upper[row == "Random effects meta-analysis"],
    ppi50_lower = ppi_50_lower[row == "Predictive Intervals (50% and 95%)"],
    ppi50_upper = ppi_50_upper[row == "Predictive Intervals (50% and 95%)"],
    ppi95_lower = ppi_95_lower[row == "Predictive Intervals (50% and 95%)"],
    ppi95_upper = ppi_95_upper[row == "Predictive Intervals (50% and 95%)"],
    n_studies = first(n_studies),
    .by = subset
  )

subset_domains <- c(
  g1 = "vote buying underreporting",
  g2 = "turnout overreporting",
  g4 = "racial prejudice underreporting",
  g5 = "religious prejudice underreporting",
  g7 = "sexual orientation underreporting",
  g6 = "sexual orientation overreporting",
  g9 = "authoritarian underreporting",
  g8 = "authoritarian overreporting",
  g10 = "all results underreporting",
  g11 = "all results overreporting"
)

quantity_labels <- c(
  mu_estimate = "mu estimate",
  mu_se = "mu SE",
  ci_lower = "95% CI lower",
  ci_upper = "95% CI upper",
  ppi50_lower = "50% PPI lower",
  ppi50_upper = "50% PPI upper",
  ppi95_lower = "95% PPI lower",
  ppi95_upper = "95% PPI upper",
  n_studies = "N studies"
)

t4_rows <-
  rewrite_t4 |>
  pivot_longer(-subset, names_to = "quantity", values_to = "value_rewrite") |>
  left_join(
    archive_t4 |> pivot_longer(-subset, names_to = "quantity", values_to = "value_script"),
    by = c("subset", "quantity")
  ) |>
  transmute(
    claim_id = str_c("t4_", subset, "_", quantity),
    table_figure = "Table 4",
    claim = str_c(unname(subset_domains[subset]), " ", unname(quantity_labels[quantity])),
    value_script,
    value_paper = map_chr(claim_id, paper),
    value_rewrite,
    locus = "environment",
    notes = "The archive's own meta_analysis.R, run from a scratch copy with the Stan model recompiled and the fct_reorder call repaired, returns the value in value_script"
  )

# Table H.8 ----
# The table repeats a domain label only on its first row, so the blank second row of each
# pair carries down from the row above before the label is assembled.

h8_slugs <- c(
  "Vote buying Underreporting" = "vote_buying_under",
  "Turnout Overreporting" = "turnout_over",
  "Racial prejudice Underreporting" = "race_under",
  "Religious prejudice Underreporting" = "religion_under",
  "Sexual orientation prejudice Underreporting" = "sexual_orientation_under",
  "Sexual orientation prejudice Overreporting" = "sexual_orientation_over",
  "Support for authoritarian regimes Underreporting" = "authoritarian_under",
  "Support for authoritarian regimes Overreporting" = "authoritarian_over",
  "Total" = "total"
)

mode_slugs <- c(online = "online", person = "person", `self-report` = "self_report",
                telephone = "telephone")

rewrite_h8 <-
  h8 |>
  mutate(domain = final_category, .keep = "unused") |>
  fill(domain, .direction = "down") |>
  pivot_longer(c(online, person, `self-report`, telephone),
               names_to = "mode", values_to = "value_rewrite") |>
  mutate(label = str_squish(str_c(domain, " ", replace_na(polarity, ""))))

archive_h8 <-
  read_csv(here::here("ground_truth", "archive_mode_table.csv"), show_col_types = FALSE) |>
  fill(final_category, .direction = "down") |>
  pivot_longer(c(online, person, `self-report`, telephone),
               names_to = "mode", values_to = "value_script") |>
  mutate(label = str_squish(str_c(final_category, " ", replace_na(polarity, ""))))

h8_rows <-
  rewrite_h8 |>
  select(label, mode, value_rewrite) |>
  left_join(select(archive_h8, label, mode, value_script), by = c("label", "mode")) |>
  transmute(
    claim_id = str_c("h8_", unname(h8_slugs[label]), "_", unname(mode_slugs[mode])),
    table_figure = "Table H.8",
    claim = str_c(label, ", ", mode),
    value_script,
    value_paper = map_chr(claim_id, paper),
    value_rewrite,
    notes = ""
  )

stopifnot(!any(is.na(h8_rows$claim_id)), nrow(h8_rows) == 36)

# Tables G.1 to G.7, cell by cell ----
# Every number the seven study level tables print, compared row by row in the order the
# pages print them. Rows are matched positionally rather than by study label because two
# rows of Table G.1 carry identical study and item labels and nothing else tells them
# apart. The rewrite reproduces the published sort order, so position is a genuine key.

g_table_of <- c(
  "Vote Buying" = "Table G.1",
  "Race" = "Table G.2",
  "Sexual Orientation" = "Table G.3",
  "Religion" = "Table G.4",
  "Authoritarian" = "Table G.5",
  "Turnout" = "Table G.6",
  "Other" = "Table G.7"
)

rewrite_g <-
  tg |>
  drop_na(category) |>
  mutate(table = unname(g_table_of[category])) |>
  mutate(position = row_number(), .by = table)

stopifnot(!any(is.na(rewrite_g$table)))

g_matched <-
  published_g |>
  inner_join(rewrite_g, by = c("table", "position"), suffix = c("_paper", "_rewrite"))

stopifnot(nrow(g_matched) == nrow(published_g), nrow(g_matched) == nrow(rewrite_g))

g_cell_rows <-
  g_matched |>
  summarize(
    printed = 3 * n(),
    value_rewrite = sum(direct_paper == direct_rewrite) +
      sum(list_paper == list_rewrite) +
      sum(n_paper == n_rewrite),
    .by = table
  ) |>
  transmute(
    claim_id = str_c("table_g", str_extract(table, "[1-7]"), "_cells"),
    table_figure = table,
    claim = str_c("cells reproduced, of ", printed, " printed in the table"),
    value_script = NA_real_,
    value_paper = map_chr(claim_id, paper),
    value_rewrite,
    notes = "Every direct question estimate and standard error, list experiment estimate and standard error, and sample size the table prints, read off the published page positionally and compared in the order the page prints them"
  )

archive_g_counts <-
  read_csv(here::here("ground_truth", "archive_table_of_data_counts.csv"), show_col_types = FALSE) |>
  mutate(table = unname(g_table_of[category])) |>
  select(table, value_script = n_rows)

g_count_rows <-
  count(rewrite_g, table, name = "value_rewrite") |>
  left_join(archive_g_counts, by = "table") |>
  transmute(
    claim_id = str_c("table_g", str_extract(table, "[1-7]"), "_studies_tabulated"),
    table_figure = table,
    claim = str_c("studies tabulated (", table, ")"),
    value_script,
    value_paper = map_chr(claim_id, paper),
    value_rewrite,
    notes = ""
  )

# Corstange (2017) is the one vote buying study with a single row in Table G.1, so its
# three numbers can be located by study name alone rather than by position. It is the spot
# check against a rendered page that the positional parse above is held to.
corstange <- filter(rewrite_g, study == "corstange2017clientelism")
stopifnot(nrow(corstange) == 1)

first_number <- function(x) as.numeric(str_extract(x, "^[-0-9.]+"))

g_spot_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "table_g_total_studies_tabulated", "Tables G.1 to G.7", "studies tabulated across all seven tables",
    sum(archive_g_counts$value_script), paper("table_g_total_studies_tabulated"), nrow(rewrite_g),
    "The meta-analysis uses 264 studies, so 25 appear in no supplementary table. Those 25 carry no value in the data file's final_category column, which is the variable the tables are split on",
  "table_g1_corstange_direct", "Table G.1", "Corstange (2017), direct question estimate",
    NA_real_, paper("table_g1_corstange_direct"), first_number(corstange$direct),
    "The one vote buying study with a single row, so it can be located by name",
  "table_g1_corstange_list", "Table G.1", "Corstange (2017), list experiment estimate",
    NA_real_, paper("table_g1_corstange_list"), first_number(corstange$list), "",
  "table_g1_corstange_n", "Table G.1", "Corstange (2017), N",
    NA_real_, paper("table_g1_corstange_n"), corstange$n, ""
)

# Figures E.3 to E.9 ----
# The full size versions of the eight panels of Figure 4. They print no estimate on their
# faces, so what the pages state and the pipeline can be held to is how many study
# estimates each plot draws. Figure E.5 draws both sexual orientation subsets in one
# panel, which is why seven figures cover eight subsets.

e_subsets <- list(
  "Figure E.3" = "g1", "Figure E.4" = "g4", "Figure E.5" = c("g7", "g6"),
  "Figure E.6" = "g5", "Figure E.7" = "g9", "Figure E.8" = "g8", "Figure E.9" = "g2"
)

e_rows <-
  tibble(
    table_figure = names(e_subsets),
    value_rewrite = map_dbl(e_subsets, function(s) sum(rewrite_t4$n_studies[rewrite_t4$subset %in% s])),
    value_script = map_dbl(e_subsets, function(s) sum(archive_t4$n_studies[archive_t4$subset %in% s]))
  ) |>
  transmute(
    claim_id = str_c("fig_e", 3:9, "_studies_plotted"),
    table_figure,
    claim = "study estimates plotted",
    value_script,
    value_paper = map_chr(claim_id, paper),
    value_rewrite,
    notes = "The panel prints no estimate on its face, so the check is the number of study rows it draws, counted off the published page"
  )

# The extraction's counts and the page parse must be the same reading of the same figures.
stopifnot(identical(as.character(published_e$studies_plotted), e_rows$value_paper))

# Figures 2 and 3 ----
# The article states these as one sided bounds rather than as printed values, so the test
# is whether the bound holds rather than whether it agrees to a decimal place. The two
# kinds of sentence point in opposite directions. A sentence saying that a bias above some
# level is enough to prefer the list experiment is true when the curve crosses at or below
# that level. A sentence saying that a bias must exceed some level to reach 80% power is
# true when the curve sits at or above it.

# value_script comes from the deposit's own Figure2ab.R, which computes these two curves
# but prints them to the console rather than saving them. Figure3a.R and Figure3b.R do not
# compute their thresholds at all, so those rows have no archive value.

archive_curves <-
  read_csv(here::here("ground_truth", "archive_indifference_curves.csv"), show_col_types = FALSE)

archive_curve <- function(size, quantity) 100 * archive_curves[[quantity]][archive_curves$N == size]

bound_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~bound, ~locus, ~notes,
  "fig2a_rmse_n1000", "Figure 2a", "bias at which list and direct RMSE are equal, N = 1000, points",
    archive_curve(1000, "rmse_target"), paper("fig2a_rmse_n1000"),
    pick_claim(thresholds, "Figure 2a: bias at which RMSE of list equals RMSE of direct, N = 1000", "value_points"),
    "less_or_equal", "paper_internal",
    "Article: at 1,000 subjects the bias must be greater than 5.5 points to prefer a list experiment. The curve the article's own Figure 2a plots crosses at 5.57, so a bias of 5.55 points would not in fact prefer the list experiment and the sentence is marginally too generous",
  "fig2a_rmse_n2000", "Figure 2a", "bias at which list and direct RMSE are equal, N = 2000, points",
    archive_curve(2000, "rmse_target"), paper("fig2a_rmse_n2000"),
    pick_claim(thresholds, "Figure 2a: bias at which RMSE of list equals RMSE of direct, N = 2000", "value_points"),
    "less_or_equal", NA_character_,
    "Article: at 2,000 the bias must be greater than 4 points. The curve crosses at 3.94, so the stated bound holds",
  "fig2b_power_n1000", "Figure 2b", "bias detectable with 80% power, N = 1000, points",
    archive_curve(1000, "power_target"), paper("fig2b_power_n1000"),
    pick_claim(thresholds, "Figure 2b: bias detectable with 80% power, N = 1000", "value_points"),
    "greater_or_equal", NA_character_,
    "Article: the bias would need to exceed 16 percentage points to reach 80% power. The curve puts the threshold at 16.75, so the stated bound holds",
  "fig3a_prevalence_n2000", "Figure 3a", "difference in prevalence detectable with 80% power, N = 2000, points",
    NA_real_, paper("fig3a_prevalence_n2000"),
    pick_claim(thresholds, "Figure 3a: difference in prevalence detectable with 80% power, N = 2000", "value_points"),
    "greater_or_equal", NA_character_,
    "Article: differences must exceed 20 percentage points before a 2,000 unit study has 80% power. The curve puts the threshold at 22.9, so the bound holds but understates the requirement by nearly 3 points. No deposited script prints this quantity",
  "fig3a_prevalence_n3000", "Figure 3a", "difference in prevalence detectable with 80% power, N = 3000, points",
    NA_real_, paper("fig3a_prevalence_n3000"),
    pick_claim(thresholds, "Figure 3a: difference in prevalence detectable with 80% power, N = 3000", "value_points"),
    "greater_or_equal", NA_character_,
    "Article: they must be 18 points or more in the case of a 3,000 unit sample. The curve gives 18.7. No deposited script prints this quantity",
  NA_character_, "Figure 3b", "difference in sensitivity bias detectable with 80% power, N = 2000, points",
    NA_real_, NA_character_,
    pick_claim(thresholds, "Figure 3b: difference in sensitivity bias detectable with 80% power, N = 2000", "value_points"),
    "none", NA_character_,
    "The article describes Figure 3b only as showing that such tests are sample hungry and prints no threshold",
  NA_character_, "Figure 3b", "difference in sensitivity bias detectable with 80% power, N = 3000, points",
    NA_real_, NA_character_,
    pick_claim(thresholds, "Figure 3b: difference in sensitivity bias detectable with 80% power, N = 3000", "value_points"),
    "none", NA_character_, ""
)

# Figure 5 ----

archive_regions <-
  read_csv(here::here("ground_truth", "archive_region_counts.csv"), show_col_types = FALSE)

region_count <- function(data, which_region) data$n[data$region == which_region]

f5_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~locus, ~notes,
  "fig5_below_rmse_curve", "Figure 5", "studies below the RMSE curve",
    region_count(archive_regions, "small"), paper("fig5_below_rmse_curve"),
    region_count(regions, "small"), NA_character_, "",
  "fig5_total_studies", "Figure 5", "studies plotted in total",
    sum(archive_regions$n), paper("fig5_total_studies"), sum(regions$n), NA_character_, "",
  "fig5_between_curves", "Figure 5", "studies between the two curves",
    region_count(archive_regions, "medium"), paper("fig5_between_curves"),
    region_count(regions, "medium"), "unresolved",
    "The deposited Figure5.R returns the same count as the rewrite, so the published figure is one study away from what its own code now produces. That study is Luks (2010), whose estimated bias of 0.2000 sits exactly one grid step, 0.0001, above the power curve at its sample size, which puts it above the curve rather than between the curves. Nothing in the computation draws at random, so the difference is not sampling. What changed between 2020 and now has not been established",
  "fig5_above_both_curves", "Figure 5", "studies above both curves",
    region_count(archive_regions, "big"), paper("fig5_above_both_curves"),
    region_count(regions, "big"), "unresolved", "The counterpart of the row above: the same single study"
)

# Figure F.10 ----

f10_rows <-
  deming |>
  transmute(
    claim_id = NA_character_,
    table_figure = "Figure F.10",
    claim = str_c("Deming regression ",
                  if_else(term == "(Intercept)", "intercept", "slope"),
                  ", ",
                  if_else(polarity == "Prediction: Underreporting", "underreporting", "overreporting")),
    value_script = NA_real_,
    value_paper = NA_character_,
    value_rewrite = estimate,
    notes = "The supplementary text says only that both slopes are close to 1 and prints no coefficient, and plot_direct_v_list.R fits the same regression but saves only the figure. The sentence itself is checked as a descriptive claim"
  )

# In-text quantities ----

theory_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~bound, ~notes,
  "theory_se_direct_prop", "Text, bias-variance trade-off", "SE of the direct question, N = 2000",
    NA_real_, paper("theory_se_direct_prop"), pick_claim(theory, "SE of direct question, N = 2000"), "none",
    "The archive computes this only in a commented block at the foot of theory_list_power_function.R, and that block uses a sensitivity bias of 0.05 and a control item covariance of 0.05 rather than the 0.1 and 0.025 the published sentence implies",
  "theory_se_direct_points", "Text, bias-variance trade-off", "SE of the direct question, N = 2000, points",
    NA_real_, paper("theory_se_direct_points"), 100 * pick_claim(theory, "SE of direct question, N = 2000"), "none", "",
  "theory_se_list_prop", "Text, bias-variance trade-off", "SE of the list experiment, N = 2000",
    NA_real_, paper("theory_se_list_prop"), pick_claim(theory, "SE of list experiment, N = 2000"), "none", "",
  "theory_se_list_points", "Text, bias-variance trade-off", "SE of the list experiment, N = 2000, points",
    NA_real_, paper("theory_se_list_points"), 100 * pick_claim(theory, "SE of list experiment, N = 2000"), "none", "",
  "theory_variance_ratio", "Text, bias-variance trade-off", "ratio of list to direct variance at N = 2000",
    NA_real_, paper("theory_variance_ratio"), pick_claim(theory, "Ratio of variances"), "none",
    "Article: approximately 14 times more variable",
  "discussion_noise_ratio", "Text, discussion", "ratio of list to direct variance, restated in the discussion",
    NA_real_, paper("discussion_noise_ratio"), pick_claim(theory, "Ratio of variances"), "none",
    "The discussion restates the trade-off section's figure. One quantity stated in two places drifts in two places, so the two sentences carry a row each and are checked against each other",
  "theory_n_equal_precision", "Text, bias-variance trade-off", "N at which a list experiment matches a 2,000 subject direct question",
    NA_real_, paper("theory_n_equal_precision"), pick_claim(theory, "N at which list experiment SE first reaches"), "less_or_equal",
    "Article: a researcher would need a sample of 28,000 subjects to match a 2,000 subject direct question. The standard errors cross at 27,904, which the article rounds up to the nearest thousand, so a sample of 28,000 does what the sentence says it does"
)

data_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "design_n_experiments", "Text, meta-analysis design", "list experiments in the meta-analysis",
    NA_real_, paper("design_n_experiments"), pick_claim(summary_stats, "list experiments in the meta-analysis data"), "",
  "design_n_papers", "Text, meta-analysis design", "papers in the meta-analysis",
    NA_real_, paper("design_n_papers"), pick_claim(summary_stats, "papers in the meta-analysis data"), "",
  "vb_range_min", "Figure 4a", "vote buying: most negative study estimate, points",
    NA_real_, paper("vb_range_min"), pick_claim(summary_stats, "most negative study estimate"), "",
  "vb_range_max", "Figure 4a", "vote buying: most positive study estimate, points",
    NA_real_, paper("vb_range_max"), pick_claim(summary_stats, "most positive study estimate"), "",
  "vb_top_estimate", "Figure 4a", "vote buying top row, Kiewiet de Jonge (2015): estimate, points",
    NA_real_, paper("vb_top_estimate"), pick_claim(summary_stats, "top row of Figure 4a (kiewiet2015lies), estimate"), "",
  "vb_top_ci_lower", "Figure 4a", "vote buying top row, Kiewiet de Jonge (2015): 95% CI lower, points",
    NA_real_, paper("vb_top_ci_lower"), pick_claim(summary_stats, "top row of Figure 4a (kiewiet2015lies), 95% CI lower"), "",
  "vb_top_ci_upper", "Figure 4a", "vote buying top row, Kiewiet de Jonge (2015): 95% CI upper, points",
    NA_real_, paper("vb_top_ci_upper"), pick_claim(summary_stats, "top row of Figure 4a (kiewiet2015lies), 95% CI upper"), "",
  "auth_max_estimate", "Text, authoritarian regimes", "largest overreporting study estimate, Kalinin (2015), points",
    NA_real_, paper("auth_max_estimate"), pick_claim(summary_stats, "largest overreporting estimate"), "",
  "auth_min_estimate", "Text, authoritarian regimes", "largest underreporting study estimate, Weghorst (2015), points",
    NA_real_, paper("auth_min_estimate"), pick_claim(summary_stats, "largest underreporting estimate"), ""
)

# In-text restatements of Table 4 ----
# Every one of these is a quantity the article also prints in Table 4, so where the prose
# and the table disagree the pipeline cannot match both.

points <- function(which_subset, quantity) {
  100 * rewrite_t4[[quantity]][rewrite_t4$subset == which_subset]
}

points_archive <- function(which_subset, quantity) {
  100 * archive_t4[[quantity]][archive_t4$subset == which_subset]
}

studies <- function(which_subset) rewrite_t4$n_studies[rewrite_t4$subset == which_subset]

studies_archive <- function(which_subset) archive_t4$n_studies[archive_t4$subset == which_subset]

prose_spec <- tribble(
  ~claim_id, ~table_figure, ~claim, ~subset, ~quantity,
  "vb_n_studies", "Text, vote buying", "studies in the meta-analysis", "g1", "n_studies",
  "vb_mu", "Text, vote buying", "meta-analysis estimate, points", "g1", "mu_estimate",
  "vb_ci_lower", "Text, vote buying", "95% credible interval lower, points", "g1", "ci_lower",
  "vb_ci_upper", "Text, vote buying", "95% credible interval upper, points", "g1", "ci_upper",
  "vb_ppi50_lower", "Text, vote buying", "50% predictive interval lower, points", "g1", "ppi50_lower",
  "vb_ppi50_upper", "Text, vote buying", "50% predictive interval upper, points", "g1", "ppi50_upper",
  "vb_ppi95_lower", "Text, vote buying", "95% predictive interval lower, points", "g1", "ppi95_lower",
  "vb_ppi95_upper", "Text, vote buying", "95% predictive interval upper, points", "g1", "ppi95_upper",
  "turnout_n_studies", "Text, turnout", "studies in the meta-analysis", "g2", "n_studies",
  "turnout_mu", "Text, turnout", "meta-analysis estimate, points", "g2", "mu_estimate",
  "race_n_studies", "Text, racial prejudice", "studies in the meta-analysis", "g4", "n_studies",
  "race_mu", "Text, racial prejudice", "meta-analysis estimate, points", "g4", "mu_estimate",
  "race_ci_lower", "Text, racial prejudice", "95% credible interval lower, points", "g4", "ci_lower",
  "race_ci_upper", "Text, racial prejudice", "95% credible interval upper, points", "g4", "ci_upper",
  "race_ppi50_lower", "Text, racial prejudice", "50% predictive interval lower, points", "g4", "ppi50_lower",
  "race_ppi50_upper", "Text, racial prejudice", "50% predictive interval upper, points", "g4", "ppi50_upper",
  "race_ppi95_lower", "Text, racial prejudice", "95% predictive interval lower, points", "g4", "ppi95_lower",
  "race_ppi95_upper", "Text, racial prejudice", "95% predictive interval upper, points", "g4", "ppi95_upper",
  "religion_mu", "Text, religious prejudice", "meta-analysis estimate, points", "g5", "mu_estimate",
  "so_over_mu", "Text, sexual orientation", "overreporting meta-analysis estimate, points", "g6", "mu_estimate",
  "so_under_mu", "Text, sexual orientation", "underreporting meta-analysis estimate, points", "g7", "mu_estimate",
  "auth_over_n_studies", "Text, authoritarian regimes", "overreporting studies in the meta-analysis", "g8", "n_studies",
  "auth_over_mu", "Text, authoritarian regimes", "overreporting meta-analysis estimate, points", "g8", "mu_estimate",
  "auth_over_ppi50_lower", "Text, authoritarian regimes", "overreporting 50% predictive interval lower, points", "g8", "ppi50_lower",
  "auth_over_ppi50_upper", "Text, authoritarian regimes", "overreporting 50% predictive interval upper, points", "g8", "ppi50_upper",
  "auth_under_n_studies", "Text, authoritarian regimes", "underreporting studies in the meta-analysis", "g9", "n_studies",
  "auth_under_mu", "Text, authoritarian regimes", "underreporting meta-analysis estimate, points", "g9", "mu_estimate",
  "auth_under_ppi50_lower", "Text, authoritarian regimes", "underreporting 50% predictive interval lower, points", "g9", "ppi50_lower",
  "auth_under_ppi50_upper", "Text, authoritarian regimes", "underreporting 50% predictive interval upper, points", "g9", "ppi50_upper",
  "appf_under_mu", "Appendix F", "all underreporting: meta-analysis estimate, points", "g10", "mu_estimate",
  "appf_under_se", "Appendix F", "all underreporting: standard error, points", "g10", "mu_se",
  "appf_over_mu", "Appendix F", "all overreporting: meta-analysis estimate, points", "g11", "mu_estimate",
  "appf_over_se", "Appendix F", "all overreporting: standard error, points", "g11", "mu_se"
)

prose_rows <-
  prose_spec |>
  transmute(
    claim_id,
    table_figure,
    claim,
    value_script = if_else(
      quantity == "n_studies",
      map_dbl(subset, studies_archive),
      map2_dbl(subset, quantity, points_archive)
    ),
    value_paper = map_chr(claim_id, paper),
    value_rewrite = if_else(
      quantity == "n_studies",
      map_dbl(subset, studies),
      map2_dbl(subset, quantity, points)
    ),
    notes = ""
  )

# The three interval widths the prose states instead of the bounds themselves.
width_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "turnout_ppi50_width", "Text, turnout", "width of the 50% predictive interval, points",
    points_archive("g2", "ppi50_upper") - points_archive("g2", "ppi50_lower"),
    paper("turnout_ppi50_width"),
    points("g2", "ppi50_upper") - points("g2", "ppi50_lower"), "",
  "turnout_ppi95_width", "Text, turnout", "width of the 95% predictive interval, points",
    points_archive("g2", "ppi95_upper") - points_archive("g2", "ppi95_lower"),
    paper("turnout_ppi95_width"),
    points("g2", "ppi95_upper") - points("g2", "ppi95_lower"),
    "Table 4's own interval is [-0.12, 0.26], which is 38 points wide, so the sentence disagrees with the table as well as with the code",
  "religion_ci_width", "Text, religious prejudice", "width of the 95% credible interval, points",
    points_archive("g5", "ci_upper") - points_archive("g5", "ci_lower"),
    paper("religion_ci_width"),
    points("g5", "ci_upper") - points("g5", "ci_lower"),
    "Table 4's own interval is [-0.07, 0.04], which is 11 points wide"
)

# Every quantity in these two blocks is also printed in Table 4, and in every case the
# pipeline agrees with the table to the precision the table prints. A mismatch here is
# therefore the article's prose disagreeing with the article's own table, never the
# pipeline.
prose_rows <- bind_rows(prose_rows, width_rows) |> mutate(locus = "paper_internal")

# Descriptive claims ----
# A sentence about shape, sign or count is a claim, and pooled output cannot answer it.
# These read the study level estimates behind Figure 4 and the coefficients behind Figure
# F.10. Where a sentence reduces to a boolean it gets a verdict; where it does not, the
# row carries the evidence and no verdict, because a threshold the sentence never states
# is not ours to invent.

study_estimates <- filter(meta, !row %in% c("Random effects meta-analysis",
                                            "Predictive Intervals (50% and 95%)"))

subset_estimates <- function(s) 100 * study_estimates$estimate[study_estimates$subset == s]

mu_points <- 100 * rewrite_t4$mu_estimate
prejudice_mu <- mu_points[rewrite_t4$subset %in% c("g4", "g5", "g6", "g7")]
deming_slopes <- deming$estimate[deming$term == "list_est"]
sniderman <- subset_estimates("g4")
under_within_five <- 100 * mean(abs(subset_estimates("g10")) <= 5)

# The domain by polarity cells the meta-analysis data contains that Table 4 does not
# break out, which is what its note is about.
broken_out <- c("Vote Buying Under", "Turnout Over", "Race Under", "Religion Under",
                "Sexual Orientation Under", "Sexual Orientation Over",
                "Authoritarian Under", "Authoritarian Over")

left_out_cells <-
  tg |>
  mutate(category = replace_na(category, "Uncategorised")) |>
  count(category, polarity, name = "studies") |>
  filter(!str_c(category, " ", polarity) %in% broken_out)

descriptive_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~bound, ~digits, ~notes,
  "abstract_typical_bias", "Text, abstract", "largest absolute meta-analytic estimate across the ten subsets, points",
    NA_real_, paper("abstract_typical_bias"), max(abs(mu_points)), "approximate", 1L,
    "Abstract: sensitivity biases are typically smaller than 10 percentage points. Nine of the ten subset averages are under 10 points; the tenth, authoritarian overreporting, is 14.2. Typically is a hedge and states no bound the pipeline can be held to, so the row records both numbers and returns no verdict",
  "abstract_some_domains_zero", "Text, abstract", "smallest absolute meta-analytic estimate across the ten subsets, points",
    NA_real_, NA_character_, min(abs(mu_points)), "none", NA_integer_,
    "Abstract: in some domains are approximately zero. Religious prejudice and sexual orientation underreporting are the two smallest and both sit under one point",
  "fig2b_power_n2000_below_target", "Figure 2b", "bias detectable with 80% power at N = 2000, points",
    NA_real_, paper("fig2b_power_n2000_below_target"),
    pick_claim(thresholds, "Figure 2b: bias detectable with 80% power, N = 2000", "value_points"),
    "greater_or_equal", NA_integer_,
    "Article: even at a sample size of 2,000, power to detect biases of 10 percentage points is well below the conventional power target. The curve puts the 80% power threshold at 11.87 points, above 10, so a 10 point bias is indeed under-powered",
  "vb_range_very_large_to_none", "Text, vote buying", "range of the 19 vote buying study estimates, points",
    NA_real_, NA_character_, max(subset_estimates("g1")) - min(subset_estimates("g1")), "none", 1L,
    "Article: a considerable range of bias from very large to none at all. The 19 estimates run from -24.0 to +5.1 points",
  "turnout_ci_crosses_zero", "Text, turnout", "the 95% credible interval on the turnout average contains zero",
    NA_real_, NA_character_,
    as.numeric(points("g2", "ci_lower") < 0 & points("g2", "ci_upper") > 0), "holds", 0L,
    "Article: the confidence interval on the meta-analytic average crosses zero",
  "race_not_significant", "Text, racial prejudice", "the 95% credible interval on the racial prejudice average contains zero",
    NA_real_, NA_character_,
    as.numeric(points("g4", "ci_lower") < 0 & points("g4", "ci_upper") > 0), "holds", 0L,
    "Article: over the nine studies in our sample, the difference between direct questions and list experiments is not statistically significant",
  "multi_investigator_policy", "Text, racial prejudice", "most negative Sniderman et al. (1994) study estimate, points",
    NA_real_, paper("multi_investigator_policy"), min(sniderman), "approximate", 1L,
    "Article: underreporting on the scale of -10 percentage points for policy attitude questions. The two policy items are -12.3 and -9.5 points. On the scale of states no precision to be matched at, so the row records both and returns no verdict",
  "multi_investigator_nonpolicy", "Text, racial prejudice", "most positive Sniderman et al. (1994) study estimate, points",
    NA_real_, paper("multi_investigator_nonpolicy"), max(sniderman), "approximate", 1L,
    "Article: but +10 percentage points for non-policy attitudes like interracial dating and a black family moving in next door. The two non-policy items are +12.0 and +12.4 points",
  "religion_ci_includes_zero", "Text, religious prejudice", "the 95% credible interval on the religious prejudice average contains zero",
    NA_real_, NA_character_,
    as.numeric(points("g5", "ci_lower") < 0 & points("g5", "ci_upper") > 0), "holds", 0L,
    "Article: this estimate is in the expected direction, but the credible interval is 13 points wide and includes zero",
  "religion_biases_scale", "Text, religious prejudice", "largest absolute bound of the religious prejudice 50% predictive interval, points",
    NA_real_, paper("religion_biases_scale"),
    max(abs(c(points("g5", "ppi50_lower"), points("g5", "ppi50_upper")))), "approximate", 1L,
    "Article: biases on the order of approximately five points are consistent with the set of studies in our meta-analysis. The 50% predictive interval runs from -5.0 to 2.4 points, so the largest bias the middle half of the distribution admits is 5.0",
  "so_unexpected_sign", "Text, sexual orientation", "both sexual orientation averages are negative",
    NA_real_, NA_character_,
    as.numeric(points("g6", "mu_estimate") < 0 & points("g7", "mu_estimate") < 0), "holds", 0L,
    "Article: these estimates both have an unexpected sign relative to the predictions and are not distinguishable from zero. Overreporting is predicted for one subset and underreporting for the other, so a negative estimate is against prediction in the first and with it in the second",
  "so_range_low", "Text, sexual orientation", "largest absolute bound of the sexual orientation underreporting 50% predictive interval, points",
    NA_real_, paper("so_range_low"),
    max(abs(c(points("g7", "ppi50_lower"), points("g7", "ppi50_upper")))), "approximate", 1L,
    "Article: the range of plausible sensitivity biases in this literature is on the scale of 5 to 10 points. The two 50% predictive intervals run from -8.1 to 6.2 and from -12.4 to 8.4 points",
  "so_range_high", "Text, sexual orientation", "largest absolute bound of the sexual orientation overreporting 50% predictive interval, points",
    NA_real_, paper("so_range_high"),
    max(abs(c(points("g6", "ppi50_lower"), points("g6", "ppi50_upper")))), "approximate", 1L, "",
  "summary_honest_prejudice", "Text, summary", "largest absolute average across the four prejudice subsets, points",
    NA_real_, NA_character_, max(abs(prejudice_mu)), "none", 1L,
    "Article: subjects appear to honestly report their prejudices based on race, religion, and sexual orientation. Every one of the four prejudice credible intervals contains zero",
  "limit_rule_out_10", "Text, limitations", "subsets whose 95% credible interval excludes an average bias of 10 points",
    NA_real_, paper("limit_rule_out_10"),
    sum(100 * rewrite_t4$ci_upper < 10 | 100 * rewrite_t4$ci_lower > 10), "approximate", 0L,
    "Article: the more cautious interpretation is that we can rule out average biases as large as 10 or 15 percentage points in most cases. value_paper is the size of the bias the sentence names and value_rewrite the count of subsets, of ten, whose interval excludes one that large. Six exclude 10 points and eight exclude 15, so in most cases is doing real work",
  "limit_rule_out_15", "Text, limitations", "subsets whose 95% credible interval excludes an average bias of 15 points",
    NA_real_, paper("limit_rule_out_15"),
    sum(100 * rewrite_t4$ci_upper < 15 | 100 * rewrite_t4$ci_lower > 15), "approximate", 0L, "",
  "appf_slopes_close_to_one", "Appendix F", "largest absolute distance of a Deming slope from 1",
    NA_real_, paper("appf_slopes_close_to_one"), max(abs(deming_slopes - 1)), "approximate", 3L,
    "Appendix F: one measure of the strength of this correlation is the slope of the Deming regressions, both of which are close to 1. The overreporting slope is 0.98 and the underreporting slope is 1.28. Close to states no threshold, so the row records the distance and returns no verdict",
  "appf_overreporting_above_line", "Appendix F", "share of overreporting studies above the 45 degree line, per cent",
    NA_real_, paper("appf_overreporting_above_line"),
    100 * mean(subset_estimates("g11") > 0), "none", 1L,
    "Appendix F: for overreporting, points lie overwhelmingly above the 45 degree line. A point lies above the line when the direct question estimate exceeds the list experiment estimate, which is a positive difference",
  "appf_underreporting_cluster", "Appendix F", "share of underreporting studies within 5 points of the 45 degree line, per cent",
    NA_real_, NA_character_, under_within_five, "none", 1L,
    "Appendix F: whereas for underreporting points cluster tightly around it. The sentence states no threshold, so the row carries the share of the 196 underreporting studies whose difference is within five points of zero rather than a verdict",
  "t4_note_other_categories", "Table 4", "largest domain by polarity cell Table 4 does not break out, in studies",
    NA_real_, paper("t4_note_other_categories"), max(left_out_cells$studies), "approximate", 0L,
    str_glue(
      "Table 4's note: we do not break out studies for other categories, which all have ",
      "fewer than three studies. True of the four literatures the paper names, where the ",
      "only cell left out is racial prejudice overreporting with 2 studies. Not true of ",
      "the data as a whole: the cells Table 4 leaves out are ",
      "{str_c(left_out_cells$category, ' ', left_out_cells$polarity, ' (', left_out_cells$studies, ')', collapse = ', ')}. ",
      "The residual topic categories are left out for a different reason than the sentence gives"
    )
)

# Published quantities with no source in the deposit ----
# These rows have no value because nothing in the archive can produce one. They are the
# part of the published record the deposit does not reach.

unsourced_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~locus, ~notes,
  NA_character_, "Table 1", "Possible sources of sensitivity bias in four literatures",
    NA_real_, NA_character_, NA_real_, NA_character_,
    "A conceptual table of the paper's four criteria applied to four literatures. It carries no computed quantity",
  NA_character_, "Table 2", "Observed list experiment responses",
    NA_real_, NA_character_, NA_real_, NA_character_,
    "A worked illustration of how a list experiment is read. Its fourteen counts are transcribed from Kramon (2016) and the deposit carries no respondent level data for them",
  NA_character_, "Table 3", "Variance reductions from alternative list experiment designs",
    NA_real_, NA_character_, NA_real_, NA_character_,
    "Every figure in this table is quoted from the cited paper that proposed the design. Nothing in the deposit computes any of them",
  NA_character_, "Figure 1", "Estimated prevalence of vote buying, Kramon (2016)",
    NA_real_, NA_character_, NA_real_, "archive",
    "The deposit contains no script and no respondent level data for this figure. It cannot be reproduced from the archive at all",
  NA_character_, "Figure A.1", "Beliefs about who sent the interviewer",
    NA_real_, NA_character_, NA_real_, "archive",
    "The deposit contains no script and no Afrobarometer data for this figure",
  NA_character_, "Figure D.2", "Empirical distribution of each simulation parameter",
    NA_real_, NA_character_, NA_real_, "archive",
    "Two of the four panels, sample size and sensitivity bias, plot quantities the deposited data does carry. The other two do not exist in it: estimates_df.RDS has no variance of the control item count and no covariance between the control item count and the direct question, so the two parameters the theory section quotes as 0.75 and 0.025 cannot be recovered from the deposit at all. No deposited script plots any of the four panels",
  "limit_de_test_n", "Text, limitations", "list experiments with sufficient information for the design effects test",
    NA_real_, paper("limit_de_test_n"), NA_real_, "archive",
    "The design effects test of Blair and Imai (2012) needs the item count distribution by treatment condition, which the deposited study level file does not carry. Neither this count nor the 160 that pass it can be checked against the deposit",
  "limit_de_test_pass", "Text, limitations", "list experiments that pass the design effects test",
    NA_real_, paper("limit_de_test_pass"), NA_real_, "archive",
    "See the row above"
)

# Assembly ----

gt <-
  bind_rows(
    t4_rows,
    h8_rows,
    g_cell_rows,
    g_count_rows,
    g_spot_rows,
    e_rows,
    bound_rows,
    f5_rows,
    f10_rows,
    theory_rows,
    data_rows,
    prose_rows,
    descriptive_rows,
    unsourced_rows
  ) |>
  mutate(bound = replace_na(bound, "none"), digits = replace_na(digits, NA_integer_))

# Agreement at the precision the paper prints ----
# value_paper is the string the page prints, so how many decimals were printed is
# recoverable from it, which a double never records: 0.80 and 0.8 are the same double and
# the article printed only one of them. A value agrees when the pipeline's number,
# formatted to that many decimals, gives the page's digits.

printed_decimals <- function(x) {
  if_else(str_detect(x, fixed(".")), nchar(str_remove(x, "^[^.]*[.]")), 0L)
}

# digits overrides the page's precision, and is set only where the page's own precision
# would destroy the evidence: a sentence saying "close to 1" prints no decimals, so
# formatting a distance of 0.277 to the page gives 0 and says nothing.
format_to_page <- function(value, target, digits = NA_integer_) {
  places <- coalesce(digits, printed_decimals(target), 1L)
  formatted <- map2_chr(value, places, function(v, d) sprintf(str_c("%.", d, "f"), v))
  # sprintf turns a small negative number into "-0.00", which no page ever prints.
  if_else(str_detect(formatted, "^-0[.]?0*$"), str_remove(formatted, "^-"), formatted)
}

# Most claims are printed values and are matched to the last digit the article prints.
# A few are stated as one sided bounds ("must exceed 16 points"), where there is no printed
# value to agree with and the question is whether the bound holds. A descriptive claim that
# reduces to a boolean carries the verdict itself in value_rewrite. The remaining kind is a
# sentence that names a rough magnitude and no precision to match it at ("approximately
# five points", "close to 1", "typically smaller than 10 percentage points"). Those carry
# both numbers and no verdict, because a threshold the sentence never states is not ours
# to invent, and a verdict computed against an invented one would be worse than none.
agrees <- function(value, target, bound) {
  case_when(
    bound == "holds" ~ value,
    bound == "approximate" ~ NA_real_,
    is.na(value) | is.na(target) ~ NA_real_,
    bound == "greater_or_equal" ~ as.numeric(value >= as.numeric(target)),
    bound == "less_or_equal" ~ as.numeric(value <= as.numeric(target)),
    .default = as.numeric(format_to_page(value, target) == target)
  )
}

gt <-
  gt |>
  mutate(
    paper_id = "blair_coppock_moor_2020",
    match = agrees(value_script, value_paper, if_else(bound == "holds", "none", bound)),
    match_rewrite = agrees(value_rewrite, value_paper, bound)
  )

# The precision each claim is printed at, kept for the cross-instrument gate below, which
# runs after the working columns have been dropped.
claim_digits <- gt |> drop_na(claim_id) |> select(claim_id, digits)

# Where a mismatch comes from ----
# The locus travels with the row that declares it rather than being matched back on by
# label afterwards. paper_internal: the article disagrees with itself, and the pipeline
# agrees with one side. environment: the archive and the rewrite agree with each other and
# not with the paper, so the distance belongs to the software this runs on rather than to
# either analysis. archive: the deposit cannot support the claim, so nothing can be
# computed to compare against. A mismatch that reaches this point without a locus is
# recorded as unresolved rather than being attributed by default.

gt <-
  gt |>
  mutate(
    defect_locus = case_when(
      !is.na(match_rewrite) & match_rewrite == 0 & !is.na(locus) ~ locus,
      !is.na(match_rewrite) & match_rewrite == 0 ~ "unresolved",
      is.na(match_rewrite) & !is.na(locus) & locus == "archive" ~ "archive",
      .default = NA_character_
    )
  )

gt <-
  gt |>
  select(paper_id, claim_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, defect_locus, notes)

# Gate: the ground truth may not invent a claim ----

stopifnot(
  all(na.omit(gt$claim_id) %in% published_claims$claim_id),
  !anyDuplicated(na.omit(gt$claim_id))
)

# Gate: every published float is covered ----
# Taken from the article's own inventory of floats rather than from what the pipeline
# happens to produce, so a float with no coverage is visible instead of absent.

published_float_inventory <- c(
  "Table 1", "Table 2", "Table 3", "Table 4",
  "Figure 1", "Figure 2a", "Figure 2b", "Figure 3a", "Figure 3b", "Figure 4a", "Figure 5",
  "Figure A.1", "Figure D.2",
  "Figure E.3", "Figure E.4", "Figure E.5", "Figure E.6", "Figure E.7", "Figure E.8",
  "Figure E.9", "Figure F.10",
  "Table G.1", "Table G.2", "Table G.3", "Table G.4", "Table G.5", "Table G.6", "Table G.7",
  "Tables G.1 to G.7", "Table H.8"
)

float_rows <- filter(gt, str_starts(table_figure, "Table|Figure"))

uncovered_floats <- setdiff(published_float_inventory, float_rows$table_figure)
unlisted_floats <- setdiff(float_rows$table_figure, published_float_inventory)

if (length(uncovered_floats) > 0 || length(unlisted_floats) > 0) {
  stop(str_glue(
    "Published floats with no ground truth row: {str_c(uncovered_floats, collapse = ', ')}. ",
    "Ground truth rows for floats the article does not have: ",
    "{str_c(unlisted_floats, collapse = ', ')}."
  ))
}

# Gate: the second instrument ran, printed, and agrees ----
# maintained/in_text_claims.R reaches the same claimed numbers out of the same outputs by
# its own path, doing its own selection, unit conversion and rounding. It is run here
# rather than read, and what it printed is counted: a block that errors at its first line,
# or that ends in a bare expression and so prints nothing under source(), passes a scan for
# markers while checking nothing at all.

# Sourced into its own environment. Both files necessarily read the same outputs and name
# objects for what they hold, so a bare source() replaces this script's own `published_claims`
# with the claims file's. Identical here, and silently wrong the moment either read diverges.
claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env())
)

printed_claims <-
  tibble(line = claims_output) |>
  filter(str_starts(line, "CLAIM ")) |>
  transmute(
    claim_id = str_match(line, "^CLAIM ([^ ]+) = ")[, 2],
    value_in_text = str_match(line, "^CLAIM [^ ]+ = (.*?) \\|\\| ")[, 2]
  )

stopifnot(!anyDuplicated(printed_claims$claim_id), !any(is.na(printed_claims$claim_id)),
          !any(is.na(printed_claims$value_in_text)))

must_check <- filter(published_claims, claim_type %in% c("pipeline", "descriptive"))

blockless <- setdiff(must_check$claim_id, printed_claims$claim_id)
rowless <- setdiff(must_check$claim_id, na.omit(gt$claim_id))
invented <- setdiff(printed_claims$claim_id, must_check$claim_id)

if (length(blockless) > 0 || length(rowless) > 0 || length(invented) > 0) {
  stop(str_glue(
    "Coverage gate failed. ",
    "Claims with no block in maintained/in_text_claims.R ({length(blockless)}): ",
    "{str_c(head(blockless, 40), collapse = ', ')}. ",
    "Claims with no ground truth row ({length(rowless)}): ",
    "{str_c(head(rowless, 40), collapse = ', ')}. ",
    "Blocks naming a claim the extraction does not require ({length(invented)}): ",
    "{str_c(head(invented, 40), collapse = ', ')}."
  ))
}

stopifnot(nrow(printed_claims) == nrow(must_check))

# The two instruments must land on the same number. in_text_claims.R prints in the
# article's units and rounding and never sees this file, so a disagreement is one of the
# two being wrong and it stops the build.
instrument_disagreements <-
  gt |>
  drop_na(claim_id) |>
  inner_join(claim_digits, by = "claim_id") |>
  inner_join(printed_claims, by = "claim_id") |>
  filter(!is.na(value_rewrite),
         format_to_page(value_rewrite, value_paper, digits) != value_in_text)

if (nrow(instrument_disagreements) > 0) {
  print(select(instrument_disagreements, claim_id, value_paper, value_rewrite, value_in_text), n = 40)
  stop(str_glue(
    "The ground truth and maintained/in_text_claims.R disagree on ",
    "{nrow(instrument_disagreements)} claims."
  ))
}

# Gate: a zero and a locus go together ----
# A zero without a locus reads as a failure of the rewrite, which it almost never is.
# A locus without a zero is a verdict nothing supports. The archive locus is the one
# exception: it marks a row nothing could compute, so it carries no verdict at all.

locus_gate <-
  gt |>
  filter(!is.na(match_rewrite),
         xor(match_rewrite == 0, !is.na(defect_locus)))

if (nrow(locus_gate) > 0) {
  print(select(locus_gate, claim_id, table_figure, claim, match_rewrite, defect_locus), n = 40)
  stop("Rows carrying a mismatch with no locus, or a locus with no mismatch.")
}

write_csv(gt, here::here("ground_truth", "blair_coppock_moor_2020_ground_truth.csv"))

# The errata spine's claim_ids ----
# errata_entries.csv names, for every published entry, the ground-truth claims it corrects.
# Every one of those ids has to exist here: a missing one is a typo or a claim that has since
# been renamed, and a dangling reference inside a document whose whole purpose is correcting
# the record is worse than a failed build.
errata_spine <- here::here("errata_entries.csv")
if (file.exists(errata_spine)) {
  cited_ids <- read_csv(errata_spine, show_col_types = FALSE)$claim_ids |>
    str_split(";") |>
    unlist() |>
    str_trim() |>
    discard(\(x) is.na(x) | x == "")
  dangling <- setdiff(cited_ids, gt$claim_id)
  if (length(dangling) > 0) print(dangling)
  stopifnot(length(dangling) == 0)
}

print(gt |> select(claim_id, table_figure, claim, value_script, value_paper, match, value_rewrite, match_rewrite, defect_locus),
      n = nrow(gt))

print(count(gt, table_figure, match_rewrite), n = 60)
print(count(gt, defect_locus))
print(count(published_claims, claim_type))

print(str_glue(
  "Ground truth: {nrow(gt)} rows over {n_distinct(gt$table_figure)} published floats and ",
  "text sections. Extraction: {nrow(published_claims)} published claims, of which ",
  "{nrow(must_check)} are pipeline or descriptive and are checked twice. ",
  "Rewrite matches published: {sum(gt$match_rewrite == 1, na.rm = TRUE)} of ",
  "{sum(!is.na(gt$match_rewrite))} comparable."
))
