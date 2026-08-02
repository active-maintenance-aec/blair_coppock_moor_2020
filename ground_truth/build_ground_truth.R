# blair_coppock_moor_2020/ground_truth/build_ground_truth.R
# Output: ground_truth/blair_coppock_moor_2020_ground_truth.csv
# Depends on: maintained/output/ (run run_all.R first)
# Description: Assemble the ground truth table. Every value_paper entry was typed from the
#   published article or its supplementary materials and is used only as a comparison
#   target; no published number is an input to any computation here or anywhere in
#   maintained/. Every value_rewrite entry is read back out of maintained/output/, so the
#   table cannot drift from the pipeline. value_script is filled only where a script in the
#   deposited archive computes the quantity itself.

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

# Typed from Table 4 on page 13 of the article, read from a rendered page rather than a
# text extraction so that no cell is mapped to the wrong column.
paper_t4 <- tribble(
  ~subset, ~domain, ~mu_estimate, ~mu_se, ~ci_lower, ~ci_upper, ~ppi50_lower, ~ppi50_upper, ~ppi95_lower, ~ppi95_upper, ~n_studies,
  "g1", "vote buying underreporting", -0.08, 0.02, -0.13, -0.04, -0.14, -0.03, -0.25, 0.08, 19,
  "g2", "turnout overreporting", 0.07, 0.04, -0.01, 0.14, 0.00, 0.13, -0.12, 0.26, 10,
  "g4", "racial prejudice underreporting", 0.04, 0.03, -0.02, 0.09, 0.01, 0.07, -0.05, 0.12, 9,
  "g5", "religious prejudice underreporting", -0.01, 0.03, -0.07, 0.04, -0.05, 0.02, -0.12, 0.09, 12,
  "g7", "sexual orientation underreporting", -0.01, 0.03, -0.08, 0.06, -0.08, 0.06, -0.22, 0.20, 16,
  "g6", "sexual orientation overreporting", -0.02, 0.09, -0.19, 0.14, -0.12, 0.08, -0.31, 0.27, 5,
  "g9", "authoritarian underreporting", -0.08, 0.04, -0.16, 0.00, -0.16, 0.00, -0.31, 0.16, 13,
  "g8", "authoritarian overreporting", 0.14, 0.03, 0.07, 0.21, 0.05, 0.24, -0.14, 0.42, 21,
  "g10", "all results underreporting", -0.04, 0.01, -0.05, -0.02, -0.11, 0.04, -0.26, 0.19, 196,
  "g11", "all results overreporting", 0.12, 0.02, 0.08, 0.15, 0.03, 0.20, -0.13, 0.36, 68
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
  paper_t4 |>
  pivot_longer(-c(subset, domain), names_to = "quantity", values_to = "value_paper") |>
  left_join(
    rewrite_t4 |> pivot_longer(-subset, names_to = "quantity", values_to = "value_rewrite"),
    by = c("subset", "quantity")
  ) |>
  left_join(
    archive_t4 |> pivot_longer(-subset, names_to = "quantity", values_to = "value_script"),
    by = c("subset", "quantity")
  ) |>
  transmute(
    table_figure = "Table 4",
    claim = paste(domain, unname(quantity_labels[quantity])),
    value_script,
    value_paper,
    value_rewrite,
    locus = "environment",
    notes = "The archive's own meta_analysis.R, run from a scratch copy with the Stan model recompiled and the fct_reorder call repaired, returns the value in value_script"
  )

# Table H.8 ----

# The table repeats a domain label only on its first row, so the blank second row of each
# pair carries down from the row above before the label is assembled.
rewrite_h8 <-
  h8 |>
  mutate(domain = final_category, .keep = "unused") |>
  fill(domain, .direction = "down") |>
  pivot_longer(c(online, person, `self-report`, telephone),
               names_to = "mode", values_to = "value_rewrite") |>
  mutate(label = str_squish(paste(domain, replace_na(polarity, ""))))

# Typed from Table H.8 on page 47 of the supplementary materials.
paper_h8 <- tribble(
  ~label, ~mode, ~value_paper,
  "Vote buying Underreporting", "online", 0,
  "Vote buying Underreporting", "person", 19,
  "Vote buying Underreporting", "self-report", 0,
  "Vote buying Underreporting", "telephone", 0,
  "Turnout Overreporting", "online", 4,
  "Turnout Overreporting", "person", 3,
  "Turnout Overreporting", "self-report", 0,
  "Turnout Overreporting", "telephone", 3,
  "Racial prejudice Underreporting", "online", 5,
  "Racial prejudice Underreporting", "person", 0,
  "Racial prejudice Underreporting", "self-report", 0,
  "Racial prejudice Underreporting", "telephone", 4,
  "Religious prejudice Underreporting", "online", 10,
  "Religious prejudice Underreporting", "person", 0,
  "Religious prejudice Underreporting", "self-report", 0,
  "Religious prejudice Underreporting", "telephone", 2,
  "Sexual orientation prejudice Underreporting", "online", 11,
  "Sexual orientation prejudice Underreporting", "person", 0,
  "Sexual orientation prejudice Underreporting", "self-report", 4,
  "Sexual orientation prejudice Underreporting", "telephone", 1,
  "Sexual orientation prejudice Overreporting", "online", 4,
  "Sexual orientation prejudice Overreporting", "person", 1,
  "Sexual orientation prejudice Overreporting", "self-report", 0,
  "Sexual orientation prejudice Overreporting", "telephone", 0,
  "Support for authoritarian regimes Underreporting", "online", 6,
  "Support for authoritarian regimes Underreporting", "person", 7,
  "Support for authoritarian regimes Underreporting", "self-report", 0,
  "Support for authoritarian regimes Underreporting", "telephone", 0,
  "Support for authoritarian regimes Overreporting", "online", 1,
  "Support for authoritarian regimes Overreporting", "person", 20,
  "Support for authoritarian regimes Overreporting", "self-report", 0,
  "Support for authoritarian regimes Overreporting", "telephone", 0,
  "Total", "online", 41,
  "Total", "person", 50,
  "Total", "self-report", 4,
  "Total", "telephone", 10
)

archive_h8 <-
  read_csv(here::here("ground_truth", "archive_mode_table.csv"), show_col_types = FALSE) |>
  fill(final_category, .direction = "down") |>
  pivot_longer(c(online, person, `self-report`, telephone),
               names_to = "mode", values_to = "value_script") |>
  mutate(label = str_squish(paste(final_category, replace_na(polarity, ""))))

h8_rows <-
  paper_h8 |>
  left_join(select(rewrite_h8, label, mode, value_rewrite), by = c("label", "mode")) |>
  left_join(select(archive_h8, label, mode, value_script), by = c("label", "mode")) |>
  transmute(
    table_figure = "Table H.8",
    claim = paste0(label, ", ", mode),
    value_script,
    value_paper,
    value_rewrite,
    notes = ""
  )

# Tables G.1 to G.7 ----
# The seven study level tables print no summary statistic, so what can be checked is how
# many studies each one tabulates and whether a given row's four numbers agree.

paper_g_counts <- tribble(
  ~category, ~table, ~value_paper,
  "Vote Buying", "Table G.1", 19,
  "Race", "Table G.2", 11,
  "Sexual Orientation", "Table G.3", 21,
  "Religion", "Table G.4", 12,
  "Authoritarian", "Table G.5", 34,
  "Turnout", "Table G.6", 10,
  "Other", "Table G.7", 132
)

archive_g_counts <-
  read_csv(here::here("ground_truth", "archive_table_of_data_counts.csv"), show_col_types = FALSE) |>
  rename(value_script = n_rows)

g_count_rows <-
  paper_g_counts |>
  left_join(count(tg, category, name = "value_rewrite"), by = "category") |>
  left_join(archive_g_counts, by = "category") |>
  transmute(
    table_figure = table,
    claim = paste0("studies tabulated (", category, ")"),
    value_script,
    value_paper,
    value_rewrite,
    notes = ""
  )

# Table G.1 cell by cell ----
# Every number Table G.1 prints, typed from the rendered page. The three columns that
# carry values are the direct question estimate and its standard error, the list experiment
# estimate and its standard error, and the sample size. The table has two rows for
# Bottkjaer (2017) whose study and item labels are identical, so rows are matched on the
# order the published table prints them in, which is the order the pipeline produces.

paper_g1_cells <- tribble(
  ~position, ~study, ~direct, ~list, ~n,
  1, "bottkjaer2017", "0.060 (0.008)", "0.080 (0.070)", 1287,
  2, "bottkjaer2017", "0.060 (0.008)", "0.300 (0.060)", 1284,
  3, "ccarkouglu2015gets", "0.162 (0.019)", "0.353 (0.067)", 2012,
  4, "corstange2017clientelism", "0.261 (0.018)", "0.261 (0.073)", 1007,
  5, "gonzalez2012vote", "0.024 (0.006)", "0.243 (0.072)", 995,
  6, "gonzalez2015legitimacy", "0.038 (0.006)", "0.219 (0.064)", 993,
  7, "kiewiet2015lies", "0.005 (0.001)", "0.040 (0.038)", 1987,
  8, "kiewiet2015lies", "0.007 (0.003)", "-0.018 (0.054)", 857,
  9, "kiewiet2015lies", "0.013 (0.004)", "0.059 (0.047)", 752,
  10, "kiewiet2015lies", "0.024 (0.005)", "0.243 (0.072)", 995,
  11, "kiewiet2015lies", "0.038 (0.009)", "0.140 (0.081)", 500,
  12, "kiewiet2015lies", "0.038 (0.006)", "0.219 (0.064)", 993,
  13, "kiewiet2015lies", "0.052 (0.005)", "0.076 (0.045)", 2063,
  14, "kiewiet2015lies", "0.056 (0.005)", "0.029 (0.056)", 1882,
  15, "kiewiet2015lies", "0.073 (0.006)", "0.022 (0.043)", 2086,
  16, "kiewiet2015lies", "0.081 (0.008)", "0.171 (0.050)", 1183,
  17, "kramon2016vote", "0.230 (0.010)", "0.204 (0.049)", 1275,
  18, "kramon2016vote", "0.076 (0.006)", "0.235 (0.052)", 1191,
  19, "mizuno2012does", "0.138 (0.014)", "0.267 (0.069)", 1200
)

rewrite_g1 <-
  tg |>
  filter(category == "Vote Buying") |>
  mutate(position = row_number())

stopifnot(nrow(rewrite_g1) == nrow(paper_g1_cells))

g1_cells_matched <-
  paper_g1_cells |>
  left_join(rewrite_g1, by = "position", suffix = c("_paper", "_rewrite")) |>
  summarize(
    matched = sum(direct_paper == direct_rewrite) +
      sum(list_paper == list_rewrite) +
      sum(n_paper == n_rewrite)
  ) |>
  pull(matched)

# Corstange (2017) is the one vote buying study with a single row in the table, so its
# three numbers can be located by study name alone rather than by position.
corstange <- filter(rewrite_g1, study == "corstange2017clientelism")
stopifnot(nrow(corstange) == 1)

first_number <- function(x) as.numeric(str_extract(x, "^[-0-9.]+"))

g_spot_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "Table G.1", "cells reproduced, of 57 printed in the table",
    NA_real_, 3 * nrow(paper_g1_cells), g1_cells_matched,
    "Every direct question estimate and standard error, list experiment estimate and standard error, and sample size the table prints, compared row by row in the order the table prints them",
  "Table G.1", "Corstange (2017), direct question estimate",
    NA_real_, 0.261, first_number(corstange$direct),
    "The one vote buying study with a single row, so it can be located by name",
  "Table G.1", "Corstange (2017), list experiment estimate",
    NA_real_, 0.261, first_number(corstange$list), "",
  "Table G.1", "Corstange (2017), N",
    NA_real_, 1007, corstange$n, "",
  "Tables G.1 to G.7", "studies tabulated across all seven tables",
    sum(archive_g_counts$value_script), 239, nrow(drop_na(tg, category)),
    "The meta-analysis uses 264 studies, so 25 appear in no supplementary table. Those 25 carry no value in the data file's final_category column, which is the variable the tables are split on"
)

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
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~bound, ~locus, ~notes,
  "Figure 2a", "bias at which list and direct RMSE are equal, N = 1000, points",
    archive_curve(1000, "rmse_target"), 5.5,
    pick_claim(thresholds, "Figure 2a: bias at which RMSE of list equals RMSE of direct, N = 1000", "value_points"),
    "less_or_equal", "paper_internal",
    "Article: at 1,000 subjects the bias must be greater than 5.5 points to prefer a list experiment. The curve the article's own Figure 2a plots crosses at 5.57, so a bias of 5.55 points would not in fact prefer the list experiment and the sentence is marginally too generous",
  "Figure 2a", "bias at which list and direct RMSE are equal, N = 2000, points",
    archive_curve(2000, "rmse_target"), 4,
    pick_claim(thresholds, "Figure 2a: bias at which RMSE of list equals RMSE of direct, N = 2000", "value_points"),
    "less_or_equal", NA_character_,
    "Article: at 2,000 the bias must be greater than 4 points. The curve crosses at 3.94, so the stated bound holds",
  "Figure 2b", "bias detectable with 80% power, N = 1000, points",
    archive_curve(1000, "power_target"), 16,
    pick_claim(thresholds, "Figure 2b: bias detectable with 80% power, N = 1000", "value_points"),
    "greater_or_equal", NA_character_,
    "Article: the bias would need to exceed 16 percentage points to reach 80% power. The curve puts the threshold at 16.75, so the stated bound holds",
  "Figure 3a", "difference in prevalence detectable with 80% power, N = 2000, points",
    NA_real_, 20,
    pick_claim(thresholds, "Figure 3a: difference in prevalence detectable with 80% power, N = 2000", "value_points"),
    "greater_or_equal", NA_character_,
    "Article: differences must exceed 20 percentage points before a 2,000 unit study has 80% power. The curve puts the threshold at 22.9, so the bound holds but understates the requirement by nearly 3 points. No deposited script prints this quantity",
  "Figure 3a", "difference in prevalence detectable with 80% power, N = 3000, points",
    NA_real_, 18,
    pick_claim(thresholds, "Figure 3a: difference in prevalence detectable with 80% power, N = 3000", "value_points"),
    "greater_or_equal", NA_character_,
    "Article: they must be 18 points or more in the case of a 3,000 unit sample. The curve gives 18.7. No deposited script prints this quantity",
  "Figure 3b", "difference in sensitivity bias detectable with 80% power, N = 2000, points",
    NA_real_, NA_real_,
    pick_claim(thresholds, "Figure 3b: difference in sensitivity bias detectable with 80% power, N = 2000", "value_points"),
    "none", NA_character_,
    "The article describes Figure 3b only as showing that such tests are sample hungry and prints no threshold",
  "Figure 3b", "difference in sensitivity bias detectable with 80% power, N = 3000, points",
    NA_real_, NA_real_,
    pick_claim(thresholds, "Figure 3b: difference in sensitivity bias detectable with 80% power, N = 3000", "value_points"),
    "none", NA_character_, ""
)

# Figure 5 ----

archive_regions <-
  read_csv(here::here("ground_truth", "archive_region_counts.csv"), show_col_types = FALSE)

region_count <- function(data, which_region) data$n[data$region == which_region]

f5_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~locus, ~notes,
  "Figure 5", "studies below the RMSE curve",
    region_count(archive_regions, "small"), 98, region_count(regions, "small"),
    NA_character_, "",
  "Figure 5", "studies between the two curves",
    region_count(archive_regions, "medium"), 107, region_count(regions, "medium"),
    "unresolved",
    "The deposited Figure5.R returns the same count as the rewrite, so the published figure is one study away from what its own code now produces. That study is Luks (2010), whose estimated bias of 0.2000 sits exactly one grid step, 0.0001, above the power curve at its sample size, which puts it above the curve rather than between the curves. Nothing in the computation draws at random, so the difference is not sampling. What changed between 2020 and now has not been established",
  "Figure 5", "studies above both curves",
    region_count(archive_regions, "big"), 59, region_count(regions, "big"),
    "unresolved", "The counterpart of the row above: the same single study"
)

# Figure F.10 ----

f10_rows <-
  deming |>
  transmute(
    table_figure = "Figure F.10",
    claim = paste0("Deming regression ",
                   if_else(term == "(Intercept)", "intercept", "slope"),
                   ", ",
                   if_else(polarity == "Prediction: Underreporting", "underreporting", "overreporting")),
    value_script = NA_real_,
    value_paper = NA_real_,
    value_rewrite = estimate,
    notes = "The supplementary text says only that both slopes are close to 1 and prints no coefficient, and plot_direct_v_list.R fits the same regression but saves only the figure"
  )

# In-text quantities ----

theory_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~bound, ~notes,
  "Text, bias-variance trade-off", "SE of the direct question, N = 2000, points",
    NA_real_, 1.10, 100 * pick_claim(theory, "SE of direct question, N = 2000"), "none",
    "The archive computes this only in a commented block at the foot of theory_list_power_function.R, and that block uses a sensitivity bias of 0.05 and a control item covariance of 0.05 rather than the 0.1 and 0.025 the published sentence implies",
  "Text, bias-variance trade-off", "SE of the list experiment, N = 2000, points",
    NA_real_, 4.09, 100 * pick_claim(theory, "SE of list experiment, N = 2000"), "none", "",
  "Text, bias-variance trade-off", "ratio of list to direct variance at N = 2000",
    NA_real_, 14, pick_claim(theory, "Ratio of variances"), "none",
    "Article: approximately 14 times more variable",
  "Text, bias-variance trade-off", "N at which a list experiment matches a 2,000 subject direct question",
    NA_real_, 28000, pick_claim(theory, "N at which list experiment SE first reaches"), "less_or_equal",
    "Article: a researcher would need a sample of 28,000 subjects to match a 2,000 subject direct question. The standard errors cross at 27,904, which the article rounds up to the nearest thousand, so a sample of 28,000 does what the sentence says it does"
)

data_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "Text, meta-analysis design", "list experiments in the meta-analysis",
    NA_real_, 264, pick_claim(summary_stats, "list experiments in the meta-analysis data"), "",
  "Text, meta-analysis design", "papers in the meta-analysis",
    NA_real_, 92, pick_claim(summary_stats, "papers in the meta-analysis data"), "",
  "Figure 4a", "vote buying: most negative study estimate, points",
    NA_real_, -24, pick_claim(summary_stats, "most negative study estimate"), "",
  "Figure 4a", "vote buying: most positive study estimate, points",
    NA_real_, 5, pick_claim(summary_stats, "most positive study estimate"), "",
  "Figure 4a", "vote buying top row, Kiewiet de Jonge (2015): estimate, points",
    NA_real_, 5, pick_claim(summary_stats, "top row of Figure 4a (kiewiet2015lies), estimate"), "",
  "Figure 4a", "vote buying top row, Kiewiet de Jonge (2015): 95% CI lower, points",
    NA_real_, -3, pick_claim(summary_stats, "top row of Figure 4a (kiewiet2015lies), 95% CI lower"), "",
  "Figure 4a", "vote buying top row, Kiewiet de Jonge (2015): 95% CI upper, points",
    NA_real_, 14, pick_claim(summary_stats, "top row of Figure 4a (kiewiet2015lies), 95% CI upper"), "",
  "Text, authoritarian regimes", "largest overreporting study estimate, Kalinin (2015), points",
    NA_real_, 43, pick_claim(summary_stats, "largest overreporting estimate"), "",
  "Text, authoritarian regimes", "largest underreporting study estimate, Weghorst (2015), points",
    NA_real_, -45, pick_claim(summary_stats, "largest underreporting estimate"), ""
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

prose_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "Text, vote buying", "meta-analysis estimate, points",
    points_archive("g1", "mu_estimate"), -8, points("g1", "mu_estimate"), "",
  "Text, vote buying", "95% credible interval lower, points",
    points_archive("g1", "ci_lower"), -13, points("g1", "ci_lower"), "",
  "Text, vote buying", "95% credible interval upper, points",
    points_archive("g1", "ci_upper"), -3, points("g1", "ci_upper"),
    "Table 4 prints -0.04 for the same bound, which is -3.7 points; the sentence rounds it to -3",
  "Text, vote buying", "50% predictive interval lower, points",
    points_archive("g1", "ppi50_lower"), -14, points("g1", "ppi50_lower"), "",
  "Text, vote buying", "50% predictive interval upper, points",
    points_archive("g1", "ppi50_upper"), -2, points("g1", "ppi50_upper"),
    "Table 4 prints -0.03 for the same bound",
  "Text, vote buying", "95% predictive interval lower, points",
    points_archive("g1", "ppi95_lower"), -27, points("g1", "ppi95_lower"),
    "Table 4 prints -0.25 for the same interval; the sentence says -27",
  "Text, vote buying", "95% predictive interval upper, points",
    points_archive("g1", "ppi95_upper"), 10, points("g1", "ppi95_upper"),
    "Table 4 prints 0.08 for the same interval; the sentence says +10",
  "Text, turnout", "width of the 50% predictive interval, points",
    points_archive("g2", "ppi50_upper") - points_archive("g2", "ppi50_lower"), 13,
    points("g2", "ppi50_upper") - points("g2", "ppi50_lower"), "",
  "Text, turnout", "width of the 95% predictive interval, points",
    points_archive("g2", "ppi95_upper") - points_archive("g2", "ppi95_lower"), 45,
    points("g2", "ppi95_upper") - points("g2", "ppi95_lower"),
    "Table 4's own interval is [-0.12, 0.26], which is 38 points wide, so the sentence disagrees with the table as well as with the code",
  "Text, racial prejudice", "meta-analysis estimate, points",
    points_archive("g4", "mu_estimate"), 4, points("g4", "mu_estimate"), "",
  "Text, racial prejudice", "95% credible interval lower, points",
    points_archive("g4", "ci_lower"), -3, points("g4", "ci_lower"),
    "Table 4 prints -0.02 for the same bound",
  "Text, racial prejudice", "95% credible interval upper, points",
    points_archive("g4", "ci_upper"), 9, points("g4", "ci_upper"), "",
  "Text, racial prejudice", "50% predictive interval lower, points",
    points_archive("g4", "ppi50_lower"), 1, points("g4", "ppi50_lower"), "",
  "Text, racial prejudice", "50% predictive interval upper, points",
    points_archive("g4", "ppi50_upper"), 7, points("g4", "ppi50_upper"), "",
  "Text, racial prejudice", "95% predictive interval lower, points",
    points_archive("g4", "ppi95_lower"), -16, points("g4", "ppi95_lower"),
    "Table 4 prints -0.05 for the same interval; the sentence says -16",
  "Text, racial prejudice", "95% predictive interval upper, points",
    points_archive("g4", "ppi95_upper"), 16, points("g4", "ppi95_upper"),
    "Table 4 prints 0.12 for the same interval; the sentence says +16",
  "Text, religious prejudice", "meta-analysis estimate, points",
    points_archive("g5", "mu_estimate"), -1, points("g5", "mu_estimate"), "",
  "Text, religious prejudice", "width of the 95% credible interval, points",
    points_archive("g5", "ci_upper") - points_archive("g5", "ci_lower"), 13,
    points("g5", "ci_upper") - points("g5", "ci_lower"),
    "Table 4's own interval is [-0.07, 0.04], which is 11 points wide",
  "Text, sexual orientation", "overreporting meta-analysis estimate, points",
    points_archive("g6", "mu_estimate"), -2, points("g6", "mu_estimate"), "",
  "Text, sexual orientation", "underreporting meta-analysis estimate, points",
    points_archive("g7", "mu_estimate"), 2, points("g7", "mu_estimate"),
    "Table 4 prints -0.01 for the same quantity; the sentence says +2",
  "Text, authoritarian regimes", "overreporting meta-analysis estimate, points",
    points_archive("g8", "mu_estimate"), 14, points("g8", "mu_estimate"), "",
  "Text, authoritarian regimes", "overreporting 50% predictive interval lower, points",
    points_archive("g8", "ppi50_lower"), 4, points("g8", "ppi50_lower"), "",
  "Text, authoritarian regimes", "overreporting 50% predictive interval upper, points",
    points_archive("g8", "ppi50_upper"), 24, points("g8", "ppi50_upper"), "",
  "Text, authoritarian regimes", "underreporting meta-analysis estimate, points",
    points_archive("g9", "mu_estimate"), -8, points("g9", "mu_estimate"), "",
  "Text, authoritarian regimes", "underreporting 50% predictive interval lower, points",
    points_archive("g9", "ppi50_lower"), -16, points("g9", "ppi50_lower"), "",
  "Text, authoritarian regimes", "underreporting 50% predictive interval upper, points",
    points_archive("g9", "ppi50_upper"), 0, points("g9", "ppi50_upper"), "",
  "Appendix F", "all underreporting: meta-analysis estimate, points",
    points_archive("g10", "mu_estimate"), -3, points("g10", "mu_estimate"), "",
  "Appendix F", "all underreporting: standard error, points",
    100 * archive_t4$mu_se[archive_t4$subset == "g10"], 1,
    100 * rewrite_t4$mu_se[rewrite_t4$subset == "g10"], "",
  "Appendix F", "all overreporting: meta-analysis estimate, points",
    points_archive("g11", "mu_estimate"), 12, points("g11", "mu_estimate"), "",
  "Appendix F", "all overreporting: standard error, points",
    100 * archive_t4$mu_se[archive_t4$subset == "g11"], 2,
    100 * rewrite_t4$mu_se[rewrite_t4$subset == "g11"], ""
)

# Every quantity in this block is also printed in Table 4, and in every case the pipeline
# agrees with the table to the precision the table prints. A mismatch here is therefore the
# article's prose disagreeing with the article's own table, never the pipeline.
prose_rows <- mutate(prose_rows, locus = "paper_internal")

# Published quantities with no source in the deposit ----
# These rows have no value because nothing in the archive can produce one. They are the
# part of the published record the deposit does not reach.

unsourced_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "Table 1", "Possible sources of sensitivity bias in four literatures",
    NA_real_, NA_real_, NA_real_,
    "A conceptual table of the paper's four criteria applied to four literatures. It carries no computed quantity",
  "Table 2", "Observed list experiment responses",
    NA_real_, NA_real_, NA_real_,
    "A worked illustration of how a list experiment is read, with invented counts. It carries no quantity estimated from data",
  "Table 3", "Variance reductions from alternative list experiment designs",
    NA_real_, NA_real_, NA_real_,
    "Every figure in this table is quoted from the cited paper that proposed the design. Nothing in the deposit computes any of them",
  "Figure 1", "Estimated prevalence of vote buying, Kramon (2016)",
    NA_real_, NA_real_, NA_real_,
    "The deposit contains no script and no respondent level data for this figure. It cannot be reproduced from the archive at all",
  "Figure A.1", "Beliefs about who sent the interviewer",
    NA_real_, NA_real_, NA_real_,
    "The deposit contains no script and no data for this figure",
  "Figure D.2", "Empirical distribution of each simulation parameter",
    NA_real_, NA_real_, NA_real_,
    "The four distributions are drawn from the meta-analysis data the deposit does contain, but the deposit ships no script that plots them",
  "Text, limitations", "list experiments with sufficient information for the design effects test",
    NA_real_, 166, NA_real_,
    "The design effects test of Blair and Imai (2012) needs the item count distribution by treatment condition, which the deposited study level file does not carry. Neither this count nor the 160 that pass it can be checked against the deposit",
  "Text, limitations", "list experiments that pass the design effects test",
    NA_real_, 160, NA_real_,
    "See the row above"
)

# Assembly ----

gt <-
  bind_rows(
    t4_rows,
    h8_rows,
    g_count_rows,
    g_spot_rows,
    bound_rows,
    f5_rows,
    f10_rows,
    theory_rows,
    data_rows,
    prose_rows,
    unsourced_rows
  ) |>
  mutate(bound = replace_na(bound, "none"))

# Agreement at the precision the paper prints ----

printed_decimals <- function(x) {
  map_dbl(x, function(v) {
    if (is.na(v)) return(NA_real_)
    txt <- formatC(v, format = "fg", digits = 15, flag = "#")
    txt <- str_remove(txt, "0+$")
    if (str_detect(txt, "\\.")) nchar(str_remove(txt, "^.*\\.")) else 0
  })
}

# Most claims are printed values and are matched to the last digit the article prints.
# A few are stated as one sided bounds ("must exceed 16 points"), where there is no printed
# value to agree with and the question is whether the bound holds.
agrees <- function(value, target, bound) {
  case_when(
    is.na(value) | is.na(target) ~ NA_real_,
    bound == "greater_or_equal" ~ as.numeric(value >= target),
    bound == "less_or_equal" ~ as.numeric(value <= target),
    abs(value - target) <= 0.5 * 10^(-printed_decimals(target)) ~ 1,
    .default = 0
  )
}

gt <-
  gt |>
  mutate(
    paper_id = "blair_coppock_moor_2020",
    match = agrees(value_script, value_paper, bound),
    match_rewrite = agrees(value_rewrite, value_paper, bound)
  )

# Where a mismatch comes from ----
# The locus travels with the row that declares it rather than being matched back on by
# label afterwards. paper_internal: the article disagrees with itself, and the pipeline
# agrees with one side. environment: the archive and the rewrite agree with each other and
# not with the paper, so the distance belongs to the software this runs on rather than to
# either analysis. A mismatch that reaches this point without a locus is recorded as
# unresolved rather than being attributed by default.

gt <-
  gt |>
  mutate(
    defect_locus = case_when(
      match_rewrite != 0 | is.na(match_rewrite) ~ NA_character_,
      !is.na(locus) ~ locus,
      .default = "unresolved"
    )
  )

gt <-
  gt |>
  select(paper_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, defect_locus, notes)

write_csv(gt, here::here("ground_truth", "blair_coppock_moor_2020_ground_truth.csv"))

print(gt |> select(table_figure, claim, value_script, value_paper, match, value_rewrite, match_rewrite, defect_locus),
      n = nrow(gt))

print(count(gt, table_figure, match_rewrite), n = 40)
print(count(gt, defect_locus))
