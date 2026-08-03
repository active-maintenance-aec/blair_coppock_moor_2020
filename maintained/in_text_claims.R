# blair_coppock_moor_2020/maintained/in_text_claims.R
# Output: printed to the console; no file
# Depends on: helpers.R, output/figure_4_meta_estimates.csv, output/table_h8_mode.csv,
#   output/table_g_study_estimates.csv, output/text_power_thresholds.csv,
#   output/text_theory_variance.csv, output/text_summary_stats.csv,
#   output/figure_5_region_counts.csv, output/figure_f10_deming_coefficients.csv,
#   ground_truth/published_claims.csv
# Description: Every number the article and its supplementary materials print, beside the
#   sentence that prints it, in the order a reader meets them.
#
#   This file recomputes. It reads the same maintained/output/ files the ground truth
#   reads and does its own selection, unit conversion and rounding, so the two arrive at
#   each number by separate paths and a disagreement between them is a finding rather than
#   a coincidence. ground_truth/build_ground_truth.R runs this file non-interactively,
#   counts the CLAIM lines it printed, and stops if any of them disagrees with its own
#   value. It never reads the ground truth itself, and it never refits: estimation happens
#   once, in the analysis scripts, and only derivation happens twice.
#
#   It does read ground_truth/published_claims.csv, which is the extraction rather than
#   the comparison. A block cannot print a value at the article's own precision without
#   the string the article printed, and quoting that string beside the computed number is
#   what makes the output readable without the article open.
#
#   Every claim prints one line, CLAIM <id> = <value> || <label>. That printed id is the
#   only link between a block and the claim it covers, and it is load bearing: the gate
#   reads this file as a program and matches on what it printed. The "# covers:" comments
#   below are for a reader and nothing reads them, because a comment is a second copy of
#   the link that can go stale independently of the code beside it. cat() is used because
#   a labelled line per claim is what makes the output scannable beside the sentences; it
#   is permitted here and in no other file in this repository.

source(here::here("maintained", "helpers.R"))

options(width = 200)

out <- function(f) read_csv(path_output(f), show_col_types = FALSE)

meta <- out("figure_4_meta_estimates.csv")
h8 <- out("table_h8_mode.csv")
tg <- out("table_g_study_estimates.csv")
thresholds <- out("text_power_thresholds.csv")
theory <- out("text_theory_variance.csv")
summary_stats <- out("text_summary_stats.csv")
regions <- out("figure_5_region_counts.csv")
deming <- out("figure_f10_deming_coefficients.csv")

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character())
)

# Reporting ----
# The article's own precision governs how a value is printed, and the article's own string
# is the only place that precision is recorded: 0.80 and 0.8 are the same double.

page <- function(id) {
  value <- published_claims$value_paper[published_claims$claim_id == id]
  stopifnot(length(value) == 1)
  value
}

# digits defaults to the article's own precision and is given explicitly only where that
# precision would destroy the evidence: a sentence saying "close to 1" prints no decimals,
# so a distance of 0.277 formatted to the page reads 0 and says nothing.
report <- function(id, value, gloss, digits = NULL) {
  stopifnot(length(value) == 1)
  target <- page(id)
  if (is.null(digits)) {
    digits <- if (is.na(target)) 1L
    else if (str_detect(target, fixed("."))) nchar(str_remove(target, "^[^.]*[.]")) else 0L
  }
  text <- if (is.na(value)) "NA" else sprintf(str_c("%.", digits, "f"), value)
  if (str_detect(text, "^-0[.]?0*$")) text <- str_remove(text, "^-")
  cat("CLAIM ", id, " = ", text, " || ", gloss,
      if (is.na(target)) "" else str_c(" [article: ", target, "]"), "\n", sep = "")
}

# Accessors ----
# Each stops unless the filter selects exactly one row, so a claim cannot quietly read a
# row that is not there and print a plausible number from the wrong place.

meta_row <- function(which_subset, which_row) {
  rows <- meta[meta$subset == which_subset & meta$row == which_row, ]
  stopifnot(nrow(rows) == 1)
  rows
}

mu <- function(which_subset, column = "estimate") {
  meta_row(which_subset, "Random effects meta-analysis")[[column]]
}

interval <- function(which_subset, column) {
  meta_row(which_subset, "Predictive Intervals (50% and 95%)")[[column]]
}

# The two summary rows sit in every subset, so the study count is what is left over.
n_studies <- function(which_subset) sum(meta$subset == which_subset) - 2L

# The two summary rows are the only ones with no polarity, which is what tells a study
# row from a summary row without depending on either label or position.
study_estimates <- function(which_subset) {
  meta$estimate[meta$subset == which_subset & !is.na(meta$polarity)]
}

threshold <- function(which_claim) {
  value <- thresholds$value_points[thresholds$claim == which_claim]
  stopifnot(length(value) == 1)
  value
}

quantity <- function(which_claim) {
  value <- summary_stats$value[summary_stats$claim == which_claim]
  stopifnot(length(value) == 1)
  value
}

# Abstract ----

# "We find that sensitivity biases are typically smaller than 10 percentage points and in
# some domains are approximately zero."
# covers: abstract_typical_bias, abstract_some_domains_zero
subset_ids <- unique(meta$subset)
averages <- map_dbl(subset_ids, function(s) 100 * mu(s))

report("abstract_typical_bias", max(abs(averages)),
       "largest absolute average across the ten meta-analysis subsets, points", digits = 1)
report("abstract_some_domains_zero", min(abs(averages)),
       "smallest absolute average across the ten meta-analysis subsets, points")
cat("      all ten averages, points: ",
    str_c(sprintf("%.1f", sort(averages)), collapse = ", "), "\n", sep = "")

# Trade-offs in the Choice of Measurement Design ----

# "Plugging in N = 2,000, pi* = 0.5, and delta = 0.1 and taking the square root yields a
# standard error of the direct question estimator of 0.0110, or 1.10 percentage points."
# covers: theory_se_direct_prop, theory_se_direct_points
se_direct <- theory$value[theory$claim == "SE of direct question, N = 2000, prevalence 0.5, sensitivity bias 0.1"]
stopifnot(length(se_direct) == 1)

report("theory_se_direct_prop", se_direct,
       "standard error of the direct question estimator at N = 2,000")
report("theory_se_direct_points", 100 * se_direct,
       "the same standard error in percentage points")

# "Using the average values of the variance of the control items (0.75) and their
# covariance with D*i (0.025) estimated from the set of studies in our meta-analysis, we
# obtain a standard error for the list experiment of 0.0409, or 4.09 percentage points."
# covers: theory_se_list_prop, theory_se_list_points
se_list <- theory$value[theory$claim == "SE of list experiment, N = 2000, prevalence 0.5, control item variance 0.75"]
stopifnot(length(se_list) == 1)

report("theory_se_list_prop", se_list,
       "standard error of the list experiment estimator at N = 2,000")
report("theory_se_list_points", 100 * se_list,
       "the same standard error in percentage points")

# "For the same number of subjects, the list experiment is (4.09/1.10)^2 = 14 times more
# variable than the direct question."
# covers: theory_variance_ratio
report("theory_variance_ratio", (se_list / se_direct)^2,
       "ratio of the list experiment variance to the direct question variance at N = 2,000")

# "Stated differently, a researcher would need a sample of 28,000 subjects in order to
# produce a list experiment estimate as precise as the direct question with 2,000."
# covers: theory_n_equal_precision
n_equal <- theory$value[theory$claim == "N at which list experiment SE first reaches the N = 2000 direct question SE"]
stopifnot(length(n_equal) == 1)

report("theory_n_equal_precision", n_equal,
       "smallest N at which the list experiment standard error falls to the direct question's")

# "For a study with 1,000 subjects, the bias must be greater than 5.5 points to prefer a
# list experiment; at 2,000, the bias must be greater than 4 points."
# covers: fig2a_rmse_n1000, fig2a_rmse_n2000
report("fig2a_rmse_n1000",
       threshold("Figure 2a: bias at which RMSE of list equals RMSE of direct, N = 1000"),
       "bias at which the two RMSE curves cross at N = 1,000, points")
report("fig2a_rmse_n2000",
       threshold("Figure 2a: bias at which RMSE of list equals RMSE of direct, N = 2000"),
       "bias at which the two RMSE curves cross at N = 2,000, points")

# "At 1,000 subjects, the bias would need to exceed 16 percentage points in order to reach
# 80% power; even at a sample size of 2,000, power to detect biases of 10 percentage
# points is well below the conventional power target."
# covers: fig2b_power_n1000, fig2b_power_n2000_below_target
report("fig2b_power_n1000",
       threshold("Figure 2b: bias detectable with 80% power, N = 1000"),
       "bias detectable with 80% power at N = 1,000, points")
report("fig2b_power_n2000_below_target",
       threshold("Figure 2b: bias detectable with 80% power, N = 2000"),
       "bias detectable with 80% power at N = 2,000, points; a 10 point bias is under-powered when this exceeds 10")

# "Differences must exceed 20 percentage points before a 2,000 unit study has 80% power to
# detect them; they must be 18 points or more in the case of a 3,000 unit sample."
# covers: fig3a_prevalence_n2000, fig3a_prevalence_n3000
report("fig3a_prevalence_n2000",
       threshold("Figure 3a: difference in prevalence detectable with 80% power, N = 2000"),
       "difference in prevalence rates detectable with 80% power at N = 2,000, points")
report("fig3a_prevalence_n3000",
       threshold("Figure 3a: difference in prevalence detectable with 80% power, N = 3000"),
       "difference in prevalence rates detectable with 80% power at N = 3,000, points")

# Meta-analysis Research Design ----

# "We limit all analyses to the 264 (92 papers) list experiments whose sensitive item was
# predicted by the researchers to be over- or underreported, excluding non-sensitive
# topics."
# covers: design_n_experiments, design_n_papers
report("design_n_experiments", quantity("list experiments in the meta-analysis data"),
       "list experiments in the meta-analysis data")
report("design_n_papers", quantity("papers in the meta-analysis data"),
       "distinct papers in the meta-analysis data")

# Meta-analysis Results: Clientelism ----

# "Across 19 studies, we find evidence of moderate underreporting of vote buying."
# covers: vb_n_studies
report("vb_n_studies", n_studies("g1"), "vote buying studies in the meta-analysis")

# "For example, in the top row of Figure 4a, sensitivity bias is estimated to be
# 5 percentage points (95% CI -3 to +14) based on a 2010 survey conducted in Bolivia in
# which the sensitive question read 'They gave you a gift or did you a favor.'"
# covers: vb_top_estimate, vb_top_ci_lower, vb_top_ci_upper
report("vb_top_estimate",
       quantity("vote buying: top row of Figure 4a (kiewiet2015lies), estimate in points"),
       "top row of Figure 4a, estimated sensitivity bias, points")
report("vb_top_ci_lower",
       quantity("vote buying: top row of Figure 4a (kiewiet2015lies), 95% CI lower in points"),
       "top row of Figure 4a, 95% confidence interval lower bound, points")
report("vb_top_ci_upper",
       quantity("vote buying: top row of Figure 4a (kiewiet2015lies), 95% CI upper in points"),
       "top row of Figure 4a, 95% confidence interval upper bound, points")

# "In the top row is the estimated average sensitivity bias across studies for vote-buying
# questions (black diamond), -8 points with a 95% credible interval stretching from -13 to
# -3 points."
# covers: vb_mu, vb_ci_lower, vb_ci_upper
report("vb_mu", 100 * mu("g1"), "vote buying, average sensitivity bias, points")
report("vb_ci_lower", 100 * mu("g1", "ci_lower"),
       "vote buying, 95% credible interval lower bound, points")
report("vb_ci_upper", 100 * mu("g1", "ci_upper"),
       "vote buying, 95% credible interval upper bound, points; Table 4 prints -0.04 for the same bound")

# "As is clear from the dispersion of the study-level estimates that range from -24 points
# to +5 points, the sensitivity bias attending to vote-buying questions differs from
# context to context."
# covers: vb_range_min, vb_range_max, vb_range_very_large_to_none
vote_buying <- 100 * study_estimates("g1")

report("vb_range_min", quantity("vote buying: most negative study estimate, points"),
       "vote buying, most negative study estimate, points")
report("vb_range_max", quantity("vote buying: most positive study estimate, points"),
       "vote buying, most positive study estimate, points")

# "In summary, the theoretical prediction of underreporting bias in direct questions about
# vote buying is supported on average, but there is also a considerable range of bias from
# very large to none at all."
report("vb_range_very_large_to_none", max(vote_buying) - min(vote_buying),
       "vote buying, spread between the largest and smallest study estimate, points")
cat("      all 19 study estimates, points: ",
    str_c(sprintf("%.1f", sort(vote_buying)), collapse = ", "), "\n", sep = "")

# "In the bottom row of Figure 4a, we show the 50% predictive interval from -14 to -2
# points and the 95% predictive interval from -27 to +10 points."
# covers: vb_ppi50_lower, vb_ppi50_upper, vb_ppi95_lower, vb_ppi95_upper
report("vb_ppi50_lower", 100 * interval("g1", "ppi_50_lower"),
       "vote buying, 50% predictive interval lower bound, points")
report("vb_ppi50_upper", 100 * interval("g1", "ppi_50_upper"),
       "vote buying, 50% predictive interval upper bound, points; Table 4 prints -0.03")
report("vb_ppi95_lower", 100 * interval("g1", "ppi_95_lower"),
       "vote buying, 95% predictive interval lower bound, points; Table 4 prints -0.25")
report("vb_ppi95_upper", 100 * interval("g1", "ppi_95_upper"),
       "vote buying, 95% predictive interval upper bound, points; Table 4 prints 0.08")

# Meta-analysis Results: Voter Turnout ----

# "Consistent with theory and the evidence from explicit validation studies, our posterior
# estimate based on 10 studies is that voter turnout is overreported by +7 percentage
# points, but the confidence interval on the meta-analytic average crosses zero."
# covers: turnout_n_studies, turnout_mu, turnout_ci_crosses_zero
report("turnout_n_studies", n_studies("g2"), "turnout studies in the meta-analysis")
report("turnout_mu", 100 * mu("g2"), "turnout, average sensitivity bias, points")
report("turnout_ci_crosses_zero",
       as.numeric(mu("g2", "ci_lower") < 0 & mu("g2", "ci_upper") > 0),
       str_glue("1 when the turnout 95% credible interval contains zero; it runs from ",
                "{sprintf('%.1f', 100 * mu('g2', 'ci_lower'))} to ",
                "{sprintf('%.1f', 100 * mu('g2', 'ci_upper'))} points"), digits = 0)

# "This uncertainty is also reflected in the very wide predictive intervals: the 50%
# interval is 13 points wide and the 95% interval is 45 points wide."
# covers: turnout_ppi50_width, turnout_ppi95_width
report("turnout_ppi50_width",
       100 * (interval("g2", "ppi_50_upper") - interval("g2", "ppi_50_lower")),
       "turnout, width of the 50% predictive interval, points")
report("turnout_ppi95_width",
       100 * (interval("g2", "ppi_95_upper") - interval("g2", "ppi_95_lower")),
       "turnout, width of the 95% predictive interval, points; Table 4's own interval [-0.12, 0.26] is 38 points wide")

# Meta-analysis Results: Prejudice ----

# "For questions about racial prejudice, our summary guess is that if anything, subjects
# overreport racist attitudes by approximately 4 points (95% CI -3 to +9). Over the nine
# studies in our sample, therefore, the difference between direct questions and list
# experiments is not statistically significant."
# covers: race_mu, race_ci_lower, race_ci_upper, race_n_studies, race_not_significant
report("race_mu", 100 * mu("g4"), "racial prejudice, average sensitivity bias, points")
report("race_ci_lower", 100 * mu("g4", "ci_lower"),
       "racial prejudice, 95% credible interval lower bound, points; Table 4 prints -0.02")
report("race_ci_upper", 100 * mu("g4", "ci_upper"),
       "racial prejudice, 95% credible interval upper bound, points")
report("race_n_studies", n_studies("g4"), "racial prejudice studies in the meta-analysis")
report("race_not_significant",
       as.numeric(mu("g4", "ci_lower") < 0 & mu("g4", "ci_upper") > 0),
       "1 when the racial prejudice 95% credible interval contains zero", digits = 0)

# "The 50% predictive interval reaches from +1 to +7 points, but the 95% predictive
# interval admits large negative biases (-16 points) to large positive biases up to
# 16 points."
# covers: race_ppi50_lower, race_ppi50_upper, race_ppi95_lower, race_ppi95_upper
report("race_ppi50_lower", 100 * interval("g4", "ppi_50_lower"),
       "racial prejudice, 50% predictive interval lower bound, points")
report("race_ppi50_upper", 100 * interval("g4", "ppi_50_upper"),
       "racial prejudice, 50% predictive interval upper bound, points")
report("race_ppi95_lower", 100 * interval("g4", "ppi_95_lower"),
       "racial prejudice, 95% predictive interval lower bound, points; Table 4 prints -0.05")
report("race_ppi95_upper", 100 * interval("g4", "ppi_95_upper"),
       "racial prejudice, 95% predictive interval upper bound, points; Table 4 prints 0.12")

# "Our analysis does include the 1994 Multi-Investigator Study (Sniderman, Brady, and
# Tetlock 1994), which estimated underreporting on the scale of -10 percentage points for
# policy attitude questions but +10 percentage points for non-policy attitudes like
# interracial dating and a black family moving in next door."
# covers: multi_investigator_policy, multi_investigator_nonpolicy
sniderman <- 100 * meta$estimate[meta$subset == "g4" & str_starts(meta$row, "Sniderman et al. \\(1994\\)")]
sniderman <- sniderman[!is.na(sniderman)]
stopifnot(length(sniderman) == 4)

report("multi_investigator_policy", min(sniderman),
       "most negative of the four Sniderman et al. (1994) study estimates, points", digits = 1)
report("multi_investigator_nonpolicy", max(sniderman),
       "most positive of the four Sniderman et al. (1994) study estimates, points", digits = 1)
cat("      all four items, points: ",
    str_c(sprintf("%.1f", sort(sniderman)), collapse = ", "), "\n", sep = "")

# "On average, we estimate a -1 point underreporting bias in direct questions thought to be
# prone to underreporting bias. This estimate is in the expected direction, but the
# credible interval is 13 points wide and includes zero."
# covers: religion_mu, religion_ci_width, religion_ci_includes_zero
report("religion_mu", 100 * mu("g5"), "religious prejudice, average sensitivity bias, points")
report("religion_ci_width", 100 * (mu("g5", "ci_upper") - mu("g5", "ci_lower")),
       "religious prejudice, width of the 95% credible interval, points; Table 4's own interval [-0.07, 0.04] is 11 points wide")
report("religion_ci_includes_zero",
       as.numeric(mu("g5", "ci_lower") < 0 & mu("g5", "ci_upper") > 0),
       "1 when the religious prejudice 95% credible interval contains zero", digits = 0)

# "Biases on the order of approximately five points are consistent with the set of studies
# in our meta-analysis."
# covers: religion_biases_scale
# The middle half of the distribution of true biases is what "consistent with the set of
# studies" points at, so the evidence is the largest bias its 50% predictive interval
# admits. The sentence names no precision, so nothing here returns a verdict.
report("religion_biases_scale",
       max(abs(100 * c(interval("g5", "ppi_50_lower"), interval("g5", "ppi_50_upper")))),
       "religious prejudice, largest absolute bound of the 50% predictive interval, points", digits = 1)
cat("      the interval runs from ", sprintf("%.1f", 100 * interval("g5", "ppi_50_lower")),
    " to ", sprintf("%.1f", 100 * interval("g5", "ppi_50_upper")), " points\n", sep = "")

# "For questions thought to be subject to overreporting, the average estimate of
# sensitivity bias is -2 percentage points; for underreporting, the estimate is
# +2 percentage points. These estimates both have an unexpected sign relative to the
# predictions and are not distinguishable from zero."
# covers: so_over_mu, so_under_mu, so_unexpected_sign
report("so_over_mu", 100 * mu("g6"),
       "sexual orientation prejudice, overreporting subset, average sensitivity bias, points")
report("so_under_mu", 100 * mu("g7"),
       "sexual orientation prejudice, underreporting subset, average sensitivity bias, points; Table 4 prints -0.01")
report("so_unexpected_sign", as.numeric(mu("g6") < 0 & mu("g7") < 0),
       "1 when both sexual orientation averages are negative", digits = 0)

# "The range of plausible sensitivity biases in this literature is on the scale of 5 to
# 10 points."
# covers: so_range_low, so_range_high
report("so_range_low",
       max(abs(100 * c(interval("g7", "ppi_50_lower"), interval("g7", "ppi_50_upper")))),
       "sexual orientation underreporting, largest absolute bound of the 50% predictive interval, points", digits = 1)
report("so_range_high",
       max(abs(100 * c(interval("g6", "ppi_50_lower"), interval("g6", "ppi_50_upper")))),
       "sexual orientation overreporting, largest absolute bound of the 50% predictive interval, points", digits = 1)

# Meta-analysis Results: Support for Authoritarian Regimes ----

# "Estimates of overreporting range up to a maximum of +43 points when overreporting is
# predicted (Kalinin 2015) and a minimum of -45 points when underreporting is predicted
# (Weghorst 2015)."
# covers: auth_max_estimate, auth_min_estimate
report("auth_max_estimate",
       quantity("authoritarian: largest overreporting estimate (kalinin2015exploring), points"),
       "largest authoritarian regimes study estimate, points")
report("auth_min_estimate",
       quantity("authoritarian: largest underreporting estimate (weghorst2015political), points"),
       "most negative authoritarian regimes study estimate, points")

# "Based on 21 studies, our meta-analysis estimate of the average level for studies in
# which overreporting is predicted is +14 points and the 50% predictive interval suggests a
# likely level of sensitivity bias between +4 to +24 points."
# covers: auth_over_n_studies, auth_over_mu, auth_over_ppi50_lower, auth_over_ppi50_upper
report("auth_over_n_studies", n_studies("g8"),
       "authoritarian regimes, overreporting studies in the meta-analysis")
report("auth_over_mu", 100 * mu("g8"),
       "authoritarian regimes, overreporting average sensitivity bias, points")
report("auth_over_ppi50_lower", 100 * interval("g8", "ppi_50_lower"),
       "authoritarian regimes, overreporting 50% predictive interval lower bound, points")
report("auth_over_ppi50_upper", 100 * interval("g8", "ppi_50_upper"),
       "authoritarian regimes, overreporting 50% predictive interval upper bound, points")

# "When underreporting is predicted, the meta-analysis average based on 13 studies is
# -8 points, with a 50% credible interval between -16 and 0 points."
# covers: auth_under_n_studies, auth_under_mu, auth_under_ppi50_lower, auth_under_ppi50_upper
report("auth_under_n_studies", n_studies("g9"),
       "authoritarian regimes, underreporting studies in the meta-analysis")
report("auth_under_mu", 100 * mu("g9"),
       "authoritarian regimes, underreporting average sensitivity bias, points")
report("auth_under_ppi50_lower", 100 * interval("g9", "ppi_50_lower"),
       "authoritarian regimes, underreporting 50% predictive interval lower bound, points")
report("auth_under_ppi50_upper", 100 * interval("g9", "ppi_50_upper"),
       "authoritarian regimes, underreporting 50% predictive interval upper bound, points")

# Empirical Distribution of Sensitivity Bias and Sample Size ----

# "Below the lower curve (98 of 264 studies), it is likely that direct questioning would
# have produced answers closer to the truth (in RMSE terms) than the list experiments.
# Between the two curves (107 studies), the choice between list experiments and the direct
# question depends on the goal of the research. ... The studies that are above both curves
# (59 studies) are large enough such that the list experiment is preferred for either
# purpose."
# covers: fig5_below_rmse_curve, fig5_total_studies, fig5_between_curves, fig5_above_both_curves
region <- function(which_region) regions$n[regions$region == which_region]

report("fig5_below_rmse_curve", region("small"), "studies below the RMSE indifference curve")
report("fig5_total_studies", sum(regions$n), "studies plotted in Figure 5")
report("fig5_between_curves", region("medium"), "studies between the two curves")
report("fig5_above_both_curves", region("big"), "studies above both curves")

# Summary of Empirical Results ----

# "Surprisingly to us, subjects appear to honestly report their prejudices based on race,
# religion, and sexual orientation."
# covers: summary_honest_prejudice
prejudice <- c("g4", "g5", "g6", "g7")
prejudice_mu <- map_dbl(prejudice, function(s) 100 * mu(s))
prejudice_zero <- map_lgl(prejudice, function(s) mu(s, "ci_lower") < 0 & mu(s, "ci_upper") > 0)

report("summary_honest_prejudice", max(abs(prejudice_mu)),
       "largest absolute average across the four prejudice subsets, points")
cat("      the four averages, points: ", str_c(sprintf("%.1f", prejudice_mu), collapse = ", "),
    "; credible intervals containing zero: ", sum(prejudice_zero), " of 4\n", sep = "")

# "The more cautious interpretation is that we can rule out average biases as large as
# 10 or 15 percentage points in most cases."
# covers: limit_rule_out_10, limit_rule_out_15
excludes <- function(size) {
  sum(map_lgl(subset_ids, function(s) 100 * mu(s, "ci_upper") < size | 100 * mu(s, "ci_lower") > size))
}

# The article's 10 and 15 are the size of the bias, not a count, so the number printed
# here is the count of subsets that rule one out and the bracketed figure beside it is the
# magnitude the sentence names. "In most cases" states no threshold and gets no verdict.
report("limit_rule_out_10", excludes(10),
       "subsets, of ten, whose 95% credible interval excludes an average bias of 10 points")
report("limit_rule_out_15", excludes(15),
       "subsets, of ten, whose 95% credible interval excludes an average bias of 15 points")

# "In the 166 list experiments for which we have sufficient information, 160 pass the
# design-effects test described in Blair and Imai (2012)."
# covers: limit_de_test_n, limit_de_test_pass
# The deposited study level file carries no item count distribution by treatment
# condition, which is what the design effects test needs, so neither number has a
# counterpart anywhere in the deposit. The block prints what the deposit gives, which is
# nothing, rather than inventing a quantity for it.
report("limit_de_test_n", NA_real_,
       "list experiments with enough information for the design effects test; the deposit carries no item count distribution")
report("limit_de_test_pass", NA_real_,
       "list experiments passing the design effects test; see the line above")

# Discussion ----

# "Under typical conditions, list experiments are approximately 14 times noisier than
# direct questions, which means that either the sample size or the amount of bias needs to
# be large in order to justify a list experiment."
# covers: discussion_noise_ratio
# The same quantity as theory_variance_ratio, stated a second time twelve pages later. A
# quantity stated twice drifts twice, so both sentences are printed and compared.
report("discussion_noise_ratio", (se_list / se_direct)^2,
       "ratio of the list experiment variance to the direct question variance, as restated in the discussion")

# Table 4 ----
# The article's own summary of the meta-analysis: nine quantities for each of ten subsets.
# Read cell by cell rather than through the pivot the ground truth uses.
# covers: t4_*
t4_quantities <- tribble(
  ~quantity, ~gloss,
  "mu_estimate", "average sensitivity bias",
  "mu_se", "standard error of the average",
  "ci_lower", "95% credible interval lower bound",
  "ci_upper", "95% credible interval upper bound",
  "ppi50_lower", "50% predictive interval lower bound",
  "ppi50_upper", "50% predictive interval upper bound",
  "ppi95_lower", "95% predictive interval lower bound",
  "ppi95_upper", "95% predictive interval upper bound",
  "n_studies", "studies"
)

t4_cell <- function(which_subset, which_quantity) {
  switch(
    which_quantity,
    mu_estimate = mu(which_subset),
    mu_se = mu(which_subset, "std_error"),
    ci_lower = mu(which_subset, "ci_lower"),
    ci_upper = mu(which_subset, "ci_upper"),
    ppi50_lower = interval(which_subset, "ppi_50_lower"),
    ppi50_upper = interval(which_subset, "ppi_50_upper"),
    ppi95_lower = interval(which_subset, "ppi_95_lower"),
    ppi95_upper = interval(which_subset, "ppi_95_upper"),
    n_studies = n_studies(which_subset)
  )
}

cat("\nTable 4, meta-analysis estimates of sensitivity bias\n")

# "We do not break out studies for other categories, which all have fewer than three
# studies."
# covers: t4_note_other_categories
# Table 4 breaks out eight of the domain by polarity cells the data contains. The sentence
# is true of the cells belonging to the four literatures the paper names, where the only
# one left out holds two studies. It is not true of the data as a whole: the residual
# topic categories are much larger than three and are left out for a different reason.
# The block prints both counts and returns no verdict, because the sentence does not say
# which set of categories it means.
named_literatures <- c("Vote Buying", "Turnout", "Race", "Religion",
                       "Sexual Orientation", "Authoritarian")

left_out <-
  tg |>
  mutate(category = replace_na(category, "Uncategorised")) |>
  count(category, polarity, name = "studies") |>
  anti_join(
    tribble(
      ~category, ~polarity,
      "Vote Buying", "Under", "Turnout", "Over", "Race", "Under", "Religion", "Under",
      "Sexual Orientation", "Under", "Sexual Orientation", "Over",
      "Authoritarian", "Under", "Authoritarian", "Over"
    ),
    by = c("category", "polarity")
  )

report("t4_note_other_categories", max(left_out$studies),
       str_c("largest cell Table 4 does not break out, in studies; within the four named ",
             "literatures the largest is ",
             max(left_out$studies[left_out$category %in% named_literatures])))
cat("      cells not broken out: ",
    str_c(left_out$category, " ", left_out$polarity, " (", left_out$studies, ")",
          collapse = ", "), "\n", sep = "")

walk(subset_ids, function(s) {
  walk2(t4_quantities$quantity, t4_quantities$gloss, function(q, g) {
    report(str_c("t4_", s, "_", q), t4_cell(s, q), str_c("Table 4, subset ", s, ", ", g))
  })
})

# Appendix E ----
# The full size versions of the eight panels of Figure 4, seven figures because the two
# sexual orientation subsets share one. Nothing is printed on their faces, so what can be
# checked is how many study estimates each draws.
# covers: fig_e3_studies_plotted to fig_e9_studies_plotted
e_panels <- list(
  fig_e3_studies_plotted = "g1", fig_e4_studies_plotted = "g4",
  fig_e5_studies_plotted = c("g7", "g6"), fig_e6_studies_plotted = "g5",
  fig_e7_studies_plotted = "g9", fig_e8_studies_plotted = "g8",
  fig_e9_studies_plotted = "g2"
)

cat("\nAppendix E, study estimates plotted in each figure\n")

iwalk(e_panels, function(subsets, id) {
  report(id, sum(map_dbl(subsets, n_studies)),
         str_c("study rows drawn, subsets ", str_c(subsets, collapse = " and ")))
})

# Appendix F ----

# "One measure of the strength of this correlation is the slope of the Deming regressions,
# both of which are close to 1."
# covers: appf_slopes_close_to_one
slopes <- deming$estimate[deming$term == "list_est"]
stopifnot(length(slopes) == 2)

report("appf_slopes_close_to_one", max(abs(slopes - 1)),
       "largest absolute distance of a Deming slope from 1", digits = 3)
cat("      the two slopes: ", str_c(sprintf("%.2f", slopes), collapse = ", "),
    " (underreporting, overreporting)\n", sep = "")

# "Second, as shown in Table 4, the average sensitivity bias in the case of underreporting
# is -3 points (SE: 1 point). For overreporting, the bias is much larger at +12 points
# (SE: 2 points)."
# covers: appf_under_mu, appf_under_se, appf_over_mu, appf_over_se
report("appf_under_mu", 100 * mu("g10"), "all underreporting studies, average bias, points")
report("appf_under_se", 100 * mu("g10", "std_error"),
       "all underreporting studies, standard error of the average, points")
report("appf_over_mu", 100 * mu("g11"), "all overreporting studies, average bias, points")
report("appf_over_se", 100 * mu("g11", "std_error"),
       "all overreporting studies, standard error of the average, points")

# "For overreporting, points lie overwhelmingly above the 45 degree line, whereas for
# underreporting points cluster tightly around it."
# covers: appf_overreporting_above_line, appf_underreporting_cluster
# A point lies above the 45 degree line when the direct question estimate exceeds the list
# experiment estimate, which is a positive difference. The second half of the sentence
# states no threshold, so the block prints the distribution and leaves the judgment where
# the sentence left it.
over_studies <- 100 * study_estimates("g11")
under_studies <- 100 * study_estimates("g10")

report("appf_overreporting_above_line", 100 * mean(over_studies > 0),
       str_c("share of the ", length(over_studies),
             " overreporting studies above the 45 degree line, per cent"))
report("appf_underreporting_cluster", 100 * mean(abs(under_studies) <= 5),
       str_c("share of the ", length(under_studies),
             " underreporting studies within 5 points of the line, per cent"))
cat("      underreporting studies above the line: ",
    sprintf("%.1f", 100 * mean(under_studies > 0)),
    " per cent; median absolute difference ",
    sprintf("%.1f", median(abs(under_studies))), " points\n", sep = "")

# Appendix G ----
# Seven tables of study level estimates, 239 rows carrying 717 numbers. The published
# cells were read off the page positionally by ground_truth/extract_published_values.R and
# are compared here row by row in the order the page prints them.
# covers: table_g1_cells to table_g7_cells, table_g1_studies_tabulated to
#   table_g7_studies_tabulated, table_g_total_studies_tabulated, table_g1_corstange_*
published_g <- read_csv(here::here("ground_truth", "published_g_table_cells.csv"),
                        show_col_types = FALSE)

g_tables <- c("Vote Buying", "Race", "Sexual Orientation", "Religion", "Authoritarian",
              "Turnout", "Other")

cat("\nAppendix G, study level tables\n")

walk2(g_tables, seq_along(g_tables), function(category, k) {
  rewrite <- tg[!is.na(tg$category) & tg$category == category, ]
  published <- published_g[published_g$table == str_c("Table G.", k), ]
  stopifnot(nrow(rewrite) == nrow(published))

  report(str_c("table_g", k, "_studies_tabulated"), nrow(rewrite),
         str_c("Table G.", k, " (", category, "), studies tabulated"))
  report(str_c("table_g", k, "_cells"),
         sum(rewrite$direct == published$direct) + sum(rewrite$list == published$list) +
           sum(rewrite$n == published$n),
         str_c("Table G.", k, ", cells reproduced of ", 3 * nrow(rewrite), " printed"))
})

report("table_g_total_studies_tabulated", sum(!is.na(tg$category)),
       "studies tabulated across all seven tables; the meta-analysis uses 264, so 25 appear in none of them")

# Corstange (2017) is the one vote buying study with a single row in Table G.1, so it can
# be located by name rather than by position. It is the spot check on the positional parse.
corstange <- tg[tg$study == "corstange2017clientelism", ]
stopifnot(nrow(corstange) == 1)

report("table_g1_corstange_direct", as.numeric(str_extract(corstange$direct, "^[-0-9.]+")),
       "Corstange (2017), direct question estimate")
report("table_g1_corstange_list", as.numeric(str_extract(corstange$list, "^[-0-9.]+")),
       "Corstange (2017), list experiment estimate")
report("table_g1_corstange_n", corstange$n, "Corstange (2017), N")

# Appendix H ----
# Table H.8, the count of studies in each topic by survey mode, read by row and column
# position rather than through the label the ground truth assembles.
# covers: h8_*
h8_rows <- c("vote_buying_under", "turnout_over", "race_under", "religion_under",
             "sexual_orientation_under", "sexual_orientation_over", "authoritarian_under",
             "authoritarian_over", "total")
h8_columns <- c(online = "online", person = "person", self_report = "self-report",
                telephone = "telephone")

cat("\nTable H.8, studies in each topic by survey mode\n")

walk2(h8_rows, seq_along(h8_rows), function(slug, i) {
  iwalk(h8_columns, function(column, mode) {
    report(str_c("h8_", slug, "_", mode), h8[[column]][i],
           str_c("Table H.8, ", str_replace_all(slug, "_", " "), ", ", column))
  })
})
