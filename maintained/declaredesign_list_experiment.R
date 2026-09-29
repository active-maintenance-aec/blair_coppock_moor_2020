# blair_coppock_moor_2020/maintained/declaredesign_list_experiment.R
# Output: output/declaredesign_simulation_summary.csv
# Depends on: helpers.R
# Description: The illustrative DeclareDesign declaration of a list experiment from the
#   supplementary materials, rewritten against the DeclareDesign 1.0 API. Three
#   estimators are simulated: the direct question and the list experiment, both targeting
#   the true prevalence rate, and the difference between them, targeting the sensitivity
#   bias. No number here appears in the published paper; the declaration is a worked
#   example of the design the paper analyses.
#
#   API changes from the archive: declare_population becomes declare_model, the separate
#   declare_step for the control item count folds into it, declare_potential_outcomes
#   becomes potential_outcomes() inside declare_model plus declare_measurement,
#   declare_estimand becomes declare_inquiry, declare_assignment(prob = 0.5) becomes
#   declare_assignment(Z = complete_ra(N, prob = 0.5)), model = becomes .method =, and
#   tidy_estimator() becomes label_estimator().
#
#   Bug fix: the archive computes delta on the line before pi_star is defined, so it
#   fails in a clean session. The two lines are swapped here.

source(here::here("maintained", "helpers.R"))

set.seed(42)

# Design parameters ----

sample_size <- 2000
control_item_probs <- c(0.5, 0.5, 0.3, 0.6)
control_item_corr <- cbind(
  c(1, -0.4, 0, 0),
  c(-0.4, 1, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 0, 1)
)

pi_star <- 0.5
pr_overreporters <- 0.2
pr_underreporters <- 0.0
delta <- (pr_overreporters - pr_underreporters) * pi_star

# Sensitivity bias estimator ----
# Direct minus list, with a bootstrap standard error. label_estimator is the
# DeclareDesign 1.0 successor to tidy_estimator, which the archive used here.

estimate_sensitivity_bias <- function(data, alpha = 0.05, sims = 250) {
  direct_minus_list <- function(data) {
    with(data, mean(D) - (mean(Y[Z == 1]) - mean(Y[Z == 0])))
  }
  boot_dist <-
    bootstraps(data, times = sims, apparent = TRUE) |>
    mutate(estimate = map_dbl(splits, ~ direct_minus_list(analysis(.)))) |>
    pull(estimate)

  tibble(
    estimate = direct_minus_list(data),
    std.error = sd(boot_dist),
    statistic = estimate / std.error,
    p.value = pnorm(abs(statistic), lower.tail = FALSE) * 2,
    conf.low = quantile(boot_dist, alpha / 2),
    conf.high = quantile(boot_dist, 1 - (alpha / 2))
  )
}

# Design declaration ----

list_exp_design <-
  declare_model(
    N = sample_size,
    type = sample(
      x = c("over_reporter", "truthful_reporter", "under_reporter"),
      size = N,
      replace = TRUE,
      prob = c(
        pr_overreporters,
        1 - (pr_overreporters + pr_underreporters),
        pr_underreporters
      )
    ),
    D_star = rbinom(N, size = 1, prob = pi_star),
    D = case_when(
      type == "over_reporter" ~ 1L,
      type == "truthful_reporter" & D_star == 1 ~ 1L,
      type == "truthful_reporter" & D_star == 0 ~ 0L,
      type == "under_reporter" ~ 0L
    ),
    # fabricatr 2.0 splits a matrix-valued column into control_item.1 to .4 as it
    # stores it, so the items are summed where they are drawn. The draws are unchanged.
    control_item_count = rowSums(
      rmvbin(n = N, margprob = control_item_probs, bincorr = control_item_corr)
    ),
    potential_outcomes(Y ~ control_item_count + Z * D_star)
  ) +
  declare_inquiry(pi_star = pi_star, delta = delta) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    D ~ 1,
    .method = lm_robust,
    term = "(Intercept)",
    inquiry = "pi_star",
    label = "direct"
  ) +
  declare_estimator(
    Y ~ Z,
    .method = difference_in_means,
    inquiry = "pi_star",
    label = "list"
  ) +
  declare_estimator(
    handler = label_estimator(estimate_sensitivity_bias),
    inquiry = "delta",
    label = "direct-list"
  )

# Simulate ----

simulations <- simulate_design(list_exp_design, sims = 500)

summary_df <-
  simulations |>
  group_by(estimator, inquiry) |>
  summarize(
    bias = mean(estimate - estimand),
    se = sd(estimate),
    rmse = sqrt(mean((estimate - estimand)^2)),
    power = mean(p.value < .05, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_df, path_output("declaredesign_simulation_summary.csv"))

print(summary_df)
