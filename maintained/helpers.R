# blair_coppock_moor_2020/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Packages and shared helpers for every script in maintained/. Sourced first
#   by each script; no analysis script loads a package of its own.

library(here)
library(tidyverse)
library(rstan)
library(tidybayes)
library(deming)
library(janitor)
library(patchwork)
library(DeclareDesign)
library(bindata)
library(rsample)

here::i_am("maintained/helpers.R")

# Paths ----
# original_extracted/ is the unpacked deposit, rebuilt by download_original.R.

path_original <- function(...) here::here("original_extracted", ...)
path_output <- function(...) here::here("maintained", "output", ...)

# Stan meta-analysis ----
# The archive ships data/stan_model.RDS, a Stan model compiled in 2020 under R 3.6 on
# another machine. rstan rejects it, so the model is recompiled from the deposited
# random_effects.stan in figure_4_meta_analysis_coefplots.R. Nothing compiled is written to
# output/: it would be a platform-specific binary no reader could diff, and saving one and
# reading it back inside the same session fails anyway.

# make_meta_df fits the random effects model to one subset of studies and returns the
# study level estimates, the posterior mean and credible interval, and the 50% and 95%
# predictive intervals, all in the single long frame the coefficient plots expect. The
# predictive interval row carries overreporting = NA by construction, which is what
# distinguishes it from the meta-analytic estimate row downstream. The sampler settings are
# the archive's: 10,000 iterations on four chains at a raised maximum tree depth.

make_meta_df <- function(data, stan_model) {
  ests_df <- data |> mutate(local_study = paste0("study_", seq_len(n())))

  bayes_fit <- sampling(
    stan_model,
    data = list(J = nrow(ests_df), y = ests_df$overreporting, sigma = ests_df$difference_se),
    iter = 10000,
    chains = 4,
    open_progress = FALSE,
    show_messages = FALSE,
    control = list(max_treedepth = 15)
  )

  weights_df <-
    bayes_fit |>
    spread_draws(wt[study]) |>
    ungroup() |>
    mutate(local_study = paste0("study_", study)) |>
    group_by(local_study) |>
    summarize(re_weight = mean(wt)) |>
    mutate(re_weight = re_weight / sum(re_weight))

  rescale_pt <- function(x) (max(x) - min(x)) * 0.75 + min(x)

  mu_tau_summary <-
    bayes_fit |>
    spread_draws(mu, tau) |>
    ungroup() |>
    summarize(
      mu_mean = mean(mu),
      tau_mean = mean(tau),
      ppi1.low = qnorm(p = .25, mean = mu_mean, sd = tau_mean),
      ppi1.high = qnorm(p = .75, mean = mu_mean, sd = tau_mean),
      ppi2.low = qnorm(p = .025, mean = mu_mean, sd = tau_mean),
      ppi2.high = qnorm(p = .975, mean = mu_mean, sd = tau_mean)
    )

  theta_tilde_df <-
    mu_tau_summary |>
    mutate(
      sensitive_list_item_unique = "Predictive Intervals (50% and 95%)",
      col_facet = "Sensitivity bias estimate: Direct - List",
      polarity = "",
      overreporting = NA_real_,
      difference_se = NA_real_,
      difference_ci_lower = NA_real_,
      difference_ci_upper = NA_real_
    )

  # No predictive interval columns here. They belong to the row above, and giving them to
  # the meta-analysis row as well would draw the grey predictive interval bars a second
  # time, on top of the posterior mean and its credible interval.
  mu_tau_df <-
    bayes_fit |>
    spread_draws(mu, tau) |>
    ungroup() |>
    summarize(
      overreporting = mean(mu),
      difference_se = sd(mu),
      difference_ci_lower = quantile(mu, 0.025),
      difference_ci_upper = quantile(mu, 0.975),
      re_weight = rescale_pt(weights_df$re_weight)
    ) |>
    mutate(
      sensitive_list_item_unique = "                          Random effects meta-analysis",
      col_facet = "Sensitivity bias estimate: Direct - List",
      polarity = ""
    )

  ests_df <- left_join(ests_df, weights_df, by = "local_study")

  # fct_reorder on a character vector errors in forcats 1.0.0 when the ordering variable
  # contains NA, which it always does here because the predictive interval row has no
  # point estimate. Sorting that row to -Inf puts it at the bottom, where fct_relevel
  # then moves it, and reproduces the original level order exactly.
  bind_rows(ests_df, theta_tilde_df, mu_tau_df) |>
    mutate(
      sensitive_list_item_unique = fct_reorder(
        sensitive_list_item_unique,
        if_else(is.na(overreporting), -Inf, overreporting)
      ),
      sensitive_list_item_unique = fct_relevel(
        sensitive_list_item_unique,
        "Predictive Intervals (50% and 95%)",
        "Random effects meta-analysis"
      ),
      shape = factor(
        sensitive_list_item_unique == "                          Random effects meta-analysis"
      ),
      polarity = fct_relevel(
        polarity,
        "Prediction: Underreporting",
        "Prediction: Overreporting",
        "Meta-analysis"
      )
    )
}

# Plotting helpers ----

overreporting_plot <- function(gg_df) {
  ggplot(gg_df, aes(x = overreporting, y = sensitive_list_item_unique, shape = shape)) +
    geom_point(aes(size = re_weight)) +
    geom_linerange(aes(xmin = difference_ci_lower, xmax = difference_ci_upper)) +
    geom_linerange(
      aes(xmin = ppi2.low, xmax = ppi2.high),
      color = "grey", linewidth = 1
    ) +
    geom_linerange(
      aes(xmin = ppi1.low, xmax = ppi1.high),
      color = "grey", linewidth = 4
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_shape_manual(values = c(16, 18)) +
    xlab("More underreporting        More overreporting  ") +
    facet_grid(
      rows = vars(polarity), cols = vars(col_facet),
      scales = "free_y", space = "free_y"
    ) +
    coord_cartesian(xlim = c(-.5, .5)) +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 7),
      strip.background = element_blank(),
      strip.text = element_text(size = 10),
      legend.position = "none"
    )
}

overreporting_plot_small_multiples <- function(gg_df, labels = FALSE) {
  fig1 <-
    gg_df |>
    filter(polarity != "") |>
    ggplot(aes(x = overreporting, y = sensitive_list_item_unique, shape = shape)) +
    geom_point(aes(size = re_weight), alpha = 0.6, stroke = 0) +
    geom_linerange(aes(xmin = difference_ci_lower, xmax = difference_ci_upper)) +
    geom_linerange(
      aes(xmin = ppi2.low, xmax = ppi2.high),
      color = "grey", linewidth = 1
    ) +
    geom_linerange(
      aes(xmin = ppi1.low, xmax = ppi1.high),
      color = "grey", linewidth = 4
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_shape_manual(values = c(16, 18)) +
    coord_cartesian(xlim = c(-.5, .5)) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none"
    )

  if (labels) {
    fig1 <- fig1 +
      annotate("text", -0.53, 18, label = "Study estimates", hjust = 0, size = 5)
  }

  fig2 <-
    gg_df |>
    filter(polarity == "") |>
    ggplot(aes(x = overreporting, y = sensitive_list_item_unique, shape = shape)) +
    geom_point(size = 7, alpha = 0.6) +
    geom_linerange(aes(xmin = difference_ci_lower, xmax = difference_ci_upper)) +
    geom_linerange(
      aes(xmin = ppi2.low, xmax = ppi2.high),
      color = "grey", linewidth = 1
    ) +
    geom_linerange(
      aes(xmin = ppi1.low, xmax = ppi1.high),
      color = "grey", linewidth = 4
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_shape_manual(values = c(16, 18)) +
    xlab("More underreporting        More overreporting  ") +
    coord_cartesian(xlim = c(-.5, .5)) +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size = 14, vjust = -0.5, margin = margin(t = 0, r = 0, b = 2.5, l = 0)),
      axis.text.x = element_text(size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none"
    )

  if (labels) {
    fig2 <- fig2 +
      annotate("text", .125, 2, label = "Estimated average bias", hjust = 0, size = 5) +
      annotate("text", -0.53, 2.1, label = "Meta-analysis", hjust = 0, size = 5) +
      annotate("text", .125, 1.05, label = "Predictive interval", hjust = 0, size = 5)
  }

  fig1 / fig2 +
    plot_layout(heights = c(2.5, 1)) +
    plot_annotation(theme = theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0)))
}

save_figure <- function(plot, stem, width, height) {
  ggsave(path_output(paste0(stem, ".pdf")), plot = plot, width = width, height = height)
  ggsave(path_output(paste0(stem, ".png")), plot = plot, width = width, height = height, dpi = 300)
}

# Table formatting helpers ----

add_parens <- function(x, digits = 3) {
  paste0("(", sprintf(paste0("%.", digits, "f"), as.numeric(x)), ")")
}

format_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

make_se_entry <- function(est, se, digits = 2) {
  paste0(format_num(est, digits = digits), " ", add_parens(se, digits = digits))
}

make_interval_entry <- function(conf.low, conf.high, digits = 2) {
  paste0("[", format_num(conf.low, digits = digits), ", ", format_num(conf.high, digits = digits), "]")
}
