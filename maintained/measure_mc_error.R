# Monte Carlo error of the posterior summaries in figure_4_meta_estimates.csv ----
#
# The claims audit compares every Stan summary at the precision the article prints, and on
# a platform whose draws differ from the Mac's those summaries move in the last printed
# digit however the sampler is seeded. in_text_claims.R gives each such claim an mc_band
# sized from the error measured here, so a draw that lands within it scores MEDIUM rather
# than DRIFT. The band is sized from this measurement, never from the gap it has to close.
#
# One fit per subset at figure_4_meta_analysis_coefplots.R's own settings (10,000
# iterations on four chains), and for each column the claims read, the Monte Carlo
# standard error of that summary:
#   estimate      the posterior mean of mu
#   std_error     the posterior sd of mu
#   ci_lower/upper  the 2.5% and 97.5% quantiles of mu
#   ppi_*         qnorm() of the posterior means of mu and tau, so mu_bar + z * tau_bar,
#                 whose error combines the two means' errors; ignoring their correlation
#                 overstates it
#
# Not part of run_all.R: this measures the sampler rather than reproducing the article,
# and takes several minutes. Rerun it when the model, the data, or the sampler settings
# change. Writes maintained/mc_error.csv, in proportions.

# Setup through the compiled model, read from the figure 4 script itself ----
# So the data and the model cannot drift from the ones the figure fits.
figure_4 <- readr::read_lines(here::here("maintained", "figure_4_meta_analysis_coefplots.R"))
setup_end <- which(stringr::str_detect(figure_4, "^meta_fits <- list\\(")) - 1
stopifnot(length(setup_end) == 1)
eval(parse(text = figure_4[seq_len(setup_end)]))
library(posterior)

# Subsets, as figure_4_meta_analysis_coefplots.R names them ----
subsets <- tribble(
  ~subset, ~category, ~prediction,
  "g1", "vote_buying", "Prediction: Underreporting",
  "g2", "turnout", "Prediction: Overreporting",
  "g4", "race", "Prediction: Underreporting",
  "g5", "religion", "Prediction: Underreporting",
  "g6", "sexual_orientation", "Prediction: Overreporting",
  "g7", "sexual_orientation", "Prediction: Underreporting",
  "g8", "authoritarian", "Prediction: Overreporting",
  "g9", "authoritarian", "Prediction: Underreporting",
  "g10", NA, "Prediction: Underreporting",
  "g11", NA, "Prediction: Overreporting"
)

# Fit and measure ----
measure_subset <- function(subset, category, prediction) {
  dat <- wide_df |> filter(polarity == prediction)
  if (!is.na(category)) dat <- dat |> filter(final_category == category)
  fit <- sampling(
    meta_model,
    data = list(J = nrow(dat), y = dat$overreporting, sigma = dat$difference_se),
    iter = 10000,
    chains = 4,
    control = list(max_treedepth = 15),
    refresh = 0,
    show_messages = FALSE,
    seed = 343
  )
  draws <- as_draws_df(fit)
  se_mu <- mcse_mean(draws$mu)
  se_tau <- mcse_mean(draws$tau)
  ppi_50 <- sqrt(se_mu^2 + (qnorm(0.75) * se_tau)^2)
  ppi_95 <- sqrt(se_mu^2 + (qnorm(0.975) * se_tau)^2)
  tibble(
    subset = subset,
    column = c("estimate", "std_error", "ci_lower", "ci_upper", "ppi_50_lower",
               "ppi_50_upper", "ppi_95_lower", "ppi_95_upper"),
    mcse = c(se_mu, mcse_sd(draws$mu), mcse_quantile(draws$mu, 0.025),
             mcse_quantile(draws$mu, 0.975), ppi_50, ppi_50, ppi_95, ppi_95)
  )
}

mc_error <- pmap(subsets, measure_subset) |> list_rbind()
print(mc_error, n = Inf)
write_csv(mc_error, here::here("maintained", "mc_error.csv"))
