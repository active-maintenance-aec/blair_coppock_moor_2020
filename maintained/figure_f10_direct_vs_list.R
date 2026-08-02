# blair_coppock_moor_2020/maintained/figure_f10_direct_vs_list.R
# Output: output/figure_f10_direct_vs_list.pdf, .png
#   output/figure_f10_deming_coefficients.csv
# Depends on: helpers.R, original_extracted/data/estimates_df.RDS
# Description: Figure F.10. Direct question estimates against list experiment estimates,
#   separately by predicted direction of misreporting, with a Deming errors-in-variables
#   regression fit. The fitted slopes are the "both of which are close to 1" claim in the
#   supplementary text, so they are written out alongside the figure.

source(here::here("maintained", "helpers.R"))

wide_df <-
  read_rds(path_original("data", "estimates_df.RDS")) |>
  mutate(plot_weights = 1 / difference_se^2) |>
  filter(polarity != "Prediction: Nonsensitive") |>
  mutate(
    polarity = factor(
      polarity,
      levels = c("Prediction: Underreporting", "Prediction: Overreporting")
    )
  )

# deming::deming has no broom method and no predict method, so the archive supplies both.
# They are kept here under names that do not look like S3 methods, since nothing about
# them is registered for dispatch.

deming_tidy <- function(fit) {
  tibble(
    term = names(fit$coefficients),
    estimate = as.vector(fit$coefficients),
    std.error = as.vector(fit$se),
    conf.low = fit$ci[, 1],
    conf.high = fit$ci[, 2]
  )
}

deming_predict <- function(fit, newdata) {
  tval <- qt(.05 / 2, fit$n - 1 - 1, lower.tail = FALSE)
  newdatamat <- as.matrix(newdata)
  bind_cols(
    newdata,
    tibble(
      preds = as.vector(newdatamat %*% fit$coefficients),
      var_fit = apply(newdatamat, 1, function(x) tcrossprod(crossprod(x, fit$variance), x)),
      pred_lower = preds - tval * sqrt(var_fit),
      pred_upper = preds + tval * sqrt(var_fit)
    )
  )
}

fit_deming <- function(data) {
  deming(direct_est ~ list_est, ystd = direct_se, xstd = list_se, data = data)
}

newdata_df <- data.frame(
  `(Intercept)` = 1,
  list_est = seq(-1, 2, length.out = 200),
  check.names = FALSE
)

preds_df <-
  wide_df |>
  group_by(polarity) |>
  reframe(deming_predict(fit_deming(pick(everything())), newdata = newdata_df))

coefficients_df <-
  wide_df |>
  group_by(polarity) |>
  reframe(deming_tidy(fit_deming(pick(everything()))))

write_csv(coefficients_df, path_output("figure_f10_deming_coefficients.csv"))

g1 <-
  ggplot(wide_df, aes(list_est, direct_est, group = polarity)) +
  geom_point(aes(size = plot_weights), alpha = 0.6, stroke = 0) +
  geom_linerange(aes(ymin = direct_ci_lower, ymax = direct_ci_upper), alpha = 0.1) +
  geom_linerange(aes(xmin = list_ci_lower, xmax = list_ci_upper), alpha = 0.1) +
  geom_line(data = preds_df, aes(y = preds)) +
  geom_ribbon(
    data = preds_df,
    aes(y = NULL, ymax = pred_upper, ymin = pred_lower),
    alpha = 0.1, linetype = "blank"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() +
  facet_wrap(~polarity) +
  xlab("List Experiment Estimate") +
  ylab("Direct Question Estimate") +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    panel.grid = element_blank()
  )

save_figure(g1, "figure_f10_direct_vs_list", width = 6.5, height = 3.75)

print(coefficients_df)
