# blair_coppock_moor_2020/maintained/text_theory_variance.R
# Output: output/text_theory_variance.csv
# Depends on: helpers.R, original_extracted/code/2_simulations/theory_list_power_function.R
# Description: The bias-variance figures quoted in the theory section: the standard error
#   of the direct question and of the list experiment at N = 2,000, how many times more
#   variable the list experiment is, and the sample size at which a list experiment
#   matches the precision of a direct question with 2,000 subjects. The archive computes
#   these only in a commented-out block at the foot of theory_list_power_function.R, and
#   that block uses different parameter values than the published sentence does.

source(here::here("maintained", "helpers.R"))
source(path_original("code", "2_simulations", "theory_list_power_function.R"))

se_direct_2000 <- sqrt(direct_variance(
  N = 2000,
  prevalence_rate = 0.5,
  sensitivity_bias = 0.1
))

se_list_2000 <- sqrt(list_variance(
  N = 2000,
  prevalence_rate = 0.5,
  variance_control_items = 0.75,
  covariance_control_items_prevalance = 0.025
))

se_list_28000 <- sqrt(list_variance(
  N = 28000,
  prevalence_rate = 0.5,
  variance_control_items = 0.75,
  covariance_control_items_prevalance = 0.025
))

# The sample size at which the list experiment's standard error first falls to that of a
# 2,000 subject direct question, searched over whole subjects rather than solved in
# closed form so the reported figure is the one a reader could look up.
n_grid <- seq(1000, 60000, by = 1)
se_list_grid <- sqrt(list_variance(
  N = n_grid,
  prevalence_rate = 0.5,
  variance_control_items = 0.75,
  covariance_control_items_prevalance = 0.025
))
n_equal_precision <- n_grid[which.max(se_list_grid <= se_direct_2000)]

claims_df <- tibble(
  claim = c(
    "SE of direct question, N = 2000, prevalence 0.5, sensitivity bias 0.1",
    "SE of list experiment, N = 2000, prevalence 0.5, control item variance 0.75",
    "Ratio of variances, list to direct, at N = 2000",
    "SE of list experiment at N = 28000",
    "N at which list experiment SE first reaches the N = 2000 direct question SE"
  ),
  value = c(
    se_direct_2000,
    se_list_2000,
    (se_list_2000 / se_direct_2000)^2,
    se_list_28000,
    n_equal_precision
  )
)

write_csv(claims_df, path_output("text_theory_variance.csv"))

print(claims_df)
