# blair_coppock_moor_2020/run_all.R
# Runs the whole reproduction in order: fetch, verify and unpack the deposited archive,
# then the meta-analysis, the theory simulations, the supplementary figures and tables,
# the in-text quantities, and the design declaration.
# Every script is self-contained apart from the dependencies noted below.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way, then unpacks
# the zip into original_extracted/.
source(here::here("download_original.R"))

# Meta-analysis ----
# figure_4 recompiles the Stan model, fits all ten subsets, and saves the posterior
# summaries; table_4 reads them, so it runs second. Around a minute in total.
source(here::here("maintained", "figure_4_meta_analysis_coefplots.R"))
source(here::here("maintained", "table_4_meta_summary.R"))

# Theory simulations ----
# figure_5 overlays the indifference curves figure_2ab writes, so it runs after it.
source(here::here("maintained", "figure_2ab_theory_power_rmse.R"))
source(here::here("maintained", "figure_3a_group_differences_power.R"))
source(here::here("maintained", "figure_3b_diff_in_sensitivity_power.R"))
source(here::here("maintained", "figure_5_bias_variance_overlay.R"))

# Supplementary figures and tables ----
source(here::here("maintained", "figure_f10_direct_vs_list.R"))
source(here::here("maintained", "table_g_study_estimates.R"))
source(here::here("maintained", "table_h8_mode.R"))

# In-text quantities ----
# text_power_thresholds reads the three simulation curves, so it runs after them.
source(here::here("maintained", "text_theory_variance.R"))
source(here::here("maintained", "text_power_thresholds.R"))
source(here::here("maintained", "text_summary_stats.R"))

# Design declaration ----
# 500 simulated designs, each bootstrapping the sensitivity bias estimator 250 times;
# the slow step, around ninety seconds.
source(here::here("maintained", "declaredesign_list_experiment.R"))

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
source(here::here("ground_truth", "build_ground_truth.R"))
