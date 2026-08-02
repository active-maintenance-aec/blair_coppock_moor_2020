# blair_coppock_moor_2020/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_meta_values.csv
# Depends on: original_extracted/ (run download_original.R first)
# Description: Run the deposited meta_analysis.R and record the twenty meta-analytic and
#   predictive interval rows it produces, so that the ground truth carries an archive value
#   beside the rewrite's rather than an assertion that the two agree. Two repairs are
#   applied, both to code that crashes rather than to code that computes a wrong answer:
#   the Stan model is recompiled from the deposited random_effects.stan, and fct_reorder is
#   given an ordering variable with no NA in it. Nothing else in the deposit is changed.
#
#   This script is not part of run_all.R. It answers a question about the archive, not
#   about the rewrite. It takes about a quarter of an hour, almost all of it inside the
#   deposit's Figure2ab.R, which walks a 3,410,341 row parameter grid one row at a time.
#
#   The archive is run in ARCHIVE_RUN_DIR, a scratch copy, never in original_extracted/
#   itself: the deposit's own compile_stan_model.R writes over the deposited
#   data/stan_model.RDS, so an in-place run silently replaces part of the deposit. The
#   default is a session temporary directory; set the variable to keep the archive's
#   figures and tables afterwards.

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_archive_values.R")

run_dir <- Sys.getenv("ARCHIVE_RUN_DIR", unset = file.path(tempdir(), "blair_coppock_moor_2020_archive"))
stopifnot(nzchar(run_dir), dir.exists(here::here("original_extracted")))

# Scratch copy, stripped to code and data ----
# The deposited stan_model.RDS is dropped so that a run cannot pass on a binary the
# deposit shipped while the script that is supposed to rebuild it has failed.

unlink(run_dir, recursive = TRUE)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(here::here("original_extracted", "code"), run_dir, recursive = TRUE)
file.copy(here::here("original_extracted", "data"), run_dir, recursive = TRUE)
unlink(file.path(run_dir, "data", "stan_model.RDS"))

# The deposit's scripts write into these two directories and the deposit does not carry
# them, which is why three of its scripts stop before reaching any R problem.
dir.create(file.path(run_dir, "figures"), showWarnings = FALSE)
dir.create(file.path(run_dir, "tables"), showWarnings = FALSE)

# Repair: fct_reorder on a character vector errors in forcats 1.0.0 when the ordering
# variable contains NA, which it does for the predictive interval row.
helpers_path <- file.path(run_dir, "code", "1_meta_analysis", "0_helpers.R")
helpers_lines <- read_lines(helpers_path)
stopifnot(any(str_detect(helpers_lines, "fct_reorder\\(sensitive_list_item_unique, overreporting\\)")))
write_lines(
  str_replace(
    helpers_lines,
    fixed("fct_reorder(sensitive_list_item_unique, overreporting)"),
    "fct_reorder(sensitive_list_item_unique, if_else(is.na(overreporting), -Inf, overreporting))"
  ),
  helpers_path
)

# Repair: readr removed the path argument at version 2.0.0, so the two simulation scripts
# cannot write or read their intermediate frame.
repair_readr_path <- function(script) {
  path <- file.path(run_dir, "code", "2_simulations", script)
  write_lines(
    read_lines(path) |>
      str_replace(fixed('write_rds(sim_df, path = '), 'write_rds(sim_df, file = ') |>
      str_replace(fixed('read_rds(path = '), 'read_rds('),
    path
  )
}

walk(c("Figure2ab.R", "Figure5.R"), repair_readr_path)

# Run ----
# Two separate R processes, because that is how the archive is meant to be used: one run
# of compile_stan_model.R, then one of meta_analysis.R. Compiling and then re-reading the
# saved binary inside a single session fails with "Failed to initialize module pointer".

write_lines(
  c(
    'library(rstan); library(tidyverse); library(tidybayes); library(xtable); library(patchwork)',
    'source("code/1_meta_analysis/meta_analysis.R")',
    'all_df <- bind_rows(g1 = g1, g2 = g2, g4 = g4, g5 = g5, g6 = g6, g7 = g7, g8 = g8, g9 = g9, g10 = g10, g11 = g11, .id = "subset")',
    'meta_rows <- all_df |>',
    '  filter(grepl("Random|Predictive", sensitive_list_item_unique)) |>',
    '  transmute(subset,',
    '            row = str_trim(as.character(sensitive_list_item_unique)),',
    '            estimate = overreporting, std_error = difference_se,',
    '            ci_lower = difference_ci_lower, ci_upper = difference_ci_upper,',
    '            ppi_50_lower = ppi1.low, ppi_50_upper = ppi1.high,',
    '            ppi_95_lower = ppi2.low, ppi_95_upper = ppi2.high)',
    'n_rows <- all_df |> filter(!grepl("Random|Predictive", sensitive_list_item_unique)) |> count(subset, name = "n_studies")',
    'write_csv(left_join(meta_rows, n_rows, by = "subset"), "archive_meta_values.csv")'
  ),
  file.path(run_dir, "run_meta_analysis.R")
)

# The archive's paths are relative to the archive root, so each child process is pointed
# there on its first line. The working directory of this session is never changed.
rscript <- file.path(R.home("bin"), "Rscript")

run_in_archive <- function(script) {
  bootstrap <- file.path(run_dir, "bootstrap.R")
  write_lines(c(str_glue("setwd({encodeString(run_dir, quote = '\"')})"),
                str_glue("source({encodeString(script, quote = '\"')})")),
              bootstrap)
  system2(rscript, bootstrap)
}

# Figure 2 and Figure 5 ----
# The two simulation scripts print their thresholds and region counts to the console and
# save no numbers, so the values are recomputed here from the frame Figure2ab.R writes and
# the recodes Figure5.R applies, using the archive's own code for both.

write_lines(
  c(
    'library(tidyverse)',
    'source("code/2_simulations/Figure2ab.R")',
    'curves <- sim_df |> group_by(N) |> summarize(power_target = sensitivity_bias[which.min(abs(power - 0.8))], rmse_target = sensitivity_bias[which.min(abs(list_rmse - direct_rmse))])',
    'write_csv(curves, "archive_indifference_curves.csv")',
    'rm(list = ls())',
    'source("code/2_simulations/Figure5.R")',
    'write_csv(count(wide_df, region), "archive_region_counts.csv")'
  ),
  file.path(run_dir, "run_simulations.R")
)

compile_status <- run_in_archive("code/1_meta_analysis/compile_stan_model.R")
meta_status <- run_in_archive("run_meta_analysis.R")
mode_status <- run_in_archive("code/1_meta_analysis/mode_appendix_table.R")
data_status <- run_in_archive("code/1_meta_analysis/table_of_data.R")
sim_status <- run_in_archive("run_simulations.R")

stopifnot(compile_status == 0, meta_status == 0, mode_status == 0, data_status == 0,
          sim_status == 0)

archive_values <- read_csv(file.path(run_dir, "archive_meta_values.csv"), show_col_types = FALSE)

write_csv(archive_values, here::here("ground_truth", "archive_meta_values.csv"))

# Table H.8 and Tables G.1 to G.7 ----
# The two table scripts write LaTeX table bodies with no header and no row names, one row
# per line, cells separated by ampersands. Reading them back is how the archive's numbers
# reach the ground truth without being retyped.

tex_cells <- function(path) {
  read_lines(path) |>
    str_subset("\\\\\\\\\\s*$") |>
    str_remove("\\s*\\\\\\\\\\s*$") |>
    str_split(fixed(" & ")) |>
    map(str_squish)
}

mode_rows <- tex_cells(file.path(run_dir, "tables", "mode_table.tex"))
mode_total <- tex_cells(file.path(run_dir, "tables", "mode_table_total.tex"))

archive_h8 <-
  bind_rows(
    map(mode_rows, \(cells) tibble(
      final_category = cells[1], polarity = cells[2],
      online = as.numeric(cells[3]), person = as.numeric(cells[4]),
      `self-report` = as.numeric(cells[5]), telephone = as.numeric(cells[6])
    )),
    map(mode_total, \(cells) tibble(
      final_category = "Total", polarity = "",
      online = as.numeric(cells[3]), person = as.numeric(cells[4]),
      `self-report` = as.numeric(cells[5]), telephone = as.numeric(cells[6])
    ))
  )

write_csv(archive_h8, here::here("ground_truth", "archive_mode_table.csv"))

table_of_data_files <- c(
  "Vote Buying" = "table_of_data_vote_buying.tex",
  "Race" = "table_of_data_race.tex",
  "Sexual Orientation" = "table_of_data_sexual_orientation.tex",
  "Religion" = "table_of_data_religion.tex",
  "Authoritarian" = "table_of_data_authoritarian.tex",
  "Turnout" = "table_of_data_turnout.tex",
  "Other" = "table_of_data_other.tex"
)

archive_tg <-
  imap(table_of_data_files, \(file, category) tibble(
    category = category,
    n_rows = length(tex_cells(file.path(run_dir, "tables", file)))
  )) |>
  list_rbind()

write_csv(archive_tg, here::here("ground_truth", "archive_table_of_data_counts.csv"))

archive_curves <-
  read_csv(file.path(run_dir, "archive_indifference_curves.csv"), show_col_types = FALSE) |>
  filter(N %in% c(1000, 2000, 3000))

write_csv(archive_curves, here::here("ground_truth", "archive_indifference_curves.csv"))

archive_regions <- read_csv(file.path(run_dir, "archive_region_counts.csv"), show_col_types = FALSE)

write_csv(archive_regions, here::here("ground_truth", "archive_region_counts.csv"))

print(archive_values, n = nrow(archive_values))
print(archive_h8)
print(archive_tg)
print(archive_curves)
print(archive_regions)
