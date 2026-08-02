# blair_coppock_moor_2020/maintained/table_g_study_estimates.R
# Output: output/table_g_study_estimates.csv
# Depends on: helpers.R, original_extracted/data/estimates_df.RDS
# Description: Tables G.1 to G.7, the study level estimates behind the meta-analysis. The
#   archive writes these as seven LaTeX fragments, one per domain, each escaped for a
#   \citet call; here they are one tidy file with a category column, which carries the
#   same numbers and can be read without a LaTeX toolchain.

source(here::here("maintained", "helpers.R"))

wide_df <- read_rds(path_original("data", "estimates_df.RDS"))

table_df <-
  wide_df |>
  transmute(
    category = str_to_title(str_replace(
      recode_values(
        final_category,
        c("is_politics", "not_politics") ~ "Other",
        default = final_category
      ),
      "_", " "
    )),
    polarity = recode_values(
      polarity,
      "Prediction: Overreporting" ~ "Over",
      "Prediction: Underreporting" ~ "Under",
      default = polarity
    ),
    study = cite_key,
    item = sensitive_list_item,
    direct = make_se_entry(direct_question_estimate, direct_se, digits = 3),
    list = make_se_entry(list_est, list_se, digits = 3),
    n = list_n
  ) |>
  arrange(category, polarity, study, item)

write_csv(table_df, path_output("table_g_study_estimates.csv"))

print(count(table_df, category, polarity))
