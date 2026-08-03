# blair_coppock_moor_2020/ground_truth/extract_published_values.R
# Output: ground_truth/published_g_table_cells.csv,
#         ground_truth/published_e_figure_counts.csv
# Depends on: the published supplementary materials PDF, and pdftotext (poppler)
# Description: Read the published supplementary materials positionally and write down what
#   they print. Appendix G is seven tables carrying 239 study rows and 1,195 printed
#   numbers, far too many to type, and appendix E is seven coefficient plots whose only
#   checkable published quantity is how many study estimates each one draws. Both are read
#   here once and committed, so the ground truth can compare against the page rather than
#   against a transcription somebody made by hand.
#
#   This script is not part of run_all.R. The repository does not carry the published PDFs,
#   which are the publisher's to distribute, so the article directory is taken from an
#   environment variable. The supplementary materials are free to download from
#   https://doi.org/10.1017/S0003055420000374 (Open Access). Point ARTICLE_PDF_DIR at a
#   directory holding blair_coppock_moor_2020_appendix.pdf and run this file.
#
#   Nothing here is read by any script in maintained/. A published number is a comparison
#   target and never an input to the analysis.

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_published_values.R")

article_dir <- Sys.getenv("ARTICLE_PDF_DIR", unset = tempdir())
appendix_pdf <- file.path(article_dir, "blair_coppock_moor_2020_appendix.pdf")

if (!file.exists(appendix_pdf)) {
  stop(str_glue(
    "Set ARTICLE_PDF_DIR to a directory containing ",
    "blair_coppock_moor_2020_appendix.pdf. Looked in: {article_dir}"
  ))
}

stopifnot(nzchar(Sys.which("pdftotext")))

# The page as text, in layout order ----
# -layout keeps a table row on one line, which is what makes a positional read possible.
# Reading order without it interleaves the study labels with the estimates.

text_path <- file.path(tempdir(), "blair_coppock_moor_2020_appendix.txt")
status <- system2("pdftotext", c("-layout", shQuote(appendix_pdf), shQuote(text_path)))
stopifnot(status == 0)

lines <- str_squish(read_lines(text_path))

# Appendix G: seven tables of study estimates ----
# Each study row ends in the same five numbers: the direct question estimate and its
# standard error, the list experiment estimate and its standard error, and the sample
# size. Rows are assigned to a table by which caption they fall under, and kept in the
# order the page prints them, because two rows of Table G.1 carry identical study and item
# labels and nothing else distinguishes them.

cell_pattern <- str_c(
  "(-?[0-9]+[.][0-9]+) [(]([0-9]+[.][0-9]+)[)] ",
  "(-?[0-9]+[.][0-9]+) [(]([0-9]+[.][0-9]+)[)] ",
  "([0-9]+)$"
)

cells <- str_match(lines, cell_pattern)
cell_lines <- which(!is.na(cells[, 1]))

g_captions <- str_which(lines, "^Table G[.][1-7]:")
stopifnot(length(g_captions) == 7)

g_cells <-
  tibble(
    line_no = cell_lines,
    direct_est = cells[cell_lines, 2],
    direct_se = cells[cell_lines, 3],
    list_est = cells[cell_lines, 4],
    list_se = cells[cell_lines, 5],
    n = as.integer(cells[cell_lines, 6])
  ) |>
  mutate(
    table = as.character(cut(line_no, breaks = c(g_captions, Inf),
                             labels = str_c("Table G.", 1:7), right = FALSE))
  ) |>
  mutate(position = row_number(), .by = table) |>
  transmute(
    table,
    position,
    direct = str_c(direct_est, " (", direct_se, ")"),
    list = str_c(list_est, " (", list_se, ")"),
    n
  )

# Every row must have landed under a caption, and no row may precede the first one.
stopifnot(!any(is.na(g_cells$table)), min(cell_lines) > min(g_captions))

write_csv(g_cells, here::here("ground_truth", "published_g_table_cells.csv"))

# Appendix E: seven coefficient plots ----
# The panels print no estimates on their faces. The only numerals in an E figure's region
# are the four axis ticks, so what the page states and the pipeline can be held to is how
# many study estimates each plot draws. Each study occupies two lines, the citation and
# the quoted sensitive item, and the quoted line is the one that cannot be confused with
# an axis label or a panel annotation.

e_captions <- str_which(lines, "^Figure E[.][3-9]:")
stopifnot(length(e_captions) == 7)

section_start <- max(str_which(lines, "^E[.]? Sensitivity Bias by Research Area"))
region_start <- c(section_start, head(e_captions, -1))

e_counts <-
  tibble(
    figure = str_c("Figure E.", 3:9),
    caption = str_remove(lines[e_captions], "^Figure E[.][3-9]: "),
    studies_plotted = map2_int(
      region_start, e_captions,
      function(from, to) sum(str_starts(lines[(from + 1):(to - 1)], '"'))
    )
  )

stopifnot(all(e_counts$studies_plotted > 0))

write_csv(e_counts, here::here("ground_truth", "published_e_figure_counts.csv"))

print(count(g_cells, table))
print(e_counts)
