# Reproducibility Report: Blair, Coppock and Moor (2020)


- [Summary](#summary)
  - [Does the deposited archive run?](#does-the-deposited-archive-run)
  - [Does the maintained rewrite reproduce the
    paper?](#does-the-maintained-rewrite-reproduce-the-paper)
- [Paper overview](#paper-overview)
- [Original archive reproducibility](#original-archive-reproducibility)
- [Ground truth summary](#ground-truth-summary)
- [Maintained rewrite](#maintained-rewrite)
  - [Architecture](#architecture)
  - [Deprecated patterns replaced](#deprecated-patterns-replaced)
  - [What the rewrite does not carry](#what-the-rewrite-does-not-carry)
- [Figure verification](#figure-verification)
- [Rewrite verification](#rewrite-verification)
- [R environment](#r-environment)

*Drafted by Claude Opus 5 under the supervision of Alex Coppock.*

This repository holds the actively maintained replication code for
Blair, Coppock and Moor (2020), together with the reproducibility report
that documents what the original archive did and did not do. It is part
of a program applying the maintenance proposal in Peer, Orr and Coppock
(2021, *PS: Political Science & Politics*, doi
[10.1017/S1049096521000366](https://doi.org/10.1017/S1049096521000366))
to a set of published archives.

|  |  |
|----|----|
| Article | [10.1017/S0003055420000374](https://doi.org/10.1017/S0003055420000374) |
| Replication archive | [10.7910/DVN/YUXHZT](https://doi.org/10.7910/DVN/YUXHZT) |
| Pre-analysis plans | none |

**The data are not redistributed here.** The deposit is a single 1.2 MB
zip at Harvard Dataverse, and that is the only copy this repository
points at. `download_original.R` fetches it, verifies its checksum, and
unpacks it into `original_extracted/`; `original_manifest.csv` records
the Dataverse file identifier, the size in bytes, and two checksums: the
MD5 of the bytes Dataverse serves for `?format=original`, which is what
this code was written against, and the MD5 Dataverse publishes. Here the
two agree. They do not always, so the script verifies against the served
bytes and reports any disagreement. Either way the exact bytes this code
ran on are pinned in version control even though the bytes themselves
are not.

**Repository layout.** `maintained/` is the maintained rewrite: one
script per published table or figure, writing to `output/`, which is
committed so a reader can compare a fresh run against it without
downloading anything. `ground_truth/` ties every published number to the
code that produces it. `ground_truth/build_ground_truth.R` regenerates
that table from the pipeline’s own output as the last step of
`run_all.R`, and `ground_truth/extract_archive_values.R`, which is run
on its own and takes about a quarter of an hour, records what the
deposited scripts produce so the two can be compared. `original/` and
`original_extracted/` are created by the download script and are
deliberately absent from the repository. This file is the
reproducibility report, also available as a PDF in `report/`.

**License.** CC0 1.0 Universal, matching the terms of the deposit this
repository maintains, so nothing in the chain is more restrictive than
the archive itself. See `LICENSE`.

**To reproduce.** Clone or download the repository, open
`blair_coppock_moor_2020.Rproj`, and run:

``` r
source("run_all.R")
```

That fetches the deposit, verifies it, produces every table and figure
into `maintained/output/`, and rebuilds the ground truth table from
those outputs. Required packages: tidyverse, rstan, tidybayes, deming,
janitor, patchwork, DeclareDesign, bindata, rsample, here, and for this
report knitr and kableExtra. Paths resolve through `here`, so nothing
depends on the working directory. The full run takes about two minutes:
a minute to recompile the Stan model and fit the ten meta-analyses,
another for the design simulation, and seconds for everything else.
Individual scripts can be run on their own, subject to the three
dependencies noted in `run_all.R`. A successful run overwrites
`maintained/output/`, which is committed: **`git diff` on that folder is
the reproduction check.** The CSV, PNG and RDS output comes back
byte-identical; the 21 PDF figures always show as changed, because a PDF
records the time it was written, so compare their PNG twins instead.

# Summary

Two questions, answered before the detail.

## Does the deposited archive run?

Not as deposited. The archive holds thirteen R files, two of which are
helper files the others source. Of the eleven that are meant to be run,
nine fail on a current R installation.

Three of the nine fail before reaching any R problem at all: **the
deposit does not contain the output directories its own scripts write
to.** `mode_appendix_table.R` and `table_of_data.R` stop at
`cannot open file 'tables/...'`, and `plot_direct_v_list.R` stops at
`Cannot find directory 'figures'`. Nothing in the archive creates either
folder, so a reader who unpacks the zip and runs the scripts in the
order the archive’s README implies gets three failures that have nothing
to do with the analysis and no hint that the fix is `mkdir`.

The core meta-analysis script, `meta_analysis.R`, loads
`data/stan_model.RDS`, a Stan model compiled in 2020 under R 3.6 on
another machine. rstan rejects it: “the compiled object from C++ code
for this model is invalid.” The archive ships `compile_stan_model.R` to
rebuild it, but that script writes its output back to
`data/stan_model.RDS`, so running the archive in place silently replaces
a deposited file with a machine-specific binary. Running the archive
from a scratch copy, as this report does, is the only way to leave the
deposit intact. Once the model is recompiled, `meta_analysis.R` fails
again inside `0_helpers.R`, where `fct_reorder()` is called on a
character vector whose ordering variable is `NA` for the
predictive-interval row; forcats 1.0.0 errors on that, where earlier
versions did not.

The four simulation scripts fail for two reasons that have nothing to do
with the analysis either. `Figure2ab.R` and `Figure5.R` call
`write_rds(path =)` and `read_rds(path =)`, and the `path` argument was
removed in readr 2.0.0. `Figure3a.R` and `Figure3b.R` print the finished
plot object to the console before saving it, which sends it to the
default `pdf()` device, which cannot resolve the custom font
`ETBembo-RomanLF` and raises “invalid font type.” The font is missing on
any machine but the authors’, so the bare print is fatal everywhere. The
DeclareDesign declaration fails on its first line of arithmetic,
computing `delta` from `pi_star` one line before `pi_star` is defined,
and then on the pre-1.0 DeclareDesign API throughout.

All of it is resolvable, and none of the fixes touches an estimator, a
specification or a sample definition. With them applied to a scratch
copy, the archive’s own `meta_analysis.R` reproduces the published Table
4 in 84 of its 90 cells.

## Does the maintained rewrite reproduce the paper?

It agrees with the deposited archive wherever the two compute the same
quantity, and with the paper in 168 of 189 verifiable claims.

The comparison against the archive is the sharper of the two.
`ground_truth/extract_archive_values.R` runs the deposit from a scratch
copy, with only the recompiled Stan model, the repaired `fct_reorder`
call and the two `readr` argument names, and writes what its scripts
produce into `ground_truth/`. The two pipelines compute the same
quantity in 170 places: all of Table 4, the in-text sentences that
restate it, all of Table H.8, the study counts behind Tables G.1 to G.7,
the two indifference curves behind Figure 2 and the three region counts
behind Figure 5. The largest absolute difference across all of them is
0. Any distance to the published numbers is therefore a property of the
software this runs on, not of the rewrite.

The 21 claims that do not match the paper fall into three groups, and
none of them is a defect in the rewrite.

13 are places where the article’s prose disagrees with the article’s own
tables, and in every one of them the pipeline agrees with the table. The
clearest is the turnout predictive interval: the text says the 95%
interval “is 45 points wide,” while Table 4 prints \[-0.12, 0.26\] for
that interval, which is 38 points wide, and the code returns 37.4. The
same pattern recurs for the vote-buying 95% predictive interval (text
-27 to +10, table -25 to +8), the racial-prejudice 95% predictive
interval (text -16 to +16, table -5 to +12), the width of the
religious-prejudice credible interval (text 13 points, table 11), and
the sexual-orientation underreporting estimate (text +2 points, table
-0.01). The last of the group is the RMSE indifference threshold at N =
1,000: the article says a bias above 5.5 points prefers the list
experiment, and the curve its own Figure 2a plots crosses at 5.57, so a
bias of 5.55 points would not in fact prefer it. None of these is a
reproduction failure. The pipeline and the tables agree with each other,
and the prose agrees with neither.

6 are cells of Table 4 where the archive and the rewrite return the same
posterior summary and the article prints a different one: four
predictive-interval bounds, one standard error and one credible-interval
bound. All six are in the second decimal place, and five of them are in
the two subsets with the fewest studies, where the posterior is least
well determined. Stan and its compiler have both moved since 2020 and
the deposit pins neither, so the article’s draws cannot be recovered.

2 are the counts of studies in the second and third regions of Figure 5.
The article reports 98, 107 and 59 studies; the deposited `Figure5.R`
and the rewrite both return 98, 106 and 60. One study, Luks (2010), has
an estimated bias of exactly 0.2000, which sits one grid step of 0.0001
above the power curve at its sample size and so falls above the curve
rather than between the curves. Nothing in that computation draws at
random, so the difference is not sampling noise, and what moved between
2020 and now has not been established. Recording it as unresolved is
more honest than attributing it.

14 recorded quantities cannot be checked. Four are the Deming regression
coefficients behind Figure F.10, which the supplementary text
characterizes only as “close to 1” without printing a number. Two are
the thresholds Figure 3b plots, which the text describes without
quoting. Two are the counts of list experiments that pass the Blair and
Imai (2012) design effects test, which needs an item-count distribution
the deposited study-level file does not carry. The remaining six are
published floats that carry no computed quantity at all, or that the
deposit contains no script and no data for: Figure 1 and Figure A.1 are
drawn from respondent-level surveys the deposit does not include, and
Figure D.2 comes from data the deposit does include but ships no script
to plot.

# Paper overview

**Citation**: Blair, G., Coppock, A. and Moor, M. (2020). “When to Worry
about Sensitivity Bias: A Social Reference Theory and Evidence from 30
Years of List Experiments.” *American Political Science Review*, 114(4),
1297-1315.

**Research question**: How much are survey responses on sensitive topics
distorted by respondents’ concern about how a social referent will judge
them, and when is a list experiment worth its cost?

**Design**: Three pieces. A social reference theory says sensitivity
bias requires a referent, a perceived preference of that referent, a
belief that the answer will reach the referent, and a cost. A
bias-variance analysis shows the list experiment buys unbiasedness at a
very high price in precision: at 2,000 subjects and a prevalence rate of
0.5, its variance is roughly fourteen times the direct question’s, so a
researcher would need about 28,000 subjects to match a 2,000 subject
direct question. A meta-analysis of 264 list experiments from 92 papers,
each paired with a direct question, then estimates the actual
sensitivity bias by domain with a Bayesian random effects model fitted
in Stan.

**Main finding**: Biases are typically under 10 percentage points and in
several domains indistinguishable from zero. The exception is support
for authoritarian regimes, where overreporting averages +14 points, and
the predictive intervals are wide enough everywhere that a domain-level
average is a poor guide to any particular study.

**Data**: the deposit contains no individual-level microdata.
`estimates_df.RDS` is a curated frame of 264 study-level estimates,
their standard errors, sample sizes, survey modes and predicted
polarity, assembled by the authors from published and unpublished
sources.

# Original archive reproducibility

| Script | Status |
|:---|:---|
| 1_meta_analysis/0_helpers.R | Sourced by the meta-analysis scripts; carries the fct_reorder failure |
| 1_meta_analysis/compile_stan_model.R | Runs, but writes its output over the deposited data/stan_model.RDS |
| 1_meta_analysis/meta_analysis.R | Fails: unusable Stan binary, then fct_reorder on an NA, then no figures/ or tables/ directory |
| 1_meta_analysis/meta_analysis_rma.R | Runs clean; writes no file and corresponds to no published quantity |
| 1_meta_analysis/mode_appendix_table.R | Fails: no tables/ directory in the deposit |
| 1_meta_analysis/plot_direct_v_list.R | Fails: no figures/ directory in the deposit; geom_errorbarh deprecation warning |
| 1_meta_analysis/table_of_data.R | Fails: no tables/ directory in the deposit |
| 2_simulations/theory_list_power_function.R | Sourced by the simulation scripts; runs clean |
| 2_simulations/Figure2ab.R | Fails: write_rds(path =) removed in readr 2.0.0; no figures/ directory |
| 2_simulations/Figure3a.R | Fails: bare print of the plot to a device that cannot resolve the font |
| 2_simulations/Figure3b.R | Fails: same bare print |
| 2_simulations/Figure5.R | Fails: read_rds(path =) removed; also needs Figure2ab.R to have run |
| 3_declaredesign/list_experiment_declaration.R | Fails: delta computed before pi_star; pre-1.0 DeclareDesign API throughout |

Status of each script in the deposited archive under R 4.6.0.

Four further observations about the deposit itself.

The archive’s `README.txt` describes a script named
`theory_group_differences_sensitivity_bias.R` that is not in the
deposit; the file that produces the figure it describes is `Figure3a.R`.
The README also lists the four simulation scripts by filename with no
description at all.

`compile_stan_model.R` writing to `data/stan_model.RDS` is the more
serious problem, because the output and one of the deposited inputs are
the same file. A reader who follows the archive’s own instructions ends
up with a working copy of the deposit that no longer matches the bytes
Dataverse serves, and nothing warns them. The maintained rewrite
recompiles the model in memory and never writes a compiled binary at
all.

`Figure2ab.R` walks a 3,410,341 row parameter grid one row at a time
with `map2_df`, which takes about a quarter of an hour, and then writes
the resulting 246 MB frame to a 253 MB file in the archive’s root
directory. The function it calls, `list_power`, is already fully
vectorized, so a single call over the whole grid returns the identical
frame in under a second: on a random sample of 4,000 grid points the two
routes agree exactly, cell for cell. Vectorizing is a performance
property and not an analytical one, and the rewrite takes the fast path.

The zip carries a `__MACOSX` resource-fork tree, three `.DS_Store` files
and an `.Rapp.history` alongside the 13 scripts and 2 data files.
`download_original.R` discards the resource forks on unpacking and
leaves everything else as deposited. Running the archive adds files as
well: the two simulation scripts that print a plot before saving it
leave an `Rplots.pdf` behind, and `compile_stan_model.R` overwrites
`data/stan_model.RDS`, so a working copy of the deposit stops matching
Dataverse the first time it is used. `download_original.R` refuses to
proceed if `original/` holds anything the manifest does not list, which
is why the deposit is kept as the downloaded zip and unpacked somewhere
else.

# Ground truth summary

| Table or figure               | Status                  |   N |
|:------------------------------|:------------------------|----:|
| Appendix F                    | Does not match          |   1 |
| Appendix F                    | Matches                 |   3 |
| Figure 1                      | Not stated in the paper |   1 |
| Figure 2a                     | Does not match          |   1 |
| Figure 2a                     | Matches                 |   1 |
| Figure 2b                     | Matches                 |   1 |
| Figure 3a                     | Matches                 |   2 |
| Figure 3b                     | Not stated in the paper |   2 |
| Figure 4a                     | Matches                 |   5 |
| Figure 5                      | Does not match          |   2 |
| Figure 5                      | Matches                 |   1 |
| Figure A.1                    | Not stated in the paper |   1 |
| Figure D.2                    | Not stated in the paper |   1 |
| Figure F.10                   | Not stated in the paper |   4 |
| Table 1                       | Not stated in the paper |   1 |
| Table 2                       | Not stated in the paper |   1 |
| Table 3                       | Not stated in the paper |   1 |
| Table 4                       | Does not match          |   6 |
| Table 4                       | Matches                 |  84 |
| Table G.1                     | Matches                 |   5 |
| Table G.2                     | Matches                 |   1 |
| Table G.3                     | Matches                 |   1 |
| Table G.4                     | Matches                 |   1 |
| Table G.5                     | Matches                 |   1 |
| Table G.6                     | Matches                 |   1 |
| Table G.7                     | Matches                 |   1 |
| Table H.8                     | Matches                 |  36 |
| Tables G.1 to G.7             | Matches                 |   1 |
| Text, authoritarian regimes   | Does not match          |   1 |
| Text, authoritarian regimes   | Matches                 |   7 |
| Text, bias-variance trade-off | Matches                 |   4 |
| Text, limitations             | Not stated in the paper |   2 |
| Text, meta-analysis design    | Matches                 |   2 |
| Text, racial prejudice        | Does not match          |   3 |
| Text, racial prejudice        | Matches                 |   4 |
| Text, religious prejudice     | Does not match          |   1 |
| Text, religious prejudice     | Matches                 |   1 |
| Text, sexual orientation      | Does not match          |   1 |
| Text, sexual orientation      | Matches                 |   1 |
| Text, turnout                 | Does not match          |   1 |
| Text, turnout                 | Matches                 |   1 |
| Text, vote buying             | Does not match          |   4 |
| Text, vote buying             | Matches                 |   3 |

Ground truth claims by source and outcome.

`value_paper` in `ground_truth/blair_coppock_moor_2020_ground_truth.csv`
was filled only by reading the published article and its supplementary
materials, and the three multi-column tables were read off rendered
pages rather than a text extraction so that no cell could be mapped to
the wrong column. Where the paper does not state a quantity the field is
blank and `match` is `NA`. `value_rewrite` is read back out of
`maintained/output/` by the build script and `value_script` out of the
archive run, neither of them transcribed, and no published number is an
input to any computation anywhere in `maintained/`. Nor is a published
number used to select a row: the one study-level spot check is on
Corstange (2017), which is the single vote-buying study with one row in
Table G.1 and so can be located by name.

Claims the article prints as values are matched to the last digit it
prints. A handful are stated as one-sided bounds instead (“the bias must
be greater than 5.5 points”, “differences must exceed 20 percentage
points”), and those are tested as bounds, in the direction the sentence
asserts.

| Source | Claim | Paper | Pipeline | Locus |
|:---|:---|---:|---:|:---|
| Table 4 | turnout overreporting 95% PPI upper | 0.26 | 0.255 | environment |
| Table 4 | racial prejudice underreporting 95% PPI upper | 0.12 | 0.128 | environment |
| Table 4 | sexual orientation overreporting mu SE | 0.09 | 0.079 | environment |
| Table 4 | sexual orientation overreporting 95% CI lower | -0.19 | -0.173 | environment |
| Table 4 | sexual orientation overreporting 95% PPI lower | -0.31 | -0.301 | environment |
| Table 4 | sexual orientation overreporting 95% PPI upper | 0.27 | 0.264 | environment |
| Figure 2a | bias at which list and direct RMSE are equal, N = 1000, points | 5.50 | 5.570 | paper_internal |
| Figure 5 | studies between the two curves | 107.00 | 106.000 | unresolved |
| Figure 5 | studies above both curves | 59.00 | 60.000 | unresolved |
| Text, vote buying | 95% credible interval upper, points | -3.00 | -3.690 | paper_internal |
| Text, vote buying | 50% predictive interval upper, points | -2.00 | -2.674 | paper_internal |
| Text, vote buying | 95% predictive interval lower, points | -27.00 | -24.852 | paper_internal |
| Text, vote buying | 95% predictive interval upper, points | 10.00 | 8.148 | paper_internal |
| Text, turnout | width of the 95% predictive interval, points | 45.00 | 37.368 | paper_internal |
| Text, racial prejudice | 95% credible interval lower, points | -3.00 | -2.421 | paper_internal |
| Text, racial prejudice | 95% predictive interval lower, points | -16.00 | -5.199 | paper_internal |
| Text, racial prejudice | 95% predictive interval upper, points | 16.00 | 12.777 | paper_internal |
| Text, religious prejudice | width of the 95% credible interval, points | 13.00 | 11.391 | paper_internal |
| Text, sexual orientation | underreporting meta-analysis estimate, points | 2.00 | -0.966 | paper_internal |
| Text, authoritarian regimes | overreporting 50% predictive interval lower, points | 4.00 | 4.516 | paper_internal |
| Appendix F | all underreporting: meta-analysis estimate, points | -3.00 | -3.517 | paper_internal |

Every claim that does not match, and where the difference comes from.

# Maintained rewrite

## Architecture

| Script | Output |
|:---|:---|
| helpers.R | Packages, the Stan helpers, the plotting helpers, the table formatters |
| figure_4_meta_analysis_coefplots.R | 32 figure files (Figure 4a-h and Figures E.3-E.9), figure_4_meta_estimates.csv, meta_analysis_summaries.rds |
| table_4_meta_summary.R | table_4_meta_summary_cat.csv, table_4_meta_summary_overall.csv |
| figure_2ab_theory_power_rmse.R | figure_2ab_theory_power_rmse.pdf/.png, figure_2ab_indifference_curves.csv |
| figure_3a_group_differences_power.R | figure_3a_group_differences_power.pdf/.png, figure_3a_power_curve.csv |
| figure_3b_diff_in_sensitivity_power.R | figure_3b_diff_in_sensitivity_power.pdf/.png, figure_3b_power_curve.csv |
| figure_5_bias_variance_overlay.R | figure_5_bias_variance_overlay.pdf/.png, figure_5_region_counts.csv |
| figure_f10_direct_vs_list.R | figure_f10_direct_vs_list.pdf/.png, figure_f10_deming_coefficients.csv |
| table_g_study_estimates.R | table_g_study_estimates.csv (Tables G.1-G.7 in one tidy file) |
| table_h8_mode.R | table_h8_mode.csv |
| text_theory_variance.R | text_theory_variance.csv |
| text_power_thresholds.R | text_power_thresholds.csv |
| text_summary_stats.R | text_summary_stats.csv |
| declaredesign_list_experiment.R | declaredesign_simulation_summary.csv |

The maintained rewrite, one script per output.

Three structural points are worth naming.

Every figure script writes a CSV of the values it plots. A PDF differs
on every run because it records the time it was written, so a figure
file cannot be diffed; the CSV beside it can. For Figure 4 alone that
means 389 rows of plotted estimates, standard errors, credible intervals
and predictive intervals, and the same holds for the two indifference
curves behind Figure 2, the two power curves behind Figure 3, the three
region counts behind Figure 5 and the four Deming coefficients behind
Figure F.10.

The ten Stan fits run once, in `figure_4_meta_analysis_coefplots.R`,
which saves their posterior summaries to
`output/meta_analysis_summaries.rds`; `table_4_meta_summary.R` reads
that file rather than refitting. One script per output means the figure
and its table are written by different scripts, and passing the fits
through a saved object is what stops the two from ever reporting
different draws. The archive keeps both in one script and gets the same
guarantee a different way.

Three scripts exist only to make in-text numbers traceable.
`text_theory_variance.R` computes the standard errors quoted in the
theory section, which the archive works out only in a commented block at
the foot of `theory_list_power_function.R`, and that block uses a
sensitivity bias of 0.05 and a control item covariance of 0.05 rather
than the 0.1 and 0.025 the published sentence states.
`text_power_thresholds.R` reads the three simulation curves and prints
the sample-size thresholds the design section quotes.
`text_summary_stats.R` gives the study counts and the extreme study
estimates named in the results. Where the archive prints a number to the
console and saves nothing, as `Figure5.R` does with its region counts,
the rewrite writes it to a CSV instead.

## Deprecated patterns replaced

| Original pattern | Replacement |
|:---|:---|
| `%>%` | `&#124;>` |
| `readRDS()` / `saveRDS()` | `read_rds()` / `write_rds()` |
| `write_rds(path =)`, `read_rds(path =)` | positional first argument, `file =` on write |
| `gather()` | `pivot_longer()` |
| `do(predict(...))`, `do(tidy(...))` | `reframe()` |
| `geom_errorbarh(height = 0)` | `geom_linerange()` |
| `fct_reorder()` on a character vector with NA | sort the NA row to `-Inf` before reordering |
| `geom_smooth(stat = "smooth")` | `geom_smooth(method = "loess")` |
| `xtable` + `print.xtable(file =)` | `write_csv()` to `maintained/output/` |
| `declare_population()`, `declare_step()` | `declare_model()` with the step folded in |
| `declare_potential_outcomes()` | `potential_outcomes()` plus `declare_measurement()` |
| `declare_estimand()` | `declare_inquiry()` |
| `declare_assignment(prob = 0.5)` | `declare_assignment(Z = complete_ra(N, prob = 0.5))` |
| `declare_estimator(model =)` | `declare_estimator(.method =)` |
| `tidy_estimator()` | `label_estimator()` |
| `text = element_text(family = "ETBembo-RomanLF")` | removed; the font is not distributable and the figures do not need it |
| `map2_df()` over a 3.4 million row grid | one vectorized call to the same function |
| pre-compiled `stan_model.RDS` | recompiled in memory from the deposited `.stan` source |

Deprecated and removed patterns, and what replaced them.

Two of these deserve more than a table row.

**`tidy_estimator()`.** The archive’s DeclareDesign declaration has
three estimators: the direct question, the list experiment, and the
difference between them with a bootstrapped standard error. The third is
wired in with `tidy_estimator()`, which DeclareDesign removed at version
1.0. Its direct successor is `label_estimator()`, the estimator declares
in one line, and the whole simulation with 500 designs and 250 bootstrap
resamples each runs in about ninety seconds. Across those 500 simulated
designs the direct-minus-list estimator recovers the true sensitivity
bias of 0.1 with a simulation bias of 0.002 and power of 0.71. No number
from this script appears in the published paper; the declaration is a
worked example of the design the paper analyses.

**The custom font.** Every figure in the archive sets
`family = "ETBembo-RomanLF"`, a commercial typeface that is not in the
deposit. On the authors’ machine this is invisible; anywhere else it is
either a silent fallback or, when the plot reaches the base `pdf()`
device, a hard error reading “invalid font type”. The rewrite removes
the font calls entirely, and the figures render on any machine. They are
otherwise the published figures, with one cosmetic omission: the archive
draws the two labels inside Figure 5 with a leading arrow glyph taken
from the Adobe Symbol font, and the rewrite prints the labels without
it.

## What the rewrite does not carry

`meta_analysis_rma.R` refits all ten subsets with
`metafor::rma.uni(method = "EB")` and then stops: it writes no file, and
no quantity it computes appears in the article or the supplementary
materials. It runs clean, and it was run for this report to confirm
that. It is not reproduced in `maintained/` because there is nothing
published for it to reproduce.

The two bug fixes the rewrite makes, the `fct_reorder` repair and the
`pi_star` ordering in the design declaration, are both fixes to code
that crashes rather than to code that computes the wrong answer, and
neither moves a published value. There is no erratum to report.

# Figure verification

| Figure | Rewrite output | Numbers checked against the paper |
|:---|:---|:---|
| Figure 2a, 2b | figure_2ab_theory_power_rmse | RMSE and power thresholds at N = 1,000 and N = 2,000 |
| Figure 3a | figure_3a_group_differences_power | Detectable prevalence difference at N = 2,000 and N = 3,000 |
| Figure 3b | figure_3b_diff_in_sensitivity_power | None stated in the text |
| Figure 4a-h | figure_4a\_\* through figure_4h\_\*, plus \_sm small multiples | Study-level range and the walked-through Kiewiet de Jonge estimate |
| Figure 5 | figure_5_bias_variance_overlay | Counts of studies in the three regions |
| Figure F.10 | figure_f10_direct_vs_list | None printed; slopes described only as close to 1 |

Published figures and the maintained scripts that produce them.

Figure 4 is the paper’s central display, eight panels of study-level
sensitivity bias estimates with the meta-analytic posterior and its
predictive intervals underneath. The rewrite reproduces all eight, and
the same code path produces Figures E.3 through E.9 in the supplementary
materials.

![Figure 4a as reproduced: vote buying, underreporting
predicted.](maintained/output/figure_4a_vote_buying_underreporting.png)

![Figure 5 as reproduced: where the 264 list experiments fall relative
to the two indifference
curves.](maintained/output/figure_5_bias_variance_overlay.png)

# Rewrite verification

| Domain                             | Matching | Not matching |
|:-----------------------------------|---------:|-------------:|
| vote buying underreporting         |        9 |            0 |
| turnout overreporting              |        8 |            1 |
| racial prejudice underreporting    |        8 |            1 |
| religious prejudice underreporting |        9 |            0 |
| sexual orientation underreporting  |        9 |            0 |
| sexual orientation overreporting   |        5 |            4 |
| authoritarian underreporting       |        9 |            0 |
| authoritarian overreporting        |        9 |            0 |
| all results underreporting         |        9 |            0 |
| all results overreporting          |        9 |            0 |

Table 4, by subset: how many of the nine printed quantities the rewrite
reproduces.

The rewrite and the archive agree on every one of these cells, including
the six that disagree with the paper. That is the useful comparison: a
rewrite defect would show up as the rewrite disagreeing with the
archive, while software drift shows up as both disagreeing with the
paper in the same direction by the same amount. All six sit in the
second decimal place of a posterior summary, and five of them are in the
two subsets with the fewest studies.

The Stan model draws 10,000 iterations on four chains at a fixed seed of
500, set once before the first fit. Running the pipeline twice returns
byte-identical CSV, PNG and RDS output, including the saved posterior
summaries, so the remaining distance to the published values is
attributable to the versions of rstan and R rather than to sampling
noise within a version.

# R environment

| Component     | Version |
|:--------------|:--------|
| R             | 4.6.0   |
| rstan         | 2.32.7  |
| tidyverse     | 2.0.0   |
| tidybayes     | 3.0.7   |
| DeclareDesign | 1.1.1   |
| deming        | 1.4.1   |
| janitor       | 2.2.1   |
| patchwork     | 1.3.2   |

Versions this report was produced under.

The archive was built under R 3.6.3 and run on 8 May 2020.
