# blair_coppock_moor_2020/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
#   and original_extracted/ (the unpacked contents of the deposited zip)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse, verify it, and unpack
#   it. Run this once before running anything in maintained/. Re-running is free: a file
#   already present with the right checksum is not downloaded again.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes
#   Dataverse returns for `?format=original`, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. The one file here agrees,
#   but they do not always: another deposit in this program carries three published
#   checksums that verify neither the original nor the derived tabular file, so
#   verification runs against md5_served and any disagreement is reported.
#
#   This deposit is a single zip rather than a set of loose files, so original/ holds
#   the zip alone and the unpacked tree lives beside it in original_extracted/. Both are
#   rebuilt from Dataverse and neither is redistributed here. Unpacking is done fresh
#   every time so that a stale tree cannot survive a change to the deposit, and the
#   __MACOSX resource-fork directory the zip carries is discarded.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/YUXHZT"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than the tabular
# representation Dataverse derives for ingested files.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    match = md5_downloaded == md5_served,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, md5_served, md5_downloaded, match, published_agrees)

print(verified, n = nrow(verified))

if (!all(verified$match)) {
  stop("Checksum mismatch: the downloaded archive does not match what Dataverse served when this code was written.")
}

# original/ must hold the deposit and nothing else. A script that writes its output back
# into the archive directory, or a bulk-download zip left behind by a browser, would
# otherwise sit there unnoticed and make a fresh clone differ from this one.
strays <- setdiff(list.files(here::here("original"), recursive = TRUE, all.files = TRUE,
                             no.. = TRUE),
                  manifest$file)

if (length(strays) > 0) {
  stop(str_glue("original/ holds {length(strays)} file(s) the manifest does not list: ",
                "{paste(strays, collapse = ', ')}"))
}

print(str_glue("All {nrow(verified)} files match and original/ holds nothing else. ",
               "{sum(!verified$published_agrees)} carry a published checksum that disagrees."))

# Unpack ----
extracted_dir <- here::here("original_extracted")
unlink(extracted_dir, recursive = TRUE)
dir.create(extracted_dir)
unzip(here::here("original", "blair_coppock_moor.zip"), exdir = extracted_dir)
unlink(file.path(extracted_dir, "__MACOSX"), recursive = TRUE)

print(str_glue("Unpacked to original_extracted/ ",
               "({length(list.files(extracted_dir, recursive = TRUE))} files)."))
print(str_glue("Archive: {dataset_doi}"))
