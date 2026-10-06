# =============================================================================
# 00_extract_download.R  --  get the Eurostat download into Raw Data/
#
# NOT part of run_update.R -- source this separately when a new Comext
# download arrives, BEFORE running the pipeline. It keeps your existing
# process (including the manual 7-Zip step) but automates everything
# around it and finds the newest download for you.
#
# Workflow:
#   1. Download the ESTAT zip from Comext to your Downloads folder as usual
#   2. source("00_extract_download.R")   -> unzips the main zip, then STOPS
#      and asks you to do the 7-Zip extraction (as you do now)
#   3. Extract the inner archives with 7-Zip into unzipped_main/
#   4. source("00_extract_download.R") again -> finds the .dat files and
#      copies them into Raw Data/, ready for run_update.R
# =============================================================================

library(fs)

downloads_folder <- "C:/Users/la000062/Downloads"   # <- change if needed
main_extract_dir <- "unzipped_main"
target_folder    <- "Raw Data"


## ---- Step A: find and unzip the newest ESTAT download ----------------------

dat_files <- dir_ls(path = main_extract_dir, recurse = TRUE,
                    regexp = "\\.dat$", fail = FALSE)

if (length(dat_files) == 0) {
  
  zips <- list.files(downloads_folder,
                     pattern = "^ESTAT_downloads_comext.*\\.zip$",
                     full.names = TRUE)
  
  if (length(zips) == 0) {
    stop("No ESTAT_downloads_comext...zip found in ", downloads_folder,
         ". Download it from Comext first.", call. = FALSE)
  }
  
  ## Comext splits large downloads into multiple batch zips. Batches arrive
  ## together, so: take every zip downloaded on the SAME (most recent) day.
  ## This picks up all batches of the current download while excluding any
  ## leftover zips from previous months still sitting in Downloads.
  zip_dates   <- as.Date(file.mtime(zips))
  newest_day  <- max(zip_dates)
  main_zips   <- zips[zip_dates == newest_day]
  
  message("Found ", length(main_zips), " zip(s) from ", newest_day, ":")
  for (z in main_zips) message("  - ", basename(z))
  
  for (z in main_zips) {
    unzip(z, exdir = main_extract_dir)
  }
  
  stop(length(main_zips), " zip(s) extracted to '", main_extract_dir, "'.\n",
       "Now extract the inner archives there with 7-Zip (as usual), ",
       "then source this script again to move the .dat files into Raw Data/.",
       call. = FALSE)
}


## ---- Step B: .dat files exist -> move them into Raw Data -------------------

dir_create(target_folder)

file_copy(dat_files, target_folder, overwrite = TRUE)

cat(length(dat_files), ".dat files moved to", target_folder, "\n")

## ---- Step C: clean up -------------------------------------------------------
## Empty unzipped_main now that the .dat files are safely in Raw Data.
## This matters: if old extracted files lingered here, NEXT month's run of
## this script would see them, skip unzipping the new download, and quietly
## re-copy stale data. Verify the copies landed first, then clear.

copied_ok <- file_exists(path(target_folder, path_file(dat_files)))

if (all(copied_ok)) {
  dir_delete(main_extract_dir)
  cat("Cleaned up '", main_extract_dir, "' (archives + extracted files).\n",
      sep = "")
} else {
  warning("Not all .dat files verified in Raw Data -- '", main_extract_dir,
          "' NOT deleted. Check before re-running.", call. = FALSE)
}

cat("Done. Now run:  source(\"run_update.R\")\n")