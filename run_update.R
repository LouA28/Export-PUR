# =============================================================================
# run_update.R  --  EU imports from UK PUR: the one script you run per update
#
# Update workflow:
#   1. Download the new ESTAT zip from Comext to Downloads
#   2. source("00_extract_download.R")  -> unzips, then asks you to do the
#      7-Zip step; do it, then source it again to fill Raw Data/
#   3. source("run_update.R")           -> everything below runs
#   4. Redeploy the EU app (its data/ folder has been refreshed in place)
#   5. Until you adopt the optional date-stamp fix: update the hardcoded
#      "Eurostat dd-mm-yyyy" stamp in the app before redeploying, as now
#
# Stages:
#   01_ingest.R      Raw .dat files -> PUR_export
#   02_build_core.R  PUR_export -> final_exportPUR (dated + _latest, previous
#                    vintage auto-archived to "Past updates/") + preference-
#                    type data (_latest)
#   03_app_data.R    -> the EU app's data/ folder (revisions data included)
#   04_pref data.R   -> data/pref/ (one file per partner): what the goods
#                    actually entered under, used by the app's "Preference
#                    regimes" tab and its CN8/HS6/HS4 graphs
# =============================================================================

## Load library

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(countrycode)
library(lubridate)
library(scales)

## Open generated functions through functions script

source("R/functions.R")

## Run each stage back to back - in chronological order 

stages <- c(
  "pipeline/01_ingest.R",
  "pipeline/02_build_core.R",
  "pipeline/03_app_data.R",
  "pipeline/04_pref_data.R"
)

## every stage must exist before we start, so a missing file is caught now
## rather than half way through an update
missing <- stages[!file.exists(stages)]
if (length(missing) > 0)
  stop("Stage script(s) not found: ", paste(missing, collapse = ", "),
       "\nCheck they are saved in the pipeline/ folder.", call. = FALSE)

## used to measure time

t_start <- Sys.time()

## Takes each stage and runs the script line by line

for (stage in stages) {
  message("\n", strrep("=", 70), "\n  ", stage, "\n", strrep("=", 70))
  source(stage)
}

## Once complete, message will express completion but if error then it will stop, so you can correct

message("\n", strrep("=", 70))
message("ALL STAGES COMPLETE in ",
        round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1),
        " minutes.")
message("Past data: ", Sys.Date())
message("Refreshed: data/  |  data/pref/  |  Past updates/ (previous data archived)")
message("Next step: redeploy the EU app.")
message(strrep("=", 70))