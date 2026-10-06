# =============================================================================
# STAGE 1: Extract Raw Comext .dat files, clean and generate one clean PUR_export dataframe.
#
# All partners, CN chapters 01-30. Partner is kept as a column and carried
# through every grouping in Stages 2 and 3.
# =============================================================================

message("STAGE 1: reading Eurostat .dat files ...")


## Importing data and combining annual files into one data frame

files_dat <- list.files(path = "Raw Data/", pattern = '.dat',
                        full.names = TRUE)

qa_stop(length(files_dat) > 0,
        "no .dat files in Raw Data/ -- run 00_extract_download.R first")

hs2_max <- 30

## Each file is filtered as soon as it's read, so only the rows we keep are
## ever held in memory together (reading every file in full first is what
## runs out of memory with all partners).
##   - chapters 01-30, stat_procedure 1
##   - lines whose product code contains letters are the xxs suppression lines
##   - unused columns dropped straight away
read_dat <- function(file_path) {
  d <- fread(file_path, integer64 = "double")
  setnames(d, tolower(names(d)))
  
  ## a file with no suppression lines reads product as a number and loses
  ## the leading zero (01012100 -> 1012100); put it back
  if (is.numeric(d$product)) d[, product := sprintf("%08.0f", product)]
  
  d <- d[substr(product, 1, 2) <= hs2_max & stat_procedure == 1 &
           !grepl("[[:alpha:]]", product)]
  
  drop_cols <- intersect(c("quantity_kg", "suppl_unit", "stat_procedure",
                           "value_nac", "quantity_suppl_unit"), names(d))
  d[, (drop_cols) := NULL]
  d
}

data <- rbindlist(lapply(files_dat, read_dat))
gc()


## read in class and PUR_elig files
## Class file is combination of 2022, 2023 and 2024 CN systems.

class    <- read_excel("classifications2022to26.xlsx")
PUR_elig <- read_excel("PUR eligibility.xlsx")

## Chapters missing from the classifications file get dropped in Stage 2,
## so warn now rather than lose them quietly

class_ch   <- unique(substr(str_pad(class$CN8, 8, pad = "0"), 1, 2))
missing_ch <- setdiff(sprintf("%02d", 1:hs2_max), class_ch)
qa_warn(length(missing_ch) == 0,
        paste0("classifications2022to26.xlsx has no codes for chapter(s) ",
               paste(missing_ch, collapse = ", "),
               " -- those rows will be dropped in Stage 2"))


## Convert the eligibility/import_regime columns to lowercase
## (EU data is published in upper; column names were lowercased on read)

data$eligibility   <- tolower(data$eligibility)
data$import_regime <- tolower(data$import_regime)


## Derived columns (filtering already done in read_dat)

data[, `:=`(HS2_code = substr(product, 1, 2),
            year     = substr(period, start = 1, stop = 4),
            month    = substr(period, start = 5, stop = 6))]

## Periods ending 52 are Comext's ANNUAL totals for the year. They repeat the
## monthly rows, so leaving them in double-counts every figure (and breaks the
## app's monthly charts, which read the month as a date).
n_annual <- sum(data$month == "52")
if (n_annual > 0) {
  v_annual <- sum(data$value_eur[data$month == "52"], na.rm = TRUE)
  message("  removing ", format(n_annual, big.mark = ","),
          " annual (period ...52) row(s), worth ",
          format(round(v_annual), big.mark = ","), " EUR -- these repeat the monthly rows")
  data <- data[month != "52"]
}
qa_warn(all(data$month %in% sprintf("%02d", 1:12)),
        paste0("unexpected month value(s) in the data: ",
               paste(setdiff(unique(data$month), sprintf("%02d", 1:12)), collapse = ", ")))


## QA baseline: total trade value after filtering (your overall_value check).
## Stage 2 verifies its outputs still sum to this.

qa_raw_total <- sum(data$value_eur, na.rm = TRUE)
qa_stop(nrow(data) > 0, "no rows left after filtering")


## Create combocode: eligibility + import_regime glued together

data$combocode <- paste(data$eligibility, data$import_regime, sep = "")


## QA: combocodes with no entry in the PUR eligibility table -- these would
## be silently dropped by the inner_join below, so shout and save evidence

lost_records <- data %>%
  filter(!combocode %in% PUR_elig$combination_code)

if (nrow(lost_records) > 0) {
  if (!dir.exists("QA")) dir.create("QA")
  lost_file <- paste0("QA/lost_records_", Sys.Date())
  ## Excel caps at ~1m rows; all-partner runs can exceed that
  if (nrow(lost_records) < 1e6) {
    lost_file <- paste0(lost_file, ".xlsx")
    writexl::write_xlsx(lost_records, lost_file)
  } else {
    lost_file <- paste0(lost_file, ".csv")
    fwrite(lost_records, lost_file)
  }
  warning("QA: ", nrow(lost_records),
          " record(s) have combocodes not in the PUR eligibility table: ",
          paste(unique(lost_records$combocode), collapse = ", "),
          "\nWritten to ", lost_file,
          call. = FALSE, immediate. = TRUE)
}


## Join the files together

PUR_export <- inner_join(data, PUR_elig,
                         by = c("eligibility", "import_regime"))

## change column names to the project's standard set
## (partner keeps its name; everything else is renamed in the same order
## as the GB app)

std_names <- c("cooalpha", "CN8", "eligibility", "use", "period",
               "statvalue", "HS2_code", "year", "month", "combocode",
               "eligibility_name", "use_name", "combination_code",
               "PUR_denominator", "PUR_numerator")

rename_idx <- which(names(PUR_export) != "partner")
qa_stop(length(rename_idx) == length(std_names),
        "unexpected columns in PUR_export -- has the Comext or PUR file layout changed?")
names(PUR_export)[rename_idx] <- std_names

qa_warn(all(nchar(PUR_export$CN8) == 8),
        "some product codes are not 8 characters -- check the Comext extract")

rm(data); gc()

message("STAGE 1 complete: ", format(nrow(PUR_export), big.mark = ","),
        " rows | ", n_distinct(PUR_export$partner), " partners",
        " | total value_eur = ",
        format(qa_raw_total, big.mark = ","))