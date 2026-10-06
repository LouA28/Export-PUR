# STAGE 2: PUR_export -> the two core datasets:
#   A. final_exportPUR              (monthly PUR by member state + EU bloc)
#   B. PUR_type of export preference (record-level, for treemaps/map)
#
# final_exportPUR is saved dated + _latest; the previous dated vintage is
# moved into "Past updates/" automatically, which is what the app's
# Revisions tab reads.
#
# Partner is carried through every grouping, so every figure is per
# partner (the EU bloc rows are EU totals per partner).
# =============================================================================

message("STAGE 2: Building core export PUR datasets ...")


## ---------------------------------------------------------------------------
## A1. EU bloc rows at raw level (Katie's aggregation, unchanged)
## ---------------------------------------------------------------------------

eu_terms <- c("CN8", "eligibility", "use", "period", "HS2_code", "year",
              "month", "combocode", "eligibility_name", "use_name",
              "combination_code", "PUR_denominator", "PUR_numerator", "partner")

eu_df <- aggregate(as.formula(paste("cbind(statvalue) ~",
                                    paste(eu_terms, collapse = " + "))),
                   PUR_export, FUN = sum)

eu_df$cooalpha <- "EU"

col_order_eudf <- c("cooalpha", "CN8", "eligibility", "use", "period",
                    "statvalue", "HS2_code", "year", "month", "combocode",
                    "eligibility_name", "use_name", "combination_code",
                    "PUR_denominator", "PUR_numerator", "partner")
eu_df <- eu_df[, col_order_eudf]

purcombocode <- bind_rows(PUR_export, eu_df)

## Built-in QA (your overall_value2 check, formalised): the member-state rows
## must still sum to the raw total -- the EU aggregation mustn't change it

qa_check_totals(
  qa_raw_total,
  sum(purcombocode$statvalue[purcombocode$cooalpha != "EU"], na.rm = TRUE),
  label = "purcombocode (member-state rows)"
)


## ---------------------------------------------------------------------------
## A2. Final PUR calculations, overall annual preference
##     (EU separated out so it isn't double counted -- your logic, unchanged)
## ---------------------------------------------------------------------------

final_exportPUR <- purcombocode %>%
  filter(cooalpha != "EU") %>%
  group_by(partner, cooalpha, CN8, month, year) %>%
  mutate(Pref_Trade     = sum(statvalue[PUR_numerator   == "Yes"]),
         Eligible_Trade = sum(statvalue[PUR_denominator == "Yes"]),
         PUR            = (Pref_Trade / Eligible_Trade) * 100,
         Total_ex       = sum(statvalue),
         ob = 1:n()) %>%
  select(partner, year, month, cooalpha, CN8,
         Pref_Trade, Eligible_Trade, PUR, Total_ex, period, ob) %>%
  filter(ob == 1) %>%
  select(-ob)

## EU total version, aggregated from the member-state rows

eu2_terms <- c("year", "month", "period", "CN8", "partner")

eu_df2 <- aggregate(as.formula(paste("cbind(Pref_Trade, Eligible_Trade, Total_ex) ~",
                                     paste(eu2_terms, collapse = " + "))),
                    final_exportPUR, FUN = sum) %>%
  mutate(cooalpha = "EU",
         PUR = (Pref_Trade / Eligible_Trade) * 100)

col_order_eudf2 <- c("year", "month", "period", "cooalpha", "CN8",
                     "Pref_Trade", "Eligible_Trade", "PUR", "Total_ex",
                     "partner")
eu_df2 <- eu_df2[, col_order_eudf2]

final_exportPUR <- bind_rows(final_exportPUR, eu_df2)

## Replacing NA values with "Not Eligible"
## (there were imports, but they were filtered out by the PUR methodology
##  table, e.g. MFN or otherwise ineligible)

final_exportPUR$PUR <- ifelse(is.na(final_exportPUR$PUR),
                              "Not Eligible", final_exportPUR$PUR)

## Country names (single helper; only the EU code needs an override)

final_exportPUR <- add_country_names(final_exportPUR)
purcombocode    <- add_country_names(purcombocode)

## QA: CN8s missing from the classifications file -- checked BEFORE the join
## (the old lost_records2 check ran after the inner_join, by which point the
## missing rows were already gone, so it could never find anything)

missing_cn8 <- setdiff(unique(final_exportPUR$CN8), class$CN8)
qa_warn(length(missing_cn8) == 0,
        paste0(length(missing_cn8), " CN8 code(s) missing from ",
               "classifications2022to26.xlsx and will be dropped: ",
               paste(head(missing_cn8, 20), collapse = ", ")))

## Add in the HS descriptions

final_exportPUR <- inner_join(final_exportPUR, class, by = c("CN8"))

col_order <- c("year", "month", "period", "cooalpha", "country_name", "partner",
               "HS2", "HS2_desc", "HS4", "HS4_desc", "HS6", "HS6_desc",
               "CN8", "CN8_desc", "ffd_desc", "ffdplus_desc", "DIV",
               "DIV description", "Sector", "HS_Section", "Agri",
               "HS2 combined", "HS4 combined", "HS6 combined",
               "SITC combined", "Pref_Trade", "Eligible_Trade", "Total_ex",
               "PUR")

final_exportPUR <- final_exportPUR[, col_order]

## Add zeros in front of HS codes in the HS column

final_exportPUR$HS2 <- ifelse(str_length(final_exportPUR$HS2) == 1,
                              str_c("0", final_exportPUR$HS2),
                              final_exportPUR$HS2)


## ---------------------------------------------------------------------------
## SAVE FILES
## ---------------------------------------------------------------------------

## Move the previous dated dataset into "Past updates/" -- this is what the
## app's Revisions tab reads (replaces the manual move)

archive_previous(pattern = "^final_exportPUR\\d{4}-\\d{2}-\\d{2}\\.RDS$",
                 from = ".", to = "Past updates")

## final_exportPUR: dated (the old record) + _latest (what stage 3 reads).
## Dated .xlsx only if it fits in Excel's ~1m row limit.
save_output(final_exportPUR, "final_exportPUR", dir = ".",
            excel = nrow(final_exportPUR) < 1e6)

## Preference-type data: _latest only -- no downstream use for old datasets
saveRDS(purcombocode, "PUR_type of export preference_latest.RDS")

message("STAGE 2 complete.")