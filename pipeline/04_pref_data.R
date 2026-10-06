# =============================================================================
# STAGE 4: Preference-regime breakdown -- what goods actually entered under.
#
# Source: "PUR_type of export preference_latest.RDS" (record level, from
# Stage 2). That file is far too big to aggregate live in the app, so this
# writes ONE FILE PER PARTNER into data/pref/, each holding full CN8 x member
# state x month detail for that partner. The app opens only the partner being
# viewed and rolls it up (HS2/HS4/HS6, member states combined or split) on
# demand.
#
# Also writes:
#   data/pref_labels.RDS    combocode -> names, regime group, duty-free and
#                           preference-used flags (small lookup)
#   data/pref_partners.RDS  partners that have a file
#
# Run after 03_app_data.R.
# =============================================================================

message("STAGE 4: Building preference regime files ...")

pref_dir <- file.path("data", "pref")
if (!dir.exists(pref_dir)) dir.create(pref_dir, recursive = TRUE)

pc <- readRDS("PUR_type of export preference_latest.RDS")
setDT(pc)

## EU rows are the member-state rows summed, so drop them -- the app adds
## member states up itself (and can then respect a member-state selection)
pc <- pc[cooalpha != "EU"]

qa_stop(nrow(pc) > 0, "no member-state rows in the preference file")


## ---- Labels: one row per combocode ------------------------------------------
## eligibility_name / use_name look like "GSP (e2)" and "GSP non-zero (u21)",
## so the code in brackets is the reliable part to work from.

in_brackets <- function(x) sub("^.*\\(([^)]+)\\)\\s*$", "\\1", x)

pref_labels <- unique(pc[, .(combocode, eligibility_name, use_name,
                             PUR_denominator, PUR_numerator)])
pref_labels[, `:=`(elig_code = in_brackets(eligibility_name),
                   use_code  = in_brackets(use_name))]

pref_labels[, regime := fcase(
  use_code == "u10", "MFN \u2013 zero duty",
  use_code == "u11", "MFN \u2013 duty paid",
  use_code %in% c("u20", "u21"), "GSP preference used",
  use_code %in% c("u30", "u31"), "PTA preference used",
  use_code == "uzz", "Entry regime unknown",
  default = "Not applicable")]

## duty free = entered at a zero rate, whether MFN or preferential
pref_labels[, duty_free := use_code %in% c("u10", "u20", "u30")]
## preference used = counts in the PUR numerator. Taken from the PUR
## eligibility table (not from the use code) so the PURs shown here are
## identical to the ones on the PUR pages.
pref_labels[, pref_used := PUR_numerator == "Yes"]
## eligible = counts in the PUR denominator (same rule as the PUR itself)
pref_labels[, eligible  := PUR_denominator == "Yes"]
## a preference was there but the goods came in under MFN anyway
pref_labels[, pref_not_used := eligible & !pref_used]

## detailed label for the "detailed preference types" view
pref_labels[, detail := paste0(sub(" \\(.*$", "", eligibility_name), ": ",
                               sub(" \\(.*$", "", use_name))]

saveRDS(pref_labels, "data/pref_labels.RDS")

qa_warn(!any(pref_labels$regime == "Not applicable" & pref_labels$use_code != "_z"),
        "some use codes did not map to a regime group -- check pref_labels.RDS")


## ---- Keep the same products as the PUR pages --------------------------------
## Stage 2 inner-joins to the classifications file, so CN8 codes missing from
## it never reach the app. Drop them here too, otherwise the totals in the
## two views wouldn't match.

class_cn8 <- read_excel("classifications2022to26.xlsx")$CN8
before <- nrow(pc)
pc <- pc[CN8 %in% class_cn8]
qa_warn(nrow(pc) > 0, "no rows left after matching the classifications file")
message("  dropped ", format(before - nrow(pc), big.mark = ","),
        " row(s) with CN8 codes missing from the classifications file")


## ---- Two files per partner --------------------------------------------------
## Rows are collapsed onto the four flags the app needs (regime, duty free,
## preference used, eligible) rather than the 22 combocodes, which cuts the
## size sharply. Then:
##   <PARTNER>.RDS       chapter (HS2) level  -- used for All products /
##                       All agrifood / HS2 queries, so the common case is fast
##   <PARTNER>_cn8.RDS   full CN8 detail      -- only opened for HS4/HS6/CN8
## Both are saved uncompressed: bigger on disk, much faster to read.

pc <- merge(pc, pref_labels[, .(combocode, regime, duty_free, pref_used, eligible)],
            by = "combocode", all.x = TRUE)
pc[, HS2 := substr(CN8, 1, 2)]

grp_common <- c("partner", "cooalpha", "country_name", "year", "month",
                "regime", "duty_free", "pref_used", "eligible")

detail <- pc[, .(statvalue = sum(statvalue, na.rm = TRUE)),
             by = c(grp_common, "CN8")]
summary_hs2 <- pc[, .(statvalue = sum(statvalue, na.rm = TRUE)),
                  by = c(grp_common, "HS2")]

for (nm in unique(detail$partner)) {
  dx <- detail[partner == nm][, partner := NULL]
  sx <- summary_hs2[partner == nm][, partner := NULL]
  saveRDS(sx, file.path(pref_dir, paste0(nm, ".RDS")), compress = FALSE)
  saveRDS(dx, file.path(pref_dir, paste0(nm, "_cn8.RDS")), compress = FALSE)
}

saveRDS(sort(unique(detail$partner)), "data/pref_partners.RDS")

sizes  <- file.size(list.files(pref_dir, pattern = "_cn8\\.RDS$", full.names = TRUE))
sizes2 <- file.size(setdiff(list.files(pref_dir, full.names = TRUE),
                            list.files(pref_dir, pattern = "_cn8\\.RDS$", full.names = TRUE)))
message("STAGE 4 complete: ", length(sizes), " partners\n",
        "  chapter files: ", format(nrow(summary_hs2), big.mark = ","), " rows | ",
        round(sum(sizes2) / 1024^2, 1), " MB total, largest ",
        round(max(sizes2) / 1024^2, 1), " MB\n",
        "  CN8 files    : ", format(nrow(detail), big.mark = ","), " rows | ",
        round(sum(sizes) / 1024^2, 1), " MB total, largest ",
        round(max(sizes) / 1024^2, 1), " MB")