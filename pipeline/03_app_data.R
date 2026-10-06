# STAGE 3: Core datasets -> the preprocessed RDS files the EU app reads
#          from its data/ folder.
#
# This is your existing preprocessing script with only the top changed:
# it reads the fixed "_latest" files, so no more editing dates in code.
#
# Partner is added to every grouping, so every output file has a partner
# column for the app to filter on.
# The Revisions data section is KEPT -- this app's revisions tab stays
# (Eurostat revisions have been large enough to matter to users).
# =============================================================================

message("STAGE 3: Building EU app data files ...")

PUR_exportdata <- readRDS("final_exportPUR_latest.RDS")

## Importing PUR pref data

Preftype_data <- readRDS("PUR_type of export preference_latest.RDS")

PUR_exportdata$period <- ymd(paste(PUR_exportdata$period, "01", sep = "-"))

PUR_exportdata$period <- as.factor(PUR_exportdata$period)

## 1. PUR rates by chapter

HS2_df_2 <- PUR_exportdata %>%
  group_by(partner, HS2,HS2_desc, year) %>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100, .groups = "drop") %>%
  na.omit(PUR)

## 2. PUR rates by chapter & country

HS2_df_country <- PUR_exportdata %>%
  group_by(partner, HS2, HS2_desc, country_name, year) %>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100,.groups = "drop")

## 4. PUR rates by CN8

CN8_df <- PUR_exportdata %>%
  group_by(partner, CN8,CN8_desc,year)%>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100,
            .groups="drop")  %>%
  na.omit(PUR)

## Revisions data: all past vintages + the current one (KEPT -- the EU app's
## revisions tab uses these)

files_RDS <- list.files(path = "Past updates/", pattern = "\\.RDS$",
                        full.names = TRUE)

data_list <- lapply(files_RDS, function(file) {
  # Read the RDS file
  data <- readRDS(file)
  
  # Skip vintages saved before partner was added (e.g. from the GB-only
  # app): different scope, so they can't be compared for revisions
  if (!"partner" %in% names(data)) {
    message("  skipping ", basename(file),
            " in revisions (no partner column -- saved by the GB-only pipeline)")
    return(NULL)
  }
  
  # Extract date from the file name
  date_match <- regexec("\\d{4}-\\d{2}-\\d{2}", file)
  date_stamp <- substr(file, date_match[[1]][1], date_match[[1]][1] + 9)
  formatted_date <- format(as.Date(date_stamp), "%d-%m-%Y")
  
  # Add date stamp as an extra column
  data$date_stamp <- paste("Eurostat", formatted_date, sep = " ")
  
  return(data)
})

merged_data <- rbindlist(data_list, fill = TRUE)


## Old total exports
## (no usable past vintages yet -> empty table)

old_data_exports <- if (nrow(merged_data) == 0) {
  tibble(partner = character(), month = character(), year = character(),
         date_stamp = character(), Total_exports = numeric())
} else merged_data %>%
  filter(country_name == "EU") %>%
  group_by(partner, month, year, date_stamp) %>%
  summarise(Total_exports = sum(Total_ex), .groups = "drop")

## New total exports

New_data_exports <- PUR_exportdata %>%
  filter(country_name == "EU") %>%
  group_by(partner, month, year) %>%
  summarise(Total_exports = sum(Total_ex), .groups = "drop")

## Automatic date stamp for the newest vintage (pairs with the app edits --
## the app must no longer overwrite this or hardcode the factor levels)
New_data_exports$date_stamp <- paste("Eurostat", format(Sys.Date(), "%d-%m-%Y"))


# 6. create dataframe to plot the total agri PUR across all the time periods

agri_period <- PUR_exportdata %>%
  group_by(partner, period) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")

# 7. Create agri split for time series data

agri_split <- PUR_exportdata %>%
  group_by(partner, HS_Section,year) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop") %>%
  pivot_wider(,names_from = "year", values_from = "agri_PUR")


agri_split_year <- PUR_exportdata %>%
  group_by(partner, Agri,year) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop") %>%
  rename(HS_Section = Agri) %>%
  pivot_wider(,names_from = "year", values_from = "agri_PUR")


agri_period_sector <- PUR_exportdata %>%
  group_by(partner, period,HS_Section) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")


preprocessed_data <- PUR_exportdata %>%
  group_by(partner, country_name, year, HS2, HS2_desc) %>%
  summarise(Pref_Trade = sum(Pref_Trade),Eligible_Trade = sum(Eligible_Trade),.groups = "drop")

CN8_summary <- PUR_exportdata %>%
  filter(Eligible_Trade > 0) %>%
  group_by(partner, country_name, year, CN8) %>%
  summarise(traded = n(), .groups = "drop") %>%
  group_by(partner, country_name, year) %>%
  summarise(total = n_distinct(CN8), .groups = "drop")

preprocessed_agri_PUR <- PUR_exportdata %>%
  group_by(partner, country_name, year) %>%
  summarise(Pref_Trade = sum(Pref_Trade), Eligible_Trade = sum(Eligible_Trade),.groups = "drop") %>%
  mutate(agri_PUR = round(Pref_Trade / Eligible_Trade * 100, 1))


calculateHS2all <- PUR_exportdata %>%
  group_by(partner, year) %>%
  summarise(agri_PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100, .groups = "drop")


CN8_PUR_by_country <- PUR_exportdata %>%
  filter(Eligible_Trade > 0) %>%
  group_by(partner, country_name, year, CN8, CN8_desc) %>%
  summarise(PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100, 2), .groups = "drop")

CN8_all <-  PUR_exportdata %>%
  filter(Eligible_Trade>0, !country_name %in% c("European Union","PEM")) %>%
  group_by(partner, CN8, CN8_desc, year) %>%
  summarise(PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100,2), .groups = "drop")

CN8_df_2 <- PUR_exportdata %>%
  group_by(partner, CN8, CN8_desc, country_name,year) %>%
  summarise(Pref_Trade = sum(Pref_Trade), Eligible_Trade = sum(Eligible_Trade),.groups = "drop") %>%
  na.omit(PUR)

av_agri_1 <- PUR_exportdata %>%
  group_by(partner, month, country_name,year) %>%
  summarise(Pref_Trade = sum(Pref_Trade), Eligible_Trade = sum(Eligible_Trade),.groups = "drop")


monthly_HS2 <- PUR_exportdata %>%
  group_by(partner, month, country_name, year, HS2) %>%
  summarise(Pref_Trade = sum(Pref_Trade), Eligible_Trade = sum(Eligible_Trade),.groups = "drop")


summary_data_all <- PUR_exportdata %>%
  group_by(partner, month, year, HS2) %>%
  summarise(Pref_Trade = sum(Pref_Trade),Eligible_Trade = sum(Eligible_Trade),.groups = "drop")


summary_data <- PUR_exportdata %>%
  group_by(partner, month, country_name, year, HS2) %>%
  summarise(Pref_Trade = sum(Pref_Trade),Eligible_Trade = sum(Eligible_Trade),.groups = "drop")


Totalexports_all <- PUR_exportdata %>%
  group_by(partner, year, country_name) %>%
  summarise(Total = round(sum(Total_ex) / 1000000000, 1), .groups = "drop")

Totalexports <- PUR_exportdata %>%
  group_by(partner, year, country_name) %>%
  summarise(Total = round(sum(Total_ex)/1000000000,2))


HS2_timeseries <- PUR_exportdata %>%
  group_by(partner, period, `HS2 combined`) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")


HS4_timeseries <- PUR_exportdata %>%
  group_by(partner, period, `HS4 combined`) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")


HS6_timeseries <- PUR_exportdata %>%
  group_by(partner, period, `HS6 combined`) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")

SITC_timeseries <- PUR_exportdata %>%
  group_by(partner, period, `SITC combined`) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")

FFD_timeseries <- PUR_exportdata %>%
  group_by(partner, period, ffd_desc) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")


HS2_code_lookup <- PUR_exportdata %>%
  ungroup() %>%
  select(partner, country_name, HS2) %>%
  distinct()

Code_full_lookup <- PUR_exportdata %>%
  ungroup() %>%
  select(`HS2 combined`, `HS4 combined`, `HS6 combined`, `SITC combined`, `ffd_desc`) %>%
  distinct()

preprocessed_HS4 <- PUR_exportdata %>%
  mutate(HS2 = substr(HS4, 1, 2)) %>%
  group_by(partner, year, country_name, HS2, HS4, HS4_desc) %>%
  summarise(
    Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
    Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
    Total_ex = sum(Total_ex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PUR = round((Pref_Trade / Eligible_Trade) * 100, 0))

preprocessed_HS6 <- PUR_exportdata %>%
  mutate(HS2 = substr(HS6, 1, 2)) %>%
  group_by(partner, year, country_name, HS2, HS6, HS6_desc) %>%
  summarise(
    Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
    Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
    Total_ex = sum(Total_ex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PUR = round((Pref_Trade / Eligible_Trade) * 100, 0))


## SITC tables: the app offers SITC as a code level, so it needs this as
## well as the SITC_timeseries file used by the graphs
preprocessed_SITC <- PUR_exportdata %>%
  group_by(partner, year, country_name, `SITC combined`) %>%
  summarise(
    Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
    Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
    Total_ex = sum(Total_ex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PUR = round((Pref_Trade / Eligible_Trade) * 100, 0))


preprocessed_CN8 <- PUR_exportdata %>%
  mutate(HS2 = substr(CN8, 1, 2)) %>%
  group_by(partner, year, country_name, HS2, CN8, CN8_desc) %>%
  summarise(
    Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
    Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
    Total_ex = sum(Total_ex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PUR = round((Pref_Trade / Eligible_Trade) * 100, 0))

Map1 <- Preftype_data %>%
  group_by(partner, cooalpha, year, country_name)%>%
  mutate(Pref_Trade = sum(statvalue[PUR_numerator == "Yes"]),
         Eligible_Trade = sum(statvalue[PUR_denominator == "Yes"]),
         PUR = (Pref_Trade/Eligible_Trade))%>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")%>%
  mutate(agri_PUR = as.numeric(ifelse(is.na(agri_PUR),"Not Eligible", agri_PUR)))%>%
  filter(agri_PUR != "Not Eligible")


treemap_eligibility <- Preftype_data %>%
  group_by(partner, country_name, year, eligibility_name) %>%
  summarise(Value = sum(statvalue, na.rm = TRUE), .groups = "drop") %>%
  rename(item = eligibility_name)

# Preprocess treemap data grouped by use_name
treemap_use <- Preftype_data %>%
  group_by(partner, country_name, year, use_name) %>%
  summarise(Value = sum(statvalue, na.rm = TRUE), .groups = "drop") %>%
  rename(item = use_name)

# Preprocess treemap data grouped by chapter
treemap_combination_code <- Preftype_data %>%
  group_by(partner, country_name, year, combination_code) %>%
  summarise(Value = sum(statvalue, na.rm = TRUE), .groups = "drop") %>%
  rename(item = combination_code)

eligi <- Preftype_data %>%
  group_by(partner, country_name, year, eligibility_name) %>%
  summarise(Value = sum(statvalue), .groups = "drop")

saveRDS(HS2_df_2, "data/HS2_df_2_processed.RDS")
saveRDS(HS2_df_country, "data/HS2_df_country_processed.RDS")
saveRDS(CN8_df, "data/CN8_df_processed.RDS")
saveRDS(old_data_exports, "data/old_data_exports.RDS")
saveRDS(New_data_exports, "data/New_data_exports.RDS")
saveRDS(agri_period, "data/agri_period.RDS")
saveRDS(agri_split, "data/agri_split.RDS")
saveRDS(agri_split_year, "data/agri_split_year.RDS")
saveRDS(agri_period_sector, "data/agri_period_sector.RDS")
saveRDS(CN8_summary, "data/CN8_summary.RDS")
saveRDS(CN8_all, "data/CN8_all.RDS")
saveRDS(HS2_timeseries, "data/HS2_timeseries.RDS")
saveRDS(summary_data_all, "data/summary_data_all.RDS")
saveRDS(CN8_PUR_by_country, "data/CN8_PUR_by_country.RDS")
saveRDS(HS2_code_lookup, "data/HS2_code_lookup.RDS")
saveRDS(preprocessed_data, "data/preprocessed_PUR_data.RDS")
saveRDS(preprocessed_agri_PUR, "data/preprocessed_agri_PUR.RDS")
saveRDS(Code_full_lookup, "data/Code_full_lookup.RDS")
saveRDS(calculateHS2all, "data/calculateHS2all.RDS")
saveRDS(Totalexports, "data/Totalexports.RDS")
saveRDS(Totalexports_all, "data/Totalexports_all.RDS")
saveRDS(CN8_df_2, "data/CN8_df_2.RDS")
saveRDS(av_agri_1, "data/av_agri_1.RDS")
saveRDS(monthly_HS2, "data/monthly_HS2.RDS")
saveRDS(summary_data, "data/summary.RDS")
saveRDS(HS4_timeseries, "data/HS4_timeseries.RDS")
saveRDS(HS6_timeseries, "data/HS6_timeseries.RDS")
saveRDS(SITC_timeseries, "data/SITC_timeseries.RDS")
saveRDS(FFD_timeseries, "data/FFD_timeseries.RDS")
saveRDS(preprocessed_HS4, "data/preprocessed_HS4_data.RDS")
saveRDS(preprocessed_HS6, "data/preprocessed_HS6_data.RDS")
saveRDS(preprocessed_SITC, "data/preprocessed_SITC.RDS")
saveRDS(preprocessed_CN8, "data/preprocessed_CN8_data.RDS")
saveRDS(Map1, "data/Map1.RDS")
saveRDS(treemap_eligibility, "data/treemap_eligibility.RDS")
saveRDS(treemap_use, "data/treemap_use.RDS")
saveRDS(treemap_combination_code, "data/treemap_combination_code.RDS")
saveRDS(eligi, "data/eligi.RDS")

message("STAGE 3 complete: EU app data/ folder refreshed.")


## ---------------------------------------------------------------------------
## Small lookups file: the lists the app needs at start-up (years, member
## states, code descriptions, partners). Without this the app has to open the
## largest data files just to fill dropdowns, which is what makes it slow to
## load.
## ---------------------------------------------------------------------------

app_lookups <- list(
  years       = sort(unique(PUR_exportdata$year)),
  full_years  = PUR_exportdata %>% ungroup() %>% distinct(year, month) %>%
    count(year) %>% filter(n == 12) %>% pull(year) %>% sort(),
  countries   = sort(setdiff(unique(PUR_exportdata$country_name), "EU")),
  partners    = sort(unique(PUR_exportdata$partner)),
  map_years   = sort(unique(Map1$year)),
  hs2_desc    = PUR_exportdata %>% ungroup() %>% distinct(HS2, HS2_desc) %>% arrange(HS2),
  hs4_desc    = PUR_exportdata %>% ungroup() %>% distinct(HS4, HS4_desc) %>% arrange(HS4),
  hs6_desc    = PUR_exportdata %>% ungroup() %>% distinct(HS6, HS6_desc) %>% arrange(HS6),
  cn8_desc    = PUR_exportdata %>% ungroup() %>% distinct(CN8, CN8_desc) %>% arrange(CN8),
  hs4_combined  = sort(unique(HS4_timeseries$`HS4 combined`)),
  hs6_combined  = sort(unique(HS6_timeseries$`HS6 combined`)),
  sitc_combined = sort(unique(SITC_timeseries$`SITC combined`))
)

saveRDS(app_lookups, "data/app_lookups.RDS")

message("  lookups file written: ", length(app_lookups$partners), " partners, ",
        length(app_lookups$countries), " member states, ",
        nrow(app_lookups$cn8_desc), " CN8 codes")