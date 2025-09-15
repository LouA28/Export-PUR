#Load libraries     


library(readxl)
library(ggplot2)
library(plotly)
library(data.table)
library(DT)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(scales)
library(lubridate)


PUR_exportdata <- readRDS("final_exportPUR2025-09-15.RDS")

## Importing PUR pref data

Preftype_data <- readRDS("PUR_type of export preference2025-09-15.RDS")

PUR_exportdata$period <- ymd(paste(PUR_exportdata$period, "01", sep = "-"))

PUR_exportdata$period <- as.factor(PUR_exportdata$period) ### MOB - converts the period column back to a factor - facilitates grouping in later steps 

## 1. PUR rates by chapter

## PUR rates by chapter 
HS2_df_2 <- PUR_exportdata %>%
  group_by(HS2,HS2_desc, year) %>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100, .groups = "drop") %>%
  na.omit(PUR)

## 2. PUR rates by chapter & country (excluding any PUR rates that are not calculated)
## This dataset is used to calculate the PUR rate by each HS chapter AND country (excluding any NAs in the original dataset)
## This is used to create country list & calculate total number of countries by HS chapter

HS2_df_country <- PUR_exportdata %>%
  group_by(HS2, HS2_desc, country_name, year) %>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100,.groups = "drop")

## 4. PUR rates by CN8 

## This dataset calculates the PUR rates at CN8 level omitting any NAs (not calculated PURS)
## This dataset is used to create the All_CN8() function that will calculate the lowest PUR for selected country & compare to total average by CN8
## This will effect the reactive text for the CN8 graph that is displayed in the PUR app 


CN8_df <- PUR_exportdata %>%
  group_by(CN8,CN8_desc,year)%>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100,
            .groups="drop")  %>%
  na.omit(PUR)

files_RDS <- list.files(path = "Past updates/", pattern = "\\.RDS$", full.names = TRUE)

data_list <- lapply(files_RDS, function(file) {
  # Read the RDS file
  data <- readRDS(file)
  
  # Extract date from the file name
  date_match <- regexec("\\d{4}-\\d{2}-\\d{2}", file)
  date_stamp <- substr(file, date_match[[1]][1], date_match[[1]][1] + 9) ##### ask louise
  formatted_date <- format(as.Date(date_stamp), "%d-%m-%Y")
  
  # Add date stamp as an extra column
  data$date_stamp <- paste("Eurostat", formatted_date, sep = " ")
  
  return(data)
})

merged_data <- rbindlist(data_list, fill = TRUE)


## Old total exports

## summarizing the total EU export values for the previous data pull & labeling its date

old_data_exports <- merged_data %>%
  filter(country_name == "EU") %>%
  group_by(month, year,date_stamp) %>%
  summarise(Total_exports = sum(Total_ex), .groups = "drop")

## New total exports

## summarizing the total EU export values for the latest data pull & labeling its date

New_data_exports <- PUR_exportdata %>%
  filter(country_name == "EU") %>%
  group_by(month, year) %>%
  summarise(Total_exports = sum(Total_ex), .groups = "drop")

# 6. create dataframe to plot the total agri PUR across all the time periods (time series graph)

## total agri PUR  over time - MOB - groups data by the period column, works out the PUR, ungroups the result
agri_period <- PUR_exportdata %>%
  group_by(period) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")

# 7. Create agri split for time series data - MOB - groups data by hs section and year, works out PUR, pivot wider changes the format of the data table

agri_split <- PUR_exportdata %>%
  group_by(HS_Section,year) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop") %>%
  pivot_wider(,names_from = "year", values_from = "agri_PUR")


agri_split_year <- PUR_exportdata %>%
  group_by(Agri,year) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop") %>%
  rename(HS_Section = Agri) %>%
  pivot_wider(,names_from = "year", values_from = "agri_PUR") 


agri_period_sector <- PUR_exportdata %>%
  group_by(period,HS_Section) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")


preprocessed_data <- PUR_exportdata %>%
  group_by(country_name, year, HS2, HS2_desc) %>%
  summarise(Pref_Trade = sum(Pref_Trade),Eligible_Trade = sum(Eligible_Trade),.groups = "drop")

CN8_summary <- PUR_exportdata %>%
  filter(Eligible_Trade > 0) %>%
  group_by(country_name, year, CN8) %>%
  summarise(traded = n(), .groups = "drop") %>%  # just to retain CN8s
  group_by(country_name, year) %>%
  summarise(total = n_distinct(CN8), .groups = "drop")  # how many CN8s per country/year

preprocessed_agri_PUR <- PUR_exportdata %>%
  group_by(country_name, year) %>%
  summarise(Pref_Trade = sum(Pref_Trade), Eligible_Trade = sum(Eligible_Trade),.groups = "drop") %>%
  mutate(agri_PUR = round(Pref_Trade / Eligible_Trade * 100, 1))


calculateHS2all <- PUR_exportdata %>%
  group_by(year) %>%
  summarise(agri_PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100, .groups = "drop")


CN8_PUR_by_country <- PUR_exportdata %>%
  filter(Eligible_Trade > 0) %>%
  group_by(country_name, year, CN8, CN8_desc) %>%
  summarise(PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100, 2), .groups = "drop")

CN8_all <-  PUR_exportdata %>%
  filter(Eligible_Trade>0, !country_name %in% c("European Union","PEM")) %>%
  group_by(CN8, CN8_desc, year) %>%
  summarise(PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100,2), .groups = "drop") 

CN8_df_2 <- PUR_exportdata %>%
  group_by(CN8, CN8_desc, country_name,year) %>%
  summarise(Pref_Trade = sum(Pref_Trade), Eligible_Trade = sum(Eligible_Trade),.groups = "drop") %>%
  na.omit(PUR)

av_agri_1 <- PUR_exportdata %>%
  group_by(month, country_name,year) %>%
  summarise(Pref_Trade = sum(Pref_Trade), Eligible_Trade = sum(Eligible_Trade),.groups = "drop")


monthly_HS2 <- PUR_exportdata %>%
  group_by(month, country_name, year, HS2) %>%
  summarise(Pref_Trade = sum(Pref_Trade), Eligible_Trade = sum(Eligible_Trade),.groups = "drop")


summary_data_all <- PUR_exportdata %>%
  group_by(month, year, HS2) %>%
  summarise(Pref_Trade = sum(Pref_Trade),Eligible_Trade = sum(Eligible_Trade),.groups = "drop")


summary_data <- PUR_exportdata %>%
  group_by(month, country_name, year, HS2) %>%
  summarise(Pref_Trade = sum(Pref_Trade),Eligible_Trade = sum(Eligible_Trade),.groups = "drop")


Totalexports_all <- PUR_exportdata %>%
  group_by(year, country_name) %>%
  summarise(Total = round(sum(Total_ex) / 1000000000, 1), .groups = "drop")

Totalexports <- PUR_exportdata %>%
  group_by(year, country_name) %>%
  summarise(Total = round(sum(Total_ex)/1000000000,2))


HS2_timeseries <- PUR_exportdata %>%
  group_by(period, `HS2 combined`) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")


HS4_timeseries <- PUR_exportdata %>%
  group_by(period, `HS4 combined`) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")


HS6_timeseries <- PUR_exportdata %>%
  group_by(period, `HS6 combined`) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")

SITC_timeseries <- PUR_exportdata %>%
  group_by(period, `SITC combined`) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")

FFD_timeseries <- PUR_exportdata %>%
  group_by(period, ffd_desc) %>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")


HS2_code_lookup <- PUR_exportdata %>%
  ungroup() %>%
  select(country_name, HS2) %>%
  distinct()

Code_full_lookup <- PUR_exportdata %>%
  ungroup() %>%
  select(`HS2 combined`, `HS4 combined`, `HS6 combined`, `SITC combined`, `ffd_desc`) %>%
  distinct()

preprocessed_HS4 <- PUR_exportdata %>%
  mutate(HS2 = substr(HS4, 1, 2)) %>%
  group_by(year, country_name, HS2, HS4, HS4_desc) %>%
  summarise(
    Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
    Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
    Total_ex = sum(Total_ex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PUR = round((Pref_Trade / Eligible_Trade) * 100, 0))

preprocessed_HS6 <- PUR_exportdata %>%
  mutate(HS2 = substr(HS6, 1, 2)) %>%
  group_by(year, country_name, HS2, HS6, HS6_desc) %>%
  summarise(
    Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
    Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
    Total_ex = sum(Total_ex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PUR = round((Pref_Trade / Eligible_Trade) * 100, 0))


preprocessed_CN8 <- PUR_exportdata %>%
  mutate(HS2 = substr(CN8, 1, 2)) %>%
  group_by(year, country_name, HS2, CN8, CN8_desc) %>%
  summarise(
    Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
    Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
    Total_ex = sum(Total_ex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(PUR = round((Pref_Trade / Eligible_Trade) * 100, 0))

Map1 <- Preftype_data %>%
  group_by(cooalpha, year, country_name)%>%
  mutate(Pref_Trade = sum(statvalue[PUR_numerator == "Yes"]),
         Eligible_Trade = sum(statvalue[PUR_denominator == "Yes"]),
         PUR = (Pref_Trade/Eligible_Trade))%>%
  summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")%>%
  mutate(agri_PUR = as.numeric(ifelse(is.na(agri_PUR),"Not Eligible", agri_PUR)))%>%
  filter(agri_PUR != "Not Eligible")


treemap_eligibility <- Preftype_data %>%
  group_by(country_name, year, eligibility_name) %>%
  summarise(Value = sum(statvalue, na.rm = TRUE), .groups = "drop") %>%
  rename(item = eligibility_name)

# Preprocess treemap data grouped by use_name
treemap_use <- Preftype_data %>%
  group_by(country_name, year, use_name) %>%
  summarise(Value = sum(statvalue, na.rm = TRUE), .groups = "drop") %>%
  rename(item = use_name)

# Preprocess treemap data grouped by chapter
treemap_combination_code <- Preftype_data %>%
  group_by(country_name, year, combination_code) %>%
  summarise(Value = sum(statvalue, na.rm = TRUE), .groups = "drop") %>%
  rename(item = combination_code)

eligi <- Preftype_data %>%
  group_by(country_name, year, eligibility_name) %>%
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
# saveRDS(CN8_pur_summary, "data/CN8_pur_summary.RDS")
saveRDS(CN8_all, "data/CN8_all.RDS")
saveRDS(HS2_timeseries, "data/HS2_timeseries.RDS")
saveRDS(summary_data_all, "data/summary_data_all.RDS")
saveRDS(CN8_PUR_by_country, "data/CN8_PUR_by_country.RDS")
saveRDS(HS2_code_lookup, "data/HS2_code_lookup.RDS")
saveRDS(preprocessed_data, "data/preprocessed_PUR_data.RDS")
saveRDS(preprocessed_agri_PUR, "data/preprocessed_agri_PUR.RDS")
# saveRDS(unique_HS2_combined, "data/unique_HS2_combined.RDS")
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
saveRDS(preprocessed_CN8, "data/preprocessed_CN8_data.RDS")
saveRDS(Map1, "data/Map1.RDS")
saveRDS(treemap_eligibility, "data/treemap_eligibility.RDS")
saveRDS(treemap_use, "data/treemap_use.RDS")
saveRDS(treemap_combination_code, "data/treemap_combination_code.RDS")
saveRDS(eligi, "data/eligi.RDS")