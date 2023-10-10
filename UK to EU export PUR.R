# load library

library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(countrycode)
library(tidyr)
library(data.table)
library(countrycode)

wd <- c("C:/Users/x951160/OneDrive - Defra/UK to EU PUR data")

## Importing data and combining annual files into one data frame - (note files are in dat format)

files_dat <- list.files(path = "Raw Data/", pattern = '.dat', full.names = TRUE)
#KE edited - as its a project file you don't need to specify wd, can just run it to the raw data folder

read_dat <- function(file_path) {
  data <- read.csv(file_path, header = TRUE)
  return(data)
}

df_list <- lapply(files_dat,  read_dat)

data <- rbindlist(df_list)

## read in class and PUR_elig files
#Class file is combination of 2022 and 2023 CN systems.
class <- read_excel("classifications20222023.xlsx")

PUR_elig <- read_excel("PUR eligibility.xlsx")

## Convert column names to lowercase
colnames(data) <- tolower(colnames(data))

## convert the eligibility/use columns to  lowercase (EU data is published in upper) 

data$eligibility <- tolower(data$eligibility)
data$use <- tolower(data$use)

## filter data - this is currently filtered for 2022-23 data
#Filtering out periods ending 52 - which is Comext's convention for annual data
data <- data %>%
  mutate(HS2_code = substr(product,1 ,2),
         year = substr(period, start = 1, stop = 4),
         month = substr(period, start = 5, stop = 6)) %>%
  filter(HS2_code <= 23 & stat_regime ==1 & partner_iso == "GB" & period!= c("202252","202152") & year == c("2022","2023")) %>%
  select(-quantity_kg,-sup_quantity,-declarant,-partner, -partner_iso, -stat_regime)

## Create new column in the PUR dataframe called "Combo code" to merge the eligibility and use codes for each line together
data$combocode <- paste(data$eligibility,data$use, sep = "")

## Join the files together 
PUR_export <- inner_join(data, PUR_elig, by = c("eligibility", "use"))

#Use of inner_join means that only records with a matching entry in the PUR elig dataframe are retained.
#This is a built in QA check to identify the records that are being lost
#Because there was no matching entry in the PUR_elig table
#e.g. helps us identify if new combinations are given in the data that we weren't expecting.

lost_records <- data %>%
  filter(!combocode %in% PUR_elig$combination_code)

#Write an excel file for manual QA
writexl::write_xlsx(data, paste0("UKexEU data for manual QA",Sys.Date(),".xlsx"))

## change column names
names(PUR_export) <- c("cooalpha","CN8","eligibility","use","period","statvalue","HS2_code","year","month","combocode","eligibility_name","use_name","combination_code","PUR_denominator","PUR_numerator")

#Katie's changed from here.
#Create an EU total aggregated data frame
eu_df <- aggregate(cbind(statvalue)~CN8 +eligibility + use + period +HS2_code + 
                     year + month +combocode + eligibility_name + use_name +
                     combination_code + PUR_denominator + PUR_numerator, PUR_export, FUN= sum)

#Add the coalpha column & reorder the columns so they'll match when we bind.
eu_df$cooalpha = "EU"

col_order_eudf <- c("cooalpha","CN8", "eligibility", "use", "period", "statvalue", "HS2_code",
                    "year", "month", "combocode", "eligibility_name", "use_name", "combination_code",
                    "PUR_denominator", "PUR_numerator")
eu_df <- eu_df[, col_order_eudf]

#Add this new EU data to the original dataframe
purcombocode <- bind_rows(PUR_export, eu_df)

#########################

# Final PUR calculations, overall annual preference
#Have to separate out EU otherwise it double counts the data as individual MS and EU in the total
final_exportPUR <- purcombocode %>%
  filter(cooalpha!= "EU")%>%
  group_by(cooalpha,CN8, month, year) %>%
  mutate(Pref_Trade = sum(statvalue[PUR_numerator == "Yes"]),
         Eligible_Trade = sum(statvalue[PUR_denominator == "Yes"]),
         PUR = (Pref_Trade/Eligible_Trade)*100, 
         Total_ex = sum(statvalue),
         ob = 1:n()) %>%
  select(year,month,cooalpha,CN8, Pref_Trade, Eligible_Trade, PUR, Total_ex, ob) %>%
  filter(ob == 1)%>%
  select(-ob)

#Create the EU total version of final dataframe
#Add the coalpha & PUR columns & reorder the columns so they'll match when we bind.
eu_df2 <- aggregate(cbind(Pref_Trade, Eligible_Trade, Total_ex)~ year + month+  CN8, final_exportPUR, FUN= sum)%>%
  mutate(cooalpha = "EU", 
         PUR = (Pref_Trade/Eligible_Trade)*100)

col_order_eudf2 <- c("year", "month", "cooalpha", "CN8", "Pref_Trade","Eligible_Trade", "PUR", "Total_ex")
eu_df2 <- eu_df2[, col_order_eudf2]

#Add this new EU data to the original dataframe
final_exportPUR <- bind_rows(final_exportPUR, eu_df2)

#Replacing NA values with "Not Eligible"
#This means that there were imports, but that they got filtered out on the basis of the PUR methodology table
#e.g. they were MFN or otherwise ineligible.
final_exportPUR$PUR <- ifelse(is.na(final_exportPUR$PUR),"Not Eligible", final_exportPUR$PUR)

#Add country names to the Country codes
final_exportPUR$country_name <- countrycode(final_exportPUR$cooalpha, origin = "genc2c", destination = "country.name")
final_exportPUR$country_name <- if_else(final_exportPUR$cooalpha == "EU", "EU", final_exportPUR$country_name)

#Add country names to the Country codes in PURcombocode
purcombocode$country_name <- countrycode(purcombocode$cooalpha, origin = "genc2c", destination = "country.name")
purcombocode$country_name <- if_else(purcombocode$cooalpha == "EU", "EU", purcombocode$country_name)

#Add in the HS descriptions to final PUR export calculations
final_exportPUR <- inner_join(final_exportPUR, class, by = c("CN8"))

#Check if any CN8s in the data aren't in our classifications file
lost_records2 <- final_exportPUR %>%
  filter(!CN8 %in% class$CN8)

col_order <- c("year","month","cooalpha", "country_name","HS2","HS2_desc","HS4", 
               "HS4_desc", "HS6", "HS6_desc", "CN8", "CN8_desc",
                "ffd_desc", "ffdplus_desc","Pref_Trade", "Eligible_Trade","Total_ex", "PUR")

final_exportPUR <- final_exportPUR[, col_order]

## Add zeros in front of HS codes in the HS column
final_exportPUR$HS2 <- ifelse(str_length(final_exportPUR$HS2) == 1, 
                             str_c("0",final_exportPUR$HS2),final_exportPUR$HS2)

#Write the output file to be uploaded into the app (Excel & RDS)
writexl::write_xlsx(final_exportPUR, paste0("final_exportPUR",Sys.Date(),".xlsx"))

saveRDS(final_exportPUR, paste0("final_exportPUR", Sys.Date(),".RDS"))

#Write an output file for analysing type of preference used.
writexl::write_xlsx(purcombocode, paste0("PUR_type of export preference",Sys.Date(),".xlsx"))

saveRDS(purcombocode, paste0("PUR_type of export preference", Sys.Date(),".RDS"))
