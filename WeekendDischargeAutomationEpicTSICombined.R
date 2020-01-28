rm(list = ls())

#Install and load necessary packages --------------------
#install.packages("readraw_dfl")
#install.packages("writeraw_dfl")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("svDialogs")
#install.packages("stringr")

#Analysis for weekend discharge tracking
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(svDialogs)
library(stringr)

# Set working directory and select raw data ----------------------------
getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data")

# Data used to establish baseline and targets remains constant
tsi_raw_baseline <- read.csv("Discharge Billing Data\\Crosstab_Discharges_YTD_Test 2019-11-22.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
tsi_raw_updates <- read.csv("Discharge Billing Data\\MSBI Discharges Oct-Dec2019 2020-01-22.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Import and bind monthly Epic data files -----------------
epic_raw_fy19 <- read_excel("Epic Daily Discharge Timing Reports\\Updated Reports\\Epic Discharge Timings Report FY2019 2020-01-27.xlsx", col_names = TRUE, na = c("", "NA"))
epic_raw_part_jan20 <- read_excel("Epic Daily Discharge Timing Reports\\Updated Reports\\Epic Discharge Timings Report 01012020 to 01202020 2020-01-27.xls", col_names = TRUE, na = c("", "NA"))

epic_historical <- rbind(epic_raw_fy19, epic_raw_part_jan20)

# epic_raw_updates <- read_excel(choose.files(caption = "Select most recent Epic data"), col_names = TRUE, na = c("", "NA"))

# Reference files and constants ----------------------------------------
ref_file <- "Analysis Reference\\Epic and TSI Data Analysis Reference 2020-01-21.xlsx"
epic_site_dict <- read_excel(ref_file, sheet = "EpicSites")
epic_unit_excl <- read_excel(ref_file, sheet = "EpicUnitExclusions")
tsi_site_dict <- read_excel(ref_file, sheet = "TSISites")
tsi_dispo_dict <- read_excel(ref_file, sheet = "TSIDispo")
tsi_dispo_dict[is.na(tsi_dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"
tsi_service_line_excl <- read_excel(ref_file, sheet = "TSIServiceLines")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL")
DischDOW_Order <- c("Sat-Mon", "Tue", "Wed", "Thu", "Fri")

rpi_start <- as.Date("10/26/2019", "%m/%d/%Y")

# output_location <- choose.dir(caption = "Select script output folder", default = "J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management")

# Function to determine week number of year using Sat as first DOW --------------------------------------------------
weeknum <- function(x) {
  # yr <<- year(x)
  # new_yr <<- as.Date(paste0("1/1/", yr), format = "%m/%d/%Y")
  # new_yr_wkday <<- wday(new_yr, label = FALSE)
  # new_yr_sat <<- new_yr + (7 - new_yr_wkday)
  first_sat_2019 <<- as.Date("1/5/19", format = "%m/%d/%y")
  elapsed_days <<- as.numeric(x - first_sat_2019)
  week_number <<- ifelse(elapsed_days < 0, 1, as.integer(elapsed_days/7)+2)
  week_number
}

# Create custom functions to preprocess and format raw TSI and Epic data -----------------
preprocess_tsi <- function(tsi_raw_df) {
  # Format admit and discharge dates and pull out year, month, DOW, etc.
  tsi_raw_df[ , c("AdmitDate", "DischDate")] <- lapply(tsi_raw_df[ , c("Admit.Dt.Src", "Dsch.Dt.Src")], as.Date, "%m/%d/%Y")
  tsi_raw_df$DischDateTime <- as.POSIXct(paste(tsi_raw_df$Dsch.Dt.Src, tsi_raw_df$Dsch.Time.Src),  tz = "UTC", format = "%m/%d/%Y %H:%M")
  tsi_raw_df[ , c("AdmitYr", "AdmitMo")] <- c(year(tsi_raw_df$AdmitDate), month(tsi_raw_df$AdmitDate))
  tsi_raw_df[ , c("DischYr", "DischMo", "DischHr")] <- c(year(tsi_raw_df$DischDate), month(tsi_raw_df$DischDate), hour(tsi_raw_df$DischDateTime))
  tsi_raw_df$DischDOW <- wday(tsi_raw_df$DischDate, label = TRUE, abbr = TRUE)
  
  # Lookup tables for site, discharge disposition, service line inclusion/exclusion ----------------------------------------------
  # Site lookup
  # tsi_raw_df$Facility.Msx <- as.character(tsi_raw_df$Facility.Msx)
  tsi_raw_df <- left_join(tsi_raw_df, tsi_site_dict, by = c("Facility.Msx" = "Facility Msx"))
  # tsi_raw_df$Site <- factor(tsi_raw_df$Site, levels = site_order, ordered = TRUE)
  
  # Discharge disposition formatting and lookup
  # tsi_raw_df$Discharge.Disposition.Desc.Msx <- factor(tsi_raw_df$Discharge.Disposition.Desc.Msx, levels = c(levels(tsi_raw_df$Discharge.Disposition.Desc.Msx), "Blank"))
  # tsi_raw_df[is.na(tsi_raw_df$Discharge.Disposition.Desc.Msx), "Discharge.Disposition.Desc.Msx"] <- "Blank"
  # tsi_raw_df$Discharge.Disposition.Desc.Msx <- as.character(tsi_raw_df$Discharge.Disposition.Desc.Msx)
  tsi_raw_df <- left_join(tsi_raw_df, tsi_dispo_dict, by = c("Discharge.Disposition.Desc.Msx" = "Discharge Disposition Desc Msx"))
  colnames(tsi_raw_df)[ncol(tsi_raw_df)] <- "DispoRollUp"
  tsi_raw_df[is.na(tsi_raw_df$DispoRollUp), "DispoRollUp"] <- "Unknown"
  
  # Service line inclusion / exclusion lookup
  colnames(tsi_raw_df)[colnames(tsi_raw_df) == "Service.Desc.Msx"] <- "ServiceLine"
  # tsi_raw_df$ServiceLine <- as.character(tsi_raw_df$ServiceLine)
  tsi_raw_df$ServiceLine <- ifelse(is.na(tsi_raw_df$ServiceLine), "Unknown", tsi_raw_df$ServiceLine)
  tsi_raw_df <- left_join(tsi_raw_df, tsi_service_line_excl[ , c(1,3)], by = c("ServiceLine" = "Service Desc Msx"))
  colnames(tsi_raw_df)[ncol(tsi_raw_df)] <- "ServiceLineInclude"
  
  # Exclude encounters with expired disposition and specified service lines
  tsi_raw_df$Include <- ifelse(tsi_raw_df$DispoRollUp == "Expired" | tsi_raw_df$ServiceLineInclude == "No", FALSE, TRUE)
  
  tsi_raw_df$Week_Num <- weeknum(tsi_raw_df$DischDate)
  tsi_raw_df$Weekend <- ifelse(tsi_raw_df$DischDOW == "Sat" | tsi_raw_df$DischDOW == "Sun" | tsi_raw_df$DischDOW == "Mon", TRUE, FALSE)
  
  # Create output list with preprocessed data and preprocessed data to be included  --------------------------------
  tsi_output <- list(tsi_raw_df, tsi_raw_df[tsi_raw_df$Include == TRUE, ])
  return(tsi_output)
}

# Create a function to preprocess and format billing data with the input as the raw Epic data -----------------
preprocess_epic <- function(epic_raw_df) {
  # Format admit and discharge dates and pull out year, month, DOW, etc. --------
  epic_raw_df[ , c("AdmitDate", "DischDate")] <- lapply(epic_raw_df[ , c("HOSP ADMISSION TIME", "HOSP DISCHARGE TIME")], as.Date, tz = "UTC", format = "%m/%d/%Y")
  epic_raw_df[ , c("AdmitYr", "AdmitMo")] <- c(year(epic_raw_df$AdmitDate), month(epic_raw_df$AdmitDate))
  epic_raw_df[ , c("DischYr", "DischMo", "DischHr")] <- c(year(epic_raw_df$DischDate), month(epic_raw_df$DischDate), hour(epic_raw_df$`HOSP DISCHARGE TIME`))
  epic_raw_df$DischDOW <- wday(epic_raw_df$DischDate, label = TRUE, abbr = TRUE)
  
  # Lookup tables for site and discharge unit inclusion/exclusion ----------------------------------------------
  # Site lookup
  epic_raw_df <- left_join(epic_raw_df, epic_site_dict, by = c("REVENUE LOCATION" = "Revenue Location"))
  # epic_raw_df$Site <- factor(epic_raw_df$Site, levels = site_order, ordered = TRUE)
  
  # Discharge disposition - replace missing disposition with Unknown
  epic_raw_df[is.na(epic_raw_df$DISPOSITION), "DISPOSITION"] <- "Unknown"

  # Unit Inclusion / Exclusion
  epic_raw_df <- left_join(epic_raw_df, epic_unit_excl[ , c("Units", "Exclude")], by = c("DISCHARGE UNIT" = "Units"))
  epic_raw_df$IncludeUnit <- ifelse(is.na(epic_raw_df$Exclude), "Yes", "No")
  epic_raw_df$Exclude <- NULL
  
  # Exclude encounters with expired disposition and specified discharge units
  epic_raw_df$Include <- ifelse(epic_raw_df$DISPOSITION == "Expired" | epic_raw_df$IncludeUnit == "No", FALSE, TRUE)
  
  # Determine elapsed week number since 1/1/19 and specify whether discharge date is a weekend date
  epic_raw_df$Week_Num <- weeknum(epic_raw_df$DischDate)
  epic_raw_df$Weekend <- ifelse(epic_raw_df$DischDOW == "Sat" | epic_raw_df$DischDOW == "Sun" | epic_raw_df$DischDOW == "Mon", TRUE, FALSE)
  
  # Create output list with preprocessed data and preprocessed data to be included  --------------------------------
  epic_output <- list(epic_raw_df, epic_raw_df[epic_raw_df$Include == TRUE, ])
  return(epic_output)
}

# Preprocess baseline data and updated weekly datasets -----------------------------
tsi_baseline_output <- preprocess_tsi(tsi_raw_baseline)

tsi_baseline_preprocessed <- tsi_baseline_output[[1]]
tsi_baseline_include <- tsi_baseline_output[[2]]

tsi_jansep19_include <- tsi_baseline_include[tsi_baseline_include$Site == "MSBI" & tsi_baseline_include$DischMo <= 9, ]

epic_hist_output <- preprocess_epic(epic_historical)
epic_hist_preprocessed <- epic_hist_output[[1]]
epic_hist_include <- epic_hist_output[[2]]

epic_jansep19_include <- epic_hist_include[epic_hist_include$DischMo <= 9 & epic_hist_include$DischYr == 2019, ]
epic_df2_include <- epic_hist_include[(epic_hist_include$DischMo > 10 & epic_hist_include$DischYr == 2019) | epic_hist_include$DischYr == 2020, ]

# Data preprocessing and analysis for baseline period of Jan-Sep 2019 -------------------------------------------------------------
# Subset TSI and Epic data with columns to be included in master dataframe
tsi_jansep19_subset <- tsi_jansep19_include[ , c("Encounter.No", "Msmrn", "ServiceLine", "Unit.Desc.Msx", 
                                                 "AdmitDate", "DischDate", "DischDateTime", "AdmitYr", "AdmitMo",
                                                 "DischYr", "DischMo", "DischHr", "DischDOW",
                                                 "Site", "DispoRollUp", "ServiceLineInclude", "Include", "Week_Num", "Weekend")]

colnames(tsi_jansep19_subset) <- c("EncounterNo", "MRN", "ServiceLine", "DischUnit",
                                   "AdmitDate", "DischDate", "DischDateTime", "AdmitYr", "AdmitMo",
                                   "DischYr", "DischMo", "DischHr", "DischDOW",
                                   "Site", "Disposition", "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")

epic_jansep19_subset <- epic_jansep19_include[ , c("VISIT ID", "MRN", "DISCHARGE UNIT", "HOSP DISCHARGE TIME",
                                                   "DISPOSITION", "DBS", "AdmitDate", "DischDate", "AdmitYr", "AdmitMo",
                                                   "DischYr", "DischMo", "DischHr", "DischDOW", 
                                                   "Site", "IncludeUnit", "Include", "Week_Num", "Weekend")]

colnames(epic_jansep19_subset) <- c("EncounterNo", "MRN", "DischUnit", "DischDateTime",
                                    "Disposition", "ServiceLine", "AdmitDate", "DischDate", "AdmitYr", "AdmitMo",
                                    "DischYr", "DischMo", "DischHr", "DischDOW",
                                    "Site", "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")

tsi_jansep19_subset <- tsi_jansep19_subset[ , c("Site", "EncounterNo", "MRN", "AdmitDate", "DischDate", "DischDateTime",
                                                "DischUnit", "ServiceLine", "Disposition", 
                                                "AdmitYr", "AdmitMo", "DischYr", "DischMo", "DischHr", "DischDOW",
                                                "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")]

epic_jansep19_subset <- epic_jansep19_subset[ , c("Site", "EncounterNo", "MRN", "AdmitDate", "DischDate", "DischDateTime",
                                                "DischUnit", "ServiceLine", "Disposition", 
                                                "AdmitYr", "AdmitMo", "DischYr", "DischMo", "DischHr", "DischDOW",
                                                "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")]

# Combine data from both reporting systems for FY2019 into one dataframe
comb_jansep19_subset <- rbind(tsi_jansep19_subset, epic_jansep19_subset)
comb_jansep19_subset <- comb_jansep19_subset[!is.na(comb_jansep19_subset$Site), ]

# Format combined data
comb_jansep19_subset$Site <- factor(comb_jansep19_subset$Site, levels = site_order, ordered = TRUE)
comb_jansep19_subset[ , c("DischUnit", "ServiceLine", "Disposition", "IncludeUnitOrServiceLine")] <- lapply(comb_jansep19_subset[ , c("DischUnit", "ServiceLine", "Disposition", "IncludeUnitOrServiceLine")], factor)

# Aggregate and format baseline data from Jan-Sep 2019 for future use --------------------------------
baseline_site_dischunit_dispo_daily <- as.data.frame(comb_jansep19_subset %>%
                                                            group_by(Site, DischDate, DischUnit, Disposition, DischDOW) %>%
                                                            summarize(TotalDisch = n()))

baseline_site_disch_daily <- as.data.frame(comb_jansep19_subset %>%
                                   group_by(Site, DischDate, DischDOW) %>%
                                   summarize(TotalDisch = n()))

baseline_site_total_avg_disch_dow <- as.data.frame(baseline_site_disch_daily %>%
                                       group_by(Site, DischDOW) %>%
                                       summarize(TotalDischarges = as.numeric(sum(TotalDisch)), AverageDischarges = mean(TotalDisch)))



# Total discharges by day of week for each site
baseline_total_disch_dow <- dcast(baseline_site_total_avg_disch_dow, Site ~DischDOW, value.var = "TotalDischarges")
baseline_total_disch_dow$Total <- rowSums(baseline_total_disch_dow[ , 2:ncol(baseline_total_disch_dow)])
rownames(baseline_total_disch_dow) <- 1:nrow(baseline_total_disch_dow)

# Average discharges by day of week for each site
baseline_avg_disch_dow <- dcast(baseline_site_total_avg_disch_dow, Site ~DischDOW, value.var = "AverageDischarges")
rownames(baseline_avg_disch_dow) <- 1:nrow(baseline_avg_disch_dow)

baseline_avg_stats_summary <- cbind(baseline_avg_disch_dow, 
                             WeekendTotal = round(baseline_avg_disch_dow$Sat + baseline_avg_disch_dow$Sun + baseline_avg_disch_dow$Mon, 0),
                             TargetDelta = round(baseline_avg_disch_dow$Mon*0.1, 0))

baseline_avg_stats_summary$TargetTotal = round(baseline_avg_stats_summary$WeekendTotal, 0) + round(baseline_avg_stats_summary$TargetDelta, 0)


baseline_avg_stats_targets_print <- format(baseline_avg_stats_summary, digits = 0)

hosp_baseline_target <- baseline_avg_stats_summary[ , c("Site", "WeekendTotal", "TargetDelta", "TargetTotal")]
colnames(hosp_baseline_target) <- c("Site", "Weekend Baseline", "Target Change", "Weekend Target")

baseline_outputs <- list("Baseline_EnctrLevel_Incl " = comb_jansep19_subset,
                         "Baseline_Daily_Disch_Unit_Dispo" = baseline_site_dischunit_dispo_daily,
                         "Baseline_Total_Daily_Disch" = baseline_site_disch_daily,
                         "Baseline_Total_Avg_Disch_DOW" = baseline_site_total_avg_disch_dow,
                         "Baseline_Avg_Disch_DOW" = baseline_avg_disch_dow,
                         "Baseline_Avg_Disch_Target_Summ" = baseline_avg_stats_targets_print,
                         "Site_Baseline_Targets" = hosp_baseline_target)


write_xlsx(baseline_outputs, path = paste0('Consolidated Script Data Outputs\\Baseline Data Jan-Sep 2019 Outputs ', Sys.Date(), '.xlsx'))

# # Data preprocessing and analysis for data after baseline period (Oct2019 onward) ----------------------------------------------------------
# # Preprocess most recent TSI data from Oct19 to YTD
# tsi_updates_output <- preprocess_tsi(tsi_raw_updates, )
# tsi_updates_preprocessed <- tsi_updates_output[[1]]
# tsi_updates_include <- tsi_updates_output[[2]]
# 
# # Subset Epic data for Oct-Dec19 (May need to change this later)
# epic_octdec19_include <- epic_fy19_include[epic_fy19_include$DischMo > 9 & epic_fy19_include$DischYr == 2019, ]
# 
# epic_updates <- epic_octdec19_include
# 
# 
# 
# tsi_updates_subset <- tsi_updates_include[tsi_updates_include$Site == "MSBI", c("Encounter.No", "Msmrn", "ServiceLine", "Unit.Desc.Msx", 
#                                                                                    "AdmitDate", "DischDate", "DischDateTime", "AdmitYr", "AdmitMo",
#                                                                                    "DischYr", "DischMo", "DischHr", "DischDOW",
#                                                                                    "Site", "DispoRollUp", "ServiceLineInclude", "Include", "Week_Num", "Weekend")]
# 
# colnames(tsi_updates_subset) <- c("EncounterNo", "MRN", "ServiceLine", "DischUnit",
#                                    "AdmitDate", "DischDate", "DischDateTime", "AdmitYr", "AdmitMo",
#                                    "DischYr", "DischMo", "DischHr", "DischDOW",
#                                    "Site", "Disposition", "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")
# 
# epic_updates_subset <- epic_updates[ , c("VISIT ID", "MRN", "DISCHARGE UNIT", "HOSP DISCHARGE TIME",
#                                                    "DISPOSITION", "DBS", "AdmitDate", "DischDate", "AdmitYr", "AdmitMo",
#                                                    "DischYr", "DischMo", "DischHr", "DischDOW", 
#                                                    "Site", "IncludeUnit", "Include", "Week_Num", "Weekend")]
# 
# colnames(epic_updates_subset) <- c("EncounterNo", "MRN", "DischUnit", "DischDateTime",
#                                     "Disposition", "ServiceLine", "AdmitDate", "DischDate", "AdmitYr", "AdmitMo",
#                                     "DischYr", "DischMo", "DischHr", "DischDOW",
#                                     "Site", "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")
# 
# tsi_updates_subset <- tsi_updates_subset[ , c("Site", "EncounterNo", "MRN", "AdmitDate", "DischDate", "DischDateTime",
#                                                 "DischUnit", "ServiceLine", "Disposition", 
#                                                 "AdmitYr", "AdmitMo", "DischYr", "DischMo", "DischHr", "DischDOW",
#                                                 "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")]
# 
# epic_updates_subset <- epic_updates_subset[ , c("Site", "EncounterNo", "MRN", "AdmitDate", "DischDate", "DischDateTime",
#                                                   "DischUnit", "ServiceLine", "Disposition", 
#                                                   "AdmitYr", "AdmitMo", "DischYr", "DischMo", "DischHr", "DischDOW",
#                                                   "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")]
# 
# 
# 
# 
# 
# # Track performance after RPI sessions began
# tsi_updates_subset$PostRPI <- tsi_updates_subset$DischDate >= rpi_start
# epic_updates_subset$PostRPI <- epic_updates_subset$DischDate >= rpi_start
# 
# tsi_updates_subset <- tsi_updates_subset[tsi_updates_subset$PostRPI == TRUE, ]
# epic_updates_subset <- epic_updates_subset[epic_updates_subset$PostRPI == TRUE, ]
# 
# 
# 
# 
# 
# # Determine RPI end date for TSI sites
# tsi_disch_date_df <- unique(tsi_updates_subset[ , c("DischDate", "DischDOW")])
# tsi_rpi_end <- max(tsi_disch_date_df[tsi_disch_date_df$DischDOW == "Fri", "DischDate"])
# 
# tsi_updates_subset <- tsi_updates_subset[tsi_updates_subset$DischDate >= rpi_start &  tsi_updates_subset$DischDate <= tsi_rpi_end, ]
# 
# 
# epic_disch_date_df <- unique(epic_updates_subset[ , c("DischDate", "DischDOW")])
# epic_rpi_end <- max(epic_disch_date_df[epic_disch_date_df$DischDOW == "Mon", "DischDate"])
# epic_rpi_end <- max(epic_disch_date_df[epic_disch_date_df$DischDOW == "Fri", "DischDate"])
# 
# epic_updates_subset <- epic_updates_subset[epic_updates_subset$DischDate >= rpi_start &  epic_updates_subset$DischDate <= epic_rpi_end, ]
# 
# 
# 
# # Create daily list of discharges by site
# site_daily_disch_vol <- as.data.frame(comb_rpi_subset %>%
#                                         group_by(Site, DischDate, WeekNumber, DischDOW, Weekend, PostRPI) %>%
#                                         summarize(TotalDisch = n()))
# 
# site_daily_disch_vol2 <- as.data.frame(comb_rpi_subset %>%
#                                         group_by(Site, DischUnit, Disposition, DischDate, WeekNumber, DischDOW, Weekend, PostRPI) %>%
#                                         summarize(TotalDisch = n()))
# 
# 
# # Create a list of week dates starting with Sat and ending with Fri
# week_num_dates <- as.data.frame(site_daily_disch_vol %>%
#                                   group_by(WeekNumber) %>%
#                                   summarize(SatDate = min(DischDate), FriDate = SatDate + 6, MonDate = SatDate + 2))
# 
# week_num_dates[ , c("SatDate", "FriDate", "MonDate")] <- lapply(week_num_dates[ , c("SatDate", "FriDate", "MonDate")], format, "%m/%d/%y")
# 
# week_num_dates$WeekOf <- paste0(week_num_dates$SatDate, "-", week_num_dates$FriDate)
# week_num_dates$WeekendOf <- paste0(week_num_dates$SatDate, "-", week_num_dates$MonDate)
# week_num_dates <- week_num_dates[ , c("WeekNumber", "SatDate", "WeekOf", "WeekendOf")]

