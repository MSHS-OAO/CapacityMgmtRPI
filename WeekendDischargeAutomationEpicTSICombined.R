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
tsi_raw_base <- read.csv("Discharge Billing Data\\Crosstab_Discharges_YTD_Test 2019-11-22.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
tsi_raw_octdec <- read.csv("Discharge Billing Data\\MSBI Discharges Oct-Dec2019 2020-01-22.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Import and bind monthly Epic data files -----------------
epic_raw_fy19 <- read_excel("Epic Daily Discharge Timings\\FY2019 Consolidated Reports\\FY2019 Epic Discharge Data 2020-01-21.xlsx", col_names = TRUE, na = c("", "NA"))

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

output_location <- choose.dir(caption = "Select script output folder", default = "J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management")

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
  tsi_raw_df$DischDateTime <- as.POSIXct(paste(tsi_raw_df$Dsch.Dt.Src, tsi_raw_df$Dsch.Time.Src),  tz = "", format = "%m/%d/%Y %H:%M")
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
  epic_raw_df[ , c("AdmitDate", "DischDate")] <- lapply(epic_raw_df[ , c("HOSP ADMISSION TIME", "HOSP DISCHARGE TIME")], as.Date, tz = "", format = "%m/%d/%Y")
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
tsi_baseline_output <- preprocess_tsi(tsi_raw_base)

tsi_baseline_preprocessed <- tsi_baseline_output[[1]]
tsi_baseline_include <- tsi_baseline_output[[2]]

tsi_jansep_include <- tsi_baseline_include[tsi_baseline_include$Site == "MSBI" & tsi_baseline_include$DischMo <= 9, ]

tsi_octdec_output <- preprocess_tsi(tsi_raw_octdec)
tsi_octdec_preprocessed <- tsi_octdec_output[[1]]
tsi_octdec_include <- tsi_octdec_output[[2]]

tsi_fy19_include <- rbind(tsi_jansep_include, tsi_octdec_include)

epic_fy19_output <- preprocess_epic(epic_raw_fy19)
epic_fy19_preprocessed <- epic_fy19_output[[1]]
epic_fy19_include <- epic_fy19_output[[2]]

# Subset TSI and Epic data with columns to be included in master dataframe
tsi_fy19_subset <- tsi_fy19_include[tsi_fy19_include$Site == "MSBI", c("Encounter.No", "Msmrn", "ServiceLine", "Unit.Desc.Msx", 
                                                 "AdmitDate", "DischDate", "DischDateTime", "AdmitYr", "AdmitMo",
                                                 "DischYr", "DischMo", "DischHr", "DischDOW",
                                                 "Site", "DispoRollUp", "ServiceLineInclude", "Include", "Week_Num", "Weekend")]

colnames(tsi_fy19_subset) <- c("EncounterNo", "MRN", "ServiceLine", "DischUnit",
                                   "AdmitDate", "DischDate", "DischDateTime", "AdmitYr", "AdmitMo",
                                   "DischYr", "DischMo", "DischHr", "DischDOW",
                                   "Site", "Disposition", "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")

epic_fy19_subset <- epic_fy19_include[ , c("VISIT ID", "MRN", "DISCHARGE UNIT", "HOSP DISCHARGE TIME",
                                                   "DISPOSITION", "DBS", "AdmitDate", "DischDate", "AdmitYr", "AdmitMo",
                                                   "DischYr", "DischMo", "DischHr", "DischDOW", 
                                                   "Site", "IncludeUnit", "Include", "Week_Num", "Weekend")]

colnames(epic_fy19_subset) <- c("EncounterNo", "MRN", "DischUnit", "DischDateTime",
                                    "Disposition", "ServiceLine", "AdmitDate", "DischDate", "AdmitYr", "AdmitMo",
                                    "DischYr", "DischMo", "DischHr", "DischDOW",
                                    "Site", "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")

tsi_fy19_subset <- tsi_fy19_subset[ , c("Site", "EncounterNo", "MRN", "AdmitDate", "DischDate", "DischDateTime",
                                                "DischUnit", "ServiceLine", "Disposition", 
                                                "AdmitYr", "AdmitMo", "DischYr", "DischMo", "DischHr", "DischDOW",
                                                "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")]

epic_fy19_subset <- epic_fy19_subset[ , c("Site", "EncounterNo", "MRN", "AdmitDate", "DischDate", "DischDateTime",
                                                "DischUnit", "ServiceLine", "Disposition", 
                                                "AdmitYr", "AdmitMo", "DischYr", "DischMo", "DischHr", "DischDOW",
                                                "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")]

# Combine data from both reporting systems for FY2019 into one dataframe
comb_fy19_subset <- rbind(tsi_fy19_subset, epic_fy19_subset)
comb_fy19_subset <- comb_fy19_subset[!is.na(comb_fy19_subset$Site), ]

# Format combined data
comb_fy19_subset$Site <- factor(comb_fy19_subset$Site, levels = site_order, ordered = TRUE)
comb_fy19_subset[ , c("DischUnit", "ServiceLine", "Disposition", "IncludeUnitOrServiceLine")] <- lapply(comb_fy19_subset[ , c("DischUnit", "ServiceLine", "Disposition", "IncludeUnitOrServiceLine")], factor)

# Subset combined data to include data from baseline period (Jan-Sep 2019)
comb_baseline_subset <- comb_fy19_subset[comb_fy19_subset$DischMo <= 9 & comb_fy19_subset$DischYr == 2019, ]

# Aggregate and format baseline data from Jan-Sep 2019 for future use --------------------------------
comb_site_daily <- as.data.frame(comb_baseline_subset %>%
                                   group_by(Site, DischDate, DischDOW) %>%
                                   summarize(TotalDisch = n()))

comb_site_disch_dow <- as.data.frame(comb_site_daily %>%
                                       group_by(Site, DischDOW) %>%
                                       summarize(TotalDischarges = as.numeric(sum(TotalDisch)), AverageDischarges = mean(TotalDisch)))


# Total discharges by day of week for each site
comb_total_site_disch_dow_tbl <- dcast(comb_site_disch_dow, Site ~DischDOW, value.var = "TotalDischarges")
rownames(comb_total_site_disch_dow_tbl) <- 1:nrow(comb_total_site_disch_dow_tbl)


# Average discharges by day of week for each site
comb_avg_site_disch_dow_tbl <- dcast(comb_site_disch_dow, Site ~DischDOW, value.var = "AverageDischarges")
rownames(comb_avg_site_disch_dow_tbl) <- 1:nrow(comb_avg_site_disch_dow_tbl)

comb_avg_site_stats_summary <- cbind(comb_avg_site_disch_dow_tbl, 
                             WeekendTotal = round(comb_avg_site_disch_dow_tbl$Sat + comb_avg_site_disch_dow_tbl$Sun + comb_avg_site_disch_dow_tbl$Mon, 0),
                             TargetDelta = round(comb_avg_site_disch_dow_tbl$Mon*0.1, 0))

comb_avg_site_disch_dow_print <- format(comb_avg_site_disch_dow_tbl, digits = 0)
comb_avg_site_stats_print <- format(comb_avg_site_stats_summary, digits = 0)

# Track performance after RPI sessions began
comb_rpi_subset <- comb_fy19_subset[comb_fy19_subset$DischDate >= as.Date("10/01/2019", format = "%m/%d/%Y"), ]
comb_rpi_subset$PostRPI <- comb_rpi_subset$DischDate >= rpi_start

# Determine RPI end date for TSI sites
tsi_disch_date_df <- unique(comb_rpi_subset[comb_rpi_subset$Site == "MSBI", c("DischDate", "DischDOW")])
tsi_rpi_end <- max(tsi_disch_date_df[tsi_disch_date_df$DischDOW == "Fri", "DischDate"])

epic_disch_date_df <- unique(comb_rpi_subset[comb_rpi_subset$Site != "MSBI", c("DischDate", "DischDOW")])
epic_rpi_end <- max(tsi_disch_date_df[tsi_disch_date_df$DischDOW == "Mon", "DischDate"])

comb_rpi_subset <- comb_rpi_subset[(comb_rpi_subset$Site != "MSBI" & comb_rpi_subset$DischDate <= epic_rpi_end)| (comb_rpi_subset$Site == "MSBI" & comb_rpi_subset$DischDate <= rpi_end), ]
comb_rpi_subset <- comb_rpi_subset[comb_rpi_subset$PostRPI == TRUE, ]

# Create daily list of discharges by site
site_daily_disch_vol <- as.data.frame(comb_rpi_subset %>%
                                        group_by(Site, DischDate, WeekNumber, DischDOW, Weekend, PostRPI) %>%
                                        summarize(TotalDisch = n()))

# Create a list of week dates starting with Sat and ending with Fri
week_num_dates <- as.data.frame(site_daily_disch_vol %>%
                                  group_by(WeekNumber) %>%
                                  summarize(SatDate = min(DischDate), FriDate = SatDate + 6, MonDate = SatDate + 2))

week_num_dates[ , c("SatDate", "FriDate", "MonDate")] <- lapply(week_num_dates[ , c("SatDate", "FriDate", "MonDate")], format, "%m/%d/%y")

week_num_dates$WeekOf <- paste0(week_num_dates$SatDate, "-", week_num_dates$FriDate)
week_num_dates$WeekendOf <- paste0(week_num_dates$SatDate, "-", week_num_dates$MonDate)
week_num_dates <- week_num_dates[ , c("WeekNumber", "SatDate", "WeekOf", "WeekendOf")]

