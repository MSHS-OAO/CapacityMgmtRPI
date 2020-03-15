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
#install.packages("formattable")
#install.packages("ggpubr")
#install.packages("timeDate")

#Analysis for weekend discharge tracking
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(svDialogs)
library(stringr)
library(formattable)
library(scales)
library(ggpubr)
library(timeDate)

# Set working directory and select raw data ----------------------------
getwd()
#setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data")
setwd("J:\\deans\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data")

# User inputs to determine scenario
initial_run <- dlgInput(message = "Is this the initial run of the script (Yes/No)? (During the initial run, baseline data is preprocessed and exported and the Epic historical repository is initiated.)")$res
if (initial_run == "Yes") {
  initial_run <- TRUE
} else if (initial_run == "No") {
  initial_run <- FALSE
} else {
  initial_run <- NULL
}

new_epic_data <- dlgInput(message = "Is there new Epic data to be added to the analysis and repository (Yes/No)? (This typically happens on Tuesdays.)")$res
if (new_epic_data == "Yes") {
  new_epic_data <- TRUE
} else if (new_epic_data == "No") {
  new_epic_data <- FALSE
} else {
  new_epic_data <- NULL
}

# Set two parameters at beginning of script to specify data update scenario
# initial_run: Select TRUE if this is the initial run. 
# During the initial run, the baseline data will be preprocessed and exported for future use and the initial repository of Epic data will be created. The Epic repo will be updated in subsequent runs.
# initial_run <- FALSE 

# new_epic_data: Select TRUE if new week's worth of Epic data has been received and the Epic repository needs to be updated. This will typically happen on Tuesdays. 
# If only TSI data has been updated, then this is set to FALSE
# new_epic_data <- TRUE

# Reference files and constants ----------------------------------------
ref_file <- "Analysis Reference\\Epic and TSI Data Analysis Reference 2020-03-03.xlsx"
epic_site_dict <- read_excel(ref_file, sheet = "EpicSites")
epic_unit_excl <- read_excel(ref_file, sheet = "EpicUnitExclusions")
tsi_site_dict <- read_excel(ref_file, sheet = "TSISites")
tsi_dispo_dict <- read_excel(ref_file, sheet = "TSIDispo")
tsi_dispo_dict[is.na(tsi_dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"
tsi_service_line_excl <- read_excel(ref_file, sheet = "TSIServiceLines")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM")
DischDOW_Order <- c("Sat-Mon", "Tue", "Wed", "Thu", "Fri")

rpi_start <- as.Date("10/26/2019", "%m/%d/%Y")

graphs_tables_output_location <- choose.dir(caption = "Select folder to save graphs and tables", default = "J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Script Graphs and Tables")

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

# Create custom functions to preprocess billing data with input as raw billing dataframe -----------------
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
  
  # Subset only data that meets include criteria
  tsi_raw_df_include <- tsi_raw_df[tsi_raw_df$Include == TRUE, ]
  
  # Further subset desired columns and reorganize these columsn for master dataframes
  tsi_raw_df_subset <- tsi_raw_df_include[ , c("Encounter.No", "Msmrn", "ServiceLine", "Unit.Desc.Msx",
                                                   "AdmitDate", "DischDate", "DischDateTime", "AdmitYr", "AdmitMo",
                                                   "DischYr", "DischMo", "DischHr", "DischDOW",
                                                   "Site", "DispoRollUp", "ServiceLineInclude", "Include", "Week_Num", "Weekend")]
  
  colnames(tsi_raw_df_subset) <- c("EncounterNo", "MRN", "ServiceLine", "DischUnit",
                                     "AdmitDate", "DischDate", "DischDateTime", "AdmitYr", "AdmitMo",
                                     "DischYr", "DischMo", "DischHr", "DischDOW",
                                     "Site", "Disposition", "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")
  
  tsi_raw_df_subset <- tsi_raw_df_subset[ , c("Site", "EncounterNo", "MRN", "AdmitDate", "DischDate", "DischDateTime",
                                                  "DischUnit", "ServiceLine", "Disposition",
                                                  "AdmitYr", "AdmitMo", "DischYr", "DischMo", "DischHr", "DischDOW",
                                                  "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")]
  
  # Create output list with preprocessed data and preprocessed data to be included  --------------------------------
  tsi_output <- list(tsi_raw_df, tsi_raw_df_include, tsi_raw_df_subset)
  return(tsi_output)
}

# Create a function to preprocess and format Epic data with the input as the raw Epic dataframe -----------------
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
  epic_raw_df$Include <- ifelse(epic_raw_df$DISPOSITION == "Expired" | epic_raw_df$IncludeUnit == "No" | is.na(epic_raw_df$Site), FALSE, TRUE)
  
  # Determine elapsed week number since 1/1/19 and specify whether discharge date is a weekend date
  epic_raw_df$Week_Num <- weeknum(epic_raw_df$DischDate)
  epic_raw_df$Weekend <- ifelse(epic_raw_df$DischDOW == "Sat" | epic_raw_df$DischDOW == "Sun" | epic_raw_df$DischDOW == "Mon", TRUE, FALSE)
  
  # Subset only data that meets include criteria
  epic_raw_df_include <- epic_raw_df[epic_raw_df$Include == TRUE, ]
  
  # Further subset desired columns and reorganize these columsn for master dataframes
  epic_raw_df_subset <- epic_raw_df_include[ , c("VISIT ID", "MRN", "DISCHARGE UNIT", "HOSP DISCHARGE TIME",
                                                   "DISPOSITION", "DBS", "AdmitDate", "DischDate", "AdmitYr", "AdmitMo",
                                                   "DischYr", "DischMo", "DischHr", "DischDOW",
                                                   "Site", "IncludeUnit", "Include", "Week_Num", "Weekend")]
  
  colnames(epic_raw_df_subset) <- c("EncounterNo", "MRN", "DischUnit", "DischDateTime",
                                     "Disposition", "ServiceLine", "AdmitDate", "DischDate", "AdmitYr", "AdmitMo",
                                     "DischYr", "DischMo", "DischHr", "DischDOW",
                                     "Site", "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")
  
  epic_raw_df_subset <- epic_raw_df_subset[ , c("Site", "EncounterNo", "MRN", "AdmitDate", "DischDate", "DischDateTime",
                                                  "DischUnit", "ServiceLine", "Disposition",
                                                  "AdmitYr", "AdmitMo", "DischYr", "DischMo", "DischHr", "DischDOW",
                                                  "IncludeUnitOrServiceLine", "Include", "WeekNumber", "Weekend")]
  
  # Create output list with preprocessed data and preprocessed data to be included  --------------------------------
  epic_output <- list(epic_raw_df, epic_raw_df_include, epic_raw_df_subset)
  return(epic_output)
}


if (initial_run == TRUE) {
  # Preprocess and export baseline data and updated weekly datasets -----------------------------
  # Import TSI and Epic data for baseline timeframe
  tsi_raw_baseline <- read.csv("Discharge Billing Data\\Crosstab_Discharges_YTD_Test 2019-11-22.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
  epic_raw_jansep19 <- read_excel("Epic Daily Discharge Timing Reports\\Updated Reports\\Epic Discharge Timings Report JanSep19 2020-01-28.xlsx", col_names = TRUE, na = c("", "NA"))

  # Preprocess raw data using custom functions
  tsi_baseline_output <- preprocess_tsi(tsi_raw_baseline)
  tsi_baseline_preprocessed <- tsi_baseline_output[[1]]
  tsi_baseline_include <- tsi_baseline_output[[2]]
  tsi_baseline_subset <- tsi_baseline_output[[3]]
  
  # Subset dataframes with relevant columns
  tsi_jansep19_subset <- tsi_baseline_subset[tsi_baseline_subset$Site == "MSBI" & tsi_baseline_subset$DischMo <= 9, ]

  epic_jansep19_output <- preprocess_epic(epic_raw_jansep19)
  epic_jansep19_preprocessed <- epic_jansep19_output[[1]]
  epic_jansep19_include <- epic_jansep19_output[[2]]
  epic_jansep19_subset <- epic_jansep19_output[[3]]

  # Combine data from both reporting systems for baseline period into one dataframe
  comb_jansep19_subset <- rbind(tsi_jansep19_subset, epic_jansep19_subset)
  comb_jansep19_subset <- comb_jansep19_subset[!is.na(comb_jansep19_subset$Site), ]

  # Format combined data
  comb_jansep19_subset$Site <- factor(comb_jansep19_subset$Site, levels = site_order, ordered = TRUE)
  comb_jansep19_subset[ , c("DischUnit", "ServiceLine", "Disposition", "IncludeUnitOrServiceLine")] <- lapply(comb_jansep19_subset[ , c("DischUnit", "ServiceLine", "Disposition", "IncludeUnitOrServiceLine")], factor)

  # Aggregate and format baseline data from Jan-Sep 2019 for future use
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

  # Average discharges by DOW and summary stats for each site
  baseline_avg_stats_summary$TargetTotal = round(baseline_avg_stats_summary$WeekendTotal, 0) + round(baseline_avg_stats_summary$TargetDelta, 0)


  baseline_avg_stats_targets_print <- format(baseline_avg_stats_summary, digits = 0)

  hosp_baseline_target <- baseline_avg_stats_summary[ , c("Site", "WeekendTotal", "TargetDelta", "TargetTotal")]
  colnames(hosp_baseline_target) <- c("Site", "Weekend Baseline", "Target Change", "Weekend Target")

  # Export baseline data for future use
  baseline_list_names <- c("Baseline_EnctrLevel_Incl",
                              "Baseline_Daily_Disch_Unit_Dispo", 
                              "Baseline_Total_Daily_Disch",
                              "Baseline_Total_Avg_Disch_DOW",
                              "Baseline_Avg_Disch_DOW",
                              "Baseline_Avg_Disch_Target_Summ",
                              "Site_Baseline_Targets")
  baseline_list <- list("Baseline_EnctrLevel_Incl" = comb_jansep19_subset,
                           "Baseline_Daily_Disch_Unit_Dispo" = baseline_site_dischunit_dispo_daily,
                           "Baseline_Total_Daily_Disch" = baseline_site_disch_daily,
                           "Baseline_Total_Avg_Disch_DOW" = baseline_site_total_avg_disch_dow,
                           "Baseline_Avg_Disch_DOW" = baseline_avg_disch_dow,
                           "Baseline_Avg_Disch_Target_Summ" = baseline_avg_stats_targets_print,
                           "Site_Baseline_Targets" = hosp_baseline_target)
  for (i in 1:length(baseline_list_names)) {
    assign(baseline_list_names[i], baseline_list[[i]])
  }
  
  # Export baseline outputs to Excel to be imported for future runs
  write_xlsx(baseline_list, path = paste0('Script Data Outputs\\Baseline Data Jan-Sep 2019 Outputs ', Sys.Date(), '.xlsx'))
  
  # Preprocess historical Epic data and export to begin creating historical repository to be used for future runs -----------------------------
  # Import and bind monthly Epic data files
  epic_raw_octdec19 <- read_excel("Epic Daily Discharge Timing Reports\\Updated Reports\\Epic Discharge Timings Report OctDec19 2020-01-28.xlsx", col_names = TRUE, na = c("", "NA"))
  epic_raw_part_jan20 <- read_excel("Epic Daily Discharge Timing Reports\\Updated Reports\\Epic Discharge Timings Report 01012020 to 01202020 2020-01-27.xls", col_names = TRUE, na = c("", "NA"))
  epic_raw_updates <- rbind(epic_raw_octdec19, epic_raw_part_jan20)
  
  # Preprocess Epic data
  epic_updates_output <- preprocess_epic(epic_raw_updates)
  epic_updates_preprocessed <- epic_updates_output[[1]]
  epic_updates_include <- epic_updates_output[[2]]
  epic_updates_subset <- epic_updates_output[[3]]
  
  # Determine whether discharge occurred after RPI cycles began
  epic_updates_subset$PostRPI <- epic_updates_subset$DischDate >= rpi_start
  epic_updates_subset <- epic_updates_subset[epic_updates_subset$PostRPI == TRUE, ]
  
  # Determine RPI end date for TSI sites
  epic_disch_date_df <- as.data.frame(unique(epic_updates_subset[ , c("DischDate", "DischDOW")]))
  epic_rpi_end <- max(epic_disch_date_df[epic_disch_date_df$DischDOW == "Mon", "DischDate"])
  
  epic_updates_subset <- epic_updates_subset[epic_updates_subset$DischDate >= rpi_start &  epic_updates_subset$DischDate <= epic_rpi_end, ]
  
  # Summarize data based on site, discharge unit, disposition, and service line
  epic_site_dischunit_dispo_daily <- as.data.frame(epic_updates_subset %>%
                                                     group_by(Site, DischDate, DischUnit, Disposition, DischYr, DischMo, DischDOW, WeekNumber, Weekend) %>%
                                                     summarize(TotalDisch = n()))
  
  # Export historical data to Excel to begin creating repository
  write_xlsx(epic_site_dischunit_dispo_daily, path = paste0('Script Data Outputs\\Epic Repo\\Disch Repo ', rpi_start, ' to ', epic_rpi_end, ' Updated ', Sys.Date(), '.xlsx'))
  
  # Resave summarized data as historical repository
  epic_hist_repo <- epic_site_dischunit_dispo_daily
  
} else {
  # Import preprocessed baseline data ------------------------------------------
  baseline_data_file <- choose.files(default = paste0(getwd(), "/Script Data Outputs/*.*"), caption = "Select Excel file with preprocessed data for baseline period")
  sheet_names <- excel_sheets(baseline_data_file)
  baseline_list <- lapply(sheet_names, function(x) read_excel(baseline_data_file, sheet = x))
  names(baseline_list) <- sheet_names
  
  for (i in 1:length(sheet_names)) {
    assign(sheet_names[i], baseline_list[[i]])
  }
  
  # Import Epic historical repository ------------------------------------------
  getwd()
  epic_hist_repo <- read_excel(choose.files(default = paste0(getwd(), "/Script Data Outputs/Epic Repo/*.*"), caption = "Select Excel file with Epic historical repository"), col_names = TRUE, na = c("", "NA"))

}

if (new_epic_data == TRUE) {
  # Preprocess weekly report and bind with historical data for future runs ------------------------------------
  # Import this week's Epic report
  epic_raw_updates <- read_excel(choose.files(default = paste0(getwd(), "/Epic Daily Discharge Timing Reports/Updated Reports/*.*"), caption = "Select Epic report with discharges for past week"), col_names = TRUE, na = c("", "NA"))
  
  # Preprocess this week's Epic report
  epic_updates_output <- preprocess_epic(epic_raw_updates)
  epic_updates_preprocessed <- epic_updates_output[[1]]
  epic_updates_include <- epic_updates_output[[2]]
  epic_updates_subset <- epic_updates_output[[3]]
  
  # Determine whether discharge occurred after RPI cycles began
  epic_updates_subset$PostRPI <- epic_updates_subset$DischDate >= rpi_start
  epic_updates_subset <- epic_updates_subset[epic_updates_subset$PostRPI == TRUE, ]
  
  # Determine RPI end date for TSI sites
  epic_disch_date_df <- as.data.frame(unique(epic_updates_subset[ , c("DischDate", "DischDOW")]))
  epic_rpi_end <- max(epic_disch_date_df[epic_disch_date_df$DischDOW == "Mon", "DischDate"])
  
  epic_updates_subset <- epic_updates_subset[epic_updates_subset$DischDate >= rpi_start &  epic_updates_subset$DischDate <= epic_rpi_end, ]
  
  
  # Summarize data based on site, discharge unit, disposition, and service line

  epic_site_dischunit_dispo_daily <- as.data.frame(epic_updates_subset %>%
                                                     group_by(Site, DischDate, DischUnit, Disposition, DischYr, DischMo, DischDOW, WeekNumber, Weekend) %>%
                                                     summarize(TotalDisch = n()))
  
  # Bind historical repository and this week's update
  epic_site_dischunit_dispo_daily <- rbind(epic_hist_repo, epic_site_dischunit_dispo_daily)
  # Ensure there are no duplicate entries
  epic_site_dischunit_dispo_daily <- unique(epic_site_dischunit_dispo_daily)
  
  # Update historical repository with most recent data
  write_xlsx(epic_site_dischunit_dispo_daily, path = paste0('Script Data Outputs\\Epic Repo\\Disch Repo ', rpi_start, ' to ', epic_rpi_end, ' Updated ', Sys.Date(), '.xlsx'))
  
} else {
  epic_site_dischunit_dispo_daily <- epic_hist_repo
}

# Import TSI data from after baseline period ------------------------------------------------
# Data used to establish baseline and targets remains constant
tsi_raw_octdec19 <- read.csv("Discharge Billing Data\\MSBI Data Pulls\\MSBI Discharges Oct-Dec2019 2020-01-22.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
tsi_raw_ytd20 <- read.csv(choose.files(caption = "Select 2020 YTD TSI report for MSBI"), header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

tsi_raw_updates <- rbind(tsi_raw_octdec19, tsi_raw_ytd20)

# Preprocess TSI updated raw data using custom functions --------------------------------------------
tsi_updates_output <- preprocess_tsi(tsi_raw_updates)
tsi_updates_preprocessed <- tsi_updates_output[[1]]
tsi_updates_include <- tsi_updates_output[[2]]
tsi_updates_subset <- tsi_updates_output[[3]]

# Subset dataframes with relevant columns
tsi_updates_subset <- tsi_updates_subset[tsi_updates_subset$Site == "MSBI", ]

# Determine whether discharge occurred after RPI cycles began
tsi_updates_subset$PostRPI <- tsi_updates_subset$DischDate >= rpi_start
tsi_updates_subset <- tsi_updates_subset[tsi_updates_subset$PostRPI == TRUE, ]

# Determine RPI end date for TSI sites
tsi_disch_date_df <- unique(tsi_updates_subset[ , c("DischDate", "DischDOW")])
tsi_rpi_end <- max(tsi_disch_date_df[tsi_disch_date_df$DischDOW == "Fri", "DischDate"])

tsi_updates_subset <- tsi_updates_subset[tsi_updates_subset$DischDate >= rpi_start &  tsi_updates_subset$DischDate <= tsi_rpi_end, ]

# Summarize data based on site, discharge unit, disposition, and service line
tsi_site_dischunit_dispo_daily <- as.data.frame(tsi_updates_subset %>%
                                                            group_by(Site, DischDate, DischUnit, Disposition, DischYr, DischMo, DischDOW, WeekNumber, Weekend) %>%
                                                            summarize(TotalDisch = n()))

# Combine historical and most recent data from both data systems
comb_site_dischunit_dispo_daily <- rbind(tsi_site_dischunit_dispo_daily, epic_site_dischunit_dispo_daily)

# Format combined data
comb_site_dischunit_dispo_daily$Site <- factor(comb_site_dischunit_dispo_daily$Site, levels = site_order, ordered = TRUE)


# Create daily list of discharges by site
site_daily_disch_vol <- as.data.frame(comb_site_dischunit_dispo_daily %>%
                                        group_by(Site, DischDate, WeekNumber, DischDOW, Weekend) %>%
                                        summarize(TotalDisch = sum(TotalDisch)))

# Create a list of week dates starting with Sat and ending with Fri
week_num_dates <- as.data.frame(site_daily_disch_vol %>%
                                  group_by(WeekNumber) %>%
                                  summarize(SatDate = min(DischDate), FriDate = SatDate + 6, MonDate = SatDate + 2))

week_num_dates[ , c("SatDate", "FriDate", "MonDate")] <- lapply(week_num_dates[ , c("SatDate", "FriDate", "MonDate")], format, "%m/%d/%y")

week_num_dates$WeekOf <- paste0(week_num_dates$SatDate, "-", week_num_dates$FriDate)
week_num_dates$WeekendOf <- paste0(week_num_dates$SatDate, "-", week_num_dates$MonDate)
week_num_dates <- week_num_dates[ , c("WeekNumber", "SatDate", "WeekOf", "WeekendOf")]
week_num_dates[ , c("SatDate", "WeekOf", "WeekendOf")] <- lapply(week_num_dates[ , c("SatDate", "WeekOf", "WeekendOf")], function(x) factor(x, levels = unique(x)))

site_daily_disch_vol <- left_join(site_daily_disch_vol, week_num_dates, by = c("WeekNumber" = "WeekNumber"))

wkday_daily_disch_vol <- site_daily_disch_vol[site_daily_disch_vol$Weekend != TRUE, ]
wkend_daily_disch_vol <- site_daily_disch_vol[site_daily_disch_vol$Weekend == TRUE, ]

wkend_summary_disch_vol <- as.data.frame(wkend_daily_disch_vol %>%
                                           group_by(Site, WeekNumber, Weekend, SatDate, WeekOf, WeekendOf) %>%
                                           summarize(DischDOW = "Sat-Mon", DischDate = min(DischDate), AvgDisch = mean(TotalDisch), TotalDisch = sum(TotalDisch)))

wkday_summary_disch_vol <- as.data.frame(wkday_daily_disch_vol %>%
                                           group_by(Site, WeekNumber, Weekend, SatDate, WeekOf, WeekendOf) %>%
                                           summarize(DischDOW = "Tue-Fri", DischDate = min(DischDate), AvgDisch = mean(TotalDisch), TotalDisch = sum(TotalDisch)))

# Create data frame summarizing weekend and weekday average and total discharges
wkend_wkday_summary_disch_vol <- rbind(wkend_summary_disch_vol, wkday_summary_disch_vol)
wkend_wkday_summary_disch_vol <- wkend_wkday_summary_disch_vol[order(wkend_wkday_summary_disch_vol$Site, wkend_wkday_summary_disch_vol$WeekNumber), ]
rownames(wkend_wkday_summary_disch_vol) <- 1:nrow(wkend_wkday_summary_disch_vol)

wkend_comb_disch_vol <- wkend_summary_disch_vol[ , c("Site", "WeekNumber", "SatDate", "WeekOf", "WeekendOf", "DischDate", "DischDOW", "Weekend", "TotalDisch")]
wkday_daily_disch_vol <- wkday_daily_disch_vol[ , c("Site", "WeekNumber", "SatDate", "WeekOf", "WeekendOf", "DischDate", "DischDOW", "Weekend", "TotalDisch")]

site_summary_daily_disch_vol <- rbind(wkend_comb_disch_vol, wkday_daily_disch_vol)
site_summary_daily_disch_vol <- site_summary_daily_disch_vol[order(site_summary_daily_disch_vol$Site, site_summary_daily_disch_vol$WeekNumber, site_summary_daily_disch_vol$DischDate), ]
site_summary_daily_disch_vol$DischDOW <- factor(site_summary_daily_disch_vol$DischDOW, levels = DischDOW_Order)
rownames(site_summary_daily_disch_vol) <- 1:nrow(site_summary_daily_disch_vol)

# Create a table with total weekend discharges for each site
wkend_total_table <- dcast(wkend_comb_disch_vol, Site ~ WeekendOf, value.var = "TotalDisch")
Site_Baseline_Targets$Site <- factor(Site_Baseline_Targets$Site, levels = site_order, ordered = TRUE)
wkend_total_rpi_tracker <- left_join(Site_Baseline_Targets[ , c("Site", "Weekend Baseline", "Weekend Target")], wkend_total_table, by = c("Site" = "Site"))

# Create 2 new tables with original and updated targets
Site_Original_Updated_Targets$Site <- factor(Site_Original_Updated_Targets$Site, levels = site_order, ordered = TRUE)
wkend_total_rpi_tracker_original_targets <- left_join(Site_Original_Updated_Targets[ , c("Site", "Weekend Baseline", "Original Change", "Original Target")], wkend_total_table, by = c("Site" = "Site"))
wkend_total_rpi_tracker_updated_targets <- left_join(Site_Original_Updated_Targets[ , c("Site", "Weekend Baseline", "Updated Change", "Updated Target")], wkend_total_table, by = c("Site" = "Site"))


# Create a table to track weekly status including total discharges, % weekend discharges, avg weekday discharges, avg weekend discharges
weekly_totals <- as.data.frame(site_summary_daily_disch_vol %>%
                                 group_by(Site, WeekNumber, SatDate, WeekOf, WeekendOf) %>%
                                 summarize(WkendTotal = sum(TotalDisch[Weekend == TRUE]), WklyTotal = sum(TotalDisch), WkendPercent = WkendTotal/WklyTotal, WkendAvg = TotalDisch[Weekend == TRUE]/3, WkdayAvg = mean(TotalDisch[Weekend != TRUE])))

weekly_totals$WkendPercent[weekly_totals$WkendPercent == 1] <- NA
weekly_totals$WkdayAvg[!is.finite(weekly_totals$WkdayAvg)] <- NA

##adding a holiday check column
with_holidays <- weekly_totals

#years <- 2020
NYSE_Holidays <- c(holidayNYSE(2019:2020))
#nofriday <- NYSE_Holidays()
MSHS_Holidays <- NYSE_Holidays[c(1,2,3,5,6,7,8,9)] #exclude Good Friday index 4
MSHS_Holidays <- as.Date(MSHS_Holidays)
#with_holidays_SatFri <- site_daily_disch_vol <- left_join(with_holidays, indiv_dates, by = c("WeekNumber" = "WeekNumber"))

with_holidays_SatFri$SatDate <- as.Date(with_holidays$SatDate,format = "%m/%d/%y")

holidays <- NULL
startyear <- 2015
year <- c(2016, 2017, 2018, 2019, 2020)
# Iterate years:
for (y in year) {
  if (y >= startyear)
    holidays <- c(holidays, as.character(USNewYearsDay(y)))
  if (y >= startyear)
    holidays <- c(holidays, as.character(USMLKingsBirthday(y)))
  if (y > startyear)
    holidays <- c(holidays, as.character(USPresidentsDay(y)))
  if (y >= startyear)
    holidays <- c(holidays, as.character(USMemorialDay(y)))
  if (y >= startyear)
    holidays <- c(holidays, as.character(USIndependenceDay(y)))
  if (y >= startyear)
    holidays <- c(holidays, as.character(USLaborDay(y)))
  if (y >= startyear)
    holidays <- c(holidays, as.character(USThanksgivingDay(y)))
  if (y >= startyear)
    holidays <- c(holidays, as.character(USChristmasDay(y)))
}



#dates <- ymd(c(MSHS_Holidays))
#MSHS_Week <- interval(ymd(with_holidays_SatFri$SatDate), ymd(with_holidays_SatFri$FriDate))


MSHS_Sats <- floor_date(as.Date(holidays), "week") - 1

with_holidays$check_holiday <- ifelse(as.Date(with_holidays$SatDate,format = "%m/%d/%y") %in% MSHS_Sats, "Holiday Week", "Not Holiday Week")


site_weekly_stats_tracker <- function(site) {
  weekly_totals_format2 <- weekly_totals[weekly_totals$Site == site, ]
  weekly_totals_format2$WklyTotal <- as.integer(weekly_totals_format2$WklyTotal)
  weekly_totals_format2$WkendPercent <- percent(weekly_totals_format2$WkendPercent, digits = 0)
  weekly_totals_format2[ , c("WkdayAvg", "WkendAvg")] <- round(weekly_totals_format2[ , c("WkdayAvg", "WkendAvg")], digits = 0)
  weekly_totals_format2[ , c("WklyTotal", "WkendPercent", "WkdayAvg", "WkendAvg")] <- lapply(weekly_totals_format2[ , c("WklyTotal", "WkendPercent", "WkdayAvg", "WkendAvg")], 
                                                                                             function(x) ifelse(!is.na(x), as.character(x), x))
  weekly_stats_tracker <- melt(weekly_totals_format2, id.vars = c("Site", "WeekOf"), measure.vars = c("WklyTotal", "WkendPercent", "WkdayAvg", "WkendAvg"))
  weekly_stats_tracker <- dcast(weekly_stats_tracker, Site + variable ~ WeekOf, value.var = "value")
  
  colnames(weekly_stats_tracker)[2] <- "Metric"
  
  weekly_stats_tracker$Metric <- c("Total Weekly Disch", "Weekend Disch as % of Total Weekly Disch", "Avg Daily Disch: Weekday", "Avg Daily Disch: Weekend")
  return(weekly_stats_tracker)
}
                             
msh_weekly_stats_table <- site_weekly_stats_tracker("MSH")
msq_weekly_stats_table <- site_weekly_stats_tracker("MSQ")
msbi_weekly_stats_table <- site_weekly_stats_tracker("MSBI")
msb_weekly_stats_table <- site_weekly_stats_tracker("MSB")
msw_weekly_stats_table <- site_weekly_stats_tracker("MSW")
msm_weekly_stats_table <- site_weekly_stats_tracker("MSM")


# Create list of tables to export to Excel
export_list <- list("Wkend Summary Orig Targets" = wkend_total_rpi_tracker_original_targets,
                    "Wkend Summary New Targets" = wkend_total_rpi_tracker_updated_targets,
                    # "WeekendSummary" = wkend_total_rpi_tracker, 
                    "MSH Weekly Stat" = msh_weekly_stats_table,
                    "MSQ Weekly Stat" = msq_weekly_stats_table,
                    "MSBI Weekly Stat" = msbi_weekly_stats_table,
                    "MSB Weekly Stat" = msb_weekly_stats_table,
                    "MSW Weekly Stat" = msw_weekly_stats_table,
                    "MSM Weekly Stat" = msm_weekly_stats_table)

write_xlsx(export_list, path = paste0(graphs_tables_output_location, "\\Weekly Discharge Stats Summary ", Sys.Date(), ".xlsx"))

# Plot discharge trends by day of week for each site ---------------------------------------------------------
sinai_colors <- c("#221f72", "#00AEEF", "#D80B8C", "#B2B3B2", "#C7C6EF", "#DDDEDD", "#FCC9E9")

stacked_bar <- function(site) {
  bar_lookback <- 8 # Use 8 week lookback for stacked bar graph
  bar_first_week <- max(site_summary_daily_disch_vol$WeekNumber[site_summary_daily_disch_vol$Site == site]) - bar_lookback + 1
  ggplot(data = site_summary_daily_disch_vol[site_summary_daily_disch_vol$Site == site & site_summary_daily_disch_vol$WeekNumber >= bar_first_week, ]) +
    geom_col(mapping = aes(x = SatDate, y = TotalDisch, fill = DischDOW), 
             position = position_stack(reverse = TRUE)) +
    labs(title = paste(site, "Discharges by DOW: 8 Week Lookback"), x = "Week Of", y = "Discharge Volume") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1)) +
    guides(fill = guide_legend(reverse = FALSE, title = "Day of Week")) +
    geom_text(aes(x = SatDate, y = TotalDisch, label = TotalDisch), color = "white", position = position_stack(vjust = 0.5)) +
    geom_text(data = weekly_totals[weekly_totals$Site == site & weekly_totals$WeekNumber >= bar_first_week, ], aes(x = SatDate, y = WklyTotal, label = WklyTotal), color = "black", vjust = -0.5) +
    scale_fill_manual(values = sinai_colors) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0))
}

stacked_bar("MSH")
stacked_bar("MSQ")
stacked_bar("MSBI")
stacked_bar("MSB")
stacked_bar("MSW")
stacked_bar("MSM")

msh_stacked_bar_dow <- stacked_bar("MSH")
msq_stacked_bar_dow <- stacked_bar("MSQ")
msbi_stacked_bar_dow <- stacked_bar("MSBI")
msb_stacked_bar_dow <- stacked_bar("MSB")
msw_stacked_bar_dow <- stacked_bar("MSW")
msm_stacked_bar_dow <- stacked_bar("MSM")

ggsave(path = graphs_tables_output_location, file = paste("MSH Stacked Bar DOW 3", Sys.Date(), ".png"), plot = msh_stacked_bar_dow, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSQ Stacked Bar DOW", Sys.Date(), ".png"), plot = msq_stacked_bar_dow, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSBI Stacked Bar DOW", Sys.Date(), ".png"), plot = msbi_stacked_bar_dow, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSB Stacked Bar DOW", Sys.Date(), ".png"), plot = msb_stacked_bar_dow, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSW Stacked Bar DOW", Sys.Date(), ".png"), plot = msw_stacked_bar_dow, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSM Stacked Bar DOW", Sys.Date(), ".png"), plot = msm_stacked_bar_dow, device = "png", width = 4.8, height = 4.2, units = "in")

# Plot weekend discharges over time for last 12 weeks ---------------------
# weekend_trend <- function(site) {
#   trends_lookback <- 12
#   trends_first_week <- max(wkend_comb_disch_vol$WeekNumber[wkend_comb_disch_vol$Site == site], na.rm = TRUE) - trends_lookback + 1
#   ggplot(data = wkend_comb_disch_vol[(wkend_comb_disch_vol$Site == site) & (wkend_comb_disch_vol$WeekNumber >= trends_first_week), ]) +
#     
#     geom_hline(aes(yintercept = Site_Baseline_Targets$'Weekend Baseline'[Site_Baseline_Targets$Site == site], color = "Baseline", linetype = "Baseline")) +
#     geom_text(aes(length(SatDate), Site_Baseline_Targets$'Weekend Baseline'[Site_Baseline_Targets$Site == site], label = Site_Baseline_Targets$'Weekend Baseline'[Site_Baseline_Targets$Site == site]), vjust = -0.5, hjust = -0.5) +
#     
#     geom_hline(aes(yintercept = Site_Baseline_Targets$'Weekend Target'[Site_Baseline_Targets$Site == site], color = "Target", linetype = "Target")) +
#     geom_text(aes(length(SatDate), Site_Baseline_Targets$'Weekend Target'[Site_Baseline_Targets$Site == site], label = Site_Baseline_Targets$'Weekend Target'[Site_Baseline_Targets$Site == site]),  vjust = -0.5, hjust = -0.5) +
#     
#     geom_line(mapping = aes(x = SatDate, y = TotalDisch, group = 1, color = "Actual", linetype = "Actual"), size = 1) + 
#     geom_point(mapping = aes(x = SatDate, y = TotalDisch), color = "#00AEEF", size = 1.5) +
#     geom_text(mapping = aes(x = SatDate, y = TotalDisch, label = TotalDisch), color = "black", vjust = -0.25, hjust = -0.25) +
#     
#     labs(title = paste(site, "Weekend Discharges: 12 Week Lookback"), x = "Week Of", y = "Discharge Volume") +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1)) +
#     
#     # scale_x_discrete(expand = c(0, 0, 0.05, 0.5)) +
#     # scale_y_continuous(expand = c(0, 5, 0.2, 0.5)) +
#     scale_x_discrete(expand = c(0, 0, 0, 1.2)) +
#     scale_y_continuous(expand = c(0.2, 0, 0.2, 0)) +
#     
#     
#     scale_linetype_manual(name = "", values = c("Baseline" = "dashed",
#                                                 "Target" = "solid",
#                                                 "Actual" = "solid")) +
#     
#     scale_color_manual(name = "", values = c("Baseline" = "#8c8c8c",
#                                              "Target" = "black",
#                                              "Actual" = "#00AEEF"))
# }

# weekend_trend(site = "MSH")
# weekend_trend(site = "MSQ")
# weekend_trend(site = "MSBI")
# weekend_trend(site = "MSB")
# weekend_trend(site = "MSW")
# weekend_trend(site = "MSM")

new_df <- wkend_comb_disch_vol

new_df$check_holiday <- ifelse(as.Date(new_df$SatDate,format = "%m/%d/%y") %in% MSHS_Sats, "Holiday Week", "Not a Holiday Week")


weekend_trend_old_new_targets <- function(site) {
  trends_lookback <- 12
  trends_first_week <- max(new_df$WeekNumber[wkend_comb_disch_vol$Site == site], na.rm = TRUE) - trends_lookback + 1
  ggplot(data =new_df[(new_df$Site == site) & (new_df$WeekNumber >= trends_first_week), ]) +
    
    geom_hline(aes(yintercept = Site_Original_Updated_Targets$'Weekend Baseline'[Site_Original_Updated_Targets$Site == site], color = "Baseline", linetype = "Baseline")) +
    geom_text(aes(length(SatDate), Site_Original_Updated_Targets$'Weekend Baseline'[Site_Original_Updated_Targets$Site == site], label = Site_Original_Updated_Targets$'Weekend Baseline'[Site_Original_Updated_Targets$Site == site]), vjust = -0.5, hjust = -0.5) +
    ## Only plot new targets
    # geom_hline(aes(yintercept = Site_Original_Updated_Targets$'Original Target'[Site_Original_Updated_Targets$Site == site], color = "Original Target", linetype = "Original Target")) +
    # geom_text(aes(length(SatDate), Site_Original_Updated_Targets$'Original Target'[Site_Original_Updated_Targets$Site == site], label = Site_Original_Updated_Targets$'Original Target'[Site_Original_Updated_Targets$Site == site]),  vjust = -0.5, hjust = -0.5) +
    # 
    geom_hline(aes(yintercept = Site_Original_Updated_Targets$'Updated Target'[Site_Original_Updated_Targets$Site == site], color = "Updated Target", linetype = "Updated Target")) +
    geom_text(aes(length(SatDate), Site_Original_Updated_Targets$'Updated Target'[Site_Original_Updated_Targets$Site == site], label = Site_Original_Updated_Targets$'Updated Target'[Site_Original_Updated_Targets$Site == site]),  vjust = -0.5, hjust = -0.5) +
    
    geom_line(mapping = aes(x = SatDate, y = TotalDisch, group = 1, color = "Actual", linetype = "Actual"), size = 1) + 
    geom_point(mapping = aes(x = SatDate, y = TotalDisch, fill = factor(check_holiday)), shape = 21, size = 1.5) +
    geom_text(mapping = aes(x = SatDate, y = TotalDisch, label = TotalDisch), color = "black", vjust = -0.25, hjust = -0.25) +
    
    labs(title = paste(site, "Weekend Discharges:", trends_lookback, "Week Lookback"), x = "Week Of", y = "Discharge Volume") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.justification = "left", legend.key.width = unit(.375,"inch"), axis.text.x = element_text(angle = 30, hjust = 1)) +
    
    # scale_x_discrete(expand = c(0, 0, 0.05, 0.5)) +
    # scale_y_continuous(expand = c(0, 5, 0.2, 0.5)) +
    scale_x_discrete(expand = c(0, 0, 0, 1.2)) +
    scale_y_continuous(expand = c(0.2, 0, 0.2, 0)) +
    
    
    scale_linetype_manual(name = "", values = c("Baseline" = "dashed",
                                                # "Original Target" = "dashed",
                                                "Updated Target" = "solid", 
                                                "Actual" = "solid"),
                          # labels = c("Actual", "Baseline", "Old Target", "New Target")) +
                          labels = c("Actual", "Baseline", "Target")) +
    
    #scale_fill_manual(values=c("red","green"))+
    scale_color_manual(name = "", values = c("Baseline" = "#8c8c8c",
                                             # "Original Target" = "black",
                                             "Updated Target" = "black",
                                             "Actual" = "#00AEEF"),
                       # labels = c("Actual", "Baseline", "Old Target", "New Target")) +
                       labels = c("Actual", "Baseline", "Target", "Holiday Week", "Not a Holiday Week"))
  
  
}

weekend_trend_old_new_targets(site = "MSH")
weekend_trend_old_new_targets(site = "MSQ")
weekend_trend_old_new_targets(site = "MSBI")
weekend_trend_old_new_targets(site = "MSB")
weekend_trend_old_new_targets(site = "MSW")
weekend_trend_old_new_targets(site = "MSM")

msh_graph_wkendtrend <- weekend_trend_old_new_targets("MSH")
msq_graph_wkendtrend <- weekend_trend_old_new_targets("MSQ")
msbi_graph_wkendtrend <- weekend_trend_old_new_targets("MSBI")
msb_graph_wkendtrend <- weekend_trend_old_new_targets("MSB")
msw_graph_wkendtrend <- weekend_trend_old_new_targets("MSW")
msm_graph_wkendtrend <- weekend_trend_old_new_targets("MSM")

ggsave(path = graphs_tables_output_location, file = paste("MSH Weekend Discharge Trends", Sys.Date(), ".png"), plot = msh_graph_wkendtrend, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSQ Weekend Discharge Trends", Sys.Date(), ".png"), plot = msq_graph_wkendtrend, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSBI Weekend Discharge Trends", Sys.Date(), ".png"), plot = msbi_graph_wkendtrend, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSB Weekend Discharge Trends", Sys.Date(), ".png"), plot = msb_graph_wkendtrend, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSW Weekend Discharge Trends", Sys.Date(), ".png"), plot = msw_graph_wkendtrend, device = "png", width = 4.8, height = 4.2, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSM Weekend Discharge Trends", Sys.Date(), ".png"), plot = msm_graph_wkendtrend, device = "png", width = 4.8, height = 4.2, units = "in")

# Plot weekend discharges as percent of total weekly discharges over time -------------------------------

weekend_percent_trends <- function(site) {
  trends_lookback <- 16
  trends_first_week <- max(weekly_totals$WeekNumber[weekly_totals$Site == site & !is.na(weekly_totals$WkendPercent)], na.rm = TRUE) - trends_lookback + 1
  ggplot(data = weekly_totals[(weekly_totals$Site == site) & (weekly_totals$WeekNumber >= trends_first_week) & !is.na(weekly_totals$WkendPercent), ]) +
  geom_line(mapping = aes(x = SatDate, y = WkendPercent, group = 1, color = "Actual", linetype = "Actual"), size = 1) + 
  geom_point(mapping = aes(x = SatDate, y = WkendPercent, color = "Actual"), size = 2) +
    
  labs(title = paste(site, "Weekend Discharges as Percent of\n Total Weekly Discharges:", trends_lookback, "Week Lookback"), x = "Week Of", y = "Percent of Discharges") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) +
    
  scale_x_discrete(expand = c(0, 0.5, 0, .5)) +
  scale_y_continuous(expand = c(0.2, 0, 0.2, 0), labels = percent_format(accuracy = 1)) +
  scale_linetype_manual(name = "", values = c("Actual" = "solid"), labels = c("Actual")) +
  scale_color_manual(name = "", values = c("Actual" = "#00AEEF"), labels = c("Actual"))
  
}

weekend_percent_trends("MSH")
weekend_percent_trends("MSQ")
weekend_percent_trends("MSBI")
weekend_percent_trends("MSB")
weekend_percent_trends("MSW")
weekend_percent_trends("MSM")

msh_graph_wkend_percent_trend <- weekend_percent_trends("MSH")
msq_graph_wkend_percent_trend <- weekend_percent_trends("MSQ")
msbi_graph_wkend_percent_trend <- weekend_percent_trends("MSBI")
msb_graph_wkend_percent_trend <- weekend_percent_trends("MSB")
msw_graph_wkend_percent_trend <- weekend_percent_trends("MSW")
msm_graph_wkend_percent_trend <- weekend_percent_trends("MSM")


ggsave(path = graphs_tables_output_location, file = paste("MSH Weekend Percent Discharge Trends", Sys.Date(), ".png"), plot = msh_graph_wkend_percent_trend, device = "png", width = 5.8, height = 4.8, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSQ Weekend Percent Discharge Trends", Sys.Date(), ".png"), plot = msq_graph_wkend_percent_trend, device = "png", width = 5.8, height = 4.8, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSBI Weekend Percent Discharge Trends", Sys.Date(), ".png"), plot = msbi_graph_wkend_percent_trend, device = "png", width = 5.8, height = 4.8, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSB Weekend Percent Discharge Trends", Sys.Date(), ".png"), plot = msb_graph_wkend_percent_trend, device = "png", width = 5.8, height = 4.8, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSW Weekend Percent Discharge Trends", Sys.Date(), ".png"), plot = msw_graph_wkend_percent_trend, device = "png", width = 5.8, height = 4.8, units = "in")
ggsave(path = graphs_tables_output_location, file = paste("MSM Weekend Percent Discharge Trends", Sys.Date(), ".png"), plot = msm_graph_wkend_percent_trend, device = "png", width = 5.8, height = 4.8, units = "in")

# Plot weekend discharges as a percent of total weekly discharges over time for all sites on one graph -----------------------------

mshs_colors <- c("#221F72", "#00AEEF", "#D80B8C", "#B2B3B2", "#C7C6EF", "#FCC9E9")

trends_lookback <- 12
trends_first_week <- max(weekly_totals$WeekNumber[!is.na(weekly_totals$WkendPercent)], na.rm = TRUE) - trends_lookback + 1
mshs_wkend_percent <- ggplot(data = weekly_totals[(weekly_totals$WeekNumber >= trends_first_week) & !is.na(weekly_totals$WkendPercent), ]) +
  geom_line(mapping = aes(x = SatDate, y = WkendPercent, group = Site, color = Site, linetype = Site), size = 1) + 
  geom_point(mapping = aes(x = SatDate, y = WkendPercent, color = Site), size = 2) +
  
  labs(title = paste("MSHS Weekend Discharges as Percent of\n Total Weekly Discharges:", trends_lookback, "Week Lookback"), x = "Week Of", y = "Percent of Discharges") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  scale_x_discrete(expand = c(0, 0.5, 0, .5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.15, 0.45), breaks = seq(0.15, 0.45, 0.1)) +
  scale_color_manual(name = "Site", values = mshs_colors) +
  guides(color = guide_legend(nrow = 1))

ggsave(path = graphs_tables_output_location, file = paste("MSHS Weekend Percent Discharge Trends", Sys.Date(), ".png"), plot = mshs_wkend_percent, device = "png", width = 6, height = 5, units = "in")


# Create a plot with average weekend and weekday daily discharges ---------------------------------
avg_wkend_wkday_daily_disch <- function(site) {
  trends_lookback <- 12
  trends_first_week <- max(weekly_totals$WeekNumber[weekly_totals$Site == site], na.rm = TRUE) - trends_lookback + 1
  ggplot(data = weekly_totals[(weekly_totals$Site == site) & (weekly_totals$WeekNumber >= trends_first_week), ]) +
    geom_line(mapping = aes(x = SatDate, y = WkendAvg, group = 1, color = "Weekend", linetype = "Weekend"), size = 1) + 
    geom_point(mapping = aes(x = SatDate, y = WkendAvg, color = "Weekend"), size = 2) +
    
    geom_line(mapping = aes(x = SatDate, y = WkdayAvg, group = 1, color = "Weekday", linetype = "Weekday"), size = 1, na.rm = TRUE) + 
    geom_point(mapping = aes(x = SatDate, y = WkdayAvg, color = "Weekday"), size = 2, na.rm = TRUE) +
    
    labs(title = paste(site, "Average Daily Discharges:", trends_lookback, "Week Lookback"), x = "Week Of", y = "Discharge Volume") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1)) +
    
    scale_x_discrete(expand = c(0, 0.5, 0, .5)) +
    scale_y_continuous(expand = c(0.2, 0, 0.2, 0)) +
    scale_linetype_manual(name = "", values = c("Weekend" = "solid", "Weekday" = "dashed")) +
    scale_color_manual(name = "", values = c("Weekend" = "#00AEEF", "Weekday" = "#221f72"))
  
}

avg_wkend_wkday_daily_disch("MSH")
avg_wkend_wkday_daily_disch("MSQ")
avg_wkend_wkday_daily_disch("MSBI")
avg_wkend_wkday_daily_disch("MSB")
avg_wkend_wkday_daily_disch("MSW")
avg_wkend_wkday_daily_disch("MSM")


# Create new functions for graphing multiple plots in one plot area -----------------------------

# Select lookback period to use for all graphs
lookback_period <- 12 # 12 week lookback period
trends_first_week <- trends_first_week <- max(wkend_comb_disch_vol$WeekNumber[wkend_comb_disch_vol$Site == site], na.rm = TRUE) - lookback_period + 1


stacked_bar_multi <- function(site) {
  ggplot(data = site_summary_daily_disch_vol[site_summary_daily_disch_vol$Site == site & site_summary_daily_disch_vol$WeekNumber >= trends_first_week, ]) +
    geom_col(mapping = aes(x = SatDate, y = TotalDisch, fill = DischDOW), 
             position = position_stack(reverse = TRUE)) +
    labs(title = paste(site, "Discharges by DOW"), y = "Discharge Volume") + #, x = "Week Of" +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11), 
          legend.position = "bottom", legend.box.spacing = unit(0, "cm"), legend.key.size = unit(0.5, "cm"),
          legend.title = element_text(size = 10), 
          axis.text.x = element_text(angle = 30, hjust = 1), 
          axis.title.x = element_blank(), axis.title.y = element_text(size = 10)) +
    guides(fill = guide_legend(reverse = FALSE, title = "Day of Week")) +
    geom_text(aes(x = SatDate, y = TotalDisch, label = TotalDisch), color = "white", position = position_stack(vjust = 0.5), size = 3) +
    geom_text(data = weekly_totals[weekly_totals$Site == site & weekly_totals$WeekNumber >= trends_first_week, ], aes(x = SatDate, y = WklyTotal, label = WklyTotal), color = "black", vjust = -0.5, size = 3) +
    scale_fill_manual(values = sinai_colors) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0))
}


new_df$check_holiday <- as.factor(new_df$check_holiday)
weekend_volume_new_targets_multi <- function(site) {
  ggplot(data = new_df[(new_df$Site == site) & (new_df$WeekNumber >= trends_first_week), ], aes(x = SatDate, y = TotalDisch)) +
    
    geom_hline(aes(yintercept = Site_Original_Updated_Targets$'Weekend Baseline'[Site_Original_Updated_Targets$Site == site], color = "Baseline", linetype = "Baseline")) +
    geom_label(aes(length(SatDate), Site_Original_Updated_Targets$'Weekend Baseline'[Site_Original_Updated_Targets$Site == site], label = Site_Original_Updated_Targets$'Weekend Baseline'[Site_Original_Updated_Targets$Site == site]), vjust = 0.5, hjust = 0.25, size = 3) +
    
    geom_hline(aes(yintercept = Site_Original_Updated_Targets$'Updated Target'[Site_Original_Updated_Targets$Site == site], color = "Updated Target", linetype = "Updated Target")) +
    geom_label(aes(length(SatDate), Site_Original_Updated_Targets$'Updated Target'[Site_Original_Updated_Targets$Site == site], label = Site_Original_Updated_Targets$'Updated Target'[Site_Original_Updated_Targets$Site == site]),  vjust = 0.5, hjust = 0.25, size = 3) +
    
    geom_line(mapping = aes(x = SatDate, y = TotalDisch, group = 1, color = "Actual", linetype = "Actual"), size = 1) + 
    #geom_point(mapping = aes(x = SatDate, y = TotalDisch) fill = check_holiday), shape = 8, size = 1.5) +
    geom_point(aes(shape = check_holiday, color = check_holiday), size = 2)+
    scale_shape_manual(values=c(17, 16))+
    #scale_color_manual(values=c('#999999','#E69F00'))+
    geom_text(mapping = aes(x = SatDate, y = TotalDisch, label = TotalDisch), color = "black", vjust = -0.25, hjust = 1, size = 3) +
    
    labs(title = paste(site, "Weekend Discharge Volume"), y = "Discharge Volume") + #, x = "Week Of" +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          legend.position = "bottom", legend.justification = "center", legend.key.width = unit(.375,"inch"), legend.box.spacing = unit(0, "cm"),
          axis.text.x = element_text(angle = 30, hjust = 1),
          axis.title.x = element_blank(), axis.title.y = element_text(size = 10)) +
    
    scale_x_discrete(expand = c(0, 0.5, 0, 0.65)) +
    scale_y_continuous(expand = c(0.2, 0, 0.2, 0))+
    
    scale_linetype_manual(name = "", values = c("Baseline" = "dashed", "Updated Target" = "solid", "Actual" = "solid"), labels = c("Actual", "Baseline", "Target")) +
    scale_color_manual(name = "", values = c("Baseline" = "#8c8c8c", "Updated Target" = "black", "Actual" = "#00AEEF", "Holiday Week" = "red", "Not a Holiday Week" = "#00AEEF"), labels = c("Actual", "Baseline", "Target", "Holiday Week", "Not a Holiday Week"))
    #guides(linetype = guide_legend(override.aes = list(shape = c(16, NA, NA))))
  
}

weekend_volume_new_targets_multi("MSM")

weekend_percent_multi <- function(site) {
  ggplot(data = weekly_totals[(weekly_totals$Site == site) & (weekly_totals$WeekNumber >= trends_first_week), ]) +
    geom_line(mapping = aes(x = SatDate, y = WkendPercent, group = 1, color = "Actual", linetype = "Actual"), size = 1, na.rm = TRUE) + 
    geom_point(mapping = aes(x = SatDate, y = WkendPercent, color = "Actual"), size = 2, na.rm = TRUE) +
    
    labs(title = paste(site, "Weekend as Percent of Total Weekly Discharges"), y = "Percent of Discharges") + #, x = "Week Of" +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11), 
          legend.position = "bottom", legend.justification = "center", legend.key.width = unit(.375,"inch"), legend.box.spacing = unit(0, "cm"), 
          axis.text.x = element_text(angle = 30, hjust = 1), 
          axis.title.x = element_blank(), axis.title.y = element_text(size = 10)) +
    
    scale_x_discrete(expand = c(0, 0.5, 0, 0.65)) +
    scale_y_continuous(expand = c(0.2, 0, 0.2, 0), labels = percent_format(accuracy = 1)) +
    scale_linetype_manual(name = "", values = c("Actual" = "solid"), labels = c("Actual")) +
    scale_color_manual(name = "", values = c("Actual" = "#00AEEF"), labels = c("Actual"))
  
}

# weekend_percent_multi("MSH")

avg_wkend_wkday_disch_multi <- function(site) {
  ggplot(data = weekly_totals[(weekly_totals$Site == site) & (weekly_totals$WeekNumber >= trends_first_week), ]) +
    geom_line(mapping = aes(x = SatDate, y = WkendAvg, group = 1, color = "Weekend", linetype = "Weekend"), size = 1) + 
    geom_point(mapping = aes(x = SatDate, y = WkendAvg, color = "Weekend", shape = "Weekend"), size = 2) +
    
    geom_line(mapping = aes(x = SatDate, y = WkdayAvg, group = 1, color = "Weekday", linetype = "Weekday"), size = 1, na.rm = TRUE) + 
    geom_point(mapping = aes(x = SatDate, y = WkdayAvg, color = "Weekday", shape = "Weekday"), size = 2, na.rm = TRUE) +
    
    labs(title = paste(site, "Average Daily Discharges"), y = "Discharge Volume") + #, x = "Week Of" +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11), 
          legend.position = "bottom", legend.justification = "center", legend.key.width = unit(.375,"inch"), legend.box.spacing = unit(0, "cm"), 
          axis.text.x = element_text(angle = 30, hjust = 1),
          axis.title.x = element_blank(), axis.title.y = element_text(size = 10)) +
    
    scale_x_discrete(expand = c(0, 0.5, 0, .5)) +
    scale_y_continuous(expand = c(0.2, 0, 0.2, 0)) +
    scale_linetype_manual(name = "", values = c("Weekend" = "solid", "Weekday" = "solid")) +
    scale_color_manual(name = "", values = c("Weekend" = "#00AEEF", "Weekday" = "#221f72")) +
    scale_shape_manual(name = "", values = c("Weekend" = 16, "Weekday" = 17), guide_legend(show = FALSE)) +
    
    guides(linetype = guide_legend(reverse = TRUE, override.aes = list(shape = c(16, 17))), color = guide_legend(reverse = TRUE))
  
}

# avg_wkend_wkday_disch_multi("MSH")

# p1 <- weekend_volume_new_targets_multi("MSH")
# p2 <- stacked_bar_multi("MSH")
# p3 <- weekend_percent_multi("MSH")
# p4 <- avg_wkend_wkday_disch_multi("MSH")
# 
# figure <- ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
# 
# ggsave(path = graphs_tables_output_location, file = paste("MSH Multiplot Test v6", Sys.Date(), ".png"), plot = figure, device = "png", width = 9.6, height = 6.1, units = "in")
# 

# Create a function to plot all 4 metrics and save as file for each site
metrics_multi_plot <- function(site) {
  p1 <- weekend_volume_new_targets_multi(site)
  p2 <- stacked_bar_multi(site)
  p3 <- weekend_percent_multi(site)
  p4 <- avg_wkend_wkday_disch_multi(site)
  multi_fig <- ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
  ggsave(path = graphs_tables_output_location, file = paste(site, "Multiplot Test", Sys.Date(), ".png"), plot = multi_fig, device = "png", width = 9.6, height = 6.1, units = "in")
}

metrics_multi_plot("MSH")
metrics_multi_plot("MSQ")
metrics_multi_plot("MSBI")
metrics_multi_plot("MSB")
metrics_multi_plot("MSW")
metrics_multi_plot("MSM")

#adding a line