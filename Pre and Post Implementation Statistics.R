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

# Set working directory and select raw data ----------------------------
getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data")

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

# graphs_tables_output_location <- choose.dir(caption = "Select folder to save graphs and tables", default = "J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Script Graphs and Tables")

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


# Import preprocessed baseline data ------------------------------------------
baseline_data_file <- choose.files(default = paste0(getwd(), "/Script Data Outputs/*.*"), caption = "Select Excel file with preprocessed data for baseline period")
sheet_names <- excel_sheets(baseline_data_file)
baseline_list <- lapply(sheet_names, function(x) read_excel(baseline_data_file, sheet = x))
names(baseline_list) <- sheet_names
  
for (i in 1:length(sheet_names)) {
  assign(sheet_names[i], baseline_list[[i]])
}
  
# Import Epic historical repository ------------------------------------------
epic_hist_repo <- read_excel(choose.files(default = paste0(getwd(), "/Script Data Outputs/Epic Repo/*.*"), caption = "Select Excel file with Epic historical repository"), col_names = TRUE, na = c("", "NA"))
  
epic_site_dischunit_dispo_daily <- epic_hist_repo

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
wkday_comb_disch_vol <- wkday_summary_disch_vol[ , c("Site", "WeekNumber", "SatDate", "WeekOf", "WeekendOf", "DischDate", "DischDOW", "Weekend", "TotalDisch")]
# wkday_daily_disch_vol <- wkday_daily_disch_vol[ , c("Site", "WeekNumber", "SatDate", "WeekOf", "WeekendOf", "DischDate", "DischDOW", "Weekend", "TotalDisch")]

site_summary_disch_vol <- rbind(wkend_comb_disch_vol, wkday_comb_disch_vol)
site_summary_disch_vol <- site_summary_disch_vol[order(site_summary_disch_vol$Site, site_summary_disch_vol$WeekNumber, site_summary_disch_vol$DischDate), ]
site_summary_disch_vol$DischDOW <- factor(site_summary_disch_vol$DischDOW, levels = c("Sat-Mon", "Tue-Fri"))
rownames(site_summary_disch_vol) <- 1:nrow(site_summary_disch_vol)

# Create a table with total weekend discharges for each site
wkend_total_table <- dcast(wkend_comb_disch_vol, Site ~ WeekendOf, value.var = "TotalDisch")
Site_Baseline_Targets$Site <- factor(Site_Baseline_Targets$Site, levels = site_order, ordered = TRUE)
wkend_total_rpi_tracker <- left_join(Site_Baseline_Targets[ , c("Site", "Weekend Baseline", "Weekend Target")], wkend_total_table, by = c("Site" = "Site"))

# Create a table to track weekly status including total discharges, % weekend discharges, avg weekday discharges, avg weekend discharges
weekly_totals <- as.data.frame(site_summary_disch_vol %>%
                                 group_by(Site, WeekNumber, SatDate, WeekOf, WeekendOf) %>%
                                 summarize(WkendTotal = sum(TotalDisch[Weekend == TRUE]), WklyTotal = sum(TotalDisch), WkendPercent = WkendTotal/WklyTotal, WkendAvg = TotalDisch[Weekend == TRUE]/3, WkdayAvg = mean(TotalDisch[Weekend != TRUE])))

weekly_totals$WkendPercent[weekly_totals$WkendPercent == 1] <- NA
weekly_totals$WkdayAvg[!is.finite(weekly_totals$WkdayAvg)] <- NA

# Manipulate baseline data ------------------------------

baseline_daily_disch_vol <- Baseline_Total_Daily_Disch

baseline_daily_disch_vol$DischDate <- as.Date(baseline_daily_disch_vol$DischDate, format = "%m/%d/%Y")

baseline_daily_disch_vol$WeekNumber <- weeknum(baseline_daily_disch_vol$DischDate)

baseline_week_num_dates <- as.data.frame(baseline_daily_disch_vol %>%
                                           group_by(WeekNumber) %>%
                                           summarize(SatDate = min(DischDate), FriDate = SatDate + 6, MonDate = SatDate + 2))


baseline_week_num_dates[ , c("SatDate", "FriDate", "MonDate")] <- lapply(baseline_week_num_dates[ , c("SatDate", "FriDate", "MonDate")], format, "%m/%d/%y")

baseline_week_num_dates$WeekOf <- paste0(baseline_week_num_dates$SatDate, "-", baseline_week_num_dates$FriDate)
baseline_week_num_dates$WeekendOf <- paste0(baseline_week_num_dates$SatDate, "-", baseline_week_num_dates$MonDate)
baseline_week_num_dates <- baseline_week_num_dates[ , c("WeekNumber", "SatDate", "WeekOf", "WeekendOf")]
baseline_week_num_dates[ , c("SatDate", "WeekOf", "WeekendOf")] <- lapply(baseline_week_num_dates[ , c("SatDate", "WeekOf", "WeekendOf")], function(x) factor(x, levels = unique(x)))

baseline_daily_disch_vol <- left_join(baseline_daily_disch_vol, baseline_week_num_dates, by = c("WeekNumber" = "WeekNumber"))

baseline_daily_disch_vol$Weekend <- ifelse(baseline_daily_disch_vol$DischDOW == "Sat" | baseline_daily_disch_vol$DischDOW == "Sun" | baseline_daily_disch_vol$DischDOW == "Mon", TRUE, FALSE)

baseline_wkday_daily_disch_vol <- baseline_daily_disch_vol[baseline_daily_disch_vol$Weekend != TRUE, ]
baseline_wkend_daily_disch_vol <- baseline_daily_disch_vol[baseline_daily_disch_vol$Weekend == TRUE, ]

baseline_wkend_summary_disch_vol <- as.data.frame(baseline_wkend_daily_disch_vol %>%
                                           group_by(Site, WeekNumber, Weekend, SatDate, WeekOf, WeekendOf) %>%
                                           summarize(DischDOW = "Sat-Mon", DischDate = min(DischDate), AvgDisch = mean(TotalDisch), TotalDisch = sum(TotalDisch)))

baseline_wkday_summary_disch_vol <- as.data.frame(baseline_wkday_daily_disch_vol %>%
                                           group_by(Site, WeekNumber, Weekend, SatDate, WeekOf, WeekendOf) %>%
                                           summarize(DischDOW = "Tue-Fri", DischDate = min(DischDate), AvgDisch = mean(TotalDisch), TotalDisch = sum(TotalDisch)))

# Create data frame summarizing weekend and weekday average and total discharges
baseline_wkend_wkday_summary_disch_vol <- rbind(baseline_wkend_summary_disch_vol, baseline_wkday_summary_disch_vol)
baseline_wkend_wkday_summary_disch_vol <- baseline_wkend_wkday_summary_disch_vol[order(baseline_wkend_wkday_summary_disch_vol$Site, baseline_wkend_wkday_summary_disch_vol$WeekNumber), ]
rownames(baseline_wkend_wkday_summary_disch_vol) <- 1:nrow(baseline_wkend_wkday_summary_disch_vol)

baseline_wkend_comb_disch_vol <- baseline_wkend_summary_disch_vol[ , c("Site", "WeekNumber", "SatDate", "WeekOf", "WeekendOf", "DischDate", "DischDOW", "Weekend", "TotalDisch")]
baseline_wkday_comb_disch_vol <- baseline_wkday_summary_disch_vol[ , c("Site", "WeekNumber", "SatDate", "WeekOf", "WeekendOf", "DischDate", "DischDOW", "Weekend", "TotalDisch")]


baseline_summary_disch_vol <- rbind(baseline_wkend_comb_disch_vol, baseline_wkday_comb_disch_vol)
baseline_summary_disch_vol <- baseline_summary_disch_vol[order(baseline_summary_disch_vol$Site, baseline_summary_disch_vol$WeekNumber, baseline_summary_disch_vol$DischDate), ]
baseline_summary_disch_vol$DischDOW <- factor(baseline_summary_disch_vol$DischDOW, levels = c("Sat-Mon", "Tue-Fri"))
rownames(baseline_summary_disch_vol) <- 1:nrow(baseline_summary_disch_vol)

# Create a table to track weekly status including total discharges, % weekend discharges, avg weekday discharges, avg weekend discharges
baseline_weekly_totals <- as.data.frame(baseline_summary_disch_vol[baseline_summary_disch_vol$WeekNumber > 1, ] %>%
                                 group_by(Site, WeekNumber, SatDate, WeekOf, WeekendOf) %>%
                                 summarize(WkendTotal = sum(TotalDisch[Weekend == TRUE]), WklyTotal = sum(TotalDisch), WkendPercent = WkendTotal/WklyTotal, WkendAvg = TotalDisch[Weekend == TRUE]/3, WkdayAvg = mean(TotalDisch[Weekend != TRUE])))

baseline_weekly_totals$WkendPercent[baseline_weekly_totals$WkendPercent == 1] <- NA
baseline_weekly_totals$WkdayAvg[!is.finite(baseline_weekly_totals$WkdayAvg)] <- NA

baseline_weekly_totals$Site <- factor(baseline_weekly_totals$Site, levels = site_order, ordered = TRUE)
baseline_weekly_totals <- baseline_weekly_totals[order(baseline_weekly_totals$Site), ]

baseline_weekly_totals$PrePost <- "Pre"
weekly_totals$PrePost <- "Post"

pre_post_all_sites <- rbind(baseline_weekly_totals, weekly_totals)

pre_post_all_sites$WkendPercent <- round(pre_post_all_sites$WkendPercent*100, digits = 1)

pre_post_all_sites$PrePost <- factor(pre_post_all_sites$PrePost, levels = c("Pre", "Post"))
pre_post_all_sites <- pre_post_all_sites[order(pre_post_all_sites$PrePost), ]

# Create a table with summary stats for each site pre- and post-implementation
pre_post_compare <- as.data.frame(pre_post_all_sites %>%
                                    group_by(Site, PrePost) %>%
                                    summarize(WkendDischAvg = mean(WkendTotal, na.rm = TRUE), WkendDischMedian = median(WkendTotal, na.rm = TRUE),
                                              WkendPercentAvg = mean(WkendPercent, na.rm = TRUE), WkendPercentMedian = median(WkendPercent, na.rm = TRUE)))

pre_post_compare_2 <- melt(pre_post_compare, id = c("Site", "PrePost"))
pre_post_compare_2 <- dcast(pre_post_compare_2, Site + variable ~ PrePost, value.var = "value")


# Check for normality of data and equality of variance to determine if t-test can be used to compare pre- and post-implementation weekend discharge volumes
pre_msh_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSH" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
post_msh_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSH" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
pre_post_msh_var <- ifelse(var.test(pre_post_all_sites[pre_post_all_sites$Site == "MSH" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"], 
                            pre_post_all_sites[pre_post_all_sites$Site == "MSH" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Equality of Variance", "No Equality of Variance")
print(paste0("MSH Pre-Implementation Distribution: ", pre_msh_norm))
print(paste0("MSH Post-Implementation Distribution: ", post_msh_norm))
print(paste0("MSH Pre- and Post-Implementation Variance: ", pre_post_msh_var))


pre_msq_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSQ" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
post_msq_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSQ" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
pre_post_msq_var <- ifelse(var.test(pre_post_all_sites[pre_post_all_sites$Site == "MSQ" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"], 
                                    pre_post_all_sites[pre_post_all_sites$Site == "MSQ" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Equality of Variance", "No Equality of Variance")
print(paste0("MSQ Pre-Implementation Distribution: ", pre_msq_norm))
print(paste0("MSQ Post-Implementation Distribution: ", post_msq_norm))
print(paste0("MSQ Pre- and Post-Implementation Variance: ", pre_post_msq_var))

pre_msbi_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSBI" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
post_msbi_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSBI" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
pre_post_msbi_var <- ifelse(var.test(pre_post_all_sites[pre_post_all_sites$Site == "MSBI" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"], 
                                    pre_post_all_sites[pre_post_all_sites$Site == "MSBI" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Equality of Variance", "No Equality of Variance")
print(paste0("MSBI Pre-Implementation Distribution: ", pre_msbi_norm))
print(paste0("MSBI Post-Implementation Distribution: ", post_msbi_norm))
print(paste0("MSBI Pre- and Post-Implementation Variance: ", pre_post_msbi_var))

pre_msb_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSB" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
post_msb_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSB" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
pre_post_msb_var <- ifelse(var.test(pre_post_all_sites[pre_post_all_sites$Site == "MSB" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"], 
                                    pre_post_all_sites[pre_post_all_sites$Site == "MSB" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Equality of Variance", "No Equality of Variance")
print(paste0("MSB Pre-Implementation Distribution: ", pre_msb_norm))
print(paste0("MSB Post-Implementation Distribution: ", post_msb_norm))
print(paste0("MSB Pre- and Post-Implementation Variance: ", pre_post_msb_var))

pre_msw_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSW" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
post_msw_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSW" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
pre_post_msw_var <- ifelse(var.test(pre_post_all_sites[pre_post_all_sites$Site == "MSW" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"], 
                                    pre_post_all_sites[pre_post_all_sites$Site == "MSW" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Equality of Variance", "No Equality of Variance")
print(paste0("MSW Pre-Implementation Distribution: ", pre_msw_norm))
print(paste0("MSW Post-Implementation Distribution: ", post_msw_norm))
print(paste0("MSW Pre- and Post-Implementation Variance: ", pre_post_msw_var))

pre_mssl_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSSL" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
post_mssl_norm <- ifelse(shapiro.test(pre_post_all_sites[pre_post_all_sites$Site == "MSSL" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Normal", "Not Normal")
pre_post_mssl_var <- ifelse(var.test(pre_post_all_sites[pre_post_all_sites$Site == "MSSL" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"], 
                                    pre_post_all_sites[pre_post_all_sites$Site == "MSSL" & pre_post_all_sites$PrePost == "Post", "WkendTotal"])$p.value > 0.05, "Equality of Variance", "No Equality of Variance")
print(paste0("MSSL Pre-Implementation Distribution: ", pre_mssl_norm))
print(paste0("MSSL Post-Implementation Distribution: ", post_mssl_norm))
print(paste0("MSSL Pre- and Post-Implementation Variance: ", pre_post_mssl_var))


# Create boxplots comparing weekend discharges pre- and post-implementation for each site ------------------------------------
ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSH", ], aes(x = PrePost, y = WkendTotal, color = PrePost, fill = PrePost)) +
  geom_boxplot(alpha = 0.2, outlier.color = "black", outlier.shape = 20, outlier.size = 4) +
  geom_text(data = pre_post_compare[pre_post_compare$Site == "MSH", ], aes(x = PrePost, y = WkendDischMedian, label = round(WkendDischMedian, 1)), 
           vjust = -0.5, show.legend = FALSE) +
  labs(title = "MSH: Boxplot of Weekend Discharges", x = "Time Period", y = "Discharge Volume") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) +
  scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) + 
  scale_y_continuous(expand = c(0.1, 0, 0.1, 0))

ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSQ", ], aes(x = PrePost, y = WkendTotal, color = PrePost, fill = PrePost)) +
  geom_boxplot(alpha = 0.2, outlier.color = "black", outlier.shape = 20, outlier.size = 4) +
  geom_text(data = pre_post_compare[pre_post_compare$Site == "MSQ", ], aes(x = PrePost, y = WkendDischMedian, label = round(WkendDischMedian, 1)), 
            vjust = -0.5, show.legend = FALSE) +
  labs(title = "MSQ: Boxplot of Weekend Discharges", x = "Time Period", y = "Discharge Volume") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) +
  scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) + 
  scale_y_continuous(expand = c(0.1, 0, 0.1, 0))


ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSBI", ], aes(x = PrePost, y = WkendTotal, color = PrePost, fill = PrePost)) +
  geom_boxplot(alpha = 0.2, outlier.color = "black", outlier.shape = 20, outlier.size = 4) +
  geom_text(data = pre_post_compare[pre_post_compare$Site == "MSBI", ], aes(x = PrePost, y = WkendDischMedian, label = round(WkendDischMedian, 1)), 
            vjust = -0.5, show.legend = FALSE) +
  labs(title = "MSBI: Boxplot of Weekend Discharges", x = "Time Period", y = "Discharge Volume") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) +
  scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) + 
  scale_y_continuous(expand = c(0.1, 0, 0.1, 0))


ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSB", ], aes(x = PrePost, y = WkendTotal, color = PrePost, fill = PrePost)) +
  geom_boxplot(alpha = 0.2, outlier.color = "black", outlier.shape = 20, outlier.size = 4) +
  geom_text(data = pre_post_compare[pre_post_compare$Site == "MSB", ], aes(x = PrePost, y = WkendDischMedian, label = round(WkendDischMedian, 1)), 
            vjust = -0.5, show.legend = FALSE) +
  labs(title = "MSB: Boxplot of Weekend Discharges", x = "Time Period", y = "Discharge Volume") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) +
  scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) + 
  scale_y_continuous(expand = c(0.1, 0, 0.1, 0))


ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSW", ], aes(x = PrePost, y = WkendTotal, color = PrePost, fill = PrePost)) +
  geom_boxplot(alpha = 0.2, outlier.color = "black", outlier.shape = 20, outlier.size = 4) +
  geom_text(data = pre_post_compare[pre_post_compare$Site == "MSW", ], aes(x = PrePost, y = WkendDischMedian, label = round(WkendDischMedian, 1)), 
            vjust = -0.5, show.legend = FALSE) +
  labs(title = "MSW: Boxplot of Weekend Discharges", x = "Time Period", y = "Discharge Volume") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) +
  scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) + 
  scale_y_continuous(expand = c(0.1, 0, 0.1, 0))


ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSSL", ], aes(x = PrePost, y = WkendTotal, color = PrePost, fill = PrePost)) +
  geom_boxplot(alpha = 0.2, outlier.color = "black", outlier.shape = 20, outlier.size = 4) +
  geom_text(data = pre_post_compare[pre_post_compare$Site == "MSSL", ], aes(x = PrePost, y = WkendDischMedian, label = round(WkendDischMedian, 1)), 
            vjust = -0.5, show.legend = FALSE) +
  labs(title = "MSSL: Boxplot of Weekend Discharges", x = "Time Period", y = "Discharge Volume") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) +
  scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C"), labels = c("Pre-Impl (Jan-Sep '19)", "Post-Implt (10/26/19-Present)")) + 
  scale_y_continuous(expand = c(0.1, 0, 0.1, 0))



# # Create histograms of weekend discharge volume -----------------------------------------
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSH", ], aes(x = WkendTotal, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", binwidth = 4) +
#   labs(title = "MSH: Histogram of Weekend Discharges", x = "Discharge Volume", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSQ", ], aes(x = WkendTotal, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", binwidth = 2) +
#   labs(title = "MSQ: Histogram of Weekend Discharges", x = "Discharge Volume", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSBI", ], aes(x = WkendTotal, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", binwidth = 2) +
#   labs(title = "MSBI: Histogram of Weekend Discharges", x = "Discharge Volume", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSB", ], aes(x = WkendTotal, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", binwidth = 2) +
#   labs(title = "MSB: Histogram of Weekend Discharges", x = "Discharge Volume", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSW", ], aes(x = WkendTotal, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", binwidth = 2) +
#   labs(title = "MSW: Histogram of Weekend Discharges", x = "Discharge Volume", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSSL", ], aes(x = WkendTotal, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", binwidth = 2) +
#   labs(title = "MSSL: Histogram of Weekend Discharges", x = "Discharge Volume", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# # Create histograms of % weekend discharges -------------------------------
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSH", ], aes(x = WkendPercent*100, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", na.rm = TRUE) +
#   labs(title = "MSH: Histogram of Weekend Discharges as % Total Discharges", x = "% of Total Weekly Discharges", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSQ", ], aes(x = WkendPercent*100, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", na.rm = TRUE) +
#   labs(title = "MSQ: Histogram of Weekend Discharges as % Total Discharges", x = "% of Total Weekly Discharges", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSBI", ], aes(x = WkendPercent*100, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", na.rm = TRUE) +
#   labs(title = "MSBI: Histogram of Weekend Discharges as % Total Discharges", x = "% of Total Weekly Discharges", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSB", ], aes(x = WkendPercent*100, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", na.rm = TRUE) +
#   labs(title = "MSB: Histogram of Weekend Discharges as % Total Discharges", x = "% of Total Weekly Discharges", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSW", ], aes(x = WkendPercent*100, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", na.rm = TRUE) +
#   labs(title = "MSW: Histogram of Weekend Discharges as % Total Discharges", x = "% of Total Weekly Discharges", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))
# 
# ggplot(data = pre_post_all_sites[pre_post_all_sites$Site == "MSSL", ], aes(x = WkendPercent*100, color = PrePost)) +
#   geom_histogram(aes(y = ..count.., color = PrePost, fill = PrePost), alpha = 0.2, position = "identity", na.rm = TRUE) +
#   labs(title = "MSSL: Histogram of Weekend Discharges as % Total Discharges", x = "% of Total Weekly Discharges", y = "Frequency") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) +
#   scale_color_manual(name = "", values = c("Pre" = "#221f72", "Post" = "#D80B8C")) + 
#   scale_y_continuous(expand = c(0, 0))

# Test for statistical significance in total weekend discharge changes ------------------------
msh_wkend_total_pvalue <- t.test(WkendTotal  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSH", ], alternative = c("two.sided"))$p.value
print(paste0("MSH Total Weekend Discharge p-value: ", round(msh_wkend_total_pvalue, digits = 2), "; ", ifelse(msh_wkend_total_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

msq_wkend_total_pvalue <- t.test(WkendTotal  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSQ", ], alternative = c("two.sided"))$p.value
print(paste0("MSQ Total Weekend Discharge p-value: ", round(msq_wkend_total_pvalue, digits = 2), "; ", ifelse(msq_wkend_total_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

msbi_wkend_total_pvalue <- t.test(WkendTotal  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSBI", ], alternative = c("two.sided"))$p.value
print(paste0("MSBI Total Weekend Discharge p-value: ", round(msbi_wkend_total_pvalue, digits = 2), "; ", ifelse(msbi_wkend_total_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

msb_wkend_total_pvalue <- t.test(WkendTotal  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSB", ], alternative = c("two.sided"))$p.value
print(paste0("MSB Total Weekend Discharge p-value: ", round(msb_wkend_total_pvalue, digits = 2), "; ", ifelse(msb_wkend_total_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

msw_wkend_total_pvalue <- t.test(WkendTotal  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSW", ], alternative = c("two.sided"))$p.value
print(paste0("MSW Total Weekend Discharge p-value: ", round(msw_wkend_total_pvalue, digits = 2), "; ", ifelse(msw_wkend_total_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

mssl_wkend_total_pvalue <- t.test(WkendTotal  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSSL", ], alternative = c("two.sided"))$p.value
print(paste0("MSSL Total Weekend Discharge p-value: ", round(mssl_wkend_total_pvalue, digits = 2), "; ", ifelse(mssl_wkend_total_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))


msq_wkend_total_pvalue_1 <- t.test(WkendTotal  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSQ", ], alternative = "two.sided")

msq_wkend_total_pvalue_2 <- t.test(WkendTotal  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSQ", ], alternative = "less")

msq_wkend_total_pvalue_3 <- t.test(x = pre_post_all_sites[pre_post_all_sites$Site == "MSQ" & pre_post_all_sites$PrePost == "Post", "WkendTotal"], 
                                   y = pre_post_all_sites[pre_post_all_sites$Site == "MSQ" & pre_post_all_sites$PrePost == "Pre", "WkendTotal"],
                                   alternative = "greater")




# Test for statistical significance in weekend discharges as % total discharge changes ------------------------
msh_wkend_percent_pvalue <- wilcox.test(WkendPercent*100  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSH", ], alternative = c("two.sided"))$p.value
print(paste0("MSH Weekend Discharge as % Total Discharge p-value: ", round(msh_wkend_percent_pvalue, digits = 2), "; ", ifelse(msh_wkend_percent_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

msq_wkend_percent_pvalue <- wilcox.test(WkendPercent*100  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSQ", ], alternative = c("two.sided"))$p.value
print(paste0("MSQ Weekend Discharge as % Total Discharge p-value: ", round(msq_wkend_percent_pvalue, digits = 2), "; ", ifelse(msq_wkend_percent_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

msbi_wkend_percent_pvalue <- wilcox.test(WkendPercent*100  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSBI", ], alternative = c("two.sided"))$p.value
print(paste0("MSBI Weekend Discharge as % Total Discharge p-value: ", round(msbi_wkend_percent_pvalue, digits = 2), "; ", ifelse(msbi_wkend_percent_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

msb_wkend_percent_pvalue <- wilcox.test(WkendPercent*100  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSB", ], alternative = c("two.sided"))$p.value
print(paste0("MSB Weekend Discharge as % Total Discharge p-value: ", round(msb_wkend_percent_pvalue, digits = 2), "; ", ifelse(msb_wkend_percent_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

msw_wkend_percent_pvalue <- wilcox.test(WkendPercent*100  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSW", ], alternative = c("two.sided"))$p.value
print(paste0("MSW Weekend Discharge as % Total Discharge p-value: ", round(msw_wkend_percent_pvalue, digits = 2), "; ", ifelse(msw_wkend_percent_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))

mssl_wkend_percent_pvalue <- wilcox.test(WkendPercent*100  ~ PrePost, data = pre_post_all_sites[pre_post_all_sites$Site == "MSSL", ], alternative = c("two.sided"))$p.value
print(paste0("MSSL Weekend Discharge as % Total Discharge p-value: ", round(mssl_wkend_percent_pvalue, digits = 2), "; ", ifelse(mssl_wkend_percent_pvalue < 0.05, "Pre and Post are significantly different", "Pre and Post are not significantly different")))
