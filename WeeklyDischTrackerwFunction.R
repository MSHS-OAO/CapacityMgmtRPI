#Install and load necessary packages --------------------
#install.packages("readraw_dfl")
#install.packages("writeraw_dfl")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("reshape")

#Analysis for weekend discharge tracking
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape)

# Set working directory and select raw data ----------------------------
getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Discharge Billing Data")

raw_base <- read.csv("Crosstab_Discharges_YTD_Test 2019-11-22.csv", header = TRUE, na.strings = c("", "NA"))
raw_update <- read.csv(choose.files(caption = "Select Most Recent Billing Data"), header = TRUE, na.strings = c("", "NA"))

# Reference files and constants ----------------------------------------
ref_file <- "Analysis Reference 2019-11-22.xlsx"
site_dict <- read_excel(ref_file, sheet = "Sites")
dispo_dict <- read_excel(ref_file, sheet = "DischDispo")
dispo_dict[is.na(dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"
service_line_dict <- read_excel(ref_file, sheet = "ServiceLines")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL")
rpi_start <- as.Date("10/26/2019", "%m/%d/%Y")

# Week number function --------------------------------------------------
weeknum <- function(x) {
  yr <<- year(x)
  new_yr <<- as.Date(paste0("1/1/", yr), format = "%m/%d/%Y")
  new_yr_wkday <<- wday(new_yr, label = FALSE)
  new_yr_sat <<- new_yr + (7 - new_yr_wkday)
  elapsed_days <<- as.numeric(x - new_yr_sat)
  week_number <<- ifelse(elapsed_days < 0, 1, as.integer(elapsed_days/7)+2)
  week_number
}

# Create a function to preprocess and format billing data -----------------
data_base <- raw_base
data_update <- raw_update

preprocess <- function(raw_df, incl_df_as_char) {
  # Format admit and discharge dates and pull out year, month, DOW, etc.
  raw_df$AdmitDate <- as.Date(raw_df$Admit.Dt.Src, "%m/%d/%Y")
  raw_df$DischDate <- as.Date(raw_df$Dsch.Dt.Src, "%m/%d/%Y")
  raw_df$DischDateTime <- as.POSIXct(as.character(paste(raw_df$Dsch.Dt.Src, raw_df$Dsch.Time.Src)),  tz = "", format = "%m/%d/%Y %H:%M")
  raw_df[ , c("AdmitYr", "AdmitMo")] <- c(year(raw_df$AdmitDate), month(raw_df$AdmitDate))
  raw_df[ , c("DischYr", "DischMo", "DischHr")] <- c(year(raw_df$DischDate), month(raw_df$DischDate), hour(raw_df$DischDateTime))
  raw_df$DischDOW <- wday(raw_df$DischDate, label = TRUE, abbr = TRUE)
  
  # Lookup tables for site, discharge disposition, service line inclusion/eraw_dfclusion ----------------------------------------------
  # Site lookup
  raw_df$Facility.Msx <- as.character(raw_df$Facility.Msx)
  raw_df <- left_join(raw_df, site_dict, by = c("Facility.Msx" = "Facility Msx"))
  raw_df$Site <- as.factor(raw_df$Site)
  
  # Discharge disposition formatting and lookup
  raw_df$Discharge.Disposition.Desc.Msx <- factor(raw_df$Discharge.Disposition.Desc.Msx, levels = c(levels(raw_df$Discharge.Disposition.Desc.Msx), "Unknown"))
  raw_df[is.na(raw_df$Discharge.Disposition.Desc.Msx), "Discharge.Disposition.Desc.Msx"] <- "Unknown"
  raw_df$Discharge.Disposition.Desc.Msx <- as.character(raw_df$Discharge.Disposition.Desc.Msx)
  raw_df <- left_join(raw_df, dispo_dict, by = c("Discharge.Disposition.Desc.Msx" = "Discharge Disposition Desc Msx"))
  colnames(raw_df)[ncol(raw_df)] <- "DispoRollUp"
  raw_df$Discharge.Disposition.Desc.Msx <- as.factor(raw_df$Discharge.Disposition.Desc.Msx)
  raw_df$DispoRollUp <- as.factor(raw_df$DispoRollUp)
  
  # Service line inclusion / exclusion lookup
  colnames(raw_df)[colnames(raw_df) == "Service.Desc.Msx"] <- "ServiceLine"
  raw_df$ServiceLine <- as.character(raw_df$ServiceLine)
  raw_df <- left_join(raw_df, service_line_dict[ , c(1,3)], by = c("ServiceLine" = "Service Desc Msx"))
  colnames(raw_df)[ncol(raw_df)] <- "ServiceLineInclude"
  raw_df$ServiceLine <- as.factor(raw_df$ServiceLine)
  raw_df$ServiceLineInclude <- as.factor(raw_df$ServiceLineInclude)
  
  # Exclude expired patients and specified service lines
  raw_df$Include <- ifelse(raw_df$DispoRollUp == "Expired" | raw_df$ServiceLineInclude == "No", FALSE, TRUE)
  
  raw_df$Week_Num <- weeknum(raw_df$DischDate)
  raw_df$Weekend <- ifelse(raw_df$DischDOW == "Sat" | raw_df$DischDOW == "Sun" | raw_df$DischDOW == "Mon", TRUE, FALSE)
  
  # Create new dataframe with only included data --------------------------------
  assign(incl_df_as_char, raw_df[raw_df$Include == TRUE, ], envir = .GlobalEnv)
  
}

# Preprocess baseline data and updated weekly datasets -----------------------------
preprocess(data_base, "data2_base")
preprocess(data_update, "data2_update")

# Aggregate and format baseline data from Jan-Sep 2019 for future use --------------------------------
baseline <- data2_base[data2_base$DischMo <= 9, ]

serviceline_daily <- aggregate(Encounter.No ~ Site + DischDate + DischDOW + ServiceLine, data = baseline, FUN = NROW)
hosp_daily <- aggregate(Encounter.No ~ Site + DischDate + DischDOW, data = baseline, FUN = NROW)
total_disch_dow <- aggregate(Encounter.No ~ Site + DischDOW, data = hosp_daily, FUN = sum)
total_disch_dow_tbl <- cast(total_disch_dow, Site ~ DischDOW, value = "Encounter.No")
total_disch_dow_tbl <- total_disch_dow_tbl[order(factor(total_disch_dow_tbl$Site, levels = site_order)), ]
rownames(total_disch_dow_tbl) <- 1:nrow(total_disch_dow_tbl)
avg_disch_dow <- aggregate(Encounter.No ~ Site + DischDOW, data = hosp_daily, FUN = mean, na.rm = TRUE)
avg_disch_dow_tbl <- cast(avg_disch_dow, Site ~ DischDOW, value = "Encounter.No")
avg_disch_dow_tbl <- avg_disch_dow_tbl[order(factor(avg_disch_dow_tbl$Site, levels = site_order)), ]
rownames(avg_disch_dow_tbl) <- 1:nrow(avg_disch_dow_tbl)

avg_stats <- cbind(avg_disch_dow_tbl, 
                   WeekendTotal = avg_disch_dow_tbl$Sat + avg_disch_dow_tbl$Sun + avg_disch_dow_tbl$Mon, 
                   TargetDelta = (avg_disch_dow_tbl[ , "Mon"]*0.1))
avg_stats$TargetTotal = round(avg_stats$WeekendTotal, 0) + round(avg_stats$TargetDelta, 0)
avg_dow_print <- format(avg_disch_dow_tbl, digits = 0, justify = "centre")
avg_stats_print <- format(avg_stats, digits = 0, justify = "centre")

baseline_target <- avg_stats[ , c("Site", "WeekendTotal", "TargetDelta", "TargetTotal")]
colnames(baseline_target) <- c("Site", "Weekend Baseline", "Target Change", "Weekend Target")

# Aggregate, format, and track discharges by week since RPI began ----------------
weekend_totals <- as.data.frame(data2_update[data2_update$Weekend == TRUE, ] %>%
                                  group_by(Site, Week_Num) %>%
                                  summarize(Sat = min(DischDate), Mon = max(DischDate), TotalDisch = n()))

weekend_totals$WeekOf <- paste0(format(weekend_totals$Sat, "%m/%d/%y"), "-", format(weekend_totals$Mon, "%m/%d/%y"))
weekend_totals$PostRPI <- weekend_totals$Sat >= rpi_start
weekend_totals_site <- cast(weekend_totals, Week_Num + WeekOf + Sat + PostRPI ~ Site, value = "TotalDisch")
weekend_rpi_tbl <- cast(weekend_totals[weekend_totals$PostRPI == TRUE, ], Site ~ WeekOf, value = "TotalDisch")
weekend_rpi_tbl <- weekend_rpi_tbl[order(factor(weekend_rpi_tbl$Site, levels = site_order)), ]
rownames(weekend_rpi_tbl) <- 1:nrow(weekend_rpi_tbl)
weekend_rpi_tracker <- merge(baseline_target[ , c("Site", "Weekend Baseline", "Weekend Target")], weekend_rpi_tbl,
                             by.x = "Site", by.y = "Site")
weekend_rpi_tracker <- weekend_rpi_tracker[order(factor(weekend_rpi_tracker$Site, levels = site_order)), ]
weekend_rpi_tracker_print <- format(weekend_rpi_tracker, digits = 0)

# write_xlsx(weekend_rpi_tracker_print, path = paste0("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Script Outputs",
#                                                     "\\Weekend Discharge RPI Tracker ", Sys.Date(), ".xlsx"))


weekly_totals <- data2_update %>%
  group_by(Site, Week_Num) %>%
  summarize(Sat = min(DischDate), Mon = max(DischDate), TotalDisch = n(), WeekendDisch = sum(Weekend == TRUE), WkndPercent = WeekendDisch/TotalDisch*100)
