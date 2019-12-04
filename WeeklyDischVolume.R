#Install and load necessary packages --------------------
#install.packages("readxl")
#install.packages("writexl")
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

# Format and preprocess baseline raw data in new dataframe called "data_base" and use for baseline and target calculations ------------------------
data_base <- raw_base
# Format admit and discharge dates and pull out year, month, DOW, etc.
data_base$AdmitDate <- as.Date(data_base$Admit.Dt.Src, "%m/%d/%Y")
data_base$DischDate <- as.Date(data_base$Dsch.Dt.Src, "%m/%d/%Y")
data_base$DischDateTime <- as.POSIXct(as.character(paste(data_base$Dsch.Dt.Src, data_base$Dsch.Time.Src)),  tz = "", format = "%m/%d/%Y %H:%M")
data_base[ , c("AdmitYr", "AdmitMo")] <- c(year(data_base$AdmitDate), month(data_base$AdmitDate))
data_base[ , c("DischYr", "DischMo", "DischHr")] <- c(year(data_base$DischDate), month(data_base$DischDate), hour(data_base$DischDateTime))
data_base$DischDOW <- wday(data_base$DischDate, label = TRUE, abbr = TRUE)

# Lookup tables for site, discharge disposition, service line inclusion/exclusion ----------------------------------------------
# Site lookup
data_base$Facility.Msx <- as.character(data_base$Facility.Msx)
data_base <- left_join(data_base, site_dict, by = c("Facility.Msx" = "Facility Msx"))
data_base$Site <- as.factor(data_base$Site)

# Discharge disposition formatting and lookup
data_base$Discharge.Disposition.Desc.Msx <- factor(data_base$Discharge.Disposition.Desc.Msx, levels = c(levels(data_base$Discharge.Disposition.Desc.Msx), "Unknown"))
data_base[is.na(data_base$Discharge.Disposition.Desc.Msx), "Discharge.Disposition.Desc.Msx"] <- "Unknown"
data_base$Discharge.Disposition.Desc.Msx <- as.character(data_base$Discharge.Disposition.Desc.Msx)
data_base <- left_join(data_base, dispo_dict, by = c("Discharge.Disposition.Desc.Msx" = "Discharge Disposition Desc Msx"))
colnames(data_base)[ncol(data_base)] <- "DispoRollUp"
data_base$Discharge.Disposition.Desc.Msx <- as.factor(data_base$Discharge.Disposition.Desc.Msx)
data_base$DispoRollUp <- as.factor(data_base$DispoRollUp)

# Service line inclusion / exclusion lookup
colnames(data_base)[colnames(data_base) == "Service.Desc.Msx"] <- "ServiceLine"
data_base$ServiceLine <- as.character(data_base$ServiceLine)
data_base <- left_join(data_base, service_line_dict[ , c(1,3)], by = c("ServiceLine" = "Service Desc Msx"))
colnames(data_base)[ncol(data_base)] <- "ServiceLineInclude"
data_base$ServiceLine <- as.factor(data_base$ServiceLine)
data_base$ServiceLineInclude <- as.factor(data_base$ServiceLineInclude)

# Exclude expired patients and specified service lines
data_base$Include <- ifelse(data_base$DispoRollUp == "Expired" | data_base$ServiceLineInclude == "No", FALSE, TRUE)

# Create new dataframe with only included data --------------------------------
data2_base <- data_base[data_base$Include == TRUE, ]

# Create a function to determine week of year using Sat as first day of week
weeknum <- function(x) {
  yr <<- year(x)
  new_yr <<- as.Date(paste0("1/1/", yr), format = "%m/%d/%Y")
  new_yr_wkday <<- wday(new_yr, label = FALSE)
  new_yr_sat <<- new_yr + (7 - new_yr_wkday)
  elapsed_days <<- as.numeric(x - new_yr_sat)
  week_number <<- ifelse(elapsed_days < 0, 1, as.integer(elapsed_days/7)+2)
  week_number
}

data2_base$Week_Num <- weeknum(data2_base$DischDate)
data2_base$Weekend <- ifelse(data2_base$DischDOW == "Sat" | data2_base$DischDOW == "Sun" | data2_base$DischDOW == "Mon", TRUE, FALSE)

# Baseline Analysis for Jan-Sep 2019---------------------------------
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

# Weekend discharges by week ------------------------------

weekend_totals <- as.data.frame(data2[data2$Weekend == TRUE, ] %>%
  group_by(Site, Week_Num) %>%
  summarize(Sat = min(DischDate), Mon = max(DischDate), TotalDisch = n()))
weekend_totals$WeekOf <- paste0(format(weekend_totals$Sat, "%m/%d/%y"), "-", format(weekend_totals$Mon, "%m/%d/%y"))
weekend_totals$PostRPI <- weekend_totals$Sat >= rpi_start
weekend_totals_site <- cast(weekend_totals, Week_Num + WeekOf + Sat + PostRPI ~ Site, value = "TotalDisch")
weekend_rpi_tbl <- cast(weekend_totals[weekend_totals$PostRPI == TRUE, ], Site ~ WeekOf, value = "TotalDisch")
weekend_rpi_tbl <- weekend_rpi_tbl[order(factor(weekend_rpi_tbl$Site, levels = site_order)), ]
rownames(weekend_rpi_tbl) <- 1:nrow(weekend_rpi_tbl)
weekend_rpi_tracker <- merge(avg_stats[ , c("Site", "WeekendTotal", "TargetTotal")], weekend_rpi_tbl,
                             by.x = "Site", by.y = "Site")
weekend_rpi_tracker <- weekend_rpi_tracker[order(factor(weekend_rpi_tracker$Site, levels = site_order)), ]
weekend_rpi_tracker_print <- format(weekend_rpi_tracker, digits = 0)

# Export key data to spreadsheet----------------------------------------
# setwd("..")
# setwd(".\\Script Outputs")
# export_list <- list(TotalDischDOW = total_disch_dow_tbl, 
#                     AvgDishDOW = avg_dow_print, 
#                     AvgDischSummary = avg_stats_print,
#                     WeeklyWeekendDisch = weekend_totals_site,
#                     WkndDischRPITrack = weekend_rpi_tracker)
# 
# write_xlsx(export_list, "Baseline Analysis Automated 2019-11-26 v1.xlsx")

# # Plot data -------------------------------------------
# #Plot discharges by week for all sites
# ggplot(data = weekend_totals) +
#   geom_line(aes(x = Week_Num, y = TotalDisch, color = Site))
# 
# #Plot discharges by week for MSBI
# ggplot(data = weekend_totals_site) + 
#   geom_line(aes(x = Week_Num, y = MSBI))# +
#   # geom_point(color = "red", size = 4)

# # Compare volume differences
# msw_11022019 <- data2[data2$Site == "MSW" & data2$Week_Num == 45 & data2$Weekend == TRUE, ]
# a <- as.data.frame(msw_11022019$Encounter.No)
# write_xlsx(a, "Compare MSW 2019-11-02 Encounter.xlsx")
msh_12022019 <- data2[data2$Site == "MSH" & data2$Week_Num == 45 & data2$Weekend == TRUE, ]
msh_12022019_mrn <- msh_12022019[ , c("Encounter.No", "Msmrn")]
write_xlsx(msh_12022019_mrn, "Compare MSH Discharges Wk of 11022019.xlsx")

write_xlsx(weekend_rpi_tracker, "Weekly Summary 12022019.xlsx")
