# Install and load necessary packages --------------------
# install.packages("readxl")
#install.packages("writexl")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("reshape")

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

raw <- read.csv(choose.files(), header = TRUE, na.strings = c("", "NA"))

# Reference files and constants ----------------------------------------
ref_file <- "Analysis Reference 2019-11-22.xlsx"
site_dict <- read_excel(ref_file, sheet = "Sites")
dispo_dict <- read_excel(ref_file, sheet = "DischDispo")
dispo_dict[is.na(dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"
service_line_dict <- read_excel(ref_file, sheet = "ServiceLines")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL")
rpi_start <- as.Date("10/26/2019", "%m/%d/%Y")

# Format and preprocess raw data in new dataframe called "data" ------------------------
data <- raw
# Format admit and discharge dates and pull out year, month, DOW, etc.
data$AdmitDate <- as.Date(data$Admit.Dt.Src, "%m/%d/%Y")
data$DischDate <- as.Date(data$Dsch.Dt.Src, "%m/%d/%Y")
data$DischDateTime <- as.POSIXct(as.character(paste(data$Dsch.Dt.Src, data$Dsch.Time.Src)),  tz = "", format = "%m/%d/%Y %H:%M")
data[ , c("AdmitYr", "AdmitMo")] <- c(year(data$AdmitDate), month(data$AdmitDate))
data[ , c("DischYr", "DischMo", "DischHr")] <- c(year(data$DischDate), month(data$DischDate), hour(data$DischDateTime))
data$DischDOW <- wday(data$DischDate, label = TRUE, abbr = TRUE)

# Lookup tables for site, discharge disposition, service line inclusion/exclusion ----------------------------------------------
# Site lookup
data$Facility.Msx <- as.character(data$Facility.Msx)
data <- left_join(data, site_dict, by = c("Facility.Msx" = "Facility Msx"))
data$Site <- as.factor(data$Site)

# Discharge disposition formatting and lookup
data$Discharge.Disposition.Desc.Msx <- factor(data$Discharge.Disposition.Desc.Msx, levels = c(levels(data$Discharge.Disposition.Desc.Msx), "Unknown"))
data[is.na(data$Discharge.Disposition.Desc.Msx), "Discharge.Disposition.Desc.Msx"] <- "Unknown"
data$Discharge.Disposition.Desc.Msx <- as.character(data$Discharge.Disposition.Desc.Msx)
data <- left_join(data, dispo_dict, by = c("Discharge.Disposition.Desc.Msx" = "Discharge Disposition Desc Msx"))
colnames(data)[ncol(data)] <- "DispoRollUp"
data$Discharge.Disposition.Desc.Msx <- as.factor(data$Discharge.Disposition.Desc.Msx)
data$DispoRollUp <- as.factor(data$DispoRollUp)

# Service line inclusion / exclusion lookup
colnames(data)[colnames(data) == "Service.Desc.Msx"] <- "ServiceLine"
data$ServiceLine <- as.character(data$ServiceLine)
data <- left_join(data, service_line_dict[ , c(1,3)], by = c("ServiceLine" = "Service Desc Msx"))
colnames(data)[ncol(data)] <- "ServiceLineInclude"
data$ServiceLine <- as.factor(data$ServiceLine)
data$ServiceLineInclude <- as.factor(data$ServiceLineInclude)

# Exclude expired patients and specified service lines
data$Include <- ifelse(data$DispoRollUp == "Expired" | data$ServiceLineInclude == "No", FALSE, TRUE)

# Create new dataframe with only included data --------------------------------
data2 <- data[data$Include == TRUE, ]

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

data2$Week_Num <- weeknum(data2$DischDate)
data2$Weekend <- ifelse(data2$DischDOW == "Sat" | data2$DischDOW == "Sun" | data2$DischDOW == "Mon", TRUE, FALSE)

# Baseline Analysis for Jan-Sep 2019---------------------------------
baseline <- data2[data2$DischMo <= 9, ]

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

# Plot data -------------------------------------------
#Plot discharges by week for all sites
ggplot(data = weekend_totals) +
  geom_line(aes(x = Week_Num, y = TotalDisch, color = Site))

#Plot discharges by week for MSBI
ggplot(data = weekend_totals_site) + 
  geom_line(aes(x = Week_Num, y = MSBI))# +
  # geom_point(color = "red", size = 4)


