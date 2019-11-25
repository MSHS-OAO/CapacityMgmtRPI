# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("reshape")

#Analysis for weekend discharge tracking
library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape)

getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Discharge Billing Data")

raw <- read.csv(choose.files(), header = TRUE, na.strings = c("", "NA"))

ref_file <- "Analysis Reference 2019-11-22.xlsx"
site_dict <- read_excel(ref_file, sheet = "Sites")
dispo_dict <- read_excel(ref_file, sheet = "DischDispo")
service_line_dict <- read_excel(ref_file, sheet = "ServiceLines")

dispo_dict[is.na(dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"


data <- raw
# Format admit and discharge dates and pull out year, month, DOW, etc.
data$AdmitDate <- as.Date(data$Admit.Dt.Src, "%m/%d/%Y")
data$DischDate <- as.Date(data$Dsch.Dt.Src, "%m/%d/%Y")
data$DischDateTime <- as.POSIXct(as.character(paste(data$Dsch.Dt.Src, data$Dsch.Time.Src)),  tz = "", format = "%m/%d/%Y %H:%M")
data[ , c("AdmitYr", "AdmitMo")] <- c(year(data$AdmitDate), month(data$AdmitDate))
data[ , c("DischYr", "DischMo", "DischHr")] <- c(year(data$DischDate), month(data$DischDate), hour(data$DischDateTime))
data$DischDOW <- wday(data$DischDate, label = TRUE, abbr = TRUE)

# Site lookup
data$Facility.Msx <- as.character(data$Facility.Msx)
data <- left_join(data, site_dict, by = c("Facility.Msx" = "Facility Msx"))
data$Site <- as.factor(data$Site)
data$Site2 <- site_dict[match(data$Facility.Msx, site_dict$`Facility Msx`),2]
data$Site3 <- site_dict$Site[match(data$Facility.Msx, site_dict$`Facility Msx`)]
# data <- merge(data, site_dict, by.x = "Facility.Msx", by.y = "Facility Msx", all.x = TRUE, all.y = FALSE, incomparables = NA)

# Discharge disposition formatting and lookup
data$Discharge.Disposition.Desc.Msx <- factor(data$Discharge.Disposition.Desc.Msx, levels = c(levels(data$Discharge.Disposition.Desc.Msx), "Unknown"))
data[is.na(data$Discharge.Disposition.Desc.Msx), "Discharge.Disposition.Desc.Msx"] <- "Unknown"
data$Discharge.Disposition.Desc.Msx <- as.character(data$Discharge.Disposition.Desc.Msx)
data <- left_join(data, dispo_dict, by = c("Discharge.Disposition.Desc.Msx" = "Discharge Disposition Desc Msx"))
colnames(data)[ncol(data)] <- "DispoRollUp"
data$Discharge.Disposition.Desc.Msx <- as.factor(data$Discharge.Disposition.Desc.Msx)
data$DispoRollUp <- as.factor(data$DispoRollUp)
data$Dispo2 <- dispo_dict[match(data$Discjarge.Disposition.Desc.Msx, dispo_dict$`Discharge Disposition Desc Msx`), 2]

# Service line inclusion / exclusion lookup
colnames(data)[colnames(data) == "Service.Desc.Msx"] <- "ServiceLine"
data$ServiceLine <- as.character(data$ServiceLine)
data <- left_join(data, service_line_dict[ , c(1,3)], by = c("ServiceLine" = "Service Desc Msx"))
colnames(data)[ncol(data)] <- "ServiceLineInclude"
data$ServiceLine <- as.factor(data$ServiceLine)
data$ServiceLineInclude <- as.factor(data$ServiceLineInclude)
# data$ServiceLineInclude <- service_line_dict[match(data$ServiceLine, service_line_dict$`Service Desc Msx`), 3]

data$Include <- ifelse(data$DispoRollUp == "Expired" | data$ServiceLineInclude == "No", FALSE, TRUE)

data2 <- data[data$Include == TRUE, ]

#Analysis for Jan-Sep 2019 benchmark
data2$BaselineDate <- ifelse(data2$DischYr == 2019 & data2$DischMo <= 9, TRUE, FALSE)

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
disch_dow_clean <- cast(Disch_DOW, Site ~ DischDOW, value = "Encounter.No")
avg_disch_dow <- aggregate(Encounter.No ~ Site + DischDOW, data = hosp_daily, FUN = mean, na.rm = TRUE)
avg_disch_dow_clean <- cast(avg_disch_dow, Site ~ DischDOW, value = "Encounter.No")
avg_stats <- cbind(avg_disch_dow_clean, WeekendTotal = avg_disch_dow_clean$Sat + avg_disch_dow_clean$Sun + avg_disch_dow_clean$Mon, 
                   Target = (avg_disch_dow_clean[ , "Mon"]*0.1))
avg_dow_print <- format(avg_disch_dow_clean, digits = 0, justify = "centre")
avg_stats_print <- format(avg_stats, digits = 0, justify = "centre")

