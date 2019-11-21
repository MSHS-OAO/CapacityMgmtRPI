#Analysis for weekend discharge tracking
library(readxl)
library(ggplot2)
library(lubridate)

getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Discharge Billing Data")

raw <- read.csv(choose.files(), header = TRUE, na.strings = c("", "NA"))
data <- raw
data$AdmitDate <- as.Date(data$Admit.Dt.Src, "%m/%d/%Y")
data$DischDateTime <- as.POSIXct(as.character(paste(data$Dsch.Dt.Src, data$Dsch.Time.Src)),  tz = "", format = "%m/%d/%Y %H:%M")
data[ , c("AdmitYr", "AdmitMo")] <- c(year(data$AdmitDate), month(data$AdmitDate))
data[ , c("DischYr", "DischMo", "DischHr", "DischDOW")] <- c(year(data$DischDateTime), month(data$DischDateTim), hour(data$DischDateTime), wday(data$DischDateTime, label = TRUE, abbr = TRUE))

