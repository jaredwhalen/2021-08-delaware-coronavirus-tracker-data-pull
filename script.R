library(tidyr)
library(dplyr)
library(readr)
library(httr)
library(rvest)
library(RCurl)
library(stringr)
library(jsonlite)
library(lubridate)
library(zoo)
library(stringi)

output_path = paste('./outputs/', Sys.Date(), sep='')
dir.create(output_path, showWarnings = FALSE)

updateCaseData = function() {
  source("getCases.R")
  write_csv(cases__new, paste(output_path, '/cases.csv', sep=''))
  write_csv(cases__new, "./outputs/latest/cases.csv")
}

checkForHospitalizationUpdate = function() {
  source("getHospitalizations.R")
  hospitalizations__old <- read_csv(paste("https://www.gannett-cdn.com/delaware-online/datasets/coronavirus-tracker/latest/hospitalizations.csv", stringi::stri_rand_strings(1, 50), sep='') %>%
    select(date_confirmed, delaware_hospitalized) %>% 
    mutate(date_confirmed = as.Date(date_confirmed), delaware_hospitalized = as.numeric(delaware_hospitalized)) %>% 
    arrange(date_confirmed)
  if (hospitalizations__new$date_confirmed > tail(hospitalizations__old, 1)$date_confirmed) {
    print('new entry')
    hospitalizations__updated <- bind_rows(hospitalizations__old, hospitalizations__new)
  } else {
    hospitalizations__updated <- within(hospitalizations__old, {
      delaware_hospitalized[date_confirmed == hospitalizations__new$date_confirmed] <- hospitalizations__new$delaware_hospitalized
    })
    print('update entry')
  }
  write_csv(hospitalizations__updated, paste(output_path, '/hospitalizations.csv', sep=''))
  write_csv(hospitalizations__updated, "./outputs/latest/hospitalizations.csv")
}

checkForVaccinationUpdate = function() {
  source("getVaccinations.R")
  write_csv(vaccinations__updated, paste(output_path, '/vaccinations.csv', sep=''))
  write_csv(vaccinations__updated, "./outputs/latest/vaccinations.csv")
}


updateZipCaseData = function() {
  source("getZipCases.R")
  write_csv(zip__new, paste(output_path, '/zips.csv', sep=''))
  write_csv(zip__new, "./outputs/latest/zips.csv")
}

updateCaseData()
checkForHospitalizationUpdate()
checkForVaccinationUpdate()
updateZipCaseData()

 
