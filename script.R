library(tidyr)
library(dplyr)
library(googleCloudStorageR)
library(readr)
library(httr)
library(rvest)
library(RCurl)
library(stringr)
library(jsonlite)
library(lubridate)
library(zoo)
library(slackr)


# setwd("~/Documents/GitHub/2021-01-coronavirus-tracker-react/R/de-covid-cdn-push")

output_path = paste('./outputs/', Sys.Date(), sep='')
dir.create(output_path, showWarnings = FALSE)

updateCaseData = function() {
  source("getCases.R")
  location <- 'storytelling-embeds/master/projects/2021-01-coronavirus-tracker-react/data/delaware-cases-data.csv'
  gcs_upload(cases__new, name=location)
  request = paste("https://", Sys.getenv("CDN_CURL_AUTH"), "@www.gannett-cdn.com/delaware-online/", location, sep="")
  system(paste('curl -X PURGE "', request, '" -m 10 &', sep='') , TRUE)
  rm(location)
  write_csv(cases__new, paste(output_path, '/cases.csv', sep=''))
  write_csv(cases__new, "./outputs/latest/cases.csv")
}

checkForHospitalizationUpdate = function() {
  source("getHospitalizations.R")
  location <- 'storytelling-embeds/master/projects/2021-01-coronavirus-tracker-react/data/delaware-hospitalization-data.csv'
  hospitalizations__old <- gcs_get_object(location) %>%
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
  gcs_upload(hospitalizations__updated, name=location)
  request = paste("https://", Sys.getenv("CDN_CURL_AUTH"), "@www.gannett-cdn.com/delaware-online/", location, sep="")
  system(paste('curl -X PURGE "', request, '" -m 10 &', sep='') , TRUE)
  write_csv(hospitalizations__updated, paste(output_path, '/hospitalizations.csv', sep=''))
  write_csv(hospitalizations__updated, "./outputs/latest/hospitalizations.csv")
}

checkForVaccinationUpdate = function() {
  source("getVaccinations.R")
  location <- 'storytelling-embeds/master/projects/2021-01-coronavirus-tracker-react/data/delaware-vaccination-data-3.csv'
  gcs_upload(vaccinations__updated, name=location)
  request = paste("https://", Sys.getenv("CDN_CURL_AUTH"), "@www.gannett-cdn.com/delaware-online/", location, sep="")
  system(paste('curl -X PURGE "', request, '" -m 10 &', sep='') , TRUE)
  write_csv(vaccinations__updated, paste(output_path, '/vaccinations.csv', sep=''))
  write_csv(vaccinations__updated, "./outputs/latest/vaccinations__updated.csv")
}


updateZipCaseData = function() {
  source("getZipCases.R")
  location <- 'storytelling-embeds/master/projects/2021-01-coronavirus-tracker-react/data/delaware-zip-code-case-data.csv'
  # zip__old <- gcs_get_object(location) %>%
  #   select(-X1) %>% 
  #   mutate(date_confirmed = as.Date(date_confirmed)) %>% 
  #   arrange(date_confirmed)
  # 
  # if (zip__new$date_confirmed > tail(zip__old, 1)$date_confirmed) {
  #   print('new entry')
  #   zip__updated <- bind_rows(zip__old, zip__new)
  # } else {
  #   print('update entry')
  #   zip__old <- head(zip__old, -1)
  #   zip__updated <- bind_rows(zip__old, zip__new)
  # }
  
  gcs_upload(zip__new, name=location)
  request = paste("https://", Sys.getenv("CDN_CURL_AUTH"), "@www.gannett-cdn.com/delaware-online/", location, sep="")
  system(paste('curl -X PURGE "', request, '" -m 10 &', sep='') , TRUE)
  write_csv(zip__new, paste(output_path, '/zips.csv', sep=''))
  write_csv(zip__new, "./outputs/latest/zips.csv")
}

# tryCatch(
#   {
#     updateCaseData()
#     checkForHospitalizationUpdate()
#     checkForVaccinationUpdate()
#     updateZipCaseData()
#     
#   },
#   error=function(cond) {
#     message(paste("Process failed"))
# 
#   },
#   warning=function(cond) {
# 
#   },
#   finally={
#     message("Some other message at the end")
#   }
# )

updateCaseData()
checkForHospitalizationUpdate()
checkForVaccinationUpdate()
updateZipCaseData()

 
