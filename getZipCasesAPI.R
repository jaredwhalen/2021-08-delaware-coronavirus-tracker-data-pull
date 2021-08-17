url <- 'https://myhealthycommunity.dhss.delaware.gov/locations/state/choropleth/COVID19_PREVALENCE_RATE.json?date='

fetchAndCheckForValues <- function(date) {
  print(paste('run for ', date, sep=''))
  res = GET(paste(url, date, sep=''))
  if (res$status_code == 200) {
    json <- fromJSON(rawToChar(res$content))
    zipData <- json$polygons$features$properties
    if ( 'value' %in%  names(zipData) ) {
      zip__new <- zipData %>%
        select(zip = locationName, value) %>%
        mutate(zip = gsub('Zip Code ', '', zip)) %>%
        spread(zip, value) %>%
        mutate(date_confirmed = date) %>%
        relocate(date_confirmed)
      return(zip__new)
    } else {
      date <- date - 1
      fetchAndCheckForValues(date)
    }
  }
}

date <- Sys.Date()
zip__new <- fetchAndCheckForValues(date)
