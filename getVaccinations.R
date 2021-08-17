#base url
# url <- 'https://myhealthycommunity.dhss.delaware.gov/locations/state/vaccine-tracker#vaccine_tracker'
# 
# page <- httr::GET(url, set_cookies(`policy_accepted` = "true")
# ) %>%
#   read_html()
# 
# date <- page %>% 
#   html_node('.c-dash-card__updated') %>% 
#   html_text() %>% 
#   gsub( ".*(\\d{1,2}/\\d{1,2}/\\d{4}).*", "\\1", .) %>% 
#   as.Date(., format='%m/%d/')
# 
# x <- page %>% 
#   html_nodes(".c-dash-key-metric__value") %>% 
#   html_text() 
# 
# vaccinations__new <- data.frame(
#   date_confirmed = date,
#   delaware_delivered = as.numeric(gsub(',', '', x[1])),
#   delaware_administered = as.numeric(gsub(',', '', x[3]))
# )

getData <- function(url, xpath) {
  url = url
  xpath = xpath
  
  sess <- html_session(url)
  
  download_src <- sess %>%
    read_html() %>%
    html_node(xpath = xpath) %>%
    html_attr('href')
  
  print(download_src)
  download <- jump_to(sess, paste0('https://myhealthycommunity.dhss.delaware.gov', download_src))
  
  data <- read_csv(download$response$content)
  
  
  return(data)
}


url <- "https://myhealthycommunity.dhss.delaware.gov/locations/state/vaccine-tracker#vaccine_tracker"
xpath <- '//*[@id="vaccine_tracker-data-download"]/div/div/div[2]/div[3]/a'

x <- getData(url, xpath)

vaccinations__updated <- x %>% 
  filter(
    # Unit == 'doses',
    # Statistic == 'Vaccine Doses Administered'
    Unit == 'people',
    Statistic == 'Persons Fully Vaccinated'
  ) %>% 
  mutate(
    date_confirmed =as.Date( paste(Month,Day,Year, sep='/'), '%m/%d/%Y')
  ) %>% 
  select(
    date_confirmed,
    # delaware_administered = Value
    vaccinated = Value
    ) %>% 
  mutate(
    # delaware_administered = cumsum(delaware_administered) 
    vaccinated = cumsum(vaccinated) 
  )

