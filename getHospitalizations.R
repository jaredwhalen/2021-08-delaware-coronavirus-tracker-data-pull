
url <- 'https://myhealthycommunity.dhss.delaware.gov/locations/state'

overview <- httr::GET(url, 
                         #pass cookie
                         set_cookies(`policy_accepted` = "true")
  ) %>%
    read_html() %>% 
    html_node("#overview")

  date <- overview %>% 
    html_node('.mb-1') %>% 
    html_text() %>% 
    gsub( ".*(\\d{1,2}/\\d{1,2}/\\d{4}).*", "\\1", .) %>% 
    as.Date(., format='%m/%d/')



  overview.nodes <- overview %>% 
    html_nodes('.c-dash-slat__value-block')
  
  #loop over nodes
  for (node in overview.nodes) {
    #get label
    label <- node %>% 
      html_node('.c-dash-slat__title') %>% 
      html_text()
    
    if (grepl('current hospitalizations', label, ignore.case = T)) {
      value <- node %>% 
        html_node('.c-dash-slat__value') %>% 
        html_text()
        hospitalizations__new <- data.frame(date_confirmed = date, delaware_hospitalized = as.numeric(value))
    }
  }
  
