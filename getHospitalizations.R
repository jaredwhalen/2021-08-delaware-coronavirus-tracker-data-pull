
url <- 'https://myhealthycommunity.dhss.delaware.gov/locations/state'

overview <- httr::GET(url, 
                         #pass cookie
                         set_cookies(`policy_accepted` = "true")
  ) %>%
    read_html() %>% 
    html_node("#overview")

  date <- overview %>% 
    html_node(xpath='/html/body/div[1]/div/div/main/div[1]/div/section/div/section[2]/div/div/article/article/div/header/div/div/div/span[2]') %>% 
    html_text() %>% 
    as.Date(., format='%m/%d/%Y')



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
  
