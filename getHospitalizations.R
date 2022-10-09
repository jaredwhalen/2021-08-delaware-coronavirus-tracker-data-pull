
url <- 'https://myhealthycommunity.dhss.delaware.gov/locations/state'

overview <- httr::GET(url,
                         #pass cookie
                         set_cookies(`policy_accepted` = "true")
  ) %>%
    read_html() %>%
    html_node("#overview")

  date <- overview %>%
    html_node(xpath='/html/body/div[1]/div[2]/div/main/div/div[1]/section/div/section[3]/div[1]/div/div[1]/div[1]/article/div/header/div/div/div/span[2]') %>%
    html_text() %>%
    as.Date(., format='%m/%d/%Y')



value <- overview %>%
    html_node(xpath='//*[@id="overview"]/article[1]/div/div[2]/section[2]/div[2]/div[2]/div/div[2]/div/div/span/strong[1]') %>%
    html_text() %>%
  replace(is.na(.), 0)

hospitalizations__new <- data.frame(date_confirmed = date, delaware_hospitalized = as.numeric(value))


  #loop over nodes
  # for (node in overview.nodes) {
  #   #get label
  #   label <- node %>%
  #     html_node('.c-dash-slat__title') %>%
  #     html_text()
  #
  #   # if (grepl('current hospitalizations', label, ignore.case = T)) {
  #     value <- node %>%
  #       html_node('.c-dash-slat__value-content') %>%
  #       html_text()
  #       hospitalizations__new <- data.frame(date_confirmed = date, delaware_hospitalized = as.numeric(value))
  #   # }
  # }
