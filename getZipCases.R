zips <- c(19701, 19702, 19703, 19706, 19707, 19709, 19711, 19713, 19716, 19717, 19720, 19730, 19731, 19732, 19733, 19734, 19736, 19801, 19802, 19803, 19804, 19805, 19806, 19807, 19808, 19809, 19810, 19901, 19902, 19904, 19930, 19931, 19933, 19934, 19938, 19939, 19940, 19941, 19943, 19944, 19945, 19946, 19947, 19950, 19951, 19952, 19953, 19954, 19956, 19958, 19960, 19962, 19963, 19964, 19966, 19967, 19968, 19970, 19971, 19973, 19975, 19977, 19979)

url <- 'https://myhealthycommunity.dhss.delaware.gov/locations/zip-code-'

# zip <- zips[1]

data_list <- list()

for (zip in zips) {
  print(zip)
  html <- httr::GET(paste(url, zip, sep=''), 
                    #pass cookie
                    set_cookies(`policy_accepted` = "true")
  ) %>%
    read_html()
  
  date_confirmed <- html %>% 
    html_node(xpath='/html/body/div[1]/div/div/main/div[1]/div/section/div/section[1]/div/div/article/article/div/header/div/div/div/span[2]') %>% 
    html_text() %>% 
    as.Date(., format='%m/%d/%Y')
  
  
  cases <- html %>% 
    html_node(xpath='//*[@id="positive-cases-data"]/article/div/div/div/div[1]/div[1]/div/div/div[1]/div/div[2]/div[1]') %>% 
    html_text() %>% 
    gsub('[^0-9.-]', '', .) %>% 
    as.numeric()
  
  casesPer10k <- html %>% 
    html_node(xpath='//*[@id="positive-cases-data"]/article/div/div/div/div[1]/div[1]/div/div/div[1]/div/div[2]/div[2]') %>% 
    html_text() %>% 
    sub(" per 10,000 people", "", .) %>% 
    gsub('[^0-9.-]', '', .) %>% 
    as.numeric()
  
  deaths <- html %>% 
    html_node(xpath='//*[@id="death-data"]/article/div/div/div/div[1]/div[1]/div/div/div[1]/div/div[2]/div[1]') %>% 
    html_text() %>% 
    gsub('[^0-9.-]', '', .) %>% 
    as.numeric()
  
  deathsPer10k <- html %>% 
    html_node(xpath='//*[@id="death-data"]/article/div/div/div/div[1]/div[1]/div/div/div[1]/div/div[2]/div[2]') %>% 
    html_text() %>% 
    sub(" per 10,000 people", "", .) %>% 
    gsub('[^0-9.-]', '', .) %>% 
    as.numeric()
  
    row <- data.frame(date_confirmed, zip, cases, casesPer10k, deaths, deathsPer10k)
    
    index <- length(data_list) + 1
    data_list[[index]] <- row
}

zip__new <- bind_rows(data_list)


