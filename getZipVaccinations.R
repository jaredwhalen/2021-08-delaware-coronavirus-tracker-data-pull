zips <- c(19701, 19702, 19703, 19706, 19707, 19709, 19711, 19713, 19716, 19717, 19720, 19730, 19731, 19732, 19733, 19734, 19736, 19801, 19802, 19803, 19804, 19805, 19806, 19807, 19808, 19809, 19810, 19901, 19902, 19904, 19930, 19931, 19933, 19934, 19938, 19939, 19940, 19941, 19943, 19944, 19945, 19946, 19947, 19950, 19951, 19952, 19953, 19954, 19956, 19958, 19960, 19962, 19963, 19964, 19966, 19967, 19968, 19970, 19971, 19973, 19975, 19977, 19979)


library(readr)
library(dplyr)
library(httr)
library(rvest)
library(RCurl)
library(stringr)
library(jsonlite)
library(lubridate)
library(zoo)
library(tidycensus)

noDataZips <- c()

cleaner = function(x, date, zip, category) {
  y <- x %>% 
    select(-contains('County')) %>% 
    mutate(
      location = as.character(zip)
    ) %>% 
    rename(value = starts_with('Zip Code'),
           # group = contains('Cumulative')
           group = category
           ) %>%
    mutate(date = date,
           category = category,
           value = as.numeric(gsub(' .*|,', '', value))
    ) %>%
    relocate(date, location, category) %>% 
    select(date, location, category, group, value)
  
  return(y)
}

readUrl <- function(url) {
  out <- tryCatch(
    {
      httr::GET(url, 
                #pass cookie
                set_cookies(`policy_accepted` = "true")
      ) %>%
        read_html()
      
    },
    error=function(cond) {
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      message(paste("Processed URL:", url))
    }
  )    
  return(out)
}


# zips <- c(19730)
data_list <- list()
for (zip in zips) {
  print(zip)
  
  url <- paste('https://myhealthycommunity.dhss.delaware.gov/locations/zip-code-', zip, '/covid19_vaccine_administrations', sep='')
  
  html <- readUrl(url)
  
  if (is.na(html)) {
    next
  } else {
    date <- html %>%
      html_node(xpath='/html/body/article/div/header/div/div/div/span[2]') %>%
      html_text() %>%
      gsub( ".*(\\d{1,2}/\\d{1,2}/\\d{4}).*", "\\1", .) %>%
      as.Date(., format='%m/%d/')
    
    total <- html %>%
      html_node(xpath='/html/body/article/div/div/div/div[1]/div[1]/div/div/table') %>%
      html_table() 
    
    sex <- html %>%
      html_node(xpath='/html/body/article/div/div/div/div[3]/div/div/div[1]/div/div/table') %>%
      html_table() 
    
    age <- html %>%
      html_node(xpath='//html/body/article/div/div/div/div[4]/div/div/div[1]/div/div/table') %>%
      html_table() 
    
    race <- html %>%
      html_node(xpath='/html/body/article/div/div/div/div[5]/div/div/div[1]/div/div/table') %>%
      html_table() 
    
    ethnicity <- html %>%
      html_node(xpath='/html/body/article/div/div/div/div[6]/div/div/div[1]/div/div/table') %>%
      html_table() 
    
    total2 <- total %>%
      filter(grepl('Zip Code', Location)) %>% 
      mutate(
        date = date,
        location = gsub('Zip Code |', '', Location),
        category = 'total',
        group = NA,
        value = as.numeric(gsub(' .*|,', '', `Total Doses Administered`))
      ) %>%
      mutate(location = gsub(' (count)', '', location)) %>% 
      select(
        date,
        location,
        category,
        group,
        value
      )
  
    
    row <- bind_rows(
      total2,
      cleaner(race, date, zip, 'Race'),
      cleaner(ethnicity, date, zip, 'Ethnicity'),
      cleaner(sex, date, zip, 'Sex'),
      cleaner(age, date, zip, 'Age'),
    )
    
    index <- length(data_list) + 1
    data_list[[index]] <- row
  }
  

}

df <- bind_rows(data_list)
# write_csv(df, './Desktop/zipvac.csv')



census <- get_acs(geography = "zip code tabulation area",
        variables = c("B03001_001", "B03002_003"),
        # state = "DE",
        year=2019,
        geometry = F)

census2 <- census %>% 
  mutate(
    location = gsub('ZCTA5 ', '', NAME),
    label = case_when(variable == 'B03001_001' ~ 'total', TRUE ~ 'white')
  ) %>% 
  select(
    location, label, estimate
  ) %>% 
  pivot_wider(id_cols=location, names_from=label, values_from=estimate)



df2 <- left_join(df, census2) %>% 
  mutate(
    percent_white = white/total
  ) %>% 
  select(
    -white
  )




# df.race <- 
  
  
df2 %>% 
  filter(
    category %in% c('Race')
  ) %>%
  mutate(
    group2 = case_when(
      group %in% c("White", "Black", "Asian") ~ group,
      TRUE ~ "Other"
    )
  ) %>%
  select(-group) %>% 
  group_by(date, location, category, group2, total, percent_white) %>% 
  summarise(value = sum(value, na.rm=T)) %>%
  pivot_wider(id_cols=c(location, total, percent_white), names_from=group2, values_from=value) %>% 
  left_join(select(filter(df2, category == 'total'), location, totalDosages=value))









