
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

getStatistic <- function(d, q, l, u) {
  d <- d %>% 
    mutate(date_confirmed = paste(Year, Month, Day, sep='-')) %>% 
    filter(
      Statistic == q,
      Unit == u,
    ) %>% 
    select(
      date_confirmed,
      !!l := Value
    ) 
  
  # if (q == "Deaths") {
  #   d <- d %>% 
  #   group_by(date_confirmed) %>% 
  #   slice(1)
  # }
  
  return(d)
  
}

links <- tibble::tribble(
  ~name,                                                                                              ~url, ~xpath,
  "delaware",             "https://myhealthycommunity.dhss.delaware.gov/locations/state/", '//*[@id="introduction"]/div[2]/article/div/div/div/div/ul/li/a',
  "newcastle", "https://myhealthycommunity.dhss.delaware.gov/locations/county-new-castle/", '//*[@id="introduction"]/div[2]/article/div/div/div/div/ul/li/a',
  "kent",       "https://myhealthycommunity.dhss.delaware.gov/locations/county-kent/", '//*[@id="introduction"]/div[2]/article/div/div/div/div/ul/li/a',
  "sussex",     "https://myhealthycommunity.dhss.delaware.gov/locations/county-sussex/", '//*[@id="introduction"]/div[2]/article/div/div/div/div/ul/li/a'
)

list <- list()
for (row in 1:nrow(links)) {
  name <- links[row, 'name']
  url <- links[row, 'url']
  xpath <- links[row, 'xpath']
  data <- getData(url$url, xpath$xpath)
  # data <- read_csv(paste('files/', url, '-', substring(Sys.time(), 1, 10), '.csv', sep=''))
  
  list[[length(list)+1]] <- getStatistic(data, 'Cumulative Number of Positive Cases', paste(name, 'cases', sep='_'), 'people')
  list[[length(list)+1]] <- getStatistic(data, 'Deaths', paste(name, 'deaths', sep='_'), 'people')
  list[[length(list)+1]] <- getStatistic(data, 'Persons Tested Negative', paste(name, 'negative', sep='_'), 'people')
  # list[[length(list)+1]] <- getStatistic(data, 'Total Tests', paste(name, 'tests', sep='_'), 'tests')
  list[[length(list)+1]] <- getStatistic(data, 'Total Persons Tested', paste(name, 'tests', sep='_'), 'people')
  # list[[length(list)+1]] <- getStatistic(data, 'Positive Tests', paste(name, 'positiveTests', sep='_'), 'tests')
  # list[[length(list)+1]] <- getStatistic(data, 'New Positive Tests', paste(name, 'positiveTests', sep='_'), 'tests')
}

cases__new <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  list
)

# cases__new <- cases__new %>% 
#   group_by(date_confirmed) %>% 
#   filter(delaware_deaths == max(delaware_deaths)) %>% 
#   slice(1)

head(cases__new)





