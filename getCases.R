
# getData <- function(url) {
#   x <- session(url, 'https://myhealthycommunity.dhss.delaware.gov/locations/county-kent/download_covid_19_data/overview')
#   data <- read_csv(x$response$content)  
#   return(data)
# }

getStatistic <- function(d, q, l, u) {
  d <- data %>% 
    mutate(date_confirmed = paste(year, month, day, sep='-')) %>%
    filter(
      statistic == q,
      unit == u,
    ) %>% 
    select(
      date_confirmed,
      !!l := value
    ) 
  
  # if (q == "Deaths") {
  #   d <- d %>% 
  #   group_by(date_confirmed) %>% 
  #   slice(1)
  # }
  
  return(d)
  
}

# links <- tibble::tribble(
#   ~name,                                                                                              ~url, ~xpath,
#   "delaware",             "https://myhealthycommunity.dhss.delaware.gov/locations/state/", '/html/body/div[1]/div[2]/div/main/div/div[1]/section/div/section[1]/div[2]/article/div/div/div/div/ul/li/a',
#   "newcastle", "https://myhealthycommunity.dhss.delaware.gov/locations/county-new-castle/", '/html/body/div[1]/div[2]/div/main/div/div[1]/section/div/section[1]/div[2]/article/div/div/div/div/ul/li/a',
#   "kent",       "https://myhealthycommunity.dhss.delaware.gov/locations/county-kent/", '/html/body/div[1]/div[2]/div/main/div/div[1]/section/div/section[1]/div[2]/article/div/div/div/div/ul/li/a',
#   "sussex",     "https://myhealthycommunity.dhss.delaware.gov/locations/county-sussex/", '/html/body/div[1]/div[2]/div/main/div/div[1]/section/div/section[1]/div[2]/article/div/div/div/div/ul/li/a'
# )


links <- tibble::tribble(
  ~name,                                                                                              ~url,
  "delaware",             "https://myhealthycommunity.dhss.delaware.gov/locations/state/download_covid_19_data/overview",
  "newcastle", "https://myhealthycommunity.dhss.delaware.gov/locations/county-new-castle/download_covid_19_data/overview",
  "kent",       "https://myhealthycommunity.dhss.delaware.gov/locations/county-kent/download_covid_19_data/overview",
  "sussex",     "https://myhealthycommunity.dhss.delaware.gov/locations/county-sussex/download_covid_19_data/overview"
)



list <- list()
for (i in 1:nrow(links)) {
  row <- links[i,]
  url <- row$url
  name <- row$name
  sess <- session(url)
  data <- read_csv(sess$response$content)

  list[[length(list)+1]] <- getStatistic(data, 'Cumulative Number of Positive Cases', paste(name, 'cases', sep='_'), 'people')
  list[[length(list)+1]] <- getStatistic(data, 'Deaths', paste(name, 'deaths', sep='_'), 'people')
  list[[length(list)+1]] <- getStatistic(data, 'Persons Tested Negative', paste(name, 'negative', sep='_'), 'people')
  list[[length(list)+1]] <- getStatistic(data, 'Total Persons Tested', paste(name, 'tests', sep='_'), 'people')
  Sys.sleep(60)
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





