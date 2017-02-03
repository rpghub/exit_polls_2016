rm(list=ls())
library(htmltab)
library(dplyr)

# Load Data ----

wd <- 'C:/Projects/Post Election/County Results/Results 2012'
setwd(wd)

#load county-fips-state file
state_county_fips <- read.csv(
  file.path(wd, 'state_county_fips.csv'), stringsAsFactors=FALSE)
#unique state file
unique_state <- read.csv(file.path(wd, 'unique_state.csv'), 
                         stringsAsFactors=FALSE)


# Scrape ------

grab_data <- function(state){
  url <- sprintf("http://townhall.com/election/2012/president/%s/county", state)
  # Try to grab
  state_result <- try(htmltab::htmltab(
    doc = url, which = '//*[@id="election-live"]/table[2]'))
  if('data.frame' %in% class(state_result)){
    # clean up names
    state_result <- state_result %>%
      rename(county = `County Results >> County`,
             candidate = `County Results >> Candidate`,
             votes = Votes,
             percent_won = `% Won`) %>%
      mutate(abbr_state = state,
             percent_complete = gsub('[[:alpha:]]|%|[[:blank:]]|[[:punct:]]', '', county),
             county = gsub('[[:digit:]]|%', '', county))
      
  } else {
    state_result <- data.frame(abbr_state = state, stringsAsFactors = F)
  }
  state_result
}

#loop over unique state file
results <- lapply(unique_state$abbr_state, function(i){
  print(i)
  res <- grab_data(i)
  # sometimes the page refreshes, so if that happens, 
  # wait for 5 seconds and try again.
  if(nrow(res) == 1){
    Sys.sleep(5)
    print('sleepy time!')
    res <- grab_data(i)
  }
  return(res)
})

# Export ---
z <- dplyr::bind_rows(results)

# merge to get fips
z$county <- gsub("[.]", "", z$county) #remove period
state_county_fips$county <- gsub("[.]", "", state_county_fips$county) #remove period
res <- merge(
  z %>% mutate(county = stringr::str_trim(tolower(county))),
  state_county_fips %>% mutate(county = stringr::str_trim(county)),
                 by = c("abbr_state", "county"), all = TRUE)

# convert to numeric
res$votes <- as.numeric(gsub(",", "", res$votes))
res$percent_won <- as.numeric(sub("%", "", res$percent_won))
res$percent_complete <- as.numeric(res$percent_complete) #this is funky, sorry!

# manual corrections for dc & ak
dc <- 
  data.frame(abbr_state = 'dc'
             , county = 'washington'
             , candidate = c('Barack Obama', 'Mitt Romney', 'Other'
                             , 'Gary Johnson')
             , votes = c(222332, 17337, 2045, 1634)
             , percent_won = c(91.4, 7.1, .8, .7)
             , percent_complete = 100
             , fips = '11001')

ak <- 
  data.frame(abbr_state = 'ak'
             , county = 'alaska'
             , candidate = c('Barack Obama', 'Mitt Romney'
                             , 'Gary Johnson', 'Other')
             , votes = c(136848, 102138, 6131, 2366)
             , percent_won = c(55.3, 41.3, 2.5, 1)
             , percent_complete = 100
             , fips = NA)

res <- filter(res, !(abbr_state %in% c('dc', 'ak')))
res <- rbind(res, dc, ak)

# remove missing candidate records
res <- filter(res, !is.na(candidate) & !is.na(votes))

write.csv(res, "county_election_results_2012.csv", row.names=FALSE)
