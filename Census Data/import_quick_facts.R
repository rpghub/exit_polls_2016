###   download census data

library(curl)
library(data.table)

# pars
save_loc <- 'c:/projects/post election/census data'
template_loc <- 'c:/projects/post election/census data'
fips_loc <- 'C:/Projects/Post Election/County Results/Results 2016'

# template
setwd(template_loc)
template <- fread('quick_facts_template.csv')
setwd(fips_loc)
fips <- fread('county_election_results_2016.csv')
fips[, fips := ifelse(nchar(fips) == 4, paste('0', fips, sep = '')
                         , fips)]
fips <- fips[!is.na(fips), .N
             , .(state = abbr_state, county
                 , state_fips = substr(fips, 1, 2)
                 , county_fips = substr(fips, 3, 5))]



fips_add <- 
  data.table(state = c('nc', 'ak')
             , county = c('vance', 'kusilvak census area')
             , state_fips = c('37', '02')
             , county_fips = c('181', '158'))
fips <- rbind(fips, fips_add)

# function to read and conv
f_quick_facts <- function(fips_row) {
  if(exists('dat')) rm(dat)
  state <- fips[fips_row]
  sfips <- state$state_fips 
  cfips <- state$county_fips 
  file <- paste('https://www.census.gov/quickfacts/download.php?fips='
                , sfips, cfips, '&type=csv', sep = '')
  tryCatch(
    dat <- read.csv(text=paste0(head(readLines(file), - 33), collapse="\n"))
    , error = function(e) {
                           print(paste('Cannot download: ', sfips, cfips
                                       , sep = ''))})
  if (exists('dat')) {
    fields <- c(2, 3, 9, 10, 11, 12, 13, 14, 30, 31, 18, 19, 20, 21, 22, 23, 24
                , 25, 26, 27, 28, 29, 50, 51, 53, 56, 57, 67, 84)
    
    vals <- as.character(dat[[2]][fields])
    vals <- gsub('(1)', '', vals, fixed = TRUE)
    
    data.table(state_fips = sfips, county_fips = cfips
               , state = state$state, county = state$county
               , census_date = template$census_date
               , field = template$field
               , field_resp = template$field_resp
               , value = as.numeric(vals))
  }
}

system.time(
  census <- do.call('rbind', lapply(seq_len(nrow(fips)), f_quick_facts)))


# 46113 changed to 46102, use new one for old code
fix <- census[state_fips == '46'& county_fips == '102']
fix[, county_fips := '113']
fix[, county := 'shannon']
census <- rbind(census, fix)

# 51515 changed to 51019 use new one for old code
fix <- census[state_fips == '51'& county_fips == '019']
fix[, county_fips := '515']
fix[, county := 'bedford city']
census <- rbind(census, fix) 

# 02270 changed to  02158 use new one for old code
fix <- census[state_fips == '02'& county_fips == '158']
fix[, county_fips := '270']
fix[, county := 'wade hampton census area']
census <- rbind(census, fix) 



setwd(save_loc)
write.csv(census, 'county_quickfacts.csv', row.names = FALSE)