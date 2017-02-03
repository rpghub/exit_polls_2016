###    scrape exit poll data

library(data.table)
library(htmltab)
library(stringr)
library(rvest)
library(rjson)

# directory
base_directory <- 'C:/Projects/Post Election'

# paths
saveloc <- './Exit Polls'
maploc <- './Exit Polls'

# load map of question_ids from year to year
setwd(maploc)
map <- 
  fread('question_id_map.csv'
        , colClasses = c('integer', 'character', 'integer', 'character'))
cat_map <- 
  fread('question_category_map.csv'
        , colClasses =  c('character', 'character', 'integer', 'character'))
rm(maploc)

# functions to scrape exit poll data
scrape_state_polls_nbc_12 <- function(state_abbr, n_tbls = 17) {
  questions <- 
    c('sex', 'age', 'age', 'race', 'sex x race', 'lean', 'ideology'
      , 'last grade', 'last grade', 'last grade', 'income'
      , 'income', 'income', 'income', 'church', 'church'
      , 'religion', 'white religion', 'church x religion'
      , 'white born again')
  url <- sprintf('http://elections.nbcnews.com/ns/politics/2012/%s/president'
                 , state_abbr)
  tryCatch({
    do.call('rbind', lapply(seq_len(n_tbls), function(x) {
      doc <- as.data.table(htmltab(url, which = x))
      doc <- melt.data.table(doc, id = 'Category')
      doc[, question := x]
      doc[, .(state = state_abbr, year = 2012
              , src = 'nbc'
              , question_id = question
              , question = questions[x]
              , category = Category, name = variable
              , value)]
      }))
    }
    , error = function(e) {
      print(paste('No Exit Poll Data for:', state_abbr))
      data.table(state = state_abbr, year = 2012, src = 'nbc'
                 , question_id = NA
                 , question = NA, category = NA, name = NA
                 , value = NA)
  })
}

scrape_state_polls_nbc_16 <- function(state_abbr) {
  print(state_abbr)
  url <- sprintf('http://www.nbcnews.com/politics/2016-election/%s'
                 , state_abbr)
  tryCatch({
    doc <- as.data.table(htmltab(url, rm_nodata_cols = FALSE, which = 1))
    setnames(doc, 1, 'voter')
    doc[!is.na(voter), question_id := seq_len(.N)]
    while (nrow(doc[is.na(voter)]) > 0) {
      doc[, voter := ifelse(is.na(voter), shift(voter), voter)]
      doc[, question_id := ifelse(is.na(question_id)
                                  , shift(question_id), question_id)]
    }
    doc[, V1 := str_replace(V1, "([' ']+[' '])+", " ")]
    doc[, V1 := str_replace(V1, '% of voters', '')]
    doc[, V1 := str_replace(V1, '%', '')]
    doc[, Total := str_extract(V1, "([' ']+[0-9]+)")]
    doc[, V1 := str_replace(V1, "([' ']+[0-9]+)", "")]
    doc[, V1 := trimws(V1)]
    doc <- 
      doc[, .(state = state_abbr, year = 2016, src = 'nbc'
              , question = voter, question_id
              , category = V1, Clinton, Trump, Johnson, Stein, Total)]
    melt.data.table(doc, id.vars = c('state', 'year', 'src', 'question_id'
                                     , 'question', 'category')
                    , measure.vars = c('Clinton', 'Trump', 'Johnson', 'Stein'
                                       , 'Total')
                    , variable.name = 'name')
  }, error = function(e) {
      print(paste('No Exit Poll Data for:', state_abbr))
      data.table(state = state_abbr, year = 2016, src = 'nbc'
                 , question_id = NA
                 , question = NA, category = NA, name = NA
                 , value = NA)
      })
}


scrape_state_polls_cnn <- function(state_abbr, year) {
  f_process_poll <- function(poll) {
    if (is.null(poll$qid)) {
      id_field <- 'pollname'
    } else {
      id_field <- 'qid'
    }
    poll_tbl <- 
      lapply(poll$answers
             , function(a) {
               rbind(
                 data.table(question_id = as.character(poll[[id_field]])
                            , question = poll$question
                            , category = a$answer
                            , id = -1
                            , value = a$pct
                            , respondents = poll$numrespondents)
                 , do.call('rbind'
                           , lapply(a$candidateanswers
                                    , function(c) {
                                      data.table(question_id = as.character(poll[[id_field]])
                                                 , question = poll$question
                                                 , category = a$answer
                                                 , id = c$id
                                                 , value = c$pct
                                                 , respondents = poll$numrespondents)})))})
    poll_tbl <- do.call('rbind', poll_tbl)
    
    candidate_tbl <-
      lapply(poll$candidates, function(c) {
        data.table(id = c$id, name  = c$lname)
      })
    candidate_tbl <- do.call('rbind', candidate_tbl)
    candidate_tbl <- rbind(candidate_tbl
                           , data.table(id = -1, name = 'Total'))
    setkey(candidate_tbl, id)
    setkey(poll_tbl, id)
    candidate_tbl[poll_tbl]
  }
  
  tryCatch(
    {
      if (year == 2008) {
        url <- sprintf('http://www.cnn.com/ELECTION/2008/json/%s/xpoll/Pp1.html?'
                       , state_abbr)
        poll_html <- read_html(url)
        poll_txt <- 
          poll_html %>% 
          html_node('textarea') %>%
          html_text() %>%
          fromJSON()
      } else {
        url <- sprintf('http://data.cnn.com/ELECTION/%s/%s/xpoll/Pfull.json'
                       , year, state_abbr)
        poll_html <- read_html(url)
        poll_txt <- 
          poll_html %>% 
          html_text() %>%
          fromJSON()
        rm(poll_html)
      }
      
      poll_tbl <- do.call('rbind', lapply(poll_txt$polls, f_process_poll))
      poll_tbl[, .(state = state_abbr, year = year, src = 'cnn'
                   , question_id, question
                   , category, name, value, respondents)] 
    }
    , error = function(e) {
      print(paste('No Exit Poll Data:', year, state_abbr))
      data.table(state = state_abbr, year = year, src = 'cnn', question_id = NA
                 , question = NA, category = NA, name = NA, value = NA
                 , respondents = NA)
    })
}


# get state exit poll data for last 3 elections
exit08_cnn <- do.call('rbind', lapply(state.abb, scrape_state_polls_cnn
                      , year = 2008))
exit12_cnn <- do.call('rbind', lapply(state.abb, scrape_state_polls_cnn
                      , year = 2012))
#exit12_nbc <- do.call('rbind', lapply(state.abb, scrape_state_polls_nbc_12))
exit16_cnn <- do.call('rbind', lapply(state.abb, scrape_state_polls_cnn
                      , year = 2016))
#exit16_nbc <- do.call('rbind', lapply(state.abb, scrape_state_polls_nbc_16))

# combine exit polls
exit_polls <- rbind(exit08_cnn, exit12_cnn, exit16_cnn)
rm(exit08_cnn, exit12_cnn, exit16_cnn)

# standardize questions
setkey(exit_polls, year, question_id)
setkey(map, year, question_id)
exit_polls <- map[exit_polls]

# standardize response
setkey(exit_polls, question_id_mapped, category, year)
setkey(cat_map, question_id_mapped, category, year)
exit_polls <- cat_map[exit_polls]
exit_polls[!is.na(category_fix), category := category_fix]
exit_polls[, category_fix := NULL]

# add party name
rep <- c('McCain', 'Romney', 'Trump')
dem <- c('Obama', 'Clinton')
oth <- c('Other/No Answer', 'Johnson', 'Stein'
         , 'None of these candidates'
         , 'McMullin')
exit_polls[name %in% rep, name_party := 'republican']
exit_polls[name %in% dem, name_party := 'democrat']
exit_polls[name %in% oth, name_party := 'other']
exit_polls[name == 'Total', name_party := 'total']
rm(rep, dem, oth)


exit_polls <- exit_polls[!is.na(question_id)]

# output
setwd(base_directory)
setwd(saveloc)
write.csv(exit_polls, 'exit_polls.csv', row.names = FALSE)




