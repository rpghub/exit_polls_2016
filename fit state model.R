###   script to fit state models

library(data.table)
library(rstan)

# select state
selected_states <-  setdiff(state.abb, c('AK', 'HI'))

# directory
base_directory <- 'C:/data/tmp/exit_polls_2016'

# file paths
acs_loc <- './ACS Data'
res2008_loc <- './County Results/Results 2008'
res2012_loc <- './County Results/Results 2012'
res2016_loc <- './County Results/Results 2016'
exit_loc <- './Exit Polls'
turnout_loc <- './Benchmark Turnout'

# stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# define inv-logit function
inv_logit <- function(x) 1 / (1 + exp(-x))
logit <- function(x) log(x / (1 - x))

# create state to region map
state_region <- data.table(state = state.abb, region = state.region)

# acs data
setwd(base_directory)
setwd(acs_loc)
acs <- fread('acs_county.csv', colClasses = 'char')
acs[, perwt := as.numeric(perwt) / 100]
acs[, educ_sel := as.integer(educ_sel)]
acs[, year := as.integer(year)]
rm(acs_loc)

# 2008 data
setwd(base_directory)
setwd(res2008_loc)
res2008 <- fread('county_election_results_2008.csv')
res2008[, fips := ifelse(nchar(fips) == 4, paste('0', fips, sep = '')
                         , fips)]
res2008[, year := 2008]
rm(res2008_loc)

# 2012 data
setwd(base_directory)
setwd(res2012_loc)
res2012 <- fread('county_election_results_2012.csv')
res2012[, fips := ifelse(nchar(fips) == 4, paste('0', fips, sep = '')
                         , fips)]
res2012[, year := 2012]
rm(res2012_loc)

# 2016 data
setwd(base_directory)
setwd(res2016_loc)
res2016 <- fread('county_election_results_2016.csv')
res2016[, fips := ifelse(nchar(fips) == 4, paste('0', fips, sep = '')
                         , fips)]
res2016[, year := 2016]
rm(res2016_loc)

# exit polls
setwd(base_directory)
setwd(exit_loc)
ep <- fread('exit_polls.csv')
rm(exit_loc)

# benchmark turnout to statewide numbers
setwd(base_directory)
setwd(turnout_loc)
benchmark <- fread('state_turnout.csv')

# convert candidates to party & combine
res2016[candidate == 'Donald Trump', party := 'Republican']
res2016[candidate == 'Hillary Clinton', party := 'Democrat']
res2016[is.na(party), party := 'Other']
res2012[candidate == 'Mitt Romney', party := 'Republican']
res2012[candidate == 'Barack Obama', party := 'Democrat']
res2012[is.na(party), party := 'Other']
res2008[candidate == 'John McCain', party := 'Republican']
res2008[candidate == 'Barack Obama', party := 'Democrat']
res2008[is.na(party), party := 'Other']
res <- rbind(res2016, res2012, res2008)
rm(res2016, res2012, res2008)

# format results
res[, county := toupper(county)]
res[, state := toupper(abbr_state)]

# gross up county results to state benchmarks
setkey(res, state, year)
setkey(benchmark, state_abbr, year)
res <- benchmark[, .(state_abbr, year, ballots)][res]
setnames(res, 'state_abbr', 'state')
res[, state_votes := sum(votes), .(state, year)]
res[, adj := ballots / state_votes]
res[, votes_adj := votes * adj]
res[, state_votes := NULL]
res[, ballots := NULL]

# adjust for bedford county city which changed fips
tmp <-
  res[fips %in% c('51515', '51019')
      , .(votes = sum(votes)
          , votes_adj = sum(votes_adj)
          , adj = sum(votes_adj) / sum(votes)
          , county = 'BEDFORD CO'
          , fips = 51019
          , percent_complete = 100)
      , .(state, year, abbr_state, candidate, party)]
tmp[, percent_won := votes_adj / sum(votes_adj), year]
res <- rbind(res[!(fips %in% c('51515', '51019'))], tmp)
rm(tmp)

tmp <-
  acs[county %in% c('51515', '51019')
      , .(perwt = sum(perwt)
          , cntyname = 'Bedford VA'
          , county = '51019')
      , .(state, year, educ_sel, race_sel, inc_sel)]
acs <- rbind(acs[!(county %in% c('51515', '51019'))], tmp)
rm(tmp)


# exit poll estimates ----
# selected demographics
ep_sel <- ep[question_id_mapped %in% c(260, 917, 1951) 
             & name_party %in% c('democrat', 'total')]
suppressWarnings(ep_sel[, value := as.numeric(value)])
ep_sel[category == '$100K or More', category := '$100K or more']
ep_sel[category == 'Less Than $100K', category := 'Under $100K']
ep_sel <-
  dcast.data.table(ep_sel
                   , year + state + category + respondents
                   + question_id_mapped + question ~ name_party
                   , value.var = 'value')
setkey(ep_sel, state)
setkey(state_region, state)
ep_sel <- state_region[ep_sel]

# add states that are not in exit poll data
ep_sel_full <- ep_sel[, .(.N), .(year, category, question_id_mapped)]
ep_sel_full <- ep_sel_full[, .(state = state_region[, state]
                               , region = state_region[, region])
                           , .(year, category, question_id_mapped)]
setkey(ep_sel_full, state, region, year, category, question_id_mapped)
setkey(ep_sel, state, region, year, category, question_id_mapped)
ep_sel <- ep_sel[ep_sel_full]
rm(ep_sel_full)

# credibility weight exit poll data
ep_sel[total <= 5, cred := .25]
ep_sel[between(total, 6, 10), cred := .50]
ep_sel[between(total, 11, 15), cred := .75]
ep_sel[total > 15, cred := 1]
ep_sel[is.na(democrat), cred := 0]
ep_sel[, dem_region := weighted.mean(democrat, total * respondents / 100
                                     , na.rm = TRUE)
       , .(year, region, category, question_id_mapped)]
ep_sel[, dem_nation := weighted.mean(democrat, total * respondents / 100
                                     , na.rm = TRUE)
       , .(year, category, question_id_mapped)]
ep_sel[is.na(dem_region), dem_region := dem_nation]
ep_sel[, democrat_cred := democrat * cred + (1 - cred) * dem_region]
ep_sel[is.na(democrat), democrat_cred := dem_region]
ep_sel[, cred := NULL]
ep_sel[, dem_region := NULL]
ep_sel[, dem_nation := NULL]

# adjust categories to match acs
ep_sel[category == '$100K or more', category := '1' ]
ep_sel[category == 'Under $100K', category := '0' ]
ep_sel[category == 'no', category := '0' ]
ep_sel[category == 'yes', category := '1' ]

# acs data for turnout
acs[, state_abbr := substr(cntyname, nchar(cntyname) - 1, nchar(cntyname))]
acs[race_sel == 'hispanic', race_sel := 'latino']
acs_state <-
  acs[year %in% c(2015, 2012, 2008)
      , .(perwt = round(sum(perwt)))
      , .(year, race_sel
          , educ_sel = as.integer(educ_sel)
          , inc_sel = as.integer(inc_sel)
          , state = state_abbr)]
acs_full <- acs_state[, .N, .(year, race_sel, educ_sel, inc_sel)]
acs_full <- acs_full[, .(state = acs_state[, unique(state)])
                     , .(year, race_sel, educ_sel, inc_sel)]
setkey(acs_full, year, race_sel, educ_sel, inc_sel, state)
setkey(acs_state, year, race_sel, educ_sel, inc_sel, state)
acs_state <- acs_state[acs_full]
acs_state[is.na(perwt), perwt := 1L]
acs_state[year == 2015, year := 2016]
acs_state[, educ_sel := as.character(educ_sel)]
acs_state[, inc_sel := as.character(inc_sel)]
cols <- c('educ_sel', 'inc_sel', 'race_sel')
acs_state_agg <-
  lapply(cols, function(c) {
    setnames(acs_state, c, 'sel_sel')
    tmp <-
      acs_state[, .(perwt = sum(perwt), cat = c)
                , .(year, state, category = sel_sel)]
    setnames(acs_state, 'sel_sel', c)
    tmp
  })

acs_state_agg <- do.call('rbind', acs_state_agg)
acs_state_agg[cat == 'educ_sel', question_id_mapped := '1951']
acs_state_agg[cat == 'race_sel', question_id_mapped := '260']
acs_state_agg[cat == 'inc_sel', question_id_mapped := '917']
setkey(acs_state_agg, state, year, question_id_mapped, category)
setkey(ep_sel, state, year, question_id_mapped, category)
ep_sel <- acs_state_agg[ep_sel]

# merge in total turnout for state
res_state <- res[, .(votes = sum(votes_adj)), .(state, year)]
setkey(res_state, state, year)
setkey(ep_sel, state, year)
ep_sel <- res_state[ep_sel]

# use regional turnout and acs state population to fill totals for
# states without exit poll data
ep_sel[, turnout_region := sum(total * votes / 100, na.rm = TRUE) /
         sum(ifelse(is.na(total), NA, perwt), na.rm = TRUE)
       , .(year, region, cat, category)]
ep_sel[turnout_region > 1, turnout_region := 1]
ep_sel[is.na(total), total := turnout_region * perwt /
         sum(turnout_region * perwt) * 1E2
       , .(state, year, cat)]
ep_sel[, turnout_region := NULL]

# get vote totals
ep_sel[, votes_total := votes * total / 100]
ep_sel[votes_total > perwt, votes_total := perwt]
ep_sel[, votes_dem := votes_total * democrat_cred / 100]

# output support files
setwd(base_directory)
setwd('./county results')
fwrite(res, 'combined county results.csv', row.names = FALSE)

setwd(base_directory)
setwd('./exit polls')
fwrite(ep_sel, 'selected exit polls.csv', row.names = FALSE)


# priors base on race adjusted for education and income
ep_priors <- 
  ep_sel[cat == 'race_sel', .(state, year, race_sel = category
                              , democrat_cred = democrat_cred / 100
                              , turnout = votes * total / perwt / 100)]
setkey(ep_priors, state, year, race_sel)
setkey(acs_state, state, year, race_sel)
ep_priors <- acs_state[ep_priors]

# get relative votes by education
ep_sel_educ <- 
  ep_sel[cat == 'educ_sel', .(state, year, educ_sel = category
                              , perwt
                              , democrat_cred_educ = democrat_cred / 100
                              , turnout_educ = votes * total / perwt / 100)]
ep_sel_educ[turnout_educ > .99, turnout_educ := .99]
ep_sel_educ[democrat_cred_educ > .99, democrat_cred_educ := .99]
ep_sel_educ[, democrat_cred_educ := democrat_cred_educ / 
              weighted.mean(democrat_cred_educ, turnout_educ * perwt)
            , .(year, state)]
ep_sel_educ[, turnout_educ := turnout_educ / 
              weighted.mean(turnout_educ, perwt)
            , .(year, state)]
ep_sel_educ[, perwt := NULL]
setkey(ep_priors, state, year, educ_sel)
setkey(ep_sel_educ, state, year, educ_sel)
ep_priors <- ep_sel_educ[ep_priors]

# get relative votes by income
ep_sel_inc <- 
  ep_sel[cat == 'inc_sel', .(state, year, perwt, inc_sel = category
                             , democrat_cred_inc = democrat_cred / 100
                             , turnout_inc = votes * total / perwt / 100)]
ep_sel_inc[turnout_inc > .99, turnout_inc := .99]
ep_sel_inc[democrat_cred_inc > .99, democrat_cred_inc := .99]
ep_sel_inc[, democrat_cred_inc := democrat_cred_inc / 
             weighted.mean(democrat_cred_inc, turnout_inc * perwt)
           , .(year, state)]
ep_sel_inc[, turnout_inc := turnout_inc / 
             weighted.mean(turnout_inc, perwt)
           , .(year, state)]
ep_sel_inc[, perwt := NULL]

# adjust white votes by education and income relative vote shares
# use unadjusted percentages by race for all other races
setkey(ep_priors, state, year, inc_sel)
setkey(ep_sel_inc, state, year, inc_sel)
ep_priors <- ep_sel_inc[ep_priors]
ep_priors[is.na(democrat_cred_inc), democrat_cred_inc := 1]
ep_priors[is.na(turnout_inc), turnout_inc := 1]
ep_priors[race_sel == 'white', democrat_cred := 
            democrat_cred * democrat_cred_inc * democrat_cred_educ]
ep_priors[race_sel == 'white', turnout := 
            turnout * turnout_inc * turnout_educ]
ep_priors[turnout > .92, turnout := .92]
ep_priors[democrat_cred > .95, democrat_cred := .95]
ep_priors[turnout < .15, turnout := .15]
ep_priors[democrat_cred < .05, democrat_cred := .05]
ep_priors <- 
  ep_priors[, .(year, state, educ_sel, inc_sel, race_sel
                , wt = perwt * turnout
                , perwt
                , turnout,  dem = democrat_cred)]
ep_priors[, wt := wt / sum(wt), .(state, year)]
ep_priors[wt < .03 & turnout > .75, turnout := .75]
ep_priors[wt < .03 & turnout < .15, turnout := .15]
rm(ep_sel_educ, ep_sel_inc)

# function to fit state model
f_state_fit <- function(state_sel) {
  y <- res[state == state_sel
           & year %in% c(2016, 2012)
           , .(votes = sum(votes_adj))
           , .(year, fips)
           ][order(year, fips), votes]
  
  votes <- res[state == state_sel & party == 'Democrat'
               & year %in% c(2016, 2012)
               , .(votes = sum(votes_adj))
               , .(year, fips)
               ][order(year, fips), votes]
  
  obs <- acs[state_abbr == state_sel
             & year %in% c(2015, 2012)
             , .(perwt = sum(perwt))
             , .(year, race_sel, inc_sel, educ_sel, county)
             ][order(year, race_sel, educ_sel, inc_sel, race_sel, county)]
  
  obs_full <- acs[year %in% c(2015, 2012)
                  , .N, .(year, race_sel, educ_sel, inc_sel)]
  obs_full <- obs_full[, .(county = obs[, unique(county)])
                       , .(year, race_sel, educ_sel, inc_sel)]
  
  setkey(obs_full, year, race_sel, educ_sel, inc_sel, county)
  setkey(obs, year, race_sel, educ_sel, inc_sel, county)
  obs <- obs[obs_full]
  obs[is.na(perwt) | perwt < 1, perwt := 1]
  setkey(obs, year, educ_sel, inc_sel, race_sel, county)
  rm(obs_full)
  
  theta_prior <- ep_priors[state == state_sel & year %in% c(2012, 2016)
                           ][order(year, educ_sel, inc_sel, race_sel)
                             , logit(turnout)]
  
  
  theta_prior_votes <- ep_priors[state == state_sel & year %in% c(2012, 2016)
                                 ][order(year, educ_sel, inc_sel, race_sel)
                                   , logit(dem)]
  
  yr <- 2
  N <- res[state == state_sel & year == 2016, uniqueN(fips)]
  K <- acs[, .N, .(race_sel, educ_sel, inc_sel)][, .N]
  obs <- array(obs[, perwt], dim = c(N, K, yr))
  theta_prior <- array(theta_prior, dim = c(K, yr))
  theta_prior_votes <- array(theta_prior_votes, dim = c(K, yr))
  theta_prior_sd <- theta_prior
  theta_prior_sd_votes <- theta_prior_votes
  
  # larger prior sd for demographic cells with less population
  obs_pcnt <- apply(obs, c(2, 3), sum) / apply(obs, 3, sum)
  theta_prior_sd <- ifelse(obs_pcnt < .03,  1, .5)
  theta_prior_sd_votes <- ifelse(obs_pcnt < .03,  .5, .5)
  
  
  dat <- 
    list(N = N
         , K = K
         , Yr = yr
         , y = array(round(y), dim = c(N, yr))
         , obs = obs
         , votes = array(round(votes), dim = c(N, yr))
         , theta_prior = theta_prior
         , theta_prior_sd = theta_prior_sd
         , theta_prior_votes = theta_prior_votes
         , theta_prior_sd_votes = theta_prior_sd_votes)
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  state_fit <- stan(file = 'latent_binomial_approx.stan'
                    , data = dat, iter = 2000, chains = 4)
  
  list(dat = dat, state_fit = state_fit)
}

setwd(base_directory)
# loop through and fit each state
for (s in selected_states) { 
  fit <- f_state_fit(s)
  a <- fit$state_fit
  a <- extract(a)
  tbl <- data.table(melt(a$theta_adj))
  setnames(tbl, c('Var2', 'Var3', 'value'), c('param', 'yr', 'theta_adj'))
  tbl2 <- data.table(melt(a$theta_votes_adj))
  setnames(tbl2, c('Var2', 'Var3', 'value'), c('param', 'yr', 'theta_votes_adj'))
  setkey(tbl, iterations, param, yr)
  setkey(tbl2, iterations, param, yr)
  tbl <- tbl2[tbl]
  tbl[, state := s]
  obs <- apply(fit$dat$obs, c(2, 3), sum)
  obs <- as.data.table(melt(obs))
  setnames(obs, c('param', 'yr', 'perwt'))
  setkey(obs, param, yr)
  setkey(tbl, param, yr)
  tbl <- obs[tbl]
  priors <- 
    data.table(melt(fit$dat$theta_prior)
               , theta_prior_sd = array(fit$dat$theta_prior_sd)
               , theta_prior_votes = array(fit$dat$theta_prior_votes)
               , theta_prior_sd_votes = array(fit$dat$theta_prior_sd_votes))
  setnames(priors, 1:3, c('param', 'yr', 'theta_prior'))
  priors[, state := s]
  save(fit, file = paste('./Model Fits/', s, '.rdata', sep = ''))
  fwrite(tbl,  paste('./Model Fits/', s, '.csv', sep = ''))
  fwrite(priors,  paste('./Model Fits/', s, '_prior.csv', sep = ''))
  print(s)
  print(fit$state_fit, pars = 'theta_votes_adj[5,2]')
  rm(tbl2, a, tbl, obs, fit, priors)
  gc()
}
