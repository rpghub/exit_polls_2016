###   script to fit state models

library(data.table)
library(rstan)

# select state
state_sel <- 'PA'

# directory
base_directory <- 'C:/Projects/Post Election'
  
# file paths
acs_loc <- './ACS Data'
res2008_loc <- './County Results/Results 2008'
res2012_loc <- './County Results/Results 2012'
res2016_loc <- './County Results/Results 2016'
exit_loc <- './Exit Polls'
turnout_loc <- './Benchmark Turnout'

# define inv-logit function
inv_logit <- function(x) 1 / (1 + exp(-x))

# create state to region map
state_region <- data.table(state = state.abb, region = state.region)

# acs data
setwd(base_directory)
setwd(acs_loc)
acs <- fread('acs_county.csv', colClasses = 'char')
acs[, perwt := as.numeric(perwt) / 100]
acs[, sex := as.integer(sex)]
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

# exit poll estimates ----

# selected demographics
ep_sel <- ep[question_id_mapped %in% c(260, 917, 1951) 
             & name_party %in% c('democrat', 'total')]
ep_sel[, value := as.numeric(value)]
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
ep_sel[, votes_total := votes * total / 100]
ep_sel[votes_total > perwt, votes_total := perwt]
ep_sel[, votes_dem := votes_total * democrat_cred / 100]

# exit polls by cell for state and year
f_exit_poll_est <- function(state_sel, year_sel) {
  stan_dat <- ep_sel[state == state_sel & year == year_sel]
  stan_dat[, k := .GRP, question_id_mapped]
  stan_dat[, j := seq_len(.N), question_id_mapped]
  stan_dat[, votes_dem := round(votes_dem)]
  stan_dat[, votes_total := round(votes_total)]
  wt <- 
    stan_dat[, .(k, j, category, wt = total / 100
                 , dummy = 1)]
  
  K <- wt[, uniqueN(k)]
  wt_cross <- wt[k == K]
  for (i in (K-1):1) {
    wt_join <- wt[k == i]
    setkey(wt_join, dummy)
    setkey(wt_cross, dummy)
    wt_cross <- wt_join[wt_cross, allow.cartesian = TRUE][i.k != k]
    wt_cross[, wt := wt * i.wt]
    setnames(wt_cross, c('i.k', 'i.j', 'i.wt', 'i.category')
             , paste(c('k', 'j', 'wt', 'cat'), i + 1, sep = ''))
  }
  setnames(wt_cross, c('j', 'category'), c('j1', 'cat1'))
  cols <- paste(rep(c('j', 'cat'), each = 3), 1:K, sep = '')
  cols <- c('wt', cols)
  wt <- wt_cross[, cols, with = FALSE]
  rm(wt_join, wt_cross)
  cols <- ncol(wt)
  
  wt[, state := state_sel]
  wt[, year := year_sel]
  setkey(wt, year, state, cat1, cat2, cat3)
  setkey(acs_state, year, state, educ_sel, race_sel, inc_sel)
  wt <- acs_state[wt]
  wt[, wt := perwt / sum(perwt)]
  wt[, votes_state := round(stan_dat[1, votes])]
  wt[, wt_lim := perwt / votes_state]
  
  y <- dcast.data.table(stan_dat, k ~ j, value.var = 'votes_dem'
                        , fill = 0)
  y[, k := NULL]

  obs <- dcast.data.table(stan_dat, k ~ j, value.var = 'votes_total'
                        , fill = 0)
  obs[, k := NULL]
  
  tot <- dcast.data.table(stan_dat, k ~ j, value.var = 'perwt'
                          , fill = 0)
  tot[, k := NULL]
  
  
  f_wt <- function(x) {
    wt[, wt_fit := x / sum(x)]
    fit1 <- wt[, .(wt_fit = sum(wt_fit), cat = 'educ_sel')
               , .(category = educ_sel)]
    fit2 <- wt[, .(wt_fit = sum(wt_fit), cat = 'inc_sel')
               , .(category = inc_sel)]
    fit3 <- wt[, .(wt_fit = sum(wt_fit), cat = 'race_sel')
               , .(category = race_sel)]
    fit <- rbind(fit1, fit2, fit3)
    setkey(fit, cat, category)
    setkey(stan_dat, cat, category)
    stan_dat[fit, sum((total / 100 - wt_fit)^2)]
  }
  

  wt_fits <- optim(wt[, wt], f_wt, lower = 0, upper = wt[, wt_lim]
                   , method = 'L-BFGS-B')
  wt[, wt_fit := wt_fits$par]

  dat <- 
    list(N  = nrow(stan_dat)
         , K = stan_dat[, uniqueN(k)]
         , N_cells = nrow(wt)
         , J_max = stan_dat[, max(j)]
         , J = stan_dat[, .(J = uniqueN(j)), k][order(k), J]
         , wt = wt[, wt_fit]
         , lookup = wt[, paste('j', 1:K, sep = ''), with = FALSE]
         , y = y
         , obs = obs)
  
  setwd(base_directory)
  
  if (!exists('fit')) {
    fit <<- stan(file = 'demographics.stan', data = dat, iter = 4000
                 , chains = 4
                 , control = list(adapt_delta = .98))
    fit1 <- fit
  } else {
    fit1 <- stan(fit = fit, data = dat, iter = 4000
                 , chains = 4
                 , control = list(adapt_delta = .98))
  }
  a <- extract(fit1)
  wt[, turnout := votes_state * wt_fit / perwt]
  wt[, dem := apply(a$beta, 2, mean)]
  wt[, .(year, state, educ_sel, inc_sel, race_sel, wt = wt_fit, turnout,  dem)]
}


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
             ][order(year, educ_sel, inc_sel, race_sel, county)]
  obs <- dcast.data.table(obs, year + county ~ educ_sel + inc_sel + race_sel
                          , value.var = 'perwt'
                          , fill = 1)
  obs <- melt.data.table(obs, id.vars = c('year', 'county')
                         , value.name = 'perwt'
                         , variable.name = 'category')
  obs[perwt < 1, perwt := 1]
  obs[, perwt := round(perwt)]
  setkey(obs, year, category, county)
  
  theta_prior <- ep_priors[order(year, educ_sel, inc_sel, race_sel)
                           , turnout]
  theta_prior <- ifelse(theta_prior > .995, .995, theta_prior)
  
  theta_prior_votes <- ep_priors[order(year, educ_sel, inc_sel, race_sel)
                                 , dem]
  theta_prior_votes <- ifelse(theta_prior_votes > .995, .995, theta_prior_votes)
  
  # larger prior sd for demographic cells with less population
  obs_pcnt <- apply(dat$obs, c(2, 3), sum) / apply(dat$obs, 3, sum)
  dat$theta_prior_sd <- ifelse(obs_pcnt < .03,  .125, .05)
  dat$theta_prior_sd_votes <- ifelse(obs_pcnt < .03,  .125, .05)
  
  yr <- 2
  N <- res[state == state_sel & year == 2016, uniqueN(fips)]
  K <- acs[, .N, .(race_sel, educ_sel, inc_sel)][, .N]
  obs <- array(obs[, perwt], dim = c(N, K, yr))
  theta_prior <- array(theta_prior, dim = c(K, yr))
  theta_prior_votes <- array(theta_prior_votes, dim = c(K, yr))
  prior_sd <- array(prior_sd, dim = c(K, yr))
  
  dat <- 
    list(N = N
         , K = K
         , Yr = yr
         , y = array(round(y), dim = c(N, yr))
         , obs = obs
         , votes = array(round(votes), dim = c(N, yr))
         , theta_prior = theta_prior
         , theta_prior_sd = prior_sd
         , theta_prior_votes = theta_prior_votes
         , theta_prior_sd_votes = prior_sd)
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  state_fit <- stan(file = 'latent_binomial_approx.stan'
                    , data = dat, iter = 4000, chains = 4)
  
  list(dat = dat, fit = state_fit)
}

# loop through and fit each state
tbl_sims <- data.table(iterations = 1
                       , param = 1
                       , yr = 1
                       , value = 1.0
                       , var = 'theta_adj'
                       , state = 'xx')
tbl_sims <- tbl_sims[state != 'xx']
for (s in state.abb) {
  ep_priors <- 
    mapply(f_exit_poll_est, state_sel = s, year_sel = c(2016, 2012)
           , SIMPLIFY = FALSE)
  ep_priors <- do.call('rbind', ep_priors)
  fit <- f_state_fit(s)
  a <- extract(fit$state_fit)
  tbl <- data.table(melt(a$theta_adj))
  setnames(tbl, c('Var2', 'Var3'), c('param', 'yr'))
  tbl[, var := 'theta_adj']
  tbl2 <- data.table(melt(a$theta_votes_adj))
  setnames(tbl2, c('Var2', 'Var3'), c('param', 'yr'))
  tbl2[, var := 'theta_votes_adj']
  tbl <- rbind(tbl, tbl2)
  tbl[, state := s]
  tbl_sims <- rbind(tbl, tbl_sims)
  rm(tbl2, a, tbl)
  save(fit, file = paste('./Model Fits/', s, '.rdata', sep = ''))
}



