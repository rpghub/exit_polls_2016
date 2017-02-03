
### import ipums data

library(data.table)
library(LaF)

base_directory <- 'C:/Projects/Post Election/exit_polls_2016'


setwd(base_directory)
setwd('./ACS Data')



dat <- 
  laf_open_fwf('usa_00005.dat'
               , column_widths = 
                 c(4, 2, 8, 2, 10, 2, 2, 3, 5, 5, 1, 4, 10, 1, 3, 1, 4, 1
                   , 3, 1, 3, 3, 5, 1, 2, 4, 1, 2, 3, 4, 1, 2, 6, 8, 1, 2
                   , 7, 7, 6)
               , column_types = 
                 c("integer", "integer", "numeric", "integer", "numeric"
                   , "integer", "integer", "integer", "numeric", "numeric"
                   , "integer", "integer", "numeric", "integer", "integer"
                   , "integer", "integer", "integer", "integer", "integer"
                   , "integer", "integer", "numeric", "integer", "integer"
                   , "integer", "integer", "integer", "integer", "integer"
                   , "integer", "integer", "character", "character", "integer"
                   , "integer", "numeric", "numeric", "numeric")
               
               , column_names = 
                 c("year", "datanum", "serial", "numprec", "hhwt", "stateicp"
                   , "statefip", "countyfips", "met2013", "puma", "gq"
                   , "pernum", "perwt", "sex", "age", "marst", "birthyr"
                   , "race", "raced", "hispan", "hispand", "bpl", "bpld"
                   , "citizen", "language", "languaged", "speakeng", "educ"
                   , "educd", "occ", "classwkr", "classwkrd", "occsoc"
                   , "indnaics", "wkswork2", "uhrswork", "inctot", "ftotinc"
                   , "incwage"))
dat <- dat[,]
dat <- as.data.table(dat)
dat[, puma := as.character(puma)]
dat[, puma := substr(paste('00', puma, sep = ''), nchar(puma) + 2 - 4
                     , nchar(puma) + 2)]
dat[, statefip := as.character(statefip)]
dat[nchar(statefip) == 1, statefip := paste('0', statefip, sep = '')]
dat[, acs_id := seq_len(.N)]

setwd('./crosswalks')
puma00 <- fread('puma2000_county2010.csv')
puma00 <- puma00[-1]
puma00[, pop10 := as.numeric(pop10)]
puma00[, afact := as.numeric(afact)]


puma12 <- fread('puma2012_county2010.csv')
puma12 <- puma12[-1]
puma12[, pop10 := as.numeric(pop10)]
puma12[, afact := as.numeric(afact)]

setwd('C:/DATA/tmp/extracts/census')
census <- fread('county_quickfacts.csv')
census[, fips := paste(state_fips, county_fips, sep = '')]
census[field_resp == 'white', race_sel := 'white']
census[field_resp == 'hispanic', race_sel := 'hispanic']
census[field_resp == 'black', race_sel := 'black']
census[field_resp == 'asian', race_sel := 'asian']
census[is.na(race_sel), race_sel := 'other']
census[, county_pop := sum(value * (field_resp == 'total' 
                                     & census_date == '7/1/2015')
                           , na.rm = TRUE)
       , fips]
census <- 
  dcast.data.table(census[field == 'race' & census_date == '7/1/2015']
                   , fips + county_pop ~ race_sel, value.var = 'value'
                   , fun.aggregate = sum)
cols = c('white', 'black', 'asian', 'hispanic', 'other')
census[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x))
       , .SDcols = cols]

census[, (cols) := lapply(.SD, function(x) {
                    x = x / (asian + black + white + hispanic + other)})
       , .SDcols = cols]
census[, county_pop := as.double(county_pop / 1000)]
rm(cols)

# adjust for huricane katrina puma collapse
dat[puma %in% c('01801', '01802', '01905') & statefip == '22'
    , puma := '77777']


# collapse acs data to puma level
dat[, educ_sel := 1 * (educ > 9)]
dat[hispan > 0, race_sel := 'hispanic']
dat[is.na(race_sel) & race == 1, race_sel := 'white']
dat[is.na(race_sel) & race == 2, race_sel := 'black']
dat[is.na(race_sel) & race %in% c(4, 5, 6), race_sel := 'asian']
dat[is.na(race_sel), race_sel := 'other']
dat[marst < 3, marst_sel := 'married']
dat[marst %in% c(3, 4, 5), marst_sel := 'divorced']
dat[marst == 6, marst_sel := 'single']
dat[, inc_sel := 1 * (ftotinc >= 1E5 & ftotinc != 9999999)]
acs_puma <- 
  dat[, .(perwt = sum(perwt))
      , .(age, sex, marst_sel
          , educ_sel
          , race_sel, citizen = citizen < 3
          , inc_sel
          , year, puma, state = statefip)]


# map acs data to county
puma_join00 <- puma00[, .(year = seq(2005, 2011))
                      , .(state, puma = puma2k, county
                          , cntyname, afact)] 

puma_join12 <- puma12[, .(year = seq(2012, 2015))
                      , .(state, puma = puma12, county
                          , cntyname, afact)] 
puma_join <- rbind(puma_join00, puma_join12)
puma_join[, afact := afact / sum(afact), .(state, puma, year)]
rm(puma_join00, puma_join12)

# adjust for huricane katrina puma collapse
puma_join[puma %in% c('01801', '01802', '01905') & state == '22'
          , puma := '77777']


setkey(puma_join, puma, state, year)
setkey(acs_puma, puma, state, year)
acs_puma <- puma_join[acs_puma, allow.cartesian = TRUE]

# merge in county race totals
setkey(census, fips)
setkey(acs_puma, county)
acs_puma <- census[acs_puma]
setnames(acs_puma, 'fips', 'county')

# fix counties with missing data with puma average
puma_agg <- acs_puma[, .(perwt = sum(perwt))
                     , .(puma, state, county, asian, black, hispanic, other
                         , white)]
puma_agg <- puma_agg[!is.na(white)
                     , .(agg_asian = weighted.mean(asian, perwt)
                         , agg_black = weighted.mean(black, perwt)
                         , agg_hispanic = weighted.mean(hispanic, perwt)
                         , agg_white = weighted.mean(white, perwt)
                         , agg_other = weighted.mean(other, perwt))
                     , .(puma, state)]
setkey(puma_agg, puma, state)
setkey(acs_puma, puma, state)
acs_puma <- puma_agg[acs_puma]
acs_puma[is.na(white), white := agg_white]
acs_puma[is.na(black), black := agg_black]
acs_puma[is.na(asian), asian := agg_asian]
acs_puma[is.na(hispanic), hispanic := agg_hispanic]
acs_puma[is.na(other), other := agg_other]
cols <- c('agg_black', 'agg_hispanic', 'agg_white', 'agg_other', 'agg_asian')
acs_puma[, (cols) :=  NULL]
rm(puma_agg, cols)

# acs records to match county race distribution
acs_puma[race_sel == 'white', race_adj := white]
acs_puma[race_sel == 'black', race_adj := black]
acs_puma[race_sel == 'asian', race_adj := asian]
acs_puma[race_sel == 'hispanic', race_adj := hispanic]
acs_puma[race_sel == 'other', race_adj := other]
cols <- c('white', 'black', 'asian', 'hispanic', 'other')
#acs_puma[, (cols) := NULL]
rm(cols)

acs_puma[, wt_grp := .GRP, .(state, puma, year, age, sex, marst_sel
                             , educ_sel, race_sel, citizen, inc_sel)]
setkey(acs_puma, wt_grp)
acs_puma[, puma_race_tot := sum(race_adj), wt_grp]
acs_puma[puma_race_tot == 0L, race_adj := 1]  ## if no race data just prorate by pop
acs_puma[, puma_race_tot := sum(race_adj), wt_grp]
acs_puma[, race_adj := race_adj / puma_race_tot]
acs_puma[, race_adj := race_adj / sum(race_adj), wt_grp]
acs_puma[, adj := afact / sum(afact), wt_grp]
acs_puma[, adj := (adj * race_adj) / sum(adj * race_adj), wt_grp]
acs_puma[is.na(adj), adj := 1 / .N, wt_grp]
acs_puma[, perwt := perwt * adj]  ## proportion weight rel to county race %

# save for export
acs_puma <- acs_puma[age >= 18 & citizen == TRUE]
acs_puma <- 
  acs_puma[, .(perwt = sum(perwt))
           , .(state, county, cntyname, year
               #, age = cut(age, c(18, 30, 45, 65, Inf), right = FALSE)
               #, sex
               #, marst_sel
               , educ_sel, race_sel, inc_sel)]


write.csv(acs_puma, 'acs_county.csv', row.names = FALSE)


