### import ipums data

library(LaF)

setwd('C:/Projects/Post Election/ACS Data/Survey Data')
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


