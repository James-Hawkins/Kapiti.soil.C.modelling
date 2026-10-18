



clm.srs.gen <- data.frame(matrix(ncol=5,nrow=1))


clm.srs.gen$rain.mm <- NA


  
osv.start.date <- as.Date("2018-08-30")
osv.end.date   <- as.Date("2025-12-01")


d.all$cald.mth <- as.numeric(format(  d.all$date , "%m"))


d.all$is.sht.rn <- FALSE
d.all$is.lng.rn <- FALSE
d.all$is.sht.dr <- FALSE
d.all$is.lng.dr <- FALSE

cald.mos.sht.rn <- c(10,11,12) ; cald.mos.lng.rn <- c(3,4,5) ; cald.mos.sht.dr <- c(1,2) ; cald.mos.lng.dr <- c(6,7,8,9) 

d.all[  d.all$cald.mth %in% cald.mos.sht.rn , 'is.sht.rn'] <- TRUE
d.all[  d.all$cald.mth %in% cald.mos.lng.rn , 'is.lng.rn'] <- TRUE
d.all[  d.all$cald.mth %in% cald.mos.lng.dr , 'is.sht.dr'] <- TRUE
d.all[  d.all$cald.mth %in% cald.mos.sht.dr , 'is.lng.dr'] <- TRUE


d.all$season <- NA

ssn.SR <- 'sr' ; ssn.LR <- 'lr' ; ssn.LD <- 'ld' ; ssn.SD <- 'sd'

d.all[d.all$is.sht.rn , 'season'] <- ssn.SR
d.all[d.all$is.lng.rn , 'season'] <- ssn.LR
d.all[d.all$is.sht.dr , 'season'] <- ssn.SD
d.all[d.all$is.lng.dr , 'season'] <- ssn.LD


mean.rf.mm.d.lng.rn <- mean( na.omit(d.all[ d.all$is.lng.rn  , 'precip.osv']))
mean.rf.mm.d.sht.rn <- mean( na.omit(d.all[ d.all$is.sht.rn  , 'precip.osv']))

mean.rf.mm.d.lng.dr <- mean( na.omit(d.all[ d.all$is.lng.dr  , 'precip.osv']))
mean.rf.mm.d.sht.dr <- mean( na.omit(d.all[ d.all$is.sht.dr  , 'precip.osv']))





# FUTURE PROJECTIONS

nr.fut.start <- as.Date("2035-08-30")
nr.fut.end <- as.Date("2050-08-30")

clm.srs.gen.date <- seq(from = nr.fut.start , to = nr.fut.end, by = "day")



# MODIFICATION PARAMETERS



for (d in 1:length(clm.srs.gen.date)){
  
  clm.srs.gen[d,'date'] <- clm.srs.gen.date[d]
  cur.year <- as.numeric(format(  d.all[ d , 'date'] , "%Y"))
  
  # Season identification & period specification
  ssn <- d[d,'season']
  
  if (ssn == ssn.LD) { cur.ssn.start <- str_c(cur.year , '1') ; cur.ssn.end <- str_c(cur.year , '5') }
  
  # Parameter modification
  
  # specify that, within the current season, certain events take place
  
}









for (r in 1:365*7){
  
  
  
  
  
  
}





colnames(d.all)


unique(d.all$period)


.3*mean(   d.all$precip.osv)*365



17/136

colnames(d.all)
