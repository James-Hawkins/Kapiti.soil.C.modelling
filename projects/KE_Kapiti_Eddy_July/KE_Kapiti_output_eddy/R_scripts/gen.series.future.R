
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



{

osv.start.date <- as.Date("2018-08-30")
osv.end.date   <- as.Date("2025-12-01")

  
d.all$cald.mth <- as.numeric(format(  d.all$date , "%m"))
d.all$cald.yr <- as.numeric(format(  d.all$date , "%Y"))


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

d.all$wet.day <- FALSE
d.all[  d.all$precip.osv > 0  , 'wet.day'] <- TRUE

d.all$day_20mm <- FALSE
d.all[  d.all$precip.osv > 20  , 'day_20mm'] <- TRUE


ssn.SR <- 'sr' ; ssn.LR <- 'lr' ; ssn.LD <- 'ld' ; ssn.SD <- 'sd'  ; ssns.rny <- c(  ssn.LR , ssn.SR)

d.all[d.all$is.sht.rn , 'season'] <- ssn.SR
d.all[d.all$is.lng.rn , 'season'] <- ssn.LR
d.all[d.all$is.sht.dr , 'season'] <- ssn.SD
d.all[d.all$is.lng.dr , 'season'] <- ssn.LD



# OBSERVATION PERIOD - SUMMARY STATS

# global row conditions - d.all

# seasons
r.cnd.2019.lr <- d.all$cald.yr == '2019' & d.all$is.lng.rn 
r.cnd.2020.lr <- d.all$cald.yr == '2020' & d.all$is.lng.rn 
r.cnd.2021.lr <- d.all$cald.yr == '2021' & d.all$is.lng.rn 
r.cnd.2022.lr <- d.all$cald.yr == '2022' & d.all$is.lng.rn 
r.cnd.2023.lr <- d.all$cald.yr == '2023' & d.all$is.lng.rn 
r.cnd.2024.lr <- d.all$cald.yr == '2024' & d.all$is.lng.rn 

r.cnd.2019.sr <- d.all$cald.yr == '2019' & d.all$is.sht.rn
r.cnd.2020.sr <- d.all$cald.yr == '2020' & d.all$is.sht.rn 
r.cnd.2021.sr <- d.all$cald.yr == '2021' & d.all$is.sht.rn 
r.cnd.2022.sr <- d.all$cald.yr == '2022' & d.all$is.sht.rn
r.cnd.2023.sr <- d.all$cald.yr == '2023' & d.all$is.sht.rn 
r.cnd.2024.sr <- d.all$cald.yr == '2024' & d.all$is.sht.rn 

r.cnd.2019.ld <- d.all$cald.yr == '2019' & d.all$is.lng.dr 
r.cnd.2020.ld <- d.all$cald.yr == '2020' & d.all$is.lng.dr
r.cnd.2021.ld <- d.all$cald.yr == '2021' & d.all$is.lng.dr 
r.cnd.2022.ld <- d.all$cald.yr == '2022' & d.all$is.lng.dr 
r.cnd.2023.ld <- d.all$cald.yr == '2023' & d.all$is.lng.dr 
r.cnd.2024.ld <- d.all$cald.yr == '2024' & d.all$is.lng.dr 

r.cnd.2019.sd <- d.all$cald.yr == '2019' & d.all$is.sht.dr
r.cnd.2020.sd <- d.all$cald.yr == '2020' & d.all$is.sht.dr 
r.cnd.2021.sd <- d.all$cald.yr == '2021' & d.all$is.sht.dr 
r.cnd.2022.sd <- d.all$cald.yr == '2022' & d.all$is.sht.dr
r.cnd.2023.sd <- d.all$cald.yr == '2023' & d.all$is.sht.dr 
r.cnd.2024.sd <- d.all$cald.yr == '2024' & d.all$is.sht.dr 


# wet days within seasons
r.cnd.2019.lr.wet.d <- d.all$cald.yr == '2019' & d.all$is.lng.rn & d.all$wet.day
r.cnd.2020.lr.wet.d  <- d.all$cald.yr == '2020' & d.all$is.lng.rn & d.all$wet.day
r.cnd.2021.lr.wet.d  <- d.all$cald.yr == '2021' & d.all$is.lng.rn & d.all$wet.day
r.cnd.2022.lr.wet.d  <- d.all$cald.yr == '2022' & d.all$is.lng.rn & d.all$wet.day
r.cnd.2023.lr.wet.d  <- d.all$cald.yr == '2023' & d.all$is.lng.rn & d.all$wet.day
r.cnd.2024.lr.wet.d  <- d.all$cald.yr == '2024' & d.all$is.lng.rn & d.all$wet.day

r.cnd.2019.sr.wet.d  <- d.all$cald.yr == '2019' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2020.sr.wet.d  <- d.all$cald.yr == '2020' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2021.sr.wet.d  <- d.all$cald.yr == '2021' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2022.sr.wet.d  <- d.all$cald.yr == '2022' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2023.sr.wet.d  <- d.all$cald.yr == '2023' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2024.sr.wet.d  <- d.all$cald.yr == '2024' & d.all$is.sht.rn & d.all$wet.day

r.cnd.2019.ld.wet.d  <- d.all$cald.yr == '2019' & d.all$is.lng.dr & d.all$wet.day
r.cnd.2020.ld.wet.d  <- d.all$cald.yr == '2020' & d.all$is.lng.dr & d.all$wet.day
r.cnd.2021.ld.wet.d  <- d.all$cald.yr == '2021' & d.all$is.lng.dr & d.all$wet.day
r.cnd.2022.ld.wet.d  <- d.all$cald.yr == '2022' & d.all$is.lng.dr & d.all$wet.day
r.cnd.2023.ld.wet.d  <- d.all$cald.yr == '2023' & d.all$is.lng.dr & d.all$wet.day
r.cnd.2024.ld.wet.d  <- d.all$cald.yr == '2024' & d.all$is.lng.dr & d.all$wet.day

r.cnd.2019.sd.wet.d  <- d.all$cald.yr == '2019' & d.all$is.sht.dr & d.all$wet.day
r.cnd.2020.sd.wet.d  <- d.all$cald.yr == '2020' & d.all$is.sht.dr & d.all$wet.day
r.cnd.2021.sd.wet.d  <- d.all$cald.yr == '2021' & d.all$is.sht.dr & d.all$wet.day
r.cnd.2022.sd.wet.d  <- d.all$cald.yr == '2022' & d.all$is.sht.dr & d.all$wet.day
r.cnd.2023.sd.wet.d  <- d.all$cald.yr == '2023' & d.all$is.sht.dr & d.all$wet.day
r.cnd.2024.sd.wet.d  <- d.all$cald.yr == '2024' & d.all$is.sht.dr & d.all$wet.day

# wet days with >= 20 mm rainfall
r.cnd.2019.lr.20mm <- d.all$cald.yr == '2019' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2020.lr.20mm  <- d.all$cald.yr == '2020' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2021.lr.20mm  <- d.all$cald.yr == '2021' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2022.lr.20mm  <- d.all$cald.yr == '2022' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2023.lr.20mm  <- d.all$cald.yr == '2023' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2024.lr.20mm  <- d.all$cald.yr == '2024' & d.all$is.lng.rn & d.all$day_20mm

r.cnd.2019.sr.20mm  <- d.all$cald.yr == '2019' & d.all$is.sht.rn & d.all$day_20mm
r.cnd.2020.sr.20mm  <- d.all$cald.yr == '2020' & d.all$is.sht.rn & d.all$day_20mm
r.cnd.2021.sr.20mm  <- d.all$cald.yr == '2021' & d.all$is.sht.rn & d.all$day_20mm
r.cnd.2022.sr.20mm  <- d.all$cald.yr == '2022' & d.all$is.sht.rn & d.all$day_20mm
r.cnd.2023.sr.20mm  <- d.all$cald.yr == '2023' & d.all$is.sht.rn & d.all$day_20mm
r.cnd.2024.sr.20mm  <- d.all$cald.yr == '2024' & d.all$is.sht.rn & d.all$day_20mm

r.cnd.2019.ld.20mm  <- d.all$cald.yr == '2019' & d.all$is.lng.dr & d.all$day_20mm
r.cnd.2020.ld.20mm  <- d.all$cald.yr == '2020' & d.all$is.lng.dr & d.all$day_20mm
r.cnd.2021.ld.20mm  <- d.all$cald.yr == '2021' & d.all$is.lng.dr & d.all$day_20mm
r.cnd.2022.ld.20mm  <- d.all$cald.yr == '2022' & d.all$is.lng.dr & d.all$day_20mm
r.cnd.2023.ld.20mm  <- d.all$cald.yr == '2023' & d.all$is.lng.dr & d.all$day_20mm
r.cnd.2024.ld.20mm  <- d.all$cald.yr == '2024' & d.all$is.lng.dr & d.all$day_20mm

r.cnd.2019.sd.20mm  <- d.all$cald.yr == '2019' & d.all$is.sht.dr & d.all$day_20mm
r.cnd.2020.sd.20mm  <- d.all$cald.yr == '2020' & d.all$is.sht.dr & d.all$day_20mm
r.cnd.2021.sd.20mm  <- d.all$cald.yr == '2021' & d.all$is.sht.dr & d.all$day_20mm
r.cnd.2022.sd.20mm  <- d.all$cald.yr == '2022' & d.all$is.sht.dr & d.all$day_20mm
r.cnd.2023.sd.20mm  <- d.all$cald.yr == '2023' & d.all$is.sht.dr & d.all$day_20mm
r.cnd.2024.sd.20mm  <- d.all$cald.yr == '2024' & d.all$is.sht.dr & d.all$day_20mm


mean.rf.mm.d.lng.rn <- mean( na.omit(d.all[ d.all$is.lng.rn  , 'precip.osv']))
mean.rf.mm.d.sht.rn <- mean( na.omit(d.all[ d.all$is.sht.rn  , 'precip.osv']))

mean.rf.mm.d.lng.dr <- mean( na.omit(d.all[ d.all$is.lng.dr  , 'precip.osv']))
mean.rf.mm.d.sht.dr <- mean( na.omit(d.all[ d.all$is.sht.dr  , 'precip.osv']))

# TOTAL RAINFALL

# Long rains
total.rainf.mm.lng.rn.2019 <- sum(  d.all[ r.cnd.2019.lr.wet.d , 'precip.osv']  )
total.rainf.mm.lng.rn.2020 <- sum(  d.all[ r.cnd.2020.lr.wet.d , 'precip.osv']  )
total.rainf.mm.lng.rn.2021 <- sum(  d.all[ r.cnd.2021.lr.wet.d , 'precip.osv']  )
total.rainf.mm.lng.rn.2022 <- sum(  d.all[ r.cnd.2022.lr.wet.d , 'precip.osv']  )
total.rainf.mm.lng.rn.2023 <- sum(  d.all[ r.cnd.2023.lr.wet.d , 'precip.osv']  )
total.rainf.mm.lng.rn.2024 <- sum(  d.all[ r.cnd.2024.lr.wet.d , 'precip.osv']  )

total.rainf.mm.lng.rn.osv.all <- mean(
  total.rainf.mm.lng.rn.2019 
  ,total.rainf.mm.lng.rn.2020 
  ,total.rainf.mm.lng.rn.2021 
  ,total.rainf.mm.lng.rn.2022 
  ,total.rainf.mm.lng.rn.2023 
  ,total.rainf.mm.lng.rn.2024 
)

# Short rains
total.rainf.mm.sht.rn.2019 <- sum(  d.all[ r.cnd.2019.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2020 <- sum(  d.all[ r.cnd.2020.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2021 <- sum(  d.all[ r.cnd.2021.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2022 <- sum(  d.all[ r.cnd.2022.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2023 <- sum(  d.all[ r.cnd.2023.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2024 <- sum(  d.all[ r.cnd.2024.sr.wet.d , 'precip.osv']  )

total.rainf.mm.sht.rn.osv.all <- mean(
  total.rainf.mm.sht.rn.2019 
  ,total.rainf.mm.sht.rn.2020 
  ,total.rainf.mm.sht.rn.2021 
  ,total.rainf.mm.sht.rn.2022 
  ,total.rainf.mm.sht.rn.2023 
  ,total.rainf.mm.sht.rn.2024 
)

{
  
# Long rains
quant.wet.days.lng.rn.2019 <- sum(  r.cnd.2019.lr.wet.d  )
quant.wet.days.lng.rn.2020 <- sum(  r.cnd.2020.lr.wet.d )
quant.wet.days.lng.rn.2021 <- sum(  r.cnd.2021.lr.wet.d  )
quant.wet.days.lng.rn.2022 <- sum(  r.cnd.2022.lr.wet.d )
quant.wet.days.lng.rn.2023 <- sum(  r.cnd.2023.lr.wet.d  )
quant.wet.days.lng.rn.2024 <- sum(  r.cnd.2024.lr.wet.d  )

quant.wet.days.lng.rn.osv.mean <- mean( quant.wet.days.lng.rn.2019
                                        , quant.wet.days.lng.rn.2020
                                        , quant.wet.days.lng.rn.2021
                                        , quant.wet.days.lng.rn.2022
                                        , quant.wet.days.lng.rn.2023
                                        , quant.wet.days.lng.rn.2024
                                        )

quant.wet.days.lng.rn.osv.sd <- sd( c(quant.wet.days.lng.rn.2019
                                        , quant.wet.days.lng.rn.2020
                                        , quant.wet.days.lng.rn.2021
                                        , quant.wet.days.lng.rn.2022
                                        , quant.wet.days.lng.rn.2023
                                        , quant.wet.days.lng.rn.2024 ) 
)


# O-N-D (short) rains
quant.wet.days.sht.rn.2019 <- sum( r.cnd.2019.sr.wet.d )  
quant.wet.days.sht.rn.2020 <- sum( r.cnd.2020.sr.wet.d )
quant.wet.days.sht.rn.2021 <- sum( r.cnd.2021.sr.wet.d )
quant.wet.days.sht.rn.2022 <- sum( r.cnd.2022.sr.wet.d )
quant.wet.days.sht.rn.2023 <- sum( r.cnd.2023.sr.wet.d )
quant.wet.days.sht.rn.2024 <- sum( r.cnd.2024.sr.wet.d )

quant.wet.days.sht.rn.osv.mean <- mean( quant.wet.days.sht.rn.2019
                                        , quant.wet.days.sht.rn.2020
                                        , quant.wet.days.sht.rn.2021
                                        , quant.wet.days.sht.rn.2022
                                        , quant.wet.days.sht.rn.2023
                                        , quant.wet.days.sht.rn.2024
)

quant.wet.days.sht.rn.osv.sd <- sd( c(quant.wet.days.sht.rn.2019
                                      , quant.wet.days.sht.rn.2020
                                      , quant.wet.days.sht.rn.2021
                                      , quant.wet.days.sht.rn.2022
                                      , quant.wet.days.sht.rn.2023
                                      , quant.wet.days.sht.rn.2024 ) 
)





# Long rains
mean.per.wet.day.mm.lng.rn.2019 <- mean(  d.all[ r.cnd.2019.lr.wet.d  , 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2020 <- mean(  d.all[ r.cnd.2020.lr.wet.d, 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2021 <- mean(  d.all[ r.cnd.2021.lr.wet.d , 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2022 <- mean(  d.all[ r.cnd.2022.lr.wet.d , 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2023 <- mean(  d.all[ r.cnd.2023.lr.wet.d, 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2024 <- mean(  d.all[ r.cnd.2024.lr.wet.d , 'precip.osv']  )


mean.per.wet.day.mm.lng.rn.osv.all <- mean(
  
  mean.per.wet.day.mm.lng.rn.2019
  , mean.per.wet.day.mm.lng.rn.2020
  , mean.per.wet.day.mm.lng.rn.2021
  , mean.per.wet.day.mm.lng.rn.2022
  , mean.per.wet.day.mm.lng.rn.2023
  , mean.per.wet.day.mm.lng.rn.2024
  
)

# Standard deviation rain per wet day
sd.per.wet.day.mm.lng.rn.2019 <- sd(  d.all[ r.cnd.2019.lr.wet.d , 'precip.osv']  )
sd.per.wet.day.mm.lng.rn.2020 <- sd(  d.all[ r.cnd.2020.lr.wet.d , 'precip.osv']  )
sd.per.wet.day.mm.lng.rn.2021 <- sd(  d.all[ r.cnd.2021.lr.wet.d, 'precip.osv']  )
sd.per.wet.day.mm.lng.rn.2022 <- sd(  d.all[ r.cnd.2022.lr.wet.d , 'precip.osv']  )
sd.per.wet.day.mm.lng.rn.2023 <- sd(  d.all[ r.cnd.2023.lr.wet.d , 'precip.osv']  )
sd.per.wet.day.mm.lng.rn.2024 <- sd(  d.all[ r.cnd.2024.lr.wet.d , 'precip.osv']  )

  
sd.per.wet.day.mm.lng.rn.osv.all <- sd(  d.all[ d.all$is.lng.rn  & d.all$wet.day , 'precip.osv'] ) 



# Short rains
mean.per.wet.day.mm.sht.rn.2019 <- mean(  d.all[ r.cnd.2019.sr.wet.d , 'precip.osv']  )
mean.per.wet.day.mm.sht.rn.2020 <- mean(  d.all[ r.cnd.2020.sr.wet.d , 'precip.osv']  )
mean.per.wet.day.mm.sht.rn.2021 <- mean(  d.all[ r.cnd.2021.sr.wet.d , 'precip.osv']  )
mean.per.wet.day.mm.sht.rn.2022 <- mean(  d.all[ r.cnd.2022.sr.wet.d , 'precip.osv']  )
mean.per.wet.day.mm.sht.rn.2023 <- mean(  d.all[ r.cnd.2023.sr.wet.d , 'precip.osv']  )
mean.per.wet.day.mm.sht.rn.2024 <- mean(  d.all[ r.cnd.2024.sr.wet.d , 'precip.osv']  )

mean.per.wet.day.mm.sht.rn.osv.all <- mean(
  
  mean.per.wet.day.mm.sht.rn.2019
  , mean.per.wet.day.mm.sht.rn.2020
  , mean.per.wet.day.mm.sht.rn.2021
  , mean.per.wet.day.mm.sht.rn.2022
  , mean.per.wet.day.mm.sht.rn.2023
  , mean.per.wet.day.mm.sht.rn.2024
  
)


  
# Standard deviation rain per wet day
sd.per.wet.day.mm.sht.rn.2019 <- sd(  d.all[ r.cnd.2019.sr.wet.d , 'precip.osv']  )
sd.per.wet.day.mm.sht.rn.2020 <- sd(  d.all[ r.cnd.2020.sr.wet.d, 'precip.osv']  )
sd.per.wet.day.mm.sht.rn.2021 <- sd(  d.all[ r.cnd.2021.sr.wet.d , 'precip.osv']  )
sd.per.wet.day.mm.sht.rn.2022 <- sd(  d.all[ r.cnd.2022.sr.wet.d , 'precip.osv']  )
sd.per.wet.day.mm.sht.rn.2023 <- sd(  d.all[ r.cnd.2023.sr.wet.d , 'precip.osv']  )
sd.per.wet.day.mm.sht.rn.2024 <- sd(  d.all[ r.cnd.2024.sr.wet.d , 'precip.osv']  )

sd.per.wet.day.mm.sht.rn.osv.all <- sd(  d.all[ d.all$is.sht.rn  & d.all$wet.day , 'precip.osv']  )


} #  - - - - WET DAYS


{
  
# Quantity of days per season  
# MAM 
quant.20.mm.days.lng.rn.2019 <-  sum(  r.cnd.2019.lr.20mm )
quant.20.mm.days.lng.rn.2020 <-  sum(  r.cnd.2020.lr.20mm )
quant.20.mm.days.lng.rn.2021 <-  sum(  r.cnd.2021.lr.20mm )
quant.20.mm.days.lng.rn.2022 <-  sum(  r.cnd.2022.lr.20mm )
quant.20.mm.days.lng.rn.2023 <-  sum(  r.cnd.2023.lr.20mm )
quant.20.mm.days.lng.rn.2024 <-  sum(  r.cnd.2024.lr.20mm )

quant.20.mm.days.lng.rn.osv.mean <- mean(
  
  quant.20.mm.days.lng.rn.2019 
  , quant.20.mm.days.lng.rn.2020
  , quant.20.mm.days.lng.rn.2021 
  , quant.20.mm.days.lng.rn.2022 
  , quant.20.mm.days.lng.rn.2023 
  , quant.20.mm.days.lng.rn.2024 
  
)

quant.20.mm.days.lng.rn.osv.sd <- sd( c(
  
  quant.20.mm.days.lng.rn.2019 
  , quant.20.mm.days.lng.rn.2020
  , quant.20.mm.days.lng.rn.2021 
  , quant.20.mm.days.lng.rn.2022 
  , quant.20.mm.days.lng.rn.2023 
  , quant.20.mm.days.lng.rn.2024 
)
)


# MEAN RAINF per 20 mm day
# query: are these mutually exclusive or inclusive?
# Note: for some of the below, zero observations --> omit from period average


# Long rains
mean.per.20.mm.day.lng.rn.2019 <- mean(  d.all[ r.cnd.2019.lr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.lng.rn.2020 <- mean(  d.all[ r.cnd.2020.lr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.lng.rn.2021 <- mean(  d.all[ r.cnd.2021.lr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.lng.rn.2022 <- mean(  d.all[ r.cnd.2022.lr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.lng.rn.2023 <- mean(  d.all[ r.cnd.2023.lr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.lng.rn.2024 <- mean(  d.all[ r.cnd.2024.lr.20mm  , 'precip.osv']  )


mean.per.20.mm.day.lng.rn.osv.all <- mean(
  
  mean.per.20.mm.day.lng.rn.2019
  , mean.per.20.mm.day.lng.rn.2020
  , mean.per.20.mm.day.lng.rn.2021
  , mean.per.20.mm.day.lng.rn.2022
  , mean.per.20.mm.day.lng.rn.2023
  , mean.per.20.mm.day.lng.rn.2024
  
)

# Short rains
mean.per.20.mm.day.sht.rn.2019 <- mean(  d.all[ r.cnd.2019.sr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.sht.rn.2020 <- mean(  d.all[ r.cnd.2020.sr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.sht.rn.2021 <- mean(  d.all[ r.cnd.2021.sr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.sht.rn.2022 <- mean(  d.all[ r.cnd.2022.sr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.sht.rn.2023 <- mean(  d.all[ r.cnd.2023.sr.20mm  , 'precip.osv']  )
mean.per.20.mm.day.sht.rn.2024 <- mean(  d.all[ r.cnd.2024.sr.20mm  , 'precip.osv']  )


mean.per.20.mm.day.sht.rn.osv.all <- mean(
  
  mean.per.20.mm.day.sht.rn.2019
  # , mean.per.20.mm.day.sht.rn.2020
  #, mean.per.20.mm.day.sht.rn.2021
  #, mean.per.20.mm.day.sht.rn.2022
  , mean.per.20.mm.day.sht.rn.2023
  , mean.per.20.mm.day.sht.rn.2024
  
)

# Standard deviation rain per wet day
# Note: for some of the below, only 1 observation --> default to period average

# Long rains
sd.per.20.mm.day.lng.rn.2019 <- sd(  d.all[ r.cnd.2019.lr.20mm  , 'precip.osv']  )
sd.per.20.mm.day.lng.rn.2020 <- sd(  d.all[ r.cnd.2020.lr.20mm, 'precip.osv']  )
sd.per.20.mm.day.lng.rn.2021 <- sd(  d.all[ r.cnd.2021.lr.20mm , 'precip.osv']  )
sd.per.20.mm.day.lng.rn.2022 <- sd(  d.all[ r.cnd.2022.lr.20mm , 'precip.osv']  )
sd.per.20.mm.day.lng.rn.2023 <- sd(  d.all[ r.cnd.2023.lr.20mm , 'precip.osv']  )
sd.per.20.mm.day.lng.rn.2024 <- sd(  d.all[ r.cnd.2024.lr.20mm, 'precip.osv']  )


sd.per.20.mm.day.lng.rn.osv.all <- sd(  d.all[ d.all$is.lng.rn  & d.all$day_20mm , 'precip.osv']  )


# Short  rains
sd.per.20.mm.day.sht.rn.2019 <- sd(  d.all[ r.cnd.2019.sr.20mm  , 'precip.osv']  )
sd.per.20.mm.day.sht.rn.2020 <- sd(  d.all[ r.cnd.2020.sr.20mm , 'precip.osv']  )
sd.per.20.mm.day.sht.rn.2021 <- sd(  d.all[ r.cnd.2021.sr.20mm , 'precip.osv']  )
sd.per.20.mm.day.sht.rn.2022 <- sd(  d.all[ r.cnd.2022.sr.20mm , 'precip.osv']  )
sd.per.20.mm.day.sht.rn.2023 <- sd(  d.all[ r.cnd.2023.sr.20mm , 'precip.osv']  )
sd.per.20.mm.day.sht.rn.2024 <- sd(  d.all[ r.cnd.2024.sr.20mm, 'precip.osv']  )


sd.per.20.mm.day.sht.rn.osv.all <- sd(  d.all[ d.all$is.sht.rn  & d.all$day_20mm , 'precip.osv']  )


} # - - - - 20 mm

# EXTREME RAIN EVENTS -- frequency and severity




{
#  95 percentiles - rainfall values (mm/d)

# M - A - M rains
pctl.95.lng.rn.2019 <- as.numeric( quantile( d.all[  r.cnd.2019.lr.wet.d ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.lng.rn.2020 <- as.numeric( quantile( d.all[  r.cnd.2020.lr.wet.d ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.lng.rn.2021 <- as.numeric( quantile( d.all[  r.cnd.2021.lr.wet.d ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.lng.rn.2022 <- as.numeric( quantile( d.all[  r.cnd.2022.lr.wet.d ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.lng.rn.2023 <- as.numeric( quantile( d.all[  r.cnd.2023.lr.wet.d ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.lng.rn.2024 <- as.numeric( quantile( d.all[  r.cnd.2024.lr.wet.d ,  'precip.osv' ]  , probs = c( 0.95)))


sequence.95.pct.lng.rn.osv <- c(  pctl.95.lng.rn.2019 
                                  , pctl.95.lng.rn.2020
                                  , pctl.95.lng.rn.2021
                                  , pctl.95.lng.rn.2022
                                  , pctl.95.lng.rn.2023
                                  , pctl.95.lng.rn.2024)

rainf.pctl.95.lng.rn.osv <- mean( sequence.95.pct.lng.rn.osv )

rainf.max.lng.rn.osv <- max( sequence.95.pct.lng.rn.osv )


# O - N - D rains
pctl.95.sht.rn.2019 <- as.numeric( quantile( d.all[  r.cnd.2019.sr.wet.d    ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.sht.rn.2020 <- as.numeric( quantile( d.all[  r.cnd.2020.sr.wet.d   ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.sht.rn.2021 <- as.numeric( quantile( d.all[  r.cnd.2021.sr.wet.d   ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.sht.rn.2022 <- as.numeric( quantile( d.all[  r.cnd.2022.sr.wet.d    ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.sht.rn.2023 <- as.numeric( quantile( d.all[  r.cnd.2023.sr.wet.d   ,  'precip.osv' ]  , probs = c( 0.95)))
pctl.95.sht.rn.2024 <- as.numeric( quantile( d.all[  r.cnd.2024.sr.wet.d   ,  'precip.osv' ]  , probs = c( 0.95)))

sequence.95.pct.sht.rn.osv <- c(  pctl.95.sht.rn.2019 
                                  , pctl.95.sht.rn.2020
                                  , pctl.95.sht.rn.2021
                                  , pctl.95.sht.rn.2022
                                  , pctl.95.sht.rn.2023
                                  , pctl.95.sht.rn.2024)


rainf.pctl.95.sht.rn.osv <- mean( sequence.95.pct.sht.rn.osv)

rainf.max.sht.rn.osv <- max( sequence.95.pct.sht.rn.osv)


# Query: should the 95th percentile value used below be a long run average or a single year value?
#  Quant 95P events per year
# MAM rains
quant.95p.days.lng.rn.2019 <- sum( r.cnd.2019.lr.wet.d &  d.all$precip.osv >= rainf.pctl.95.lng.rn.osv  )
quant.95p.days.lng.rn.2020 <- sum( r.cnd.2020.lr.wet.d &  d.all$precip.osv >= rainf.pctl.95.lng.rn.osv )
quant.95p.days.lng.rn.2021 <- sum( r.cnd.2021.lr.wet.d &  d.all$precip.osv >= rainf.pctl.95.lng.rn.osv  )
quant.95p.days.lng.rn.2022 <- sum( r.cnd.2022.lr.wet.d &  d.all$precip.osv >= rainf.pctl.95.lng.rn.osv  )
quant.95p.days.lng.rn.2023 <- sum( r.cnd.2023.lr.wet.d &  d.all$precip.osv >= rainf.pctl.95.lng.rn.osv  )
quant.95p.days.lng.rn.2024 <- sum( r.cnd.2024.lr.wet.d &  d.all$precip.osv >= rainf.pctl.95.lng.rn.osv  )


# OND rains
quant.95p.days.sht.rn.2019 <- sum( r.cnd.2019.sr.wet.d &  d.all$precip.osv >= rainf.pctl.95.sht.rn.osv )
quant.95p.days.sht.rn.2020 <- sum( r.cnd.2020.sr.wet.d &  d.all$precip.osv >= rainf.pctl.95.sht.rn.osv )
quant.95p.days.sht.rn.2021 <- sum( r.cnd.2021.sr.wet.d &  d.all$precip.osv >= rainf.pctl.95.sht.rn.osv )
quant.95p.days.sht.rn.2022 <- sum( r.cnd.2022.sr.wet.d &  d.all$precip.osv >= rainf.pctl.95.sht.rn.osv )
quant.95p.days.sht.rn.2023 <- sum( r.cnd.2023.sr.wet.d &  d.all$precip.osv >= rainf.pctl.95.sht.rn.osv )
quant.95p.days.sht.rn.2024 <- sum( r.cnd.2024.sr.wet.d &  d.all$precip.osv >= rainf.pctl.95.sht.rn.osv )

# MEAN 95P days across all observed years - long rains
mean.95p.days.lng.rn.osv <- mean( 
  
  quant.95p.days.lng.rn.2019
  , quant.95p.days.lng.rn.2020
  , quant.95p.days.lng.rn.2021
  , quant.95p.days.lng.rn.2022 
  , quant.95p.days.lng.rn.2023
  , quant.95p.days.lng.rn.2024
  
  )


mean.95p.days.sht.rn.osv <- mean( 
  
  quant.95p.days.sht.rn.2019
  , quant.95p.days.sht.rn.2020
  , quant.95p.days.sht.rn.2021
  , quant.95p.days.sht.rn.2022 
  , quant.95p.days.sht.rn.2023
  , quant.95p.days.sht.rn.2024
  
)



# Standard deviation in 95P across all observed years
# Long rains
sd.95p.days.lng.rn.osv <- sd( c(
  
  quant.95p.days.lng.rn.2019
  , quant.95p.days.lng.rn.2020
  , quant.95p.days.lng.rn.2021
  , quant.95p.days.lng.rn.2022 
  , quant.95p.days.lng.rn.2023
  , quant.95p.days.lng.rn.2024
)
)

# Short rains
sd.95p.days.sht.rn.osv <- sd( c(
  
  quant.95p.days.sht.rn.2019
  , quant.95p.days.sht.rn.2020
  , quant.95p.days.sht.rn.2021
  , quant.95p.days.sht.rn.2022 
  , quant.95p.days.sht.rn.2023
  , quant.95p.days.sht.rn.2024
)
)

} # 95th - PERCENTILE RAIN EVENTS

}  # Data prep.



# ~ ~ ~ ~ FUTURE PROJECTIONS ~ ~ ~ ~
clm.srs <- data.frame(  matrix( ncol = 1 , nrow = 1 ))

clm.srs <- clm.srs[-c(1)]
clm.srs$rain.mm <- NA


nr.fut.start <- as.Date("2035-10-01")
nr.fut.end <- as.Date("2050-08-30")

clm.srs.gen.date <- seq(  from = nr.fut.start , to = nr.fut.end  ,  by = "day")

for (  d in 1 : length( clm.srs.gen.date )  )   { clm.srs[d,'date'] <- clm.srs.gen.date[d]    } 



# MODIFICATION PARAMETERS

for ( d in 1 : length( clm.srs.gen.date ) )   {
  
  # test: d <- 1
  
# 
{
  
  date <- clm.srs.gen.date[d] 
  
  
  cur.year <-  as.numeric(format(  date , "%Y"))  
  cur.month <-  as.numeric(format(  date , "%m"))  
  
  
  # Season identification & period specification
  #ssn <- d.all[d,'season']
  

  if (cur.month  %in% cald.mos.lng.dr) { cur.ssn.start <- str_c(cur.year , '-01' , '-01') ; cur.ssn.end <- str_c(cur.year , '-02' , '-01') ; ssn <- ssn.LD }
  if (cur.month  %in% cald.mos.lng.rn) { cur.ssn.start <- str_c(cur.year , '-03' , '-01') ; cur.ssn.end <- str_c(cur.year , '-05', '-01') ; ssn <- ssn.LR }
  if (cur.month  %in% cald.mos.sht.dr) { cur.ssn.start <- str_c(cur.year , '-06' , '-01') ; cur.ssn.end <- str_c(cur.year , '-09', '-01') ; ssn <- ssn.SD }
  if (cur.month  %in% cald.mos.sht.rn) { cur.ssn.start <- str_c(cur.year , '-10' , '-01') ; cur.ssn.end <- str_c(cur.year , '-12', '-01') ; ssn <- ssn.SR }
  
  
  cur.ssn.start <- as.Date(  cur.ssn.start)
  cur.ssn.end <- as.Date(  cur.ssn.end)
  
  current.period <- seq(from = cur.ssn.start , to =  cur.ssn.end , by = "day")
  
  
  
  # Parameter modification
  
  # specify that, within the current season, certain events take place
  
  if ( ssn %in% ssns.rny ) {
    
  
    if ( ssn == ssn.SR  ){
      
      
      quant.20.mm.days.osv.mean <- quant.20.mm.days.lng.rn.osv.mean 
      quant.20.mm.days.osv.sd <- quant.20.mm.days.lng.rn.osv.sd
      
      mean.95p.days.osv <- mean.95p.days.lng.rn.osv
      sd.95p.days.osv  <-  sd.95p.days.lng.rn.osv
      
      quant.wet.days.osv.mean <- quant.wet.days.lng.rn.osv.mean
      quant.wet.days.osv.sd <- quant.wet.days.lng.rn.osv.sd
      
      rainf.pctl.95.osv <- rainf.pctl.95.osv
      rainf.max.sht.rn.osv <- rainf.max.sht.rn.osv
      
      total.rainf.mm.osv.all <- total.rainf.mm.sht.rn.osv.all 
      
      
      
      
    # Frequency extreme/normal rain events
    quant.days.20.mm <- abs( rnorm(n = 1, mean = quant.20.mm.days.lng.rn.osv.mean , sd = quant.20.mm.days.lng.rn.osv.sd) ) 
      
    
    quant.days.95.pctl <- 5000
    
    while ( quant.days.95.pctl >  quant.days.20.mm ){
      
    quant.days.95.pctl <- abs( rnorm(n = 1, mean =   mean.95p.days.lng.rn.osv , sd =   sd.95p.days.lng.rn.osv  ) ) 

    }
    
    
   days.wet  <- 0
    
   while( ( days.wet < quant.days.20.mm ) | ( days.wet < quant.days.95.pctl ) ) {
      
      days.wet <- abs( rnorm( n = 1 , mean = quant.wet.days.lng.rn.osv.mean , sd = quant.wet.days.lng.rn.osv.sd ))  
      
    }
    
    # Severity  - extreme rain events
    rainf.95p.mm <-  runif(  quant.days.95.pctl , min = rainf.pctl.95.lng.rn.osv , max = rainf.max.sht.rn.osv )
    rainf.20.mm  <-  runif(  ( quant.days.20.mm - quant.days.95.pctl  ) , min = 20 , max = rainf.pctl.95.lng.rn.osv)
    
    
    # Total seasonal rainfall
    total.rainf.mm <- total.rainf.mm.sht.rn.osv.all
    
    # For quantity of days variables which by definition are discrete, round variables to nearest whole number
      
    quant.days.20.mm <- round(   quant.days.20.mm  , 0 )
    quant.days.95.pctl <- round(    quant.days.95.pctl  , 0 )
    days.wet  <- round( days.wet  , 0)
    
    } else if ( FALSE  ){
      
      quant.days.20.mm <- mean.20.mm.days.sht.rn.osv 
      quant.days.95.pctl <- mean.95p.days.sht.rn.osv
      
      rainf.95p.mm <- pctl.95.sht.rn.osv
      rainf.20.mm <- quant.20.mm.sht.rn.osv
      
      
      days.wet <- 10
      total.rainf.mm <- 150
      
      
      
    }
    
    period.days <- length( current.period  )
    period.list <- seq( 1 : period.days )
    
    # ~ ~ ~ SPECIFY DAYS WITH PRE-SPECIFIED RAINFALL EVENTS
    
    # ~ Select days > 95 percentile rainfall
    days.20.mm <- sample( 1 : period.days , quant.days.20.mm , replace = FALSE)
    
    
    days.95.pctl <- sample( 1 : quant.days.20.mm , quant.days.95.pctl , replace = FALSE)
    days.95.pctl <- days.20.mm[days.95.pctl]
    
    
    cald.dates.20.mm <- current.period[  days.20.mm  ]
    cald.dates.95.pctl <- current.period[   days.95.pctl ]
    
    for (x in 1 : length(cald.dates.95.pctl)  ){ 
    
    cald.dates.20.mm <- cald.dates.20.mm[  which( cald.dates.20.mm != cald.dates.95.pctl[x] )     ]
    
    }
    
    # ~ Select days > 20 mm rainfall

    
    

    # ~ Select days with > 0 but < 20 mm rainfall
    quant.remaining.days.wet <- days.wet - ( quant.days.20.mm + quant.days.95.pctl )
    days.non.selected <-  period.list[  which( !( period.list %in%  days.20.mm | period.list %in%  days.95.pctl)  )    ]
    days.lt.20.mm <- sample( days.non.selected ,  quant.remaining.days.wet , replace = FALSE  )
    
    cald.dates.wet.days <-  current.period[   days.lt.20.mm  ]
    
    #For the above selected calendar dates, specify the actual rainfall as random value within range
    
     sum(    cald.dates.95.pctl %in% clm.srs$date   )
     sum(    cald.dates.20.mm  %in% clm.srs$date   )
    
    clm.srs[  clm.srs$date  %in% cald.dates.95.pctl   ,'rain.mm']  <-  rainf.95p.mm 
    clm.srs[  clm.srs$date  %in% cald.dates.20.mm   ,'rain.mm']  <- rainf.20.mm
    
    #' ~ ~ For remaining wet days, specify rainfall simply as the total remaining 
    #' rainfall to fall divided by the number of days
    
    sum.rain.gt.20.mm <- sum( rainf.20.mm ) + sum( rainf.95p.mm )
      
    remaining.rainf.mm <-  total.rainf.mm - sum.rain.gt.20.mm 
    
    clm.srs[  clm.srs$date  %in% cald.dates.wet.days   ,'rain.mm']  <- remaining.rainf.mm / length(cald.dates.wet.days )
    
    # for ( wd %in% cald.dates.wet.days) {  # random method
      
    #rain.mm.today <- sample( 1 : 20 , 1  )
    
    # clm.srs[  clm.srs$date  ==  wd   ,'rain.mm']  <-
    
    #  }
      
      
  }
  
  
  } # run all
  
}


