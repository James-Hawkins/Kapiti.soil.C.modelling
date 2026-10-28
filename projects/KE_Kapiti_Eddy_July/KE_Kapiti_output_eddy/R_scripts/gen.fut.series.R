
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()




# save.image('gcm.data.RData')
#  load('gcm.data.RData')


{

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

r.cnd.2018.sr <- d.all$cald.yr == '2018' & d.all$is.sht.rn
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

r.cnd.2018.sr.wet.d  <- d.all$cald.yr == '2018' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2019.sr.wet.d  <- d.all$cald.yr == '2019' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2020.sr.wet.d  <- d.all$cald.yr == '2020' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2021.sr.wet.d  <- d.all$cald.yr == '2021' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2022.sr.wet.d  <- d.all$cald.yr == '2022' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2023.sr.wet.d  <- d.all$cald.yr == '2023' & d.all$is.sht.rn & d.all$wet.day
r.cnd.2024.sr.wet.d  <- d.all$cald.yr == '2024' & d.all$is.sht.rn & d.all$wet.day

r.cnd.2018.ld.wet.d  <- d.all$cald.yr == '2018' & d.all$is.lng.dr & d.all$wet.day
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


r.cnd.all.lr.wet.d <-   (   
  r.cnd.2019.lr.wet.d 
  |  r.cnd.2020.lr.wet.d 
  |  r.cnd.2021.lr.wet.d 
  |  r.cnd.2022.lr.wet.d 
  |  r.cnd.2023.lr.wet.d 
  |  r.cnd.2024.lr.wet.d     )

r.cnd.all.sr.wet.d <-   ( 
    r.cnd.2018.sr.wet.d 
  |  r.cnd.2019.sr.wet.d 
  |  r.cnd.2020.sr.wet.d 
  |  r.cnd.2021.sr.wet.d 
  |  r.cnd.2022.sr.wet.d 
  |  r.cnd.2023.sr.wet.d 
  |  r.cnd.2024.sr.wet.d     )

r.cnd.all.ld.wet.d <-   (   
  r.cnd.2019.ld.wet.d 
  |  r.cnd.2020.ld.wet.d 
  |  r.cnd.2021.ld.wet.d 
  |  r.cnd.2022.ld.wet.d 
  |  r.cnd.2023.ld.wet.d 
  |  r.cnd.2024.ld.wet.d     )

r.cnd.all.sd.wet.d <-   (   
  r.cnd.2019.sd.wet.d 
  |  r.cnd.2020.sd.wet.d 
  |  r.cnd.2021.sd.wet.d 
  |  r.cnd.2022.sd.wet.d 
  |  r.cnd.2023.sd.wet.d 
  |  r.cnd.2024.sd.wet.d     )



# wet days with >= 20 mm rainfall
r.cnd.2019.lr.20mm <- d.all$cald.yr == '2019' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2020.lr.20mm  <- d.all$cald.yr == '2020' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2021.lr.20mm  <- d.all$cald.yr == '2021' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2022.lr.20mm  <- d.all$cald.yr == '2022' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2023.lr.20mm  <- d.all$cald.yr == '2023' & d.all$is.lng.rn & d.all$day_20mm
r.cnd.2024.lr.20mm  <- d.all$cald.yr == '2024' & d.all$is.lng.rn & d.all$day_20mm

r.cnd.2018.sr.20mm  <- d.all$cald.yr == '2018' & d.all$is.sht.rn & d.all$day_20mm
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
total.rainf.mm.sht.rn.2018 <- sum(  d.all[ r.cnd.2018.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2019 <- sum(  d.all[ r.cnd.2019.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2020 <- sum(  d.all[ r.cnd.2020.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2021 <- sum(  d.all[ r.cnd.2021.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2022 <- sum(  d.all[ r.cnd.2022.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2023 <- sum(  d.all[ r.cnd.2023.sr.wet.d , 'precip.osv']  )
total.rainf.mm.sht.rn.2024 <- sum(  d.all[ r.cnd.2024.sr.wet.d , 'precip.osv']  )

total.rainf.mm.sht.rn.osv.all <- mean(
   total.rainf.mm.sht.rn.2018 
  ,total.rainf.mm.sht.rn.2019 
  ,total.rainf.mm.sht.rn.2020 
  ,total.rainf.mm.sht.rn.2021 
  ,total.rainf.mm.sht.rn.2022 
  ,total.rainf.mm.sht.rn.2023 
  ,total.rainf.mm.sht.rn.2024 
)


}   # Parameters prep

{
  
# Long rains
quant.wet.days.lng.rn.2019 <- sum(  r.cnd.2019.lr.wet.d  )
quant.wet.days.lng.rn.2020 <- sum(  r.cnd.2020.lr.wet.d  )
quant.wet.days.lng.rn.2021 <- sum(  r.cnd.2021.lr.wet.d  )
quant.wet.days.lng.rn.2022 <- sum(  r.cnd.2022.lr.wet.d  )
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
quant.wet.days.sht.rn.2018 <- sum( r.cnd.2018.sr.wet.d )  
quant.wet.days.sht.rn.2019 <- sum( r.cnd.2019.sr.wet.d )  
quant.wet.days.sht.rn.2020 <- sum( r.cnd.2020.sr.wet.d )
quant.wet.days.sht.rn.2021 <- sum( r.cnd.2021.sr.wet.d )
quant.wet.days.sht.rn.2022 <- sum( r.cnd.2022.sr.wet.d )
quant.wet.days.sht.rn.2023 <- sum( r.cnd.2023.sr.wet.d )
quant.wet.days.sht.rn.2024 <- sum( r.cnd.2024.sr.wet.d )

quant.wet.days.sht.rn.osv.mean <- mean( quant.wet.days.sht.rn.2018
                                        , quant.wet.days.sht.rn.2019
                                        , quant.wet.days.sht.rn.2020
                                        , quant.wet.days.sht.rn.2021
                                        , quant.wet.days.sht.rn.2022
                                        , quant.wet.days.sht.rn.2023
                                        , quant.wet.days.sht.rn.2024
)

quant.wet.days.sht.rn.osv.sd <- sd( c(  quant.wet.days.sht.rn.2018
                                      , quant.wet.days.sht.rn.2019
                                      , quant.wet.days.sht.rn.2020
                                      , quant.wet.days.sht.rn.2021
                                      , quant.wet.days.sht.rn.2022
                                      , quant.wet.days.sht.rn.2023
                                      , quant.wet.days.sht.rn.2024 ) 
)





# Long rains
mean.per.wet.day.mm.lng.rn.2019 <- round( mean(  d.all[ r.cnd.2019.lr.wet.d  , 'precip.osv']  ) , rd.decs.rain.day.mm) 
mean.per.wet.day.mm.lng.rn.2020 <- round( mean(  d.all[ r.cnd.2020.lr.wet.d, 'precip.osv']  ), rd.decs.rain.day.mm) 
mean.per.wet.day.mm.lng.rn.2021 <- round( mean(  d.all[ r.cnd.2021.lr.wet.d , 'precip.osv']  ), rd.decs.rain.day.mm) 
mean.per.wet.day.mm.lng.rn.2022 <- round( mean(  d.all[ r.cnd.2022.lr.wet.d , 'precip.osv']  ), rd.decs.rain.day.mm) 
mean.per.wet.day.mm.lng.rn.2023 <- round( mean(  d.all[ r.cnd.2023.lr.wet.d, 'precip.osv']  ), rd.decs.rain.day.mm) 
mean.per.wet.day.mm.lng.rn.2024 <- round( mean(  d.all[ r.cnd.2024.lr.wet.d , 'precip.osv']  ), rd.decs.rain.day.mm) 




mean.per.wet.day.mm.lng.rn.osv.all <- round( mean(
  
  mean.per.wet.day.mm.lng.rn.2019
  , mean.per.wet.day.mm.lng.rn.2020
  , mean.per.wet.day.mm.lng.rn.2021
  , mean.per.wet.day.mm.lng.rn.2022
  , mean.per.wet.day.mm.lng.rn.2023
  , mean.per.wet.day.mm.lng.rn.2024
  
) ,  rd.decs.rain.day.mm) 

# Standard deviation rain per wet day
sd.per.wet.day.mm.lng.rn.2019 <- round(  sd(  d.all[ r.cnd.2019.lr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.lng.rn.2020 <- round( sd(  d.all[ r.cnd.2020.lr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.lng.rn.2021 <- round( sd(  d.all[ r.cnd.2021.lr.wet.d, 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.lng.rn.2022 <- round( sd(  d.all[ r.cnd.2022.lr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.lng.rn.2023 <- round( sd(  d.all[ r.cnd.2023.lr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.lng.rn.2024 <- round( sd(  d.all[ r.cnd.2024.lr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 

  
sd.per.wet.day.mm.lng.rn.osv.all <- round(  sd(  d.all[ d.all$is.lng.rn  & d.all$wet.day , 'precip.osv'] )  , rd.decs.rain.day.mm) 



# Short rains
mean.per.wet.day.mm.sht.rn.2018 <- round( mean(  d.all[ r.cnd.2018.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
mean.per.wet.day.mm.sht.rn.2019 <- round( mean(  d.all[ r.cnd.2019.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
mean.per.wet.day.mm.sht.rn.2020 <- round( mean(  d.all[ r.cnd.2020.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
mean.per.wet.day.mm.sht.rn.2021 <- round( mean(  d.all[ r.cnd.2021.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
mean.per.wet.day.mm.sht.rn.2022 <- round( mean(  d.all[ r.cnd.2022.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
mean.per.wet.day.mm.sht.rn.2023 <- round( mean(  d.all[ r.cnd.2023.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
mean.per.wet.day.mm.sht.rn.2024 <- round( mean(  d.all[ r.cnd.2024.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 

mean.per.wet.day.mm.sht.rn.osv.all <- round( mean(
  
  mean.per.wet.day.mm.sht.rn.2018
  , mean.per.wet.day.mm.sht.rn.2019
  , mean.per.wet.day.mm.sht.rn.2020
  , mean.per.wet.day.mm.sht.rn.2021
  , mean.per.wet.day.mm.sht.rn.2022
  , mean.per.wet.day.mm.sht.rn.2023
  , mean.per.wet.day.mm.sht.rn.2024
  
) , rd.decs.rain.day.mm) 


  
# Standard deviation rain per wet day
sd.per.wet.day.mm.sht.rn.2018 <- round( sd(  d.all[ r.cnd.2018.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.sht.rn.2019 <- round( sd(  d.all[ r.cnd.2019.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.sht.rn.2020 <- round( sd(  d.all[ r.cnd.2020.sr.wet.d, 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.sht.rn.2021 <- round( sd(  d.all[ r.cnd.2021.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.sht.rn.2022 <- round( sd(  d.all[ r.cnd.2022.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.sht.rn.2023 <- round( sd(  d.all[ r.cnd.2023.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 
sd.per.wet.day.mm.sht.rn.2024 <- round( sd(  d.all[ r.cnd.2024.sr.wet.d , 'precip.osv']  ) , rd.decs.rain.day.mm) 

sd.per.wet.day.mm.sht.rn.osv.all <- round( sd(  d.all[ d.all$is.sht.rn  & d.all$wet.day , 'precip.osv']  ) , rd.decs.rain.day.mm) 


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


# OND
quant.20.mm.days.sht.rn.2018 <-  sum(  r.cnd.2018.sr.20mm )
quant.20.mm.days.sht.rn.2019 <-  sum(  r.cnd.2019.sr.20mm )
quant.20.mm.days.sht.rn.2020 <-  sum(  r.cnd.2020.sr.20mm )
quant.20.mm.days.sht.rn.2021 <-  sum(  r.cnd.2021.sr.20mm )
quant.20.mm.days.sht.rn.2022 <-  sum(  r.cnd.2022.sr.20mm )
quant.20.mm.days.sht.rn.2023 <-  sum(  r.cnd.2023.sr.20mm )
quant.20.mm.days.sht.rn.2024 <-  sum(  r.cnd.2024.sr.20mm )

quant.20.mm.days.sht.rn.osv.mean <- mean(
  
    quant.20.mm.days.sht.rn.2018
  , quant.20.mm.days.sht.rn.2019 
  , quant.20.mm.days.sht.rn.2020
  , quant.20.mm.days.sht.rn.2021 
  , quant.20.mm.days.sht.rn.2022 
  , quant.20.mm.days.sht.rn.2023 
  , quant.20.mm.days.sht.rn.2024 
  
)

quant.20.mm.days.sht.rn.osv.sd <- sd( c(
  
  quant.20.mm.days.sht.rn.2018
  , quant.20.mm.days.sht.rn.2019 
  , quant.20.mm.days.sht.rn.2020
  , quant.20.mm.days.sht.rn.2021 
  , quant.20.mm.days.sht.rn.2022 
  , quant.20.mm.days.sht.rn.2023 
  , quant.20.mm.days.sht.rn.2024 
)
)

# MEAN RAINF per 20 mm day
# query: are these mutually exclusive or inclusive?
# Note: for some of the below, zero observations --> omit from period average


# Long rains
mean.per.20.mm.day.lng.rn.2019 <- round( mean(  d.all[ r.cnd.2019.lr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.lng.rn.2020 <- round( mean(  d.all[ r.cnd.2020.lr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.lng.rn.2021 <- round( mean(  d.all[ r.cnd.2021.lr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.lng.rn.2022 <- round( mean(  d.all[ r.cnd.2022.lr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.lng.rn.2023 <- round( mean(  d.all[ r.cnd.2023.lr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.lng.rn.2024 <- round( mean(  d.all[ r.cnd.2024.lr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)


mean.per.20.mm.day.lng.rn.osv.all <- round( mean(
  
  mean.per.20.mm.day.lng.rn.2019
  , mean.per.20.mm.day.lng.rn.2020
  , mean.per.20.mm.day.lng.rn.2021
  , mean.per.20.mm.day.lng.rn.2022
  , mean.per.20.mm.day.lng.rn.2023
  , mean.per.20.mm.day.lng.rn.2024
  
) , rd.decs.rain.day.mm)

# Short rains
mean.per.20.mm.day.sht.rn.2018 <- round(mean(  d.all[ r.cnd.2018.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.sht.rn.2019 <- round(mean(  d.all[ r.cnd.2019.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.sht.rn.2020 <- round(mean(  d.all[ r.cnd.2020.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.sht.rn.2021 <- round(mean(  d.all[ r.cnd.2021.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.sht.rn.2022 <- round(mean(  d.all[ r.cnd.2022.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.sht.rn.2023 <- round(mean(  d.all[ r.cnd.2023.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
mean.per.20.mm.day.sht.rn.2024 <- round(mean(  d.all[ r.cnd.2024.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)


mean.per.20.mm.day.sht.rn.osv.all <- round(mean(
  
  mean.per.20.mm.day.sht.rn.2018
  , mean.per.20.mm.day.sht.rn.2019
  # , mean.per.20.mm.day.sht.rn.2020
  #, mean.per.20.mm.day.sht.rn.2021
  #, mean.per.20.mm.day.sht.rn.2022
  , mean.per.20.mm.day.sht.rn.2023
  , mean.per.20.mm.day.sht.rn.2024
  
) , rd.decs.rain.day.mm)

# Standard deviation rain per wet day
# Note: for some of the below, only 1 observation --> default to period average

# Long rains
sd.per.20.mm.day.lng.rn.2019 <- round(sd(  d.all[ r.cnd.2019.lr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.lng.rn.2020 <- round(sd(  d.all[ r.cnd.2020.lr.20mm, 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.lng.rn.2021 <- round(sd(  d.all[ r.cnd.2021.lr.20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.lng.rn.2022 <- round(sd(  d.all[ r.cnd.2022.lr.20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.lng.rn.2023 <- round(sd(  d.all[ r.cnd.2023.lr.20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.lng.rn.2024 <- round(sd(  d.all[ r.cnd.2024.lr.20mm, 'precip.osv']  ) , rd.decs.rain.day.mm)


sd.per.20.mm.day.lng.rn.osv.all <- round( sd(  d.all[ d.all$is.lng.rn  & d.all$day_20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)


# Short  rains
sd.per.20.mm.day.sht.rn.2018 <- round( sd(  d.all[ r.cnd.2018.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.sht.rn.2019 <- round( sd(  d.all[ r.cnd.2019.sr.20mm  , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.sht.rn.2020 <- round(sd(  d.all[ r.cnd.2020.sr.20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.sht.rn.2021 <-  round(sd(  d.all[ r.cnd.2021.sr.20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.sht.rn.2022 <-  round(sd(  d.all[ r.cnd.2022.sr.20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.sht.rn.2023 <-round( sd(  d.all[ r.cnd.2023.sr.20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)
sd.per.20.mm.day.sht.rn.2024 <- round(sd(  d.all[ r.cnd.2024.sr.20mm, 'precip.osv']  ) , rd.decs.rain.day.mm)


sd.per.20.mm.day.sht.rn.osv.all <- round( sd(  d.all[ d.all$is.sht.rn  & d.all$day_20mm , 'precip.osv']  ) , rd.decs.rain.day.mm)


} # - - - - 20 mm

{
#  95 percentiles - rainfall values (mm/d)

# MAM rains
pctl.95.lng.rn.osv.all <- as.numeric( quantile( d.all[  ( 
  r.cnd.2019.lr.wet.d 
   |  r.cnd.2020.lr.wet.d  
  #  | r.cnd.2021.lr.wet.d 
  #  |  r.cnd.2022.lr.wet.d  
  | r.cnd.2023.lr.wet.d  
  |  r.cnd.2024.lr.wet.d  
)   ,  'precip.osv' ]  , probs = c( 0.95)))

rainf.pctl.95.lng.rn.osv <- pctl.95.lng.rn.osv.all

rainf.max.lng.rn.osv <- as.numeric( max( d.all[  ( 
  r.cnd.2019.lr.wet.d 
  |  r.cnd.2020.lr.wet.d  
  | r.cnd.2021.lr.wet.d 
  |  r.cnd.2022.lr.wet.d  
  | r.cnd.2023.lr.wet.d  
  |  r.cnd.2024.lr.wet.d  
)   ,  'precip.osv' ]  ))



# O - N - D rains
pctl.95.sht.rn.osv.all <- as.numeric( quantile( d.all[  ( 
  r.cnd.2018.sr.wet.d 
  |  r.cnd.2019.sr.wet.d 
  #  |  r.cnd.2020.sr.wet.d  
  #   | r.cnd.2021.sr.wet.d 
  #  |  r.cnd.2022.sr.wet.d  
  | r.cnd.2023.sr.wet.d  
  |  r.cnd.2024.sr.wet.d  
  )   ,  'precip.osv' ]  , probs = c( 0.95)))

rainf.pctl.95.sht.rn.osv <- pctl.95.sht.rn.osv.all

rainf.max.sht.rn.osv <- as.numeric( max( d.all[  ( 
  r.cnd.2018.sr.wet.d 
  | r.cnd.2019.sr.wet.d 
   |  r.cnd.2020.sr.wet.d  
   | r.cnd.2021.sr.wet.d 
 |  r.cnd.2022.sr.wet.d  
  | r.cnd.2023.sr.wet.d  
  |  r.cnd.2024.sr.wet.d  
)   ,  'precip.osv' ]  ))
  
  


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
quant.95p.days.sht.rn.2018 <- sum( r.cnd.2018.sr.wet.d &  d.all$precip.osv >= rainf.pctl.95.sht.rn.osv )
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
  
  quant.95p.days.sht.rn.2018
  , quant.95p.days.sht.rn.2019
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

{
  
nr.fut.start <- as.Date("2030-01-01")
nr.fut.end <- as.Date("2040-01-01")
dates.nr.fut <- seq(  from = nr.fut.start , to = nr.fut.end  ,  by = "day")

  
excel.rows.nr.fut.start <- 6116 ; excel.rows.nr.fut.end <- 9492
rows.2.include <- c( 1,excel.rows.nr.fut.end  )
  
# RCP 4.5
gcm.r4.1 <- as.data.frame( read_excel(  str_c( R.dat.gcm.dir  ,   "CMIP6_ssp245_2015_2050.xlsx" ) , sheet = "INM-CM5-0"  , cell_rows( rows.2.include)  )) 
gcm.r4.2 <- as.data.frame( read_excel(  str_c( R.dat.gcm.dir  ,    "CMIP6_ssp245_2015_2050.xlsx" ) , sheet = "IPSL-CM6A-LR" , cell_rows( rows.2.include)  )) 
gcm.r4.3 <- as.data.frame( read_excel(   str_c( R.dat.gcm.dir  ,   "CMIP6_ssp245_2015_2050.xlsx" ) , sheet = "EC-Earth3"  , cell_rows( rows.2.include)  )) 
gcm.r4.4 <- as.data.frame( read_excel(   str_c( R.dat.gcm.dir  ,   "CMIP6_ssp245_2015_2050.xlsx") , sheet = "NorESM2-MM", cell_rows( rows.2.include)  )) 
gcm.r4.5 <- as.data.frame( read_excel(   str_c( R.dat.gcm.dir  ,   "CMIP6_ssp245_2015_2050.xlsx" ) , sheet = "GFDL-ESM4" , cell_rows( rows.2.include)  ) )
gcm.r4.6 <- as.data.frame( read_excel(  str_c( R.dat.gcm.dir  ,    "CMIP6_ssp245_2015_2050.xlsx") , sheet = "KIOST-ESM"  , cell_rows( rows.2.include)  )) 
gcm.r4.7 <- as.data.frame( read_excel(  str_c( R.dat.gcm.dir  ,    "CMIP6_ssp245_2015_2050.xlsx") , sheet = "TaiESM1"  , cell_rows( rows.2.include)  )) 

# RCP 8.5
gcm.r8.1 <- as.data.frame( read_excel(  str_c( R.dat.gcm.dir  ,   "CMIP6_ssp585_2015_2050.xlsx" ) , sheet = "INM-CM5-0"  , cell_rows( rows.2.include)  ) )
gcm.r8.2 <- as.data.frame( read_excel(  str_c( R.dat.gcm.dir  ,    "CMIP6_ssp585_2015_2050.xlsx" ) , sheet = "IPSL-CM6A-LR"  , cell_rows( rows.2.include)  ) )
gcm.r8.3 <- as.data.frame( read_excel(   str_c( R.dat.gcm.dir  ,   "CMIP6_ssp585_2015_2050.xlsx" ) , sheet = "EC-Earth3"   , cell_rows( rows.2.include)  ) )
gcm.r8.4 <- as.data.frame( read_excel(   str_c( R.dat.gcm.dir  ,   "CMIP6_ssp585_2015_2050.xlsx") , sheet = "NorESM2-MM"   , cell_rows( rows.2.include)  ) )
gcm.r8.5 <- as.data.frame( read_excel(   str_c( R.dat.gcm.dir  ,   "CMIP6_ssp585_2015_2050.xlsx" ) , sheet = "GFDL-ESM4"  , cell_rows( rows.2.include)  ) )
gcm.r8.6 <- as.data.frame( read_excel(  str_c( R.dat.gcm.dir  ,    "CMIP6_ssp585_2015_2050.xlsx") , sheet = "KIOST-ESM"  , cell_rows( rows.2.include)  ) )
gcm.r8.7 <- as.data.frame( read_excel(  str_c( R.dat.gcm.dir  ,    "CMIP6_ssp585_2015_2050.xlsx") , sheet = "TaiESM1"   , cell_rows( rows.2.include)  ) )

gcm.r4.1 <- gcm.r4.1[ gcm.r4.1$date %in% dates.nr.fut , ]
gcm.r4.2 <- gcm.r4.2[ gcm.r4.2$date %in% dates.nr.fut , ]
gcm.r4.3 <- gcm.r4.3[ gcm.r4.3$date %in% dates.nr.fut , ]
gcm.r4.4 <- gcm.r4.4[ gcm.r4.4$date %in% dates.nr.fut , ]
gcm.r4.5 <- gcm.r4.5[ gcm.r4.5$date %in% dates.nr.fut , ]
gcm.r4.6 <- gcm.r4.6[ gcm.r4.6$date %in% dates.nr.fut , ]
gcm.r4.7 <- gcm.r4.7[ gcm.r4.7$date %in% dates.nr.fut , ]

gcm.r8.1 <- gcm.r8.1[ gcm.r8.1$date %in% dates.nr.fut , ]
gcm.r8.2 <- gcm.r8.2[ gcm.r8.2$date %in% dates.nr.fut , ]
gcm.r8.3 <- gcm.r8.3[ gcm.r8.3$date %in% dates.nr.fut , ]
gcm.r8.4 <- gcm.r8.4[ gcm.r8.4$date %in% dates.nr.fut , ]
gcm.r8.5 <- gcm.r8.5[ gcm.r8.5$date %in% dates.nr.fut , ]
gcm.r8.6 <- gcm.r8.6[ gcm.r8.6$date %in% dates.nr.fut , ]
gcm.r8.7 <- gcm.r8.7[ gcm.r8.7$date %in% dates.nr.fut , ]


gcm.r4.1$gcm <- 1
gcm.r4.2$gcm <- 2
gcm.r4.3$gcm <- 3
gcm.r4.4$gcm <- 4
gcm.r4.5$gcm <- 5
gcm.r4.6$gcm <- 6
gcm.r4.7$gcm <- 7

gcm.r8.1$gcm <- 1
gcm.r8.2$gcm <- 2
gcm.r8.3$gcm <- 3
gcm.r8.4$gcm <- 4
gcm.r8.5$gcm <- 5
gcm.r8.6$gcm <- 6
gcm.r8.7$gcm <- 7

gcm.r4.1$rcp <- r.4
gcm.r4.2$rcp <- r.4
gcm.r4.3$rcp <- r.4
gcm.r4.4$rcp <- r.4
gcm.r4.5$rcp <- r.4
gcm.r4.6$rcp <- r.4
gcm.r4.7$rcp <- r.4

gcm.r8.1$rcp <- r.8
gcm.r8.2$rcp <- r.8
gcm.r8.3$rcp <- r.8
gcm.r8.4$rcp <- r.8
gcm.r8.5$rcp <- r.8
gcm.r8.6$rcp <- r.8
gcm.r8.7$rcp <- r.8


gcm.all <- rbind(
  
  gcm.r4.1
  , gcm.r4.2
  , gcm.r4.3
  , gcm.r4.4
  , gcm.r4.5
  , gcm.r4.6
  , gcm.r4.7
  
  
  
  , gcm.r8.1
  , gcm.r8.2
  , gcm.r8.3
  , gcm.r8.4
  , gcm.r8.5
  , gcm.r8.6
  , gcm.r8.7
  
)

gcm.all$mean.t.dC  <-  ( gcm.all$min.t.dC  + gcm.all$max.t.dC ) / 2
  
climate.cols <- colnames(    gcm.all   )[  2  :  7  ]

gcm.all$date <- as.Date(    gcm.all$date )


#View( gcm.all  )


gcm.all <- gen.mean.gcm.dat( gcm.all  , r.4 , climate.cols  , dates.nr.fut  )
gcm.all <- gen.mean.gcm.dat( gcm.all  , r.8 , climate.cols  , dates.nr.fut  ) 


gcm.all <- gcm.all[   !is.na(   gcm.all$date  )  ,  ]



#  View(    gcm.all[  gcm.all$gcm == 0 ,])


gcm.all$cald.mth <- as.numeric(format(  gcm.all$date , "%m"))
gcm.all$cald.yr <- as.numeric(format(  gcm.all$date , "%y"))

gcm.all$day.cnt <- yday( gcm.all$date ) 
  

gcm.all[, 'is.sht.rn'] <- FALSE
gcm.all[, 'is.lng.rn'] <- FALSE
gcm.all[, 'is.sht.dr'] <- FALSE
gcm.all[, 'is.lng.dr'] <- FALSE


gcm.all[  gcm.all$cald.mth %in% cald.mos.sht.rn , 'is.sht.rn'] <- TRUE
gcm.all[  gcm.all$cald.mth %in% cald.mos.lng.rn , 'is.lng.rn'] <- TRUE
gcm.all[  gcm.all$cald.mth %in% cald.mos.sht.dr , 'is.sht.dr'] <- TRUE
gcm.all[  gcm.all$cald.mth %in% cald.mos.lng.dr , 'is.lng.dr'] <- TRUE


gcm.near.fut.all.num.sht.rns.tot <- length(   unique(  gcm.all[ gcm.all$is.lng.rn & gcm.all$gcm == 0  , 'cald.yr'] )  )
gcm.near.fut.all.num.lng.rns.tot <- length(   unique(  gcm.all[ gcm.all$is.sht.rn & gcm.all$gcm == 0  , 'cald.yr'] )  )

gcm.near.fut.all.num.sht.dry.tot <- length(   unique(  gcm.all[ gcm.all$is.lng.dr & gcm.all$gcm == 0  , 'cald.yr'] )  )
gcm.near.fut.all.num.lng.dry.tot <- length(   unique(  gcm.all[ gcm.all$is.sht.dr & gcm.all$gcm == 0  , 'cald.yr'] )  )



gcm.near.fut.all.num.yrs.tot <<- length(   unique(  gcm.all[ !is.na(gcm.all$cald.yr) & gcm.all$gcm == 0  , 'cald.yr'] )  )


gcm.all[ gcm.all$is.lng.rn & gcm.all$gcm == 0 , 'rain.mm' ]


# R condition criteria for specific time periods, RCPs, and seasons
# R4 
gcm.r.cnd.near.fut.r4.gcm.1 <- gcm.all$rcp == r.4 &  gcm.all$gcm == 1 
gcm.r.cnd.near.fut.r4.gcm.2 <- gcm.all$rcp == r.4 &  gcm.all$gcm == 2 
gcm.r.cnd.near.fut.r4.gcm.3 <- gcm.all$rcp == r.4 &  gcm.all$gcm == 3 
gcm.r.cnd.near.fut.r4.gcm.4 <- gcm.all$rcp == r.4 &  gcm.all$gcm == 4 
gcm.r.cnd.near.fut.r4.gcm.5 <- gcm.all$rcp == r.4 &  gcm.all$gcm == 5 
gcm.r.cnd.near.fut.r4.gcm.6 <- gcm.all$rcp == r.4 &  gcm.all$gcm == 6 
gcm.r.cnd.near.fut.r4.gcm.7 <- gcm.all$rcp == r.4 &  gcm.all$gcm == 7 


# R8 
gcm.r.cnd.near.fut.r8.gcm.1 <- gcm.all$rcp == r.8 &  gcm.all$gcm == 1 
gcm.r.cnd.near.fut.r8.gcm.2 <- gcm.all$rcp == r.8 &  gcm.all$gcm == 2 
gcm.r.cnd.near.fut.r8.gcm.3 <- gcm.all$rcp == r.8 &  gcm.all$gcm == 3 
gcm.r.cnd.near.fut.r8.gcm.4 <- gcm.all$rcp == r.8 &  gcm.all$gcm == 4 
gcm.r.cnd.near.fut.r8.gcm.5 <- gcm.all$rcp == r.8 &  gcm.all$gcm == 5 
gcm.r.cnd.near.fut.r8.gcm.6 <- gcm.all$rcp == r.8 &  gcm.all$gcm == 6 
gcm.r.cnd.near.fut.r8.gcm.7 <- gcm.all$rcp == r.8 &  gcm.all$gcm == 7 



gcm.r.cnd.near.fut.r4.lr <- gcm.all$rcp == r.4 &  gcm.all$gcm == 0 & gcm.all$is.lng.rn
gcm.r.cnd.near.fut.r4.sr <- gcm.all$rcp == r.4 &  gcm.all$gcm == 0 & gcm.all$is.sht.rn 
gcm.r.cnd.near.fut.r4.ld <- gcm.all$rcp == r.4 &  gcm.all$gcm == 0 & gcm.all$is.lng.dr
gcm.r.cnd.near.fut.r4.sd <- gcm.all$rcp == r.4 &  gcm.all$gcm == 0 & gcm.all$is.sht.dr

gcm.r.cnd.near.fut.r8.lr <- gcm.all$rcp == r.8 &  gcm.all$gcm == 0 & gcm.all$is.lng.rn
gcm.r.cnd.near.fut.r8.sr <- gcm.all$rcp == r.8 &  gcm.all$gcm == 0 & gcm.all$is.sht.rn 
gcm.r.cnd.near.fut.r8.ld <- gcm.all$rcp == r.8 &  gcm.all$gcm == 0 & gcm.all$is.lng.dr
gcm.r.cnd.near.fut.r8.sd <- gcm.all$rcp == r.8 &  gcm.all$gcm == 0 & gcm.all$is.sht.dr


# Conditions only for individual GCMs (i.e. including mean data)
gcm.real.r.cnd.near.fut.r4.lr <- gcm.all$rcp == r.4 &  gcm.all$gcm != 0 & gcm.all$is.lng.rn
gcm.real.r.cnd.near.fut.r4.sr <- gcm.all$rcp == r.4 &  gcm.all$gcm != 0 & gcm.all$is.sht.rn 
gcm.real.r.cnd.near.fut.r4.ld <- gcm.all$rcp == r.4 &  gcm.all$gcm != 0 & gcm.all$is.lng.dr
gcm.real.r.cnd.near.fut.r4.sd <- gcm.all$rcp == r.4 &  gcm.all$gcm != 0 & gcm.all$is.sht.dr

gcm.real.r.cnd.near.fut.r8.lr <- gcm.all$rcp == r.8 &  gcm.all$gcm != 0 & gcm.all$is.lng.rn
gcm.real.r.cnd.near.fut.r8.sr <- gcm.all$rcp == r.8 &  gcm.all$gcm != 0 & gcm.all$is.sht.rn 
gcm.real.r.cnd.near.fut.r8.ld <- gcm.all$rcp == r.8 &  gcm.all$gcm != 0 & gcm.all$is.lng.dr
gcm.real.r.cnd.near.fut.r8.sd <- gcm.all$rcp == r.8 &  gcm.all$gcm != 0 & gcm.all$is.sht.dr


# ~ ~ ~ QUANT WET DAYS - - 

# ~ RCP 4.5
# mean
quant.wet.days.gcm.r4.lr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.lr & gcm.all$rain.mm > 0 ,  ]   ) / (  gcm.near.fut.all.num.lng.rns.tot * 7 )
quant.wet.days.gcm.r4.sr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.sr & gcm.all$rain.mm > 0 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )
quant.wet.days.gcm.r4.ld <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.ld & gcm.all$rain.mm > 0 ,  ]   ) / (  gcm.near.fut.all.num.lng.dry.tot * 7 )
quant.wet.days.gcm.r4.sd <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.sd & gcm.all$rain.mm > 0 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )

quant.wet.days.gcm.r4.lr <- round( quant.wet.days.gcm.r4.lr , rd.decs.quant.days )
quant.wet.days.gcm.r4.sr <- round( quant.wet.days.gcm.r4.sr , rd.decs.quant.days )
quant.wet.days.gcm.r4.ld <- round( quant.wet.days.gcm.r4.ld , rd.decs.quant.days )
quant.wet.days.gcm.r4.sd <- round( quant.wet.days.gcm.r4.sd , rd.decs.quant.days )

# Standard deviation
sd.quant.wet.days.gcm.r4.lr <-  sd ( c( 
    nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
 , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
 , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
 , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
 , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
 , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
 , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))

sd.quant.wet.days.gcm.r4.sr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))


sd.quant.wet.days.gcm.r4.lr <- round( sd.quant.wet.days.gcm.r4.lr  , rd.decs.quant.days )
sd.quant.wet.days.gcm.r4.sr <- round( sd.quant.wet.days.gcm.r4.sr  , rd.decs.quant.days )


# ~ RCP 8.5
quant.wet.days.gcm.r8.lr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.lr & gcm.all$rain.mm > 0 ,  ]   ) / (  gcm.near.fut.all.num.lng.rns.tot * 7 )
quant.wet.days.gcm.r8.sr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.sr & gcm.all$rain.mm > 0 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )
quant.wet.days.gcm.r8.ld <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.ld & gcm.all$rain.mm > 0 ,  ]   ) / (  gcm.near.fut.all.num.lng.dry.tot * 7 )
quant.wet.days.gcm.r8.sd <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.sd & gcm.all$rain.mm > 0 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )

quant.wet.days.gcm.r8.lr <- round( quant.wet.days.gcm.r8.lr , rd.decs.quant.days )
quant.wet.days.gcm.r8.sr <- round( quant.wet.days.gcm.r8.sr , rd.decs.quant.days )
quant.wet.days.gcm.r8.ld <- round( quant.wet.days.gcm.r8.ld , rd.decs.quant.days )
quant.wet.days.gcm.r8.sd <- round( quant.wet.days.gcm.r8.sd , rd.decs.quant.days )

# Standard deviation
sd.quant.wet.days.gcm.r8.lr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))

sd.quant.wet.days.gcm.r8.sr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.8 & gcm.all$rain.mm > 0 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))


sd.quant.wet.days.gcm.r8.lr <- round( sd.quant.wet.days.gcm.r8.lr  , rd.decs.quant.days )
sd.quant.wet.days.gcm.r8.sr <- round( sd.quant.wet.days.gcm.r8.sr  , rd.decs.quant.days )



# ~ ~ ~ QUANT 20 mm days - - 

# ~ RCP 4.5
quant.20mm.days.gcm.r4.lr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.lr & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.lng.rns.tot * 7 )
quant.20mm.days.gcm.r4.sr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.sr & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )
quant.20mm.days.gcm.r4.ld <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.ld & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.lng.dry.tot * 7 )
quant.20mm.days.gcm.r4.sd <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.sd & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )

quant.20mm.days.gcm.r4.lr <- round( quant.20mm.days.gcm.r4.lr , rd.decs.quant.days )
quant.20mm.days.gcm.r4.sr <- round( quant.20mm.days.gcm.r4.sr , rd.decs.quant.days )
quant.20mm.days.gcm.r4.ld <- round( quant.20mm.days.gcm.r4.ld , rd.decs.quant.days )
quant.20mm.days.gcm.r4.sd <- round( quant.20mm.days.gcm.r4.sd , rd.decs.quant.days )


# Standard deviation
sd.quant.20mm.days.gcm.r4.lr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))

sd.quant.20mm.days.gcm.r4.sr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))


sd.quant.20mm.days.gcm.r4.lr <- round( sd.quant.20mm.days.gcm.r4.lr  , rd.decs.quant.days )
sd.quant.20mm.days.gcm.r4.sr <- round( sd.quant.20mm.days.gcm.r4.sr  , rd.decs.quant.days )




# ~ RCP 8.5
quant.20mm.days.gcm.r8.lr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.lr & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.lng.rns.tot * 7 )
quant.20mm.days.gcm.r8.sr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.sr & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )
quant.20mm.days.gcm.r8.ld <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.ld & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.lng.dry.tot * 7 )
quant.20mm.days.gcm.r8.sd <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.sd & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )

quant.20mm.days.gcm.r8.lr <- round( quant.20mm.days.gcm.r8.lr , rd.decs.quant.days )
quant.20mm.days.gcm.r8.sr <- round( quant.20mm.days.gcm.r8.sr , rd.decs.quant.days )
quant.20mm.days.gcm.r8.ld <- round( quant.20mm.days.gcm.r8.ld , rd.decs.quant.days )
quant.20mm.days.gcm.r8.sd <- round( quant.20mm.days.gcm.r8.sd , rd.decs.quant.days )



# Standard deviation
sd.quant.20mm.days.gcm.r8.lr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))

sd.quant.20mm.days.gcm.r8.sr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.4 & gcm.all$rain.mm > 20 & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))


sd.quant.20mm.days.gcm.r8.lr <- round( sd.quant.20mm.days.gcm.r8.lr  , rd.decs.quant.days )
sd.quant.20mm.days.gcm.r8.sr <- round( sd.quant.20mm.days.gcm.r8.sr  , rd.decs.quant.days )




# ~ ~ ~ QUANT 95 p days - - 

# ~ RCP 4.5
quant.95p.days.gcm.r4.lr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.lr & gcm.all$rain.mm > pctl.95.lng.rn.osv.all ,  ]   ) / (  gcm.near.fut.all.num.lng.rns.tot * 7 )
quant.95p.days.gcm.r4.sr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.sr & gcm.all$rain.mm > pctl.95.sht.rn.osv.all ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )
#quant.95p.days.gcm.r4.ld <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.ld & gcm.all$rain.mm > pctl.95.sht.rn.osv.all  ,  ]   ) / (  gcm.near.fut.all.num.lng.dry.tot * 7 )
#quant.95p.days.gcm.r4.sd <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r4.sd & gcm.all$rain.mm > pctl.95.sht.rn.osv.all  ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )

quant.95p.days.gcm.r4.lr <- round( quant.95p.days.gcm.r4.lr , rd.decs.quant.days )
quant.95p.days.gcm.r4.sr <- round( quant.95p.days.gcm.r4.sr , rd.decs.quant.days )
#quant.95p.days.gcm.r4.ld <- round( quant.95p.days.gcm.r4.ld , rd.decs.quant.days )
#quant.95p.days.gcm.r4.sd <- round( quant.95p.days.gcm.r4.sd , rd.decs.quant.days )


# Standard deviation
sd.quant.95p.days.gcm.r4.lr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))

sd.quant.95p.days.gcm.r4.sr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.4 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))


sd.quant.95p.days.gcm.r4.lr <- round( sd.quant.95p.days.gcm.r4.lr  , rd.decs.quant.days )
sd.quant.95p.days.gcm.r4.sr <- round( sd.quant.95p.days.gcm.r4.sr  , rd.decs.quant.days )



# ~ RCP 8.5
quant.95p.days.gcm.r8.lr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.lr & gcm.all$rain.mm > pctl.95.lng.rn.osv.all ,  ]   ) / (  gcm.near.fut.all.num.lng.rns.tot * 7 )
quant.95p.days.gcm.r8.sr <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.sr & gcm.all$rain.mm > pctl.95.sht.rn.osv.all ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )
#quant.95p.days.gcm.r8.ld <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.ld & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.lng.dry.tot * 7 )
#quant.95p.days.gcm.r8.sd <- nrow( gcm.all[ gcm.real.r.cnd.near.fut.r8.sd & gcm.all$rain.mm > 20 ,  ]   ) / (  gcm.near.fut.all.num.sht.rns.tot * 7 )

quant.95p.days.gcm.r8.lr <- round( quant.95p.days.gcm.r8.lr , rd.decs.quant.days )
quant.95p.days.gcm.r8.sr <- round( quant.95p.days.gcm.r8.sr , rd.decs.quant.days )
#quant.95p.days.gcm.r8.ld <- round( quant.95p.days.gcm.r8.ld , rd.decs.quant.days )
#quant.95p.days.gcm.r8.sd <- round( quant.95p.days.gcm.r8.sd , rd.decs.quant.days )


# Standard deviation
sd.quant.95p.days.gcm.r8.lr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.lng.rn,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))

sd.quant.95p.days.gcm.r8.sr <-  sd ( c( 
  nrow( gcm.all[ gcm.all$gcm == 1 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 2 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 3 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 4 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 5 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 6 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
  , nrow( gcm.all[ gcm.all$gcm == 7 & gcm.all$rcp == r.8 & gcm.all$rain.mm > pctl.95.lng.rn.osv.all & gcm.all$is.sht.rn ,  ] )  /  gcm.near.fut.all.num.lng.rns.tot 
))


sd.quant.95p.days.gcm.r8.lr <- round( sd.quant.95p.days.gcm.r8.lr  , rd.decs.quant.days )
sd.quant.95p.days.gcm.r8.sr <- round( sd.quant.95p.days.gcm.r8.sr  , rd.decs.quant.days )


# Convert climate series into L-DNDC readable csv
if (  TRUE ) {



# RCP 4.5
# GCM 1
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r4.gcm.1  , ] 

LDNDC.clim.in.nf.r4.gcm.1 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r4_gcm_1_raw.txt"       

write.table( LDNDC.clim.in.nf.r4.gcm.1, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 2 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r4.gcm.2  , ] 

LDNDC.clim.in.nf.r4.gcm.2 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r4_gcm_2_raw.txt"       

write.table( LDNDC.clim.in.nf.r4.gcm.2, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 2 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r4.gcm.3  , ] 

LDNDC.clim.in.nf.r4.gcm.3 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r4_gcm_3_raw.txt"       

write.table( LDNDC.clim.in.nf.r4.gcm.3, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 2 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r4.gcm.4  , ] 

LDNDC.clim.in.nf.r4.gcm.4 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r4_gcm_4_raw.txt"       

write.table( LDNDC.clim.in.nf.r4.gcm.4, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 2 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r4.gcm.5  , ] 

LDNDC.clim.in.nf.r4.gcm.5 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r4_gcm_5_raw.txt"       

write.table( LDNDC.clim.in.nf.r4.gcm.5 , file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 2 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r4.gcm.6  , ] 

LDNDC.clim.in.nf.r4.gcm.6 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r4_gcm_6_raw.txt"       

write.table( LDNDC.clim.in.nf.r4.gcm.6, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 7 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r4.gcm.7  , ] 

LDNDC.clim.in.nf.r4.gcm.7 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r4_gcm_7_raw.txt"       

write.table( LDNDC.clim.in.nf.r4.gcm.7, file = file, sep = "\t", row.names = FALSE, quote = FALSE)


# RCP 8.5
# GCM 1
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r8.gcm.1  , ] 

LDNDC.clim.in.nf.r8.gcm.1 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r8_gcm_1_raw.txt"       

write.table( LDNDC.clim.in.nf.r8.gcm.1, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 2 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r8.gcm.2  , ] 

LDNDC.clim.in.nf.r8.gcm.2 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r8_gcm_2_raw.txt"       

write.table( LDNDC.clim.in.nf.r8.gcm.2, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 3 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r8.gcm.3  , ] 

LDNDC.clim.in.nf.r8.gcm.3 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r8_gcm_3_raw.txt"       

write.table( LDNDC.clim.in.nf.r8.gcm.3, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 4 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r8.gcm.4  , ] 

LDNDC.clim.in.nf.r8.gcm.4 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r8_gcm_4_raw.txt"       

write.table( LDNDC.clim.in.nf.r8.gcm.4, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 5
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r8.gcm.5  , ] 

LDNDC.clim.in.nf.r8.gcm.5 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r8_gcm_5_raw.txt"       

write.table( LDNDC.clim.in.nf.r8.gcm.5 , file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 6
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r8.gcm.6  , ] 

LDNDC.clim.in.nf.r8.gcm.6 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r8_gcm_6_raw.txt"       

write.table( LDNDC.clim.in.nf.r8.gcm.6, file = file, sep = "\t", row.names = FALSE, quote = FALSE)

# GCM 7 
dat.in <- gcm.all[ gcm.r.cnd.near.fut.r8.gcm.7  , ] 

LDNDC.clim.in.nf.r8.gcm.7 <- gen.clim.series( dat.in )
file <-  "../KE_Kapiti_future_clim_r8_gcm_7_raw.txt"       

write.table( LDNDC.clim.in.nf.r8.gcm.7, file = file, sep = "\t", row.names = FALSE, quote = FALSE)




# AIR CHEMISTRY
LDNDC.achem.in.nf.r4 <- gen.achem.series( dat.in )

file <-  "../KE_Kapiti_future_air_chem_r4_raw.txt"   

write.table(LDNDC.achem.in.nf.r4, file = file, sep = "\t", row.names = FALSE, quote = FALSE)


}


} # GCM Data prep


{ 
  

{
    
    # TOTAL RAINFALL PER SEASON (MM)
    col.tot.rainf.lng.rains.mm <- c(
      
    round( total.rainf.mm.lng.rn.2019 , rd.decs.rain.day.mm)
      , round( total.rainf.mm.lng.rn.2020 , rd.decs.rain.day.mm)
      , round( total.rainf.mm.lng.rn.2021 , rd.decs.rain.day.mm)
      , round( total.rainf.mm.lng.rn.2022 , rd.decs.rain.day.mm)
      , round( total.rainf.mm.lng.rn.2023 , rd.decs.rain.day.mm)
      , round( total.rainf.mm.lng.rn.2024 , rd.decs.rain.day.mm)
      
      , get.mn.sd.string (
        c(   total.rainf.mm.lng.rn.2019
            , total.rainf.mm.lng.rn.2020
            , total.rainf.mm.lng.rn.2021
            , total.rainf.mm.lng.rn.2022
            , total.rainf.mm.lng.rn.2023
            , total.rainf.mm.lng.rn.2024  
        )
        
        , rd.decs.rain.day.mm
      )
      
      
      ,  round( sum( gcm.all[ gcm.r.cnd.near.fut.r4.lr   ,   'rain.mm' ])  / ( gcm.near.fut.all.num.lng.rns.tot ) , rd.decs.rain.day.mm)
      ,   round( sum( gcm.all[ gcm.r.cnd.near.fut.r8.lr   ,   'rain.mm' ])  / (  gcm.near.fut.all.num.lng.rns.tot ) , rd.decs.rain.day.mm)
      
      
    ) 
    
    
    
    
    # FREQUENCY VARIABLES (NUMBER OF DAYS WITH SPECIFIC RAINFALL LEVELS)
  col.wet.days.lng.rn <- c(
      
      quant.wet.days.lng.rn.2019
    , quant.wet.days.lng.rn.2020
      , quant.wet.days.lng.rn.2021
      , quant.wet.days.lng.rn.2022
      , quant.wet.days.lng.rn.2023
      , quant.wet.days.lng.rn.2024
      
      
      , get.mn.sd.string(
        c(  quant.wet.days.lng.rn.2019
            , quant.wet.days.lng.rn.2020
            , quant.wet.days.lng.rn.2021
            , quant.wet.days.lng.rn.2022
            , quant.wet.days.lng.rn.2023
            , quant.wet.days.lng.rn.2024 
        )
        
        , 0
      )
      
      ,   str_c( quant.wet.days.gcm.r4.lr   , symb.mn.sd  ,  sd.quant.wet.days.gcm.r4.lr    )  
      ,   str_c( quant.wet.days.gcm.r8.lr   , symb.mn.sd  ,  sd.quant.wet.days.gcm.r8.lr )  
      
    )
    
    
    
    col.20.mm.days.lng.rn <- c(
      
     quant.20.mm.days.lng.rn.2019
      , quant.20.mm.days.lng.rn.2020
      , quant.20.mm.days.lng.rn.2021
      , quant.20.mm.days.lng.rn.2022
      , quant.20.mm.days.lng.rn.2023
      , quant.20.mm.days.lng.rn.2024
      
      , get.mn.sd.string(
        c(  quant.20.mm.days.lng.rn.2019
            , quant.20.mm.days.lng.rn.2020
            , quant.20.mm.days.lng.rn.2021
            , quant.20.mm.days.lng.rn.2022
            , quant.20.mm.days.lng.rn.2023
            , quant.20.mm.days.lng.rn.2024 
        )
        
        , 0
      )
      
      ,   str_c( quant.20mm.days.gcm.r4.lr   , symb.mn.sd  ,  sd.quant.20mm.days.gcm.r4.lr    )  
      ,   str_c( quant.20mm.days.gcm.r8.lr   , symb.mn.sd  ,  sd.quant.20mm.days.gcm.r8.lr )  
      
    )
    
    col.95.p.days.lng.rn <- c(
      
  quant.95p.days.lng.rn.2019 
      , quant.95p.days.lng.rn.2020
      , quant.95p.days.lng.rn.2021
      , quant.95p.days.lng.rn.2022 
      , quant.95p.days.lng.rn.2023
      , quant.95p.days.lng.rn.2024 
      
      , get.mn.sd.string(
        c(quant.95p.days.lng.rn.2019
              ,   quant.95p.days.lng.rn.2020
              ,   quant.95p.days.lng.rn.2021
              ,   quant.95p.days.lng.rn.2022
              ,  quant.95p.days.lng.rn.2023
              ,   quant.95p.days.lng.rn.2024
        )
        
        , 1
      )
      
      ,   str_c( quant.95p.days.gcm.r4.lr   , symb.mn.sd  ,  sd.quant.95p.days.gcm.r4.lr )  
      ,   str_c( quant.95p.days.gcm.r8.lr   , symb.mn.sd  ,  sd.quant.95p.days.gcm.r8.lr )  
      
    )
    
    
    # RAINFALL INTENSITY - Mean rain per day  
    col.mean.rain.wet.days.lng.rn <- c(
      
     str_c( mean.per.wet.day.mm.lng.rn.2019 ,  symb.mn.sd , sd.per.wet.day.mm.lng.rn.2019 )
      , str_c( mean.per.wet.day.mm.lng.rn.2020 ,  symb.mn.sd , sd.per.wet.day.mm.lng.rn.2020 )
      ,  str_c( mean.per.wet.day.mm.lng.rn.2021 ,  symb.mn.sd , sd.per.wet.day.mm.lng.rn.2021 )
      ,  str_c( mean.per.wet.day.mm.lng.rn.2022 ,  symb.mn.sd , sd.per.wet.day.mm.lng.rn.2022 )
      ,  str_c( mean.per.wet.day.mm.lng.rn.2023 ,  symb.mn.sd , sd.per.wet.day.mm.lng.rn.2023 ) 
      ,  str_c( mean.per.wet.day.mm.lng.rn.2024 ,  symb.mn.sd , sd.per.wet.day.mm.lng.rn.2024 )
      
      ,    get.mn.sd.string(   d.all[  r.cnd.all.lr.wet.d & d.all$precip.osv > 0     ,  'precip.osv' ]  , rd.decs.rain.day.mm   )  
      
      
      ,    get.mn.sd.string(   gcm.all[ gcm.r.cnd.near.fut.r4.lr & gcm.all$rain.mm > 0     ,  'rain.mm' ]  , rd.decs.rain.day.mm   )     
      ,    get.mn.sd.string(   gcm.all[ gcm.r.cnd.near.fut.r8.lr & gcm.all$rain.mm > 0     ,  'rain.mm' ]  , rd.decs.rain.day.mm   )     
      
    )
    
    col.mean.rain.20.mm.days.lng.rn <- c(
      
   str_c( mean.per.20.mm.day.lng.rn.2019 ,  symb.mn.sd , sd.per.20.mm.day.lng.rn.2019 )
      , str_c( mean.per.20.mm.day.lng.rn.2020 ,  symb.mn.sd , sd.per.20.mm.day.lng.rn.2020 )
      ,  str_c( mean.per.20.mm.day.lng.rn.2021 ,  symb.mn.sd , sd.per.20.mm.day.lng.rn.2021 )
      ,  str_c( mean.per.20.mm.day.lng.rn.2022 ,  symb.mn.sd , sd.per.20.mm.day.lng.rn.2022 )
      ,  str_c( mean.per.20.mm.day.lng.rn.2023 ,  symb.mn.sd , sd.per.20.mm.day.lng.rn.2023 ) 
      ,  str_c( mean.per.20.mm.day.lng.rn.2024 ,  symb.mn.sd , sd.per.20.mm.day.lng.rn.2024 )
      
      ,    get.mn.sd.string(   d.all[  r.cnd.all.lr.wet.d & d.all$precip.osv > 20     ,  'precip.osv' ]  , rd.decs.rain.day.mm   )  
      
      
      
      ,    get.mn.sd.string(   gcm.all[ gcm.r.cnd.near.fut.r4.lr & gcm.all$rain.mm > 20     ,  'rain.mm' ]  , rd.decs.rain.day.mm   )     
      ,    get.mn.sd.string(   gcm.all[ gcm.r.cnd.near.fut.r8.lr & gcm.all$rain.mm > 20     ,  'rain.mm' ]  , rd.decs.rain.day.mm   )     
      
      
    )
    
    
  } # LONG RAINS  
  
  
{
    
# TOTAL RAINFALL PER SEASON (MM)
col.tot.rainf.sht.rains.mm <- c(
        
        round( total.rainf.mm.sht.rn.2018 , rd.decs.rain.day.mm)
        , round( total.rainf.mm.sht.rn.2019 , rd.decs.rain.day.mm)
        , round( total.rainf.mm.sht.rn.2020 , rd.decs.rain.day.mm)
        , round( total.rainf.mm.sht.rn.2021 , rd.decs.rain.day.mm)
        , round( total.rainf.mm.sht.rn.2022 , rd.decs.rain.day.mm)
        , round( total.rainf.mm.sht.rn.2023 , rd.decs.rain.day.mm)
        , round( total.rainf.mm.sht.rn.2024 , rd.decs.rain.day.mm)
        
        , get.mn.sd.string (
          c(  total.rainf.mm.sht.rn.2018
              , total.rainf.mm.sht.rn.2019
              , total.rainf.mm.sht.rn.2020
              , total.rainf.mm.sht.rn.2021
              , total.rainf.mm.sht.rn.2022
              , total.rainf.mm.sht.rn.2023
              , total.rainf.mm.sht.rn.2024  
                  )
            
            , rd.decs.rain.day.mm
          )
          

        ,  round( sum( gcm.all[ gcm.r.cnd.near.fut.r4.sr   ,   'rain.mm' ])  / ( gcm.near.fut.all.num.sht.rns.tot ) , rd.decs.rain.day.mm)
        ,   round( sum( gcm.all[ gcm.r.cnd.near.fut.r8.sr   ,   'rain.mm' ])  / (  gcm.near.fut.all.num.sht.rns.tot ) , rd.decs.rain.day.mm)
        
        
      ) 
 
  

  
    # FREQUENCY VARIABLES (NUMBER OF DAYS WITH SPECIFIC RAINFALL LEVELS)
    col.wet.days.sht.rn <- c(
      
        quant.wet.days.sht.rn.2018
      , quant.wet.days.sht.rn.2019
      , quant.wet.days.sht.rn.2020
      , quant.wet.days.sht.rn.2021
      , quant.wet.days.sht.rn.2022
      , quant.wet.days.sht.rn.2023
      , quant.wet.days.sht.rn.2024
      
      
      , get.mn.sd.string(
        c(  quant.wet.days.sht.rn.2018
            , quant.wet.days.sht.rn.2019
            , quant.wet.days.sht.rn.2020
            , quant.wet.days.sht.rn.2021
            , quant.wet.days.sht.rn.2022
            , quant.wet.days.sht.rn.2023
            , quant.wet.days.sht.rn.2024 
        )
        
        , 0
      )
    
      ,   str_c( quant.wet.days.gcm.r4.sr   , symb.mn.sd  ,  sd.quant.wet.days.gcm.r4.sr    )  
      ,   str_c( quant.wet.days.gcm.r8.sr   , symb.mn.sd  ,  sd.quant.wet.days.gcm.r8.sr )  
    
    )
    


    col.20.mm.days.sht.rn <- c(
      
      quant.20.mm.days.sht.rn.2018
      , quant.20.mm.days.sht.rn.2019
      , quant.20.mm.days.sht.rn.2020
      , quant.20.mm.days.sht.rn.2021
      , quant.20.mm.days.sht.rn.2022
      , quant.20.mm.days.sht.rn.2023
      , quant.20.mm.days.sht.rn.2024
      
      , get.mn.sd.string(
        c(  quant.20.mm.days.sht.rn.2018
            , quant.20.mm.days.sht.rn.2019
            , quant.20.mm.days.sht.rn.2020
            , quant.20.mm.days.sht.rn.2021
            , quant.20.mm.days.sht.rn.2022
            , quant.20.mm.days.sht.rn.2023
            , quant.20.mm.days.sht.rn.2024 
        )
        
        , 0
      )
      
      ,   str_c( quant.20mm.days.gcm.r4.sr   , symb.mn.sd  ,  sd.quant.20mm.days.gcm.r4.sr    )  
      ,   str_c( quant.20mm.days.gcm.r8.sr   , symb.mn.sd  ,  sd.quant.20mm.days.gcm.r8.sr )  
      
    )
    
    col.95.p.days.sht.rn <- c(
      
      quant.95p.days.sht.rn.2018
      , quant.95p.days.sht.rn.2019 
      , quant.95p.days.sht.rn.2020
      , quant.95p.days.sht.rn.2021
      , quant.95p.days.sht.rn.2022 
      , quant.95p.days.sht.rn.2023
      , quant.95p.days.sht.rn.2024 
      
      , get.mn.sd.string(
        c(    quant.95p.days.sht.rn.2018
            ,   quant.95p.days.sht.rn.2019
            ,   quant.95p.days.sht.rn.2020
            ,   quant.95p.days.sht.rn.2021
            ,   quant.95p.days.sht.rn.2022
            ,  quant.95p.days.sht.rn.2023
            ,   quant.95p.days.sht.rn.2024
        )
        
        , 1
      )
      
      ,   str_c( quant.95p.days.gcm.r4.sr   , symb.mn.sd  ,  sd.quant.95p.days.gcm.r4.sr )  
      ,   str_c( quant.95p.days.gcm.r8.sr   , symb.mn.sd  ,  sd.quant.95p.days.gcm.r8.sr )  
      
    )
    
    
    # RAINFALL INTENSITY - Mean rain per day  
    col.mean.rain.wet.days.sht.rn <- c(
      
      str_c( mean.per.wet.day.mm.sht.rn.2018 ,  symb.mn.sd , sd.per.wet.day.mm.sht.rn.2018 )
      , str_c( mean.per.wet.day.mm.sht.rn.2019 ,  symb.mn.sd , sd.per.wet.day.mm.sht.rn.2019 )
      , str_c( mean.per.wet.day.mm.sht.rn.2020 ,  symb.mn.sd , sd.per.wet.day.mm.sht.rn.2020 )
      ,  str_c( mean.per.wet.day.mm.sht.rn.2021 ,  symb.mn.sd , sd.per.wet.day.mm.sht.rn.2021 )
      ,  str_c( mean.per.wet.day.mm.sht.rn.2022 ,  symb.mn.sd , sd.per.wet.day.mm.sht.rn.2022 )
      ,  str_c( mean.per.wet.day.mm.sht.rn.2023 ,  symb.mn.sd , sd.per.wet.day.mm.sht.rn.2023 ) 
      ,  str_c( mean.per.wet.day.mm.sht.rn.2024 ,  symb.mn.sd , sd.per.wet.day.mm.sht.rn.2024 )
      
      ,    get.mn.sd.string(   d.all[  r.cnd.all.sr.wet.d & d.all$precip.osv > 0     ,  'precip.osv' ]  , rd.decs.rain.day.mm   )  
      
      
      ,    get.mn.sd.string(   gcm.all[ gcm.r.cnd.near.fut.r4.sr & gcm.all$rain.mm > 0     ,  'rain.mm' ]  , rd.decs.rain.day.mm   )     
      ,    get.mn.sd.string(   gcm.all[ gcm.r.cnd.near.fut.r8.sr & gcm.all$rain.mm > 0     ,  'rain.mm' ]  , rd.decs.rain.day.mm   )     
      
    )
    
    col.mean.rain.20.mm.days.sht.rn <- c(
      
      str_c( mean.per.20.mm.day.sht.rn.2018 ,  symb.mn.sd , sd.per.20.mm.day.sht.rn.2018 )
      , str_c( mean.per.20.mm.day.sht.rn.2019 ,  symb.mn.sd , sd.per.20.mm.day.sht.rn.2019 )
      , str_c( mean.per.20.mm.day.sht.rn.2020 ,  symb.mn.sd , sd.per.20.mm.day.sht.rn.2020 )
      ,  str_c( mean.per.20.mm.day.sht.rn.2021 ,  symb.mn.sd , sd.per.20.mm.day.sht.rn.2021 )
      ,  str_c( mean.per.20.mm.day.sht.rn.2022 ,  symb.mn.sd , sd.per.20.mm.day.sht.rn.2022 )
      ,  str_c( mean.per.20.mm.day.sht.rn.2023 ,  symb.mn.sd , sd.per.20.mm.day.sht.rn.2023 ) 
      ,  str_c( mean.per.20.mm.day.sht.rn.2024 ,  symb.mn.sd , sd.per.20.mm.day.sht.rn.2024 )
     
      ,    get.mn.sd.string(   d.all[  r.cnd.all.sr.wet.d & d.all$precip.osv > 20     ,  'precip.osv' ]  , rd.decs.rain.day.mm   )  
      
      
      
      ,    get.mn.sd.string(   gcm.all[ gcm.r.cnd.near.fut.r4.sr & gcm.all$rain.mm > 20     ,  'rain.mm' ]  , rd.decs.rain.day.mm   )     
      ,    get.mn.sd.string(   gcm.all[ gcm.r.cnd.near.fut.r8.sr & gcm.all$rain.mm > 20     ,  'rain.mm' ]  , rd.decs.rain.day.mm   )     
      
       
    )
    
    
  } # SHORT RAINS
  
  length(col.tot.rainf.lng.rains.mm )  
  length(col.wet.days.lng.rn  )  
  length(col.mean.rain.wet.days.lng.rn )  
  length(col.20.mm.days.lng.rn )  
  length(col.mean.rain.20.mm.days.lng.rn )  
  length(col.95.p.days.lng.rn) 
  
  
  
length(col.tot.rainf.sht.rains.mm )  
length(col.wet.days.sht.rn  )  
length(col.mean.rain.wet.days.sht.rn )  
length(col.20.mm.days.sht.rn )  
length(col.mean.rain.20.mm.days.sht.rn )  
length(col.95.p.days.sht.rn) 

  
osv.rainf.table.lr <- cbind(
    
col.tot.rainf.lng.rains.mm 

,   col.wet.days.lng.rn 
,col.mean.rain.wet.days.lng.rn 

, col.20.mm.days.lng.rn
,   col.mean.rain.20.mm.days.lng.rn 

, col.95.p.days.lng.rn

)


osv.rainf.table.sr <- cbind(
  
  col.tot.rainf.sht.rains.mm 
  
  ,   col.wet.days.sht.rn 
  ,col.mean.rain.wet.days.sht.rn 
  
  , col.20.mm.days.sht.rn
  ,   col.mean.rain.20.mm.days.sht.rn 
  
  , col.95.p.days.sht.rn
  
)

  


write.csv(   osv.rainf.table.lr   , "osv.lr.rainf.table.csv", fileEncoding = "latin1", row.names = FALSE)
write.csv(   osv.rainf.table.sr   , "osv.sr.rainf.table.csv", fileEncoding = "latin1", row.names = FALSE)

 

  
} # Table out


}  # Observed data prep.







