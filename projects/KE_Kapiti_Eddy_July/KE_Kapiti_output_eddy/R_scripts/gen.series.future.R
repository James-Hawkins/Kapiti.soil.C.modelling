


clm.srs <- data.frame(matrix(ncol=5,nrow=1))


clm.srs$rain.mm <- NA




  

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



ssn.SR <- 'sr' ; ssn.LR <- 'lr' ; ssn.LD <- 'ld' ; ssn.SD <- 'sd'  ; ssns.rny <- c(  ssn.LR , ssn.SR)

d.all[d.all$is.sht.rn , 'season'] <- ssn.SR
d.all[d.all$is.lng.rn , 'season'] <- ssn.LR
d.all[d.all$is.sht.dr , 'season'] <- ssn.SD
d.all[d.all$is.lng.dr , 'season'] <- ssn.LD


# OBSERVATION PERIOD - SUMMARY STATS

mean.rf.mm.d.lng.rn <- mean( na.omit(d.all[ d.all$is.lng.rn  , 'precip.osv']))
mean.rf.mm.d.sht.rn <- mean( na.omit(d.all[ d.all$is.sht.rn  , 'precip.osv']))

mean.rf.mm.d.lng.dr <- mean( na.omit(d.all[ d.all$is.lng.dr  , 'precip.osv']))
mean.rf.mm.d.sht.dr <- mean( na.omit(d.all[ d.all$is.sht.dr  , 'precip.osv']))


# Wet days
quant.wet.days.lng.rn.2019 <- sum( d.all[ d.all$is.lng.rn & d.all$cald.yr == '2019' , 'precip.osv'] > 0 )

# Mean rain per wet day
mean.per.wet.day.mm.lng.rn.2019 <- mean(  d.all[ d.all$is.lng.rn & d.all$cald.yr == '2019' & d.all$rain.day , 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2020 <- mean(  d.all[ d.all$is.lng.rn & d.all$cald.yr == '2020' & d.all$rain.day , 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2021 <- mean(  d.all[ d.all$is.lng.rn & d.all$cald.yr == '2021' & d.all$rain.day , 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2022 <- mean(  d.all[ d.all$is.lng.rn & d.all$cald.yr == '2022' & d.all$rain.day , 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2023 <- mean(  d.all[ d.all$is.lng.rn & d.all$cald.yr == '2023' & d.all$rain.day , 'precip.osv']  )
mean.per.wet.day.mm.lng.rn.2024 <- mean(  d.all[ d.all$is.lng.rn & d.all$cald.yr == '2024' & d.all$rain.day , 'precip.osv']  )

  
  

# MAM 
quant.20.mm.lng.rn.2019 <-      sum( d.all[ d.all$is.lng.rn & d.all$cald.yr == '2019' , 'precip.osv'] > 20 )
quant.20.mm.lng.rn.2020 <-      sum( d.all[ d.all$is.lng.rn & d.all$cald.yr == '2020' , 'precip.osv'] > 20 )
quant.20.mm.lng.rn.2021 <-      sum( d.all[ d.all$is.lng.rn & d.all$cald.yr == '2021' , 'precip.osv'] > 20 )
quant.20.mm.lng.rn.2022 <-      sum( d.all[ d.all$is.lng.rn & d.all$cald.yr == '2022' , 'precip.osv'] > 20 )
quant.20.mm.lng.rn.2023 <-      sum( d.all[ d.all$is.lng.rn & d.all$cald.yr == '2023' , 'precip.osv'] > 20 )
quant.20.mm.lng.rn.2024 <-     sum( d.all[ d.all$is.lng.rn & d.all$cald.yr == '2023' , 'precip.osv'] > 20 )




# 95 - P

#  95 percentiles
pctl.95.lng.rn.2019 <- as.numeric( quantile( d.all[  d.all$rain.day    ,  'precip.osv' ]  , probs = c( 0.95)))




rainf.pctl.95.lng.rn.osv <- pctl.95.lng.rn.2019 




#  Quant 95P per year
quant.95p.days.lng.rn.2019 <- sum( d.all$cald.yr == '2019'  & d.all$rain.day & d.all$is.lng.rn &  d.all$precip.osv >= pctl.95.lng.rn.osv )
quant.95p.days.lng.rn.2020 <- sum( d.all$cald.yr == '2020'  & d.all$rain.day & d.all$is.lng.rn &  d.all$precip.osv >= pctl.95.lng.rn.osv )
quant.95p.days.lng.rn.2021 <- sum( d.all$cald.yr == '2021'  & d.all$rain.day & d.all$is.lng.rn &  d.all$precip.osv >= pctl.95.lng.rn.osv )
quant.95p.days.lng.rn.2022 <- sum( d.all$cald.yr == '2022'  & d.all$rain.day & d.all$is.lng.rn &  d.all$precip.osv >= pctl.95.lng.rn.osv )
quant.95p.days.lng.rn.2023 <- sum( d.all$cald.yr == '2023'  & d.all$rain.day & d.all$is.lng.rn &  d.all$precip.osv >= pctl.95.lng.rn.osv )
quant.95p.days.lng.rn.2024 <- sum( d.all$cald.yr == '2024'  & d.all$rain.day & d.all$is.lng.rn &  d.all$precip.osv >= pctl.95.lng.rn.osv )


# MEAN 95P across all observed years
mean.95p.days.lng.rn.osv <- mean( 
  
  quant.95p.days.lng.rn.2019
  , quant.95p.days.lng.rn.2020
  , quant.95p.days.lng.rn.2021
  , quant.95p.days.lng.rn.2022 
  , quant.95p.days.lng.rn.2023
  , quant.95p.days.lng.rn.2024
  
  )


# Standard deviation in 95P across all observed years
sd.95p.days.lng.rn.osv <- sd( c(
  
  quant.95p.days.lng.rn.2019
  , quant.95p.days.lng.rn.2020
  , quant.95p.days.lng.rn.2021
  , quant.95p.days.lng.rn.2022 
  , quant.95p.days.lng.rn.2023
  , quant.95p.days.lng.rn.2024
)
)


# REPLICATE FOR 20 MM

mean.20.mm.days.lng.rn.osv <- 5.5




quant.20.mm.lng.rn.2019
quant.20.mm.lng.rn.2020
quant.20.mm.lng.rn.2021
quant.20.mm.lng.rn.2022
quant.20.mm.lng.rn.2023
quant.20.mm.lng.rn.2024




quant.20.mm.lng.rn.osv <- 20


# FUTURE PROJECTIONS

nr.fut.start <- as.Date("2035-10-30")
nr.fut.end <- as.Date("2050-08-30")

clm.srs.gen.date <- seq(from = nr.fut.start , to = nr.fut.end, by = "day")

for (  d in 1 : length( clm.srs.gen.date )  )   { clm.srs[d,'date'] <- clm.srs.gen.date[d]    } 

# MODIFICATION PARAMETERS



for ( d in 1 : length( clm.srs.gen.date ) )   {
  
  # test: d <- 1
  
  date <- clm.srs.gen.date[d] 
  
  # clm.srs[d,'date'] <- date 
  
  
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
    
    
    # 
    if (  ssn == ssn.LR  ){
      
    # The below parameters get modified by scenario specific coefficients  
    quant.days.20.mm <- mean.20.mm.days.lng.rn.osv
    quant.days.95.pctl <- mean.95p.days.lng.rn.osv
    
    rainf.95p.mm <- pctl.95.lng.rn.osv
    rainf.20.mm <- quant.20.mm.lng.rn.osv
    
    
    days.wet <- 10
    total.rainf.mm <- 150
      
      
      
    } else if ( ssn == ssn.SR){
      
      
      
      
      
    }
    
    period.days <- length( current.period  )
    
    
    # Randomly select days > 20 mm rainfall
    days.20.mm <- sample( 1 : period.days , quant.days.20.mm , replace = FALSE)
    
    
    # Randomly select days > 95 percentile rainfall
    days.95.pctl <- sample( 1 : quant.days.20.mm , quant.days.95.pctl, replace = FALSE)
    days.95.pctl <- days.20.mm[days.95.pctl]
    

    #  Specify actual calendar dates for >20 mm and 95 pctl events
    cald.dates.95.pctl <- current.period[   days.95.pctl ]
    cald.dates.20.mm <- current.period[  days.20.mm  ]
    
    cald.dates.20.mm <- cald.dates.20.mm[  which( cald.dates.20.mm != cald.dates.95.pctl )     ]  
    
    
    # For the above selected calendar dates, specify the actual rainfall as random value within range
    
    clm.srs[  clm.srs$date  ==  cald.dates.95.pctl   ,'rain.mm']  <-  rainf.95p.mm 
    clm.srs[  clm.srs$date  ==  cald.dates.20.mm   ,'rain.mm']  <- rainf.20.mm
    
    #' then, for the other wet days, specify rainfall simply as the total remaining 
    #' rainfall to fall divided by the number of days
    
    
    
  }
  
  
}









for (r in 1:365*7){
  
  
  
  
  
  
}





colnames(d.all)


unique(d.all$period)


.3*mean(   d.all$precip.osv)*365



17/136

colnames(d.all)
