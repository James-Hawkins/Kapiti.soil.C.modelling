


d.eddy.raw <<- read.csv('Kapiti_AllYears_QC_ReddyPro.csv')
  
d.eddy.partn.raw <<- read.csv('Kapiti_Partitioned_Fluxes.csv')  


names(d.eddy.raw)[1] <- 'date'
names(d.eddy.partn.raw)[1] <- 'date'


d.eddy.raw$date <- as.Date(d.eddy.raw$date ,  format="%m/%d/%Y")
d.eddy.partn.raw$date <- as.Date(d.eddy.partn.raw$date ,  format="%m/%d/%Y")

first.date <- d.eddy.partn.raw$date[1]
last.date <- d.eddy.raw$date[nrow(d.eddy.raw)]

d.eddy.raw <- d.eddy.raw[ d.eddy.raw$date >= first.date  ,    ]
d.eddy.raw <- d.eddy.raw [ d.eddy.raw$date <= last.date ,    ]

d.eddy.raw$date[1]
d.eddy.partn.raw$date[1]

d.eddy.raw$date[nrow(d.eddy.raw)]
tail(d.eddy.partn.raw$date)

nrow(d.eddy.partn.raw)
nrow(d.eddy.raw)



no.dat.value <- -90

d.eddy.raw[d.eddy.raw$NEE < no.dat.value , 'NEE' ] <- NA
d.eddy.raw[d.eddy.raw$H < no.dat.value , 'H' ] <- NA
d.eddy.raw[d.eddy.raw$LE < no.dat.value , 'LE' ] <- NA
d.eddy.raw[d.eddy.raw$h2o_flux < no.dat.value , 'h2o_flux' ] <- NA
d.eddy.raw[d.eddy.raw$Rg < no.dat.value , 'Rg' ] <- NA

d.eddy.raw[d.eddy.raw$RH < no.dat.value , 'RH' ] <- NA
d.eddy.raw[d.eddy.raw$wind_speed < no.dat.value , 'wind_speed' ] <- NA

d.eddy.raw[d.eddy.raw$Temp < no.dat.value, 'Temp' ] <- NA
d.eddy.raw[d.eddy.raw$Precip < no.dat.value , 'Precip' ] <- NA

d.eddy.raw[d.eddy.raw$SWC_3_1_1 < no.dat.value , 'SWC_3_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$SWC_2_1_1 < no.dat.value, 'SWC_2_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$SWC_1_1_1 < no.dat.value , 'SWC_1_1_1' ] <- NA

d.eddy.raw[d.eddy.raw$Ts_1_1_1 < no.dat.value , 'Ts_1_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$Ts_2_1_1 < no.dat.value , 'Ts_2_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$Ts_3_1_1 < no.dat.value , 'Ts_3_1_1' ] <- NA

d.eddy.real<- data.frame()

unique.dates <- unique(d.eddy.raw$date)
len.unique.dates <- length(unique.dates)

View(d.eddy.raw)



# SWC_3_1_1 : 5 cm
# SWC_2_1_1 : 15
# SWC_1_1_1 : 30


for (i in 1:len.unique.dates )  {
  
  
  current.date <- unique.dates[i] 
  
  print(paste('current date is' ,  current.date))
  
  d.eddy.real[ i , 'date' ] <- as.Date(unique(d.eddy.raw[d.eddy.raw$date == current.date , 'date']))
  
  # Calculate means
  d.eddy.real[ i , 'nee.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'NEE']  ))
  
  
  d.eddy.real[ i , 'h.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'H']  ))
  d.eddy.real[ i , 'le.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'LE']  ))
  
  
  d.eddy.real[ i , 'ws.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'wind_speed']  ))
  d.eddy.real[ i , 'rh.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'RH']  ))
  
  
  d.eddy.real[ i , 'temp.avg.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']  ) )
  d.eddy.real[ i , 'temp.min.osv' ] <- min( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']   ))
  d.eddy.real[ i , 'temp.max.osv' ] <- max( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']  ) )
  
  
  d.eddy.real[ i , 'precip.osv' ] <- 48 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Precip'])  )
 
  d.eddy.real[ i , 'rg.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Rg']  ) )
  
  d.eddy.real[ i , 'swc.1.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_1_1_1'] ))
  d.eddy.real[ i , 'swc.2.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_2_1_1'] ))
  d.eddy.real[ i , 'swc.3.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_3_1_1'] ))
  
  d.eddy.real[ i , 'ts.1.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_1_1_1']))
  d.eddy.real[ i , 'ts.2.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_2_1_1']))
  d.eddy.real[ i , 'ts.3.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_3_1_1']))
  
  
  d.eddy.real[ i , 'gpp.osv' ] <- mean( na.omit( d.eddy.partn.raw[d.eddy.partn.raw$date == current.date , 'GPP_DT']))
  d.eddy.real[ i , 'reco.osv' ] <- mean( na.omit( d.eddy.partn.raw[d.eddy.partn.raw$date == current.date , 'Reco_DT']))
  
  
}


d.eddy.real[  , 'date' ] <- as.Date(d.eddy.real[  , 'date' ] )

d.eddy.real  <<- d.eddy.real 


first.date.cald <- "2018-07-28"
secd.date.cald <- "2024-05-10"

# Data clip 
# Actual (EC tower) data
# EC tower data
{
  
  # Observed data
  d.eddy.real$date <- as.Date(d.eddy.real$date , "%Y-%m-%d")
  
  
  d.eddy.real <- d.eddy.real[
    d.eddy.real$date >= first.date.cald 
    & d.eddy.real$date <= secd.date.cald
    ,  ]
  
  nrow(d.eddy.real)
}


summary(d.eddy.real$rg.osv)
max(d.eddy.real$precip.osv)

d.eddy.real[d.eddy.real$date == first.date.cald , 'precip.osv']

# HANDLE NAs
d.eddy.oc <- d.eddy.real

d.eddy.oc[  is.na(d.eddy.oc$rg.osv) , 'rg.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$rg.osv) , 'rg.osv']  ))

d.eddy.oc[  is.na(d.eddy.oc$temp.avg.osv) | d.eddy.oc$temp.avg.osv == Inf | d.eddy.oc$temp.avg.osv == -Inf, 'temp.avg.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$temp.avg.osv) & !(d.eddy.oc$temp.avg.osv == Inf) & !(d.eddy.oc$temp.avg.osv == -Inf), 'temp.avg.osv']  ))
d.eddy.oc[  is.na(d.eddy.oc$temp.min.osv) | d.eddy.oc$temp.min.osv == Inf | d.eddy.oc$temp.min.osv == -Inf, 'temp.min.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$temp.min.osv) & !(d.eddy.oc$temp.min.osv == Inf) & !(d.eddy.oc$temp.min.osv == -Inf) , 'temp.min.osv']  ))
d.eddy.oc[  is.na(d.eddy.oc$temp.max.osv)  | d.eddy.oc$temp.max.osv == Inf | d.eddy.oc$temp.max.osv == -Inf , 'temp.max.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$temp.max.osv) & !(d.eddy.oc$temp.max.osv == Inf) & !(d.eddy.oc$temp.max.osv == -Inf) , 'temp.max.osv']  ))

d.eddy.oc[  is.na(d.eddy.oc$precip.osv) , 'precip.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$precip.osv) , 'precip.osv']  ))


d.eddy.oc[  is.na(d.eddy.oc$rh.osv) , 'rh.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$rh.osv) , 'rh.osv']  ))
d.eddy.oc[  is.na(d.eddy.oc$ws.osv) , 'ws.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$ws.osv) , 'ws.osv']  ))


summary(   d.eddy.oc$temp.avg.osv)
summary( d.eddy.oc$temp.max.osv)
summary( d.eddy.oc$temp.min.osv)


summary(   d.eddy.oc$precip.osv)
summary( d.eddy.oc$rg.osv)


# climate data out

{
  
d.eddy.oc$day.count <- NA
start.day.count <- 209
d.eddy.oc[1, 'day.count'] <- start.day.count
d.eddy.oc[1, 'year'] <- "2018"

for (r in 2:nrow(d.eddy.oc)) {
  
  date <- d.eddy.oc[r, 'date']
  year <-   format(as.Date(date), "%Y")
  d.eddy.oc[r, 'year'] <-   year
  
  
  if (year ==  d.eddy.oc[r-1, 'year'] ){
    
    d.eddy.oc[r, 'day.count'] <- 1 + d.eddy.oc[r-1, 'day.count']
    
  } else {
    
    d.eddy.oc[r, 'day.count'] <- 1 
    
  }
  

}

decimal.round <- 2

d.eddy.clim.out <- data.frame(
  
  format(as.Date(d.eddy.oc$date), "%Y")
  , d.eddy.oc$day.count
  , round ( d.eddy.oc$temp.avg.osv , decimal.round ) 
  
  ,  round ( d.eddy.oc$temp.min.osv , decimal.round ) 
  ,  round ( d.eddy.oc$temp.max.osv , decimal.round ) 
  
  ,  round ( d.eddy.oc$rg.osv , decimal.round ) 
  
  ,  round ( d.eddy.oc$precip.osv , decimal.round ) 
  ,  round ( d.eddy.oc$rh.osv , decimal.round ) 
  ,  round ( d.eddy.oc$ws.osv , decimal.round ) 
)

colnames( d.eddy.clim.out ) <- c(
  '*'
  ,'*'
  , 'tavg'
  , 'tmin'
  , 'tmax'
  , 'grad'
  , 'prec'
  , 'rh'
  , 'wind'

)
  
write.table(d.eddy.clim.out, file = "KE_Kapiti_climate_eddy.txt", 
            append = FALSE, sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE)



}

write.csv(d.eddy.real,"d.eddy.real.new.csv", row.names = FALSE)
write.csv(d.eddy.oc,"d.eddy.oc.csv", row.names = FALSE)




View(d.eddy.clim.out)

