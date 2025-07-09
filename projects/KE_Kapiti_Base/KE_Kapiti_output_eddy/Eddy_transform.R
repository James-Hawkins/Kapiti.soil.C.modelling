



d.eddy.raw  <<- read.csv('Kapiti_Flux_Biomet_Data.csv')


names(d.eddy.raw)[1] <- 'date'


d.eddy.raw$date <- as.Date(d.eddy.raw$date ,  format="%m/%d/%Y")



no.dat.value <- -90

d.eddy.raw[d.eddy.raw$NEE < no.dat.value , 'NEE' ] <- NA
d.eddy.raw[d.eddy.raw$H < no.dat.value , 'H' ] <- NA
d.eddy.raw[d.eddy.raw$LE < no.dat.value , 'LE' ] <- NA
d.eddy.raw[d.eddy.raw$h2o_flux < no.dat.value , 'h2o_flux' ] <- NA
d.eddy.raw[d.eddy.raw$Rg < no.dat.value , 'Rg' ] <- NA

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

for (i in 1:len.unique.dates )  {
  
  
  current.date <- unique.dates[i] 
  
  print(paste('current date is' ,  current.date))
  
  d.eddy.real[ i , 'date' ] <- as.Date(unique(d.eddy.raw[d.eddy.raw$date == current.date , 'date']))
  
  # Calculate means
  d.eddy.real[ i , 'nee.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'NEE']  ))
  
  
  d.eddy.real[ i , 'h.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'H']  ))
  d.eddy.real[ i , 'le.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'LE']  ))
  
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
  
  
}


d.eddy.real[  , 'date' ] <- as.Date(d.eddy.real[  , 'date' ] )



d.eddy.real  <<- d.eddy.real 


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


summary(d.eddy.real$precip.osv)
max(d.eddy.real$precip.osv)

d.eddy.real[d.eddy.real$date == first.date.cald , 'precip.osv']

# HANDLE NAs
d.eddy.oc <- d.eddy.real

d.eddy.oc[  is.na(d.eddy.oc$rg.osv) , 'rg.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$rg.osv) , 'rg.osv']  ))

d.eddy.oc[  is.na(d.eddy.oc$temp.avg.osv) | d.eddy.oc$temp.avg.osv == Inf | d.eddy.oc$temp.avg.osv == -Inf, 'temp.avg.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$temp.avg.osv) & !(d.eddy.oc$temp.avg.osv == Inf) & !(d.eddy.oc$temp.avg.osv == -Inf), 'temp.avg.osv']  ))
d.eddy.oc[  is.na(d.eddy.oc$temp.min.osv) | d.eddy.oc$temp.min.osv == Inf | d.eddy.oc$temp.min.osv == -Inf, 'temp.min.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$temp.min.osv) & !(d.eddy.oc$temp.min.osv == Inf) & !(d.eddy.oc$temp.min.osv == -Inf) , 'temp.min.osv']  ))
d.eddy.oc[  is.na(d.eddy.oc$temp.max.osv)  | d.eddy.oc$temp.max.osv == Inf | d.eddy.oc$temp.max.osv == -Inf , 'temp.max.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$temp.max.osv) & !(d.eddy.oc$temp.max.osv == Inf) & !(d.eddy.oc$temp.max.osv == -Inf) , 'temp.max.osv']  ))

d.eddy.oc[  is.na(d.eddy.oc$precip.osv) , 'precip.osv'] <- mean(na.omit(d.eddy.oc[  !is.na(d.eddy.oc$precip.osv) , 'precip.osv']  ))


d.eddy.oc$temp.avg.osv
d.eddy.oc$temp.max.osv
d.eddy.oc$temp.min.osv



write.csv(d.eddy.real.new,"d.eddy.real.new.csv", row.names = FALSE)
write.csv(d.eddy.oc,"d.eddy.oc.csv", row.names = FALSE)

first.date.cald <- "2023-04-22"
secd.date.cald <- "2024-05-10"
