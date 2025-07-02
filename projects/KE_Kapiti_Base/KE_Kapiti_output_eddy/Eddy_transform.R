



d.eddy.real  <<- read.csv('Kapiti_Flux_Biomet_Data.csv')


names(d.eddy.real)[1] <- 'date.time'


d.eddy.real$date <- as.Date(d.eddy.real$date.time ,  format="%m/%d/%Y")



no.dat.value <- -90

d.eddy.real[d.eddy.real$NEE < no.dat.value , 'NEE' ] <- NA
d.eddy.real[d.eddy.real$H < no.dat.value , 'H' ] <- NA
d.eddy.real[d.eddy.real$LE < no.dat.value , 'LE' ] <- NA
d.eddy.real[d.eddy.real$h2o_flux < no.dat.value , 'h2o_flux' ] <- NA
d.eddy.real[d.eddy.real$Rg < no.dat.value , 'Rg' ] <- NA

d.eddy.real[d.eddy.real$Temp < no.dat.value, 'Temp' ] <- NA
d.eddy.real[d.eddy.real$Precip < no.dat.value , 'Precip' ] <- NA

d.eddy.real[d.eddy.real$SWC_3_1_1 < no.dat.value , 'SWC_3_1_1' ] <- NA
d.eddy.real[d.eddy.real$SWC_2_1_1 < no.dat.value, 'SWC_2_1_1' ] <- NA
d.eddy.real[d.eddy.real$SWC_1_1_1 < no.dat.value , 'SWC_1_1_1' ] <- NA

d.eddy.real[d.eddy.real$Ts_1_1_1 < no.dat.value , 'Ts_1_1_1' ] <- NA
d.eddy.real[d.eddy.real$Ts_2_1_1 < no.dat.value , 'Ts_2_1_1' ] <- NA
d.eddy.real[d.eddy.real$Ts_3_1_1 < no.dat.value , 'Ts_3_1_1' ] <- NA

d.eddy.real.new <- data.frame()

unique.dates <- unique(d.eddy.real$date)
len.unique.dates <- length(unique.dates)

for (i in 1:len.unique.dates )  {
  
  
  current.date <- unique.dates[i] 
  
  print(paste('current data is' ,  current.date))
  
  d.eddy.real.new[ i , 'date' ] <- as.Date(unique(d.eddy.real[d.eddy.real$date == current.date , 'date']))
  
  # Calculate means
  d.eddy.real.new[ i , 'nee' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'NEE']  ))
  
  
  d.eddy.real.new[ i , 'h' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'H']  ))
  d.eddy.real.new[ i , 'le' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'LE']  ))
  
  d.eddy.real.new[ i , 'temp.avg' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'Temp']  ) )
  d.eddy.real.new[ i , 'temp.min' ] <- min( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'Temp']   ))
  d.eddy.real.new[ i , 'temp.max' ] <- max( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'Temp']  ) )
  
  
  d.eddy.real.new[ i , 'precip' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'Precip']   ))
 
  d.eddy.real.new[ i , 'rg' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'Rg']  ) )
  
  d.eddy.real.new[ i , 'swc.1' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'SWC_1_1_1'] ))
  d.eddy.real.new[ i , 'swc.2' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'SWC_2_1_1'] ))
  d.eddy.real.new[ i , 'swc.3' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'SWC_3_1_1'] ))
  
  d.eddy.real.new[ i , 'ts.1' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'Ts_1_1_1']))
  d.eddy.real.new[ i , 'ts.2' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'Ts_2_1_1']))
  d.eddy.real.new[ i , 'ts.3' ] <- mean( na.omit( d.eddy.real[d.eddy.real$date == current.date , 'Ts_3_1_1']))
  
  

  print(paste('current date is' ,  current.date))
  print(paste('current nee is', d.eddy.real.new[ i , 'nee' ]))
  
}


d.eddy.real.new[  , 'date' ] <- as.Date(d.eddy.real.new[  , 'date' ] )

summary(d.eddy.real.new$nee)
summary(d.eddy.real.new$temp.avg)
summary(d.eddy.real.new$temp.min)
summary(d.eddy.real.new$temp.max)
summary(d.eddy.real.new$precip)
summary(d.eddy.real.new$swc.1)
summary(d.eddy.real.new$swc.2)
summary(d.eddy.real.new$swc.3)

View(d.eddy.real.new)
nrow(d.eddy.real.new)

#  d.eddy.real.new <- d.eddy.real.new[d.eddy.real.new$nee , ]


write.csv(d.eddy.real.new,"d.eddy.real.new.csv", row.names = FALSE)


d.eddy.real <- d.eddy.real.new

start.date <- "2023-04-22"
end.date <- "2024-05-10"
