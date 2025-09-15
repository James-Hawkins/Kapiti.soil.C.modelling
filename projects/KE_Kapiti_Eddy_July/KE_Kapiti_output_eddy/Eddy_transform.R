

# Data prep
{
d.eddy.raw <<- read.csv('Kapiti_AllYears_QC_ReddyPro.csv')
  
d.eddy.partn.raw <<- read.csv('Kapiti_Partitioned_Fluxes.csv')  

d.weather.subs <- read.csv('TA00677_wthr_data.csv')
d.weather.subs.2 <- read.csv('TA00621.csv')
d.weather.subs.3 <- read.csv('TA00814.csv')
d.weather.subs.4 <- read.csv('TA00678.csv')

names(d.eddy.raw)[1] <- 'date'

names(d.eddy.partn.raw)[1] <- 'date'

names(d.weather.subs)[1] <- 'date'
names(d.weather.subs)[2] <- 'precip'
names(d.weather.subs)[3] <- 'temp.mn'
names(d.weather.subs)[4] <- 'temp.max'
names(d.weather.subs)[5] <- 'temp.min'

names(d.weather.subs.2)[1] <- 'date'
names(d.weather.subs.2)[2] <- 'precip'
names(d.weather.subs.2)[3] <- 'temp.mn'
names(d.weather.subs.2)[4] <- 'temp.max'
names(d.weather.subs.2)[5] <- 'temp.min'

names(d.weather.subs.3)[1] <- 'date'
names(d.weather.subs.3)[2] <- 'precip'
names(d.weather.subs.3)[3] <- 'temp.mn'
names(d.weather.subs.3)[4] <- 'temp.max'
names(d.weather.subs.3)[5] <- 'temp.min'

names(d.weather.subs.4)[1] <- 'date'
names(d.weather.subs.4)[2] <- 'precip'
names(d.weather.subs.4)[3] <- 'temp.mn'
names(d.weather.subs.4)[4] <- 'temp.max'
names(d.weather.subs.4)[5] <- 'temp.min'

d.eddy.raw$date <- as.Date(d.eddy.raw$date ,  format="%m/%d/%Y")
d.eddy.partn.raw$date <- as.Date(d.eddy.partn.raw$date ,  format="%m/%d/%Y")

d.weather.subs$date <- as.Date(d.weather.subs$date ,  format="%d/%m/%Y")
d.weather.subs.2$date <- as.Date(d.weather.subs.2$date ,  format="%Y-%m-%d")
d.weather.subs.3$date <- as.Date(d.weather.subs.3$date ,  format="%Y-%m-%d")
d.weather.subs.4$date <- as.Date(d.weather.subs.4$date ,  format="%Y-%m-%d")

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

d.eddy.raw[1:365*48*3  , 'date']
summary(d.eddy.raw[(1*365*48*.5):365*48*1.5  , 'wind_dir'])
summary(d.eddy.raw[(1*365*48*.5):365*48*1.5  , 'wind_speed'])

}


# parameters
{
dry.ssn.months <- c( 1,2 , 6:10)  
rn.ssn.months <- c(3:5 , 11,12 )  
}



no.dat.value <- -9999

d.eddy.raw[d.eddy.raw$NEE == no.dat.value , 'NEE' ] <- NA
d.eddy.raw[d.eddy.raw$H == no.dat.value , 'H' ] <- NA
d.eddy.raw[d.eddy.raw$LE == no.dat.value , 'LE' ] <- NA
d.eddy.raw[d.eddy.raw$h2o_flux == no.dat.value , 'h2o_flux' ] <- NA
d.eddy.raw[d.eddy.raw$Rg == no.dat.value , 'Rg' ] <- NA

d.eddy.raw[d.eddy.raw$RH == no.dat.value , 'RH' ] <- NA
d.eddy.raw[d.eddy.raw$wind_speed == no.dat.value , 'wind_speed' ] <- NA
d.eddy.raw[d.eddy.raw$wind_dir == no.dat.value , 'wind_dir' ] <- NA
d.eddy.raw[d.eddy.raw$Temp == no.dat.value, 'Temp' ] <- NA
d.eddy.raw[d.eddy.raw$Precip == no.dat.value , 'Precip' ] <- NA

d.eddy.raw[d.eddy.raw$SWC_3_1_1 == no.dat.value , 'SWC_3_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$SWC_2_1_1 == no.dat.value, 'SWC_2_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$SWC_1_1_1 == no.dat.value , 'SWC_1_1_1' ] <- NA

d.eddy.raw[d.eddy.raw$Ts_1_1_1 == no.dat.value , 'Ts_1_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$Ts_2_1_1 == no.dat.value , 'Ts_2_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$Ts_3_1_1 == no.dat.value , 'Ts_3_1_1' ] <- NA

d.eddy.real<- data.frame()

unique.dates <- unique(d.eddy.raw$date)
len.unique.dates <- length(unique.dates)


# SWC_3_1_1 : 5 cm
# SWC_2_1_1 : 15
# SWC_1_1_1 : 30



for (i in 1:len.unique.dates ){
  
  
  current.date <- unique.dates[i] 
  
  d.eddy.real[ i , 'date' ] <- as.Date(unique(d.eddy.raw[d.eddy.raw$date == current.date , 'date']))
  
  # Calculate means
  d.eddy.real[ i , 'nee.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'NEE']  ))
  
  
  d.eddy.real[ i , 'h.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'H']  ))
  d.eddy.real[ i , 'le.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'LE']  ))
  
  
  d.eddy.real[ i , 'ws.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'wind_speed']  ))
  d.eddy.real[ i , 'rh.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'RH']  ))
  
  
  d.eddy.real[ i , 'temp.avg.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']  ) )
  
  if(  is.numeric( d.eddy.real[ i , 'temp.avg.osv' ]) & !is.na(d.eddy.real[ i , 'temp.avg.osv' ]) ){
    
    d.eddy.real[ i , 'temp.min.osv' ] <- min( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']   ))
    d.eddy.real[ i , 'temp.max.osv' ] <- max( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']  ) )
    
  } else {
    
    d.eddy.real[ i , 'temp.min.osv' ] <- NA
    d.eddy.real[ i , 'temp.max.osv' ] <- NA
    
  }
  
  
  
  #' For precipitation, take daily total as either observed values * 48
  #' if all 48 entries are numeric, otherwise infer the NA values based on global mean
  #' 
  precip.vec <- d.eddy.raw[d.eddy.raw$date == current.date  , 'Precip']
  
  precip.vec <- c(NA,5)
  
  if ( any(is.na(precip.vec) & any(is.numeric(precip.vec)) )) {
    
    quant.numerics <- length(is.numeric(precip.vec))
    quant.inferred <- 48 -  quant.numerics
    
    precip.observed <-  quant.numerics * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date  , 'Precip'])  )
    precip.inferred <-  quant.inferred * mean( na.omit( d.eddy.raw[  , 'Precip'])  )
    
    d.eddy.real[ i , 'precip.osv' ] <-  precip.observed + precip.inferred 
    
  } else {
    
    
    d.eddy.real[ i , 'precip.osv' ] <- 48 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date  , 'Precip'])  )
    
    
  }
  
  d.eddy.real[ i , 'precip.osv' ] <- 48 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date  , 'Precip'])  )
 
  d.eddy.real[ i , 'rg.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Rg']  ) )
  
  d.eddy.real[ i , 'swc.1.pc.osv' ] <- 100 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_1_1_1'] ))
  d.eddy.real[ i , 'swc.2.pc.osv' ] <- 100 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_2_1_1'] ))
  d.eddy.real[ i , 'swc.3.pc.osv' ] <- 100 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_3_1_1'] ))
  
  d.eddy.real[ i , 'ts.1.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_1_1_1']))
  d.eddy.real[ i , 'ts.2.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_2_1_1']))
  d.eddy.real[ i , 'ts.3.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_3_1_1']))
  
  
  d.eddy.real[ i , 'gpp.osv' ] <- mean( na.omit( d.eddy.partn.raw[d.eddy.partn.raw$date == current.date , 'GPP_DT']))
  d.eddy.real[ i , 'reco.osv' ] <- mean( na.omit( d.eddy.partn.raw[d.eddy.partn.raw$date == current.date , 'Reco_DT']))
  
}


summary(d.eddy.real$temp.avg.osv)
summary(d.eddy.real$temp.min.osv)
summary(d.eddy.real$temp.max.osv)


hist(d.eddy.real$precip.osv ,breaks = 100)


# Fill Data gaps
{




# Stage 1 -- Replace missing weather observations
{
weather.vars.eddy <- c(
'temp.avg.osv'
,  "temp.min.osv"
,  "temp.max.osv"
, "precip.osv"
)

weather.vars.subst <- c(
"temp.mn"
,  "temp.min"
,  "temp.max"
, "precip" 
)

    


d.eddy.real$wv.status <- NA

for (cv in weather.vars.eddy){

for (r in 1:nrow(d.eddy.real)){

current.day.value <- d.eddy.real[ r , cv ]
current.date <- d.eddy.real[ r , "date" ]

current.subs.var <- weather.vars.subst[which(weather.vars.eddy == cv)]

current.subs.value.1 <- NA
current.subs.value.2 <- NA
current.subs.value.3 <- NA
current.subs.value.4 <- NA

if(current.date %in% d.weather.subs$date){  current.subs.value.1 <- d.weather.subs[d.weather.subs$date == current.date ,current.subs.var ]  }
if(current.date %in% d.weather.subs.2$date){  current.subs.value.2 <- d.weather.subs.2[d.weather.subs.2$date == current.date ,current.subs.var ]  }
if(current.date %in% d.weather.subs.3$date){  current.subs.value.3 <- d.weather.subs.3[d.weather.subs.3$date == current.date ,current.subs.var ]  }
if(current.date %in% d.weather.subs.4$date){  current.subs.value.4 <- d.weather.subs.4[d.weather.subs.4$date == current.date ,current.subs.var ]  }



is.inf <- ((current.day.value == Inf) | (current.day.value == -Inf))


if (  is.na(current.day.value) 
| (!is.na(is.inf) & is.inf)

) {

print(paste('have identifid as na for date', current.date))

if( !is.na(current.subs.value.1)  ){

print(paste('for variable ', cv,'substituting', current.subs.value.1 , 'for date ', current.date ))

d.eddy.real[ r , cv ] <- current.subs.value.1 
d.eddy.real[ r , 'wv.status' ] <- 'subs.1'
print('substituted variable 1')

} else if ( !is.na(current.subs.value.2)  ){

print(paste('for variable ', cv,'substituting', current.subs.value.2 , 'for date ', current.date ))

d.eddy.real[ r , cv ] <- current.subs.value.2 
d.eddy.real[ r , 'wv.status' ] <- 'subs.2'
print('substituted variable 2')

} else if ( !is.na(current.subs.value.3)  ){

print(paste('for variable ', cv,'substituting', current.subs.value.3 , 'for date ', current.date ))

d.eddy.real[ r , cv ] <- current.subs.value.3 
d.eddy.real[ r , 'wv.status' ] <- 'subs.3'
print('substituted variable 3')

} else if ( !is.na(current.subs.value.4)  ){

print(paste('for variable ', cv,'substituting', current.subs.value.4 , 'for date ', current.date ))

d.eddy.real[ r , cv ] <- current.subs.value.4 
d.eddy.real[ r , 'wv.status' ] <- 'subs.4'
print('substituted variable 4')

}

}
} 
}

}
  
# Stage 2 -- fill with seasonal means 
{


d.eddy.real[  , 'variable.status' ] <- v.status.actual 
  
d.eddy.real[  , 'variable.status.temp.avg' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.temp.min' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.temp.max' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.precip' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.rg' ] <- v.status.actual
d.eddy.real[  , 'variable.status.h' ] <- v.status.actual
d.eddy.real[  , 'variable.status.rh' ] <- v.status.actual
d.eddy.real[  , 'variable.status.le' ] <- v.status.actual
d.eddy.real[  , 'variable.status.ws' ] <- v.status.actual
d.eddy.real[  , 'variable.status.swc.1' ] <- v.status.actual
d.eddy.real[  , 'variable.status.swc.2' ] <- v.status.actual
d.eddy.real[  , 'variable.status.swc.3' ] <- v.status.actual
d.eddy.real[  , 'variable.status.ts.1' ] <- v.status.actual
d.eddy.real[  , 'variable.status.ts.2' ] <- v.status.actual
d.eddy.real[  , 'variable.status.ts.3' ] <- v.status.actual
  
  var.list.mean.fill <- c(
    
    'temp.avg.osv'
    ,  "temp.min.osv"
    ,  "temp.max.osv"
    , "precip.osv"
    
    , 'rg.osv'
    , 'h.osv'
    , 'rh.osv'
    , 'le.osv'
    , 'ws.osv'
    
    , 'swc.1.pc.osv'
    , 'swc.2.pc.osv'
    , 'swc.3.pc.osv'
    
    , 'ts.1.osv'
    , 'ts.2.osv'
    , 'ts.3.osv'
    
  )
  
  
for (i in 1:nrow(d.eddy.real)){

for (v in var.list.mean.fill ){

current.day.value <- d.eddy.real[ i , v ]

if (  is.na(current.day.value)  ){


date <- d.eddy.real[ i , 'date'] 
month <- format( date, "%m")
month <- as.numeric( month )

if( month %in% dry.ssn.months){


value.to.fill <- mean( na.omit( d.eddy.real[ format(d.eddy.real$date ,"%m")  %in% dry.ssn.months , v ])  )

d.eddy.real[ i , v ] <- value.to.fill
d.eddy.real[ i , 'variable.status' ] <- 'filled'


if( v == var.list.mean.fill[1] ){ d.eddy.real[ i , 'variable.status.temp.avg' ] <- 'filled' }
if( v == var.list.mean.fill[2] ){ d.eddy.real[ i , 'variable.status.temp.min' ] <- 'filled' }
if( v == var.list.mean.fill[3] ){ d.eddy.real[ i , 'variable.status.temp.max' ] <- 'filled' }
if( v == var.list.mean.fill[4] ){ d.eddy.real[ i , 'variable.status.precip' ] <- 'filled' }
if( v == var.list.mean.fill[5] ){ d.eddy.real[ i , 'variable.status.rg' ] <- 'filled' }
if( v == var.list.mean.fill[6] ){ d.eddy.real[ i , 'variable.status.h' ] <- 'filled' }
if( v == var.list.mean.fill[7] ){ d.eddy.real[ i , 'variable.status.rh' ] <- 'filled' }
if( v == var.list.mean.fill[8] ){ d.eddy.real[ i , 'variable.status.le' ] <- 'filled' }
if( v == var.list.mean.fill[9] ){ d.eddy.real[ i , 'variable.status.ws' ] <- 'filled' }
if( v == var.list.mean.fill[10] ){ d.eddy.real[ i , 'variable.status.swc.1' ] <- 'filled' }
if( v == var.list.mean.fill[11] ){ d.eddy.real[ i , 'variable.status.swc.2' ] <- 'filled' }
if( v == var.list.mean.fill[12] ){ d.eddy.real[ i , 'variable.status.swc.3' ] <- 'filled' }
if( v == var.list.mean.fill[13] ){ d.eddy.real[ i , 'variable.status.ts.1' ] <- 'filled' }
if( v == var.list.mean.fill[14] ){ d.eddy.real[ i , 'variable.status.ts.2' ] <- 'filled' }
if( v == var.list.mean.fill[15] ){ d.eddy.real[ i , 'variable.status.ts.3' ] <- 'filled' }



print(paste('For date' ,date , 'replacing ' ,v ,' with mean of' , value.to.fill))


}  else if ( month %in% rn.ssn.months){

value.to.fill <- mean( na.omit( d.eddy.real[ format(d.eddy.real$date ,"%m")  %in% rn.ssn.months , v ])  )

d.eddy.real[ i , v ] <- value.to.fill
d.eddy.real[ i , 'variable.status' ] <- 'filled'

if( v == var.list.mean.fill[1] ){ d.eddy.real[ i , 'variable.status.temp.avg' ] <- 'filled' }
if( v == var.list.mean.fill[2] ){ d.eddy.real[ i , 'variable.status.temp.min' ] <- 'filled' }
if( v == var.list.mean.fill[3] ){ d.eddy.real[ i , 'variable.status.temp.max' ] <- 'filled' }
if( v == var.list.mean.fill[4] ){ d.eddy.real[ i , 'variable.status.precip' ] <- 'filled' }
if( v == var.list.mean.fill[5] ){ d.eddy.real[ i , 'variable.status.rg' ] <- 'filled' }
if( v == var.list.mean.fill[6] ){ d.eddy.real[ i , 'variable.status.h' ] <- 'filled' }
if( v == var.list.mean.fill[7] ){ d.eddy.real[ i , 'variable.status.rh' ] <- 'filled' }
if( v == var.list.mean.fill[8] ){ d.eddy.real[ i , 'variable.status.le' ] <- 'filled' }
if( v == var.list.mean.fill[9] ){ d.eddy.real[ i , 'variable.status.ws' ] <- 'filled' }
if( v == var.list.mean.fill[10] ){ d.eddy.real[ i , 'variable.status.swc.1' ] <- 'filled' }
if( v == var.list.mean.fill[11] ){ d.eddy.real[ i , 'variable.status.swc.2' ] <- 'filled' }
if( v == var.list.mean.fill[12] ){ d.eddy.real[ i , 'variable.status.swc.3' ] <- 'filled' }
if( v == var.list.mean.fill[13] ){ d.eddy.real[ i , 'variable.status.ts.1' ] <- 'filled' }
if( v == var.list.mean.fill[14] ){ d.eddy.real[ i , 'variable.status.ts.2' ] <- 'filled' }
if( v == var.list.mean.fill[15] ){ d.eddy.real[ i , 'variable.status.ts.3' ] <- 'filled' }


print(paste('For date' ,date , 'replacing ' ,v ,' with mean of' , value.to.fill))

}
}
}
}
}

}



# Check if any NA values remain
sum( TRUE == is.na(d.eddy.real$precip.osv)) 
sum( TRUE == is.na(d.eddy.real$temp.avg.osv))  
sum( TRUE == is.na(d.eddy.real$temp.min.osv))
sum( TRUE == is.na(d.eddy.real$temp.max.osv))



nrow(d.eddy.real[d.eddy.real$variable.status == 'filled', ])
nrow(d.eddy.real[d.eddy.real$variable.status == 'actual', ])





# Data clip 
# Actual (EC tower) data
# EC tower data
{
  
  # Observed data
  d.eddy.real$date <- as.Date(d.eddy.real$date , "%Y-%m-%d")
  
  
  d.eddy.real <- d.eddy.real[
    d.eddy.real$date >= start.date.cald 
    & d.eddy.real$date <= end.date.cald
    ,  ]
  
  nrow(d.eddy.real)
}





# HANDLE NAs
d.eddy.oc <- d.eddy.real


summary(   d.eddy.oc$temp.avg.osv)
summary( d.eddy.oc$temp.max.osv)
summary( d.eddy.oc$temp.min.osv)
summary(   d.eddy.oc$precip.osv)
summary( d.eddy.oc$rg.osv)

print('Have finished prepping Eddy data ')

stop()

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
 # ,  d.eddy.oc$wv.status
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
#  , 'status'

)
  
write.csv(d.eddy.clim.out ,"d.eddy.clim.out.csv", row.names = FALSE)


}


# Merge and export climate data
{
d.eddy.clim.pre.sim <<- read.csv('climate.pre.sim.data.csv')  

start.year <- as.numeric(d.eddy.clim.out$`*`[1])
start.day.numeric <- as.numeric(  d.eddy.clim.out[ , c(2)  ][1]  )

d.eddy.clim.pre.sim <- d.eddy.clim.pre.sim[
  (d.eddy.clim.pre.sim$year == start.year  &  d.eddy.clim.pre.sim$day < start.day.numeric  )   
  | ( d.eddy.clim.pre.sim$year < start.year  ) 
  ,    ]

colnames(d.eddy.clim.pre.sim) <- colnames(  d.eddy.clim.out )

full.clim.data <- rbind(  d.eddy.clim.pre.sim , d.eddy.clim.out  )


write.table(  full.clim.data  , file = "../KE_Kapiti_climate_eddy_raw.txt", 
              append = FALSE, sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE)



}

write.csv(d.eddy.real,"d.eddy.real.new.csv", row.names = FALSE)



# Air chemistry data

gen.air.chem <- function(){
  
d.eddy.air.chm <<- read.csv('air.chemistry.csv')  
  
colnames(d.eddy.air.chm)[1] <- 'year'
colnames(d.eddy.air.chm)[2] <- 'day'

air.chm.last.year <- d.eddy.air.chm$year[c(nrow(d.eddy.air.chm))]
air.chm.last.day <- d.eddy.air.chm$day[c(nrow(d.eddy.air.chm))]

new.air.chm <- d.eddy.air.chm
new.air.chm <- new.air.chm[-c(1:nrow(new.air.chm)) , ]


days.to.add <- end.date.cald



final.year <- format(as.Date(secd.date.cald, format="%Y-%m-%d"),"%Y")

final.year <- as.numeric(final.year )

final.cald.day.month <- format(as.Date(secd.date.cald, format="%Y-%m-%d"),"%m")
final.cald.day.day <- format(as.Date(secd.date.cald, format="%Y-%m-%d"),"%d")



final.cald.day.month <- as.numeric(final.cald.day.month)
final.cald.day.day <- as.numeric(final.cald.day.day)
  
  
final.day.numeric <- final.cald.day.day + 30 * ( final.cald.day.month - 1) 


start.row <- 1
day <- final.day.numeric + 1
year <- air.chm.last.year 

while( year <= final.year) {

if( year == final.year & day == final.day.numeric) { break }


new.air.chm[ start.row , 'no3'] <- 1


day <- day + 1
if (day == 365 ) { year <- year + 1 ; day <- 1     }

}

}



