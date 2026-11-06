
{ # Data prep
  
d.eddy.raw <<- read.csv(  str_c( R.dat.dir , 'Kapiti_AllYears_QC_ReddyPro.csv') )
  

d.eddy.raw <- d.eddy.raw[ , !(colnames(d.eddy.raw) %in% c( "u" , "H" ,  "qc_co2_flux" , "VPD" , "PPFD" , "e" ,"es" ,"Tdew" ,"max_wind_speed" ,  "Ts_1_1_1"  ,  "Ts_2_1_1"   , "Ts_3_1_1"  )) ]



d.eddy.partn.raw  <<- read.csv(   str_c( R.dat.dir ,   'Kapiti_Partitioned_Fluxes.csv'    )  )

#d.eddy.partn.raw <- d.eddy.partn.raw[ , !(colnames(d.eddy.partn.raw) %in% c( "u" , "H" ,  "qc_co2_flux" , "VPD" , "PPFD" , "e" ,"es" ,"Tdew" ,"max_wind_speed" ,  "Ts_1_1_1"  ,  "Ts_2_1_1"   , "Ts_3_1_1"  )) ]


#d.eo <<- read.csv(  str_c( R.dat.dir ,    "climate_harmonized_EC_Yidan+JH.csv")  )

#d.lai <<- read_excel('C:/Users/hawkj/Documents/Github/Kapiti spatial/LAI/Batch results2/LAI.summary.xlsx') ; d.lai <- as.data.frame(d.lai)

# d.lai <<- read_excel('LAI.summary.xlsx') ; d.lai <- as.data.frame(d.lai)

d.power <<- as.data.frame(read_excel(  str_c( R.dat.dir ,   'NASA_POWER_CONTINUOUS_SERIES.xlsx' ) ,  sheet ='Summary'))

cols.d.power.numeric <- c("Temp.max"   
                          , "Temp.min" 
                          ,"Temp.avg"      
                          , "Precip" 
                          ,"Relative.humid" 
                          ,"Wind.speed"
                          ,"Radiation"  )



# Make numeric
for (c in cols.d.power.numeric){ d.power[,c] <- as.numeric(  d.power[,c]  ) }

d.power$Radiation <- d.power$Radiation * cv.MJ.to.watts





# NASA Power - assign dates
for (r in 1: nrow( d.power)){
  
  # r <- 1
  
  day.of.year <- d.power[r ,'day']  
  year <- d.power[r , 'year'] 
  
  origin <- str_c(  year , "-01-01")
    d.power[ r  , 'date'] <- as.Date(day.of.year- 1, origin = origin)
}



#d.lai$date <- as.Date(d.lai$date ,  format="%m/%d/%Y")


# Weather stations ordered from nearest to furthest away
# 677
# 621 - Kapiti farm house  (closest to EC tower)
# 678 - Boma site 2 (next closest)
# 814 - Kapiti supply 2
# 776 - Ausquest 

d.weather.subs <- read.csv(     str_c( R.dat.dir ,  'TA00677_wthr_data.csv'))
d.weather.subs.621 <- read.csv( str_c( R.dat.dir ,'TA00621.csv'))
d.weather.subs.678 <- read.csv( str_c( R.dat.dir ,'TA00678.csv'))
d.weather.subs.814 <- read.csv( str_c( R.dat.dir ,'TA00814.csv'))


names(d.eddy.raw)[1] <- 'date'
names(d.eddy.partn.raw)[1] <- 'date'

# names(d.eo)[2] <- 'date'


#tail(d.eo$Date)

#d.eo$sol.rad.w <- cv.mj.2.watts * d.eo$sol.rad




names(d.power)[3] <- 'temp.max'
names(d.power)[4] <- 'temp.min'
names(d.power)[5] <- 'temp.mn'
names(d.power)[6] <- 'precip'
names(d.power)[7] <- 'rh'
names(d.power)[8] <- 'ws'
names(d.power)[9] <- 'rg'
names(d.power)[10] <- 'date'




names(d.weather.subs)[1] <- 'date'
names(d.weather.subs)[2] <- 'precip'
names(d.weather.subs)[3] <- 'temp.mn'
names(d.weather.subs)[4] <- 'temp.max'
names(d.weather.subs)[5] <- 'temp.min'

names(d.weather.subs.621)[1] <- 'date'
names(d.weather.subs.621)[2] <- 'precip'
names(d.weather.subs.621)[3] <- 'temp.mn'
names(d.weather.subs.621)[4] <- 'temp.max'
names(d.weather.subs.621)[5] <- 'temp.min'

names(d.weather.subs.678)[1] <- 'date'
names(d.weather.subs.678)[2] <- 'precip'
names(d.weather.subs.678)[3] <- 'temp.mn'
names(d.weather.subs.678)[4] <- 'temp.max'
names(d.weather.subs.678)[5] <- 'temp.min'

names(d.weather.subs.814)[1] <- 'date'
names(d.weather.subs.814)[2] <- 'precip'
names(d.weather.subs.814)[3] <- 'temp.mn'
names(d.weather.subs.814)[4] <- 'temp.max'
names(d.weather.subs.814)[5] <- 'temp.min'

d.eddy.raw$date <- as.Date(d.eddy.raw$date ,  format="%m/%d/%Y")
d.eddy.partn.raw$date <- as.Date(d.eddy.partn.raw$date ,  format="%m/%d/%Y")

d.weather.subs$date <- as.Date(d.weather.subs$date ,  format="%d/%m/%Y")
d.weather.subs.621$date <- as.Date(d.weather.subs.621$date ,  format="%m/%d/%Y")
d.weather.subs.678$date <- as.Date(d.weather.subs.678$date ,  format="%Y-%m-%d")
d.weather.subs.814$date <- as.Date(d.weather.subs.814$date ,  format="%Y-%m-%d")


d.eo$date <- as.Date(d.eo$date ,  format="%m/%d/%Y")



first.date <- d.eddy.raw$date[1]
last.date <- d.eddy.raw$date[nrow(d.eddy.raw)]

d.eddy.raw <- d.eddy.raw[ d.eddy.raw$date >= first.date  ,    ]
d.eddy.raw <- d.eddy.raw [ d.eddy.raw$date <= last.date ,    ]

d.eddy.partn.raw <- d.eddy.partn.raw[ d.eddy.partn.raw$date >= first.date  ,    ]
d.eddy.partn.raw <- d.eddy.partn.raw[ d.eddy.partn.raw$date <= last.date ,    ]




}


# Parameters
{

eddy.raw.precip.bias.correct.fac  <<- 1.22


}

no.dat.value <- -9999

d.eddy.raw[d.eddy.raw$NEE == no.dat.value , 'NEE' ] <- NA
#d.eddy.raw[d.eddy.raw$H == no.dat.value , 'H' ] <- NA
d.eddy.raw[d.eddy.raw$LE == no.dat.value , 'LE' ] <- NA
#d.eddy.raw[d.eddy.raw$h2o_flux == no.dat.value , 'h2o_flux' ] <- NA
d.eddy.raw[d.eddy.raw$Rg == no.dat.value , 'Rg' ] <- NA

d.eddy.raw[d.eddy.raw$RH == no.dat.value , 'RH' ] <- NA
d.eddy.raw[d.eddy.raw$wind_speed == no.dat.value , 'wind_speed' ] <- NA
d.eddy.raw[d.eddy.raw$wind_dir == no.dat.value , 'wind_dir' ] <- NA
d.eddy.raw[d.eddy.raw$Temp == no.dat.value, 'Temp' ] <- NA
d.eddy.raw[d.eddy.raw$Precip == no.dat.value , 'Precip' ] <- NA

d.eddy.raw[d.eddy.raw$SWC_3_1_1 == no.dat.value , 'SWC_3_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$SWC_2_1_1 == no.dat.value, 'SWC_2_1_1' ] <- NA
d.eddy.raw[d.eddy.raw$SWC_1_1_1 == no.dat.value , 'SWC_1_1_1' ] <- NA

#d.eddy.raw[d.eddy.raw$Ts_1_1_1 == no.dat.value , 'Ts_1_1_1' ] <- NA
#d.eddy.raw[d.eddy.raw$Ts_2_1_1 == no.dat.value , 'Ts_2_1_1' ] <- NA
#d.eddy.raw[d.eddy.raw$Ts_3_1_1 == no.dat.value , 'Ts_3_1_1' ] <- NA

d.eddy.daily <- data.frame()

#unique.dates <- unique(d.eddy.raw$date)
#len.unique.dates <- length(unique.dates)

#head(unique.dates)
#tail(unique.dates)

dates.sim.max <<- unique.dates

dates.sim <<- unique.dates[ -c( length(unique.dates) , (1+length(unique.dates)))  ]


d.eddy.raw$ET <- d.eddy.raw$LE * cv.secs.per.30.min * 1000 / ( parm.Lv * parm.pw)


#d.eddy.daily$date <- unique.dates 



# SWC_3_1_1 : 5 cm
# SWC_2_1_1 : 15
# SWC_1_1_1 : 30



for (i in 1:len.unique.dates ){
  
 # i <- 1


current.date <- unique.dates[i] 

d.eddy.daily[ i , 'date' ] <- as.Date(unique(d.eddy.raw[d.eddy.raw$date == current.date , 'date']))

# Calculate means
d.eddy.daily[ i , 'nee.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'NEE_f_clean']  )) 
d.eddy.daily[ i , 'gpp.osv' ] <- mean( na.omit( d.eddy.partn.raw[d.eddy.partn.raw$date == current.date , 'GPP_DT']))
d.eddy.daily[ i , 'reco.osv' ] <- mean( na.omit( d.eddy.partn.raw[d.eddy.partn.raw$date == current.date , 'Reco_DT']))


#d.eddy.daily[ i , 'h2o.flux.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'h2o_flux']  ))


#d.eddy.daily[ i , 'h.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'H']  ))
#d.eddy.daily[ i , 'le.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'LE']  ))


d.eddy.daily[ i , 'ws.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'wind_speed']  ))
d.eddy.daily[ i , 'rh.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'RH']  ))


d.eddy.daily[ i , 'ET.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'ET']  )) * 48


d.eddy.daily[ i , 'temp.avg.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']  ) )

if(  is.numeric( d.eddy.daily[ i , 'temp.avg.osv' ]) & !is.na(d.eddy.daily[ i , 'temp.avg.osv' ]) ){

d.eddy.daily[ i , 'temp.min.osv' ] <- min( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']   ))
d.eddy.daily[ i , 'temp.max.osv' ] <- max( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Temp']  ) )

} else {

d.eddy.daily[ i , 'temp.min.osv' ] <- NA
d.eddy.daily[ i , 'temp.max.osv' ] <- NA

}


  
  #' For precipitation, take daily total as either observed values * 48
  #' if all 48 entries are numeric, otherwise infer the NA values based on global mean
  #' 
#  precip.vec <- d.eddy.raw[d.eddy.raw$date == current.date  , 'Precip']
  
 # precip.vec <- c(NA,5)
  
 # if ( any(is.na(precip.vec) & any(is.numeric(precip.vec)) )) {
    
   # quant.numerics <- length(is.numeric(precip.vec))
   # quant.inferred <- 48 -  quant.numerics
    
   # precip.observed <-  quant.numerics * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date  , 'Precip'])  )
   # precip.inferred <-  quant.inferred * mean( na.omit( d.eddy.raw[  , 'Precip'])  )
    
   # d.eddy.daily[ i , 'precip.osv' ] <-  precip.observed + precip.inferred 
    
#  } else {
    
    
  #  d.eddy.daily[ i , 'precip.osv' ] <- 48 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date  , 'Precip'])  )
    
    
 # }
  
d.eddy.daily[ i , 'precip.osv' ] <- 48 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date  , 'Precip'])  )

d.eddy.daily[ i , 'rg.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Rg']  ) )

d.eddy.daily[ i , 'swc.1.pc.osv' ] <- 100 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_1_1_1'] ))
d.eddy.daily[ i , 'swc.2.pc.osv' ] <- 100 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_2_1_1'] ))
d.eddy.daily[ i , 'swc.3.pc.osv' ] <- 100 * mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'SWC_3_1_1'] ))

#d.eddy.daily[ i , 'ts.1.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_1_1_1']))
#d.eddy.daily[ i , 'ts.2.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_2_1_1']))
#d.eddy.daily[ i , 'ts.3.osv' ] <- mean( na.omit( d.eddy.raw[d.eddy.raw$date == current.date , 'Ts_3_1_1']))

  

}


source(   'sum.stats.R' ) # Compute biases between ECT and external series





# # Data quality control - Replace missing weather observations
{
  
d.eddy.real <- d.eddy.daily

# TAHMO 621
v.status.subs.filled.tahmo.621 <- 'tahmo.621'

# TAHMO 678
v.status.subs.filled.tahmo.678 <- 'tahmo.678'

# TAHMO 814
v.status.subs.filled.tahmo.814 <- 'tahmo.814'

# NASA POWER
v.status.subs.filled.NASA.POWER <- 'NASA.POWER'


v.status.subs.filled.all <<- c(
  v.status.subs.filled.tahmo.621 
  ,v.status.subs.filled.tahmo.678 
  , v.status.subs.filled.tahmo.814
  , v.status.subs.filled.NASA.POWER 
)

v.status.subs.eo.filled.all <<- c(
  v.status.subs.filled.NASA.POWER 
  , v.status.eo.filled
  
)

weather.vars.eddy <- c(
'temp.avg.osv'
,  "temp.min.osv"
,  "temp.max.osv"
, "precip.osv"

, "rg.osv"
, "ws.osv"
, "rh.osv"

)



weather.vars.subst <- c(
"temp.mn"
,  "temp.min"
,  "temp.max"
, "precip" 

,  "rg" 
, "ws" 
, "rh" 
)



subs.vars.NASA.POWER.only <<- weather.vars.subst[ c(5,6,7)]

d.eddy.real[  , 'variable.status.temp.avg' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.temp.min' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.temp.max' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.precip' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.rg' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.rh' ] <- v.status.actual 
d.eddy.real[  , 'variable.status.ws' ] <- v.status.actual 


{
if (  bias.control ){
    
# IDENTIFY CORRELATIONS WITH AUXILIARY DATASETS

  
# ~ ~ ~  - - - NASA POWER
precip.cmn.dates.POWER <<- intersect( d.eddy.real[  !is.na(d.eddy.real$precip.osv) ,   'date' ] , d.power$date )
temp.avg.cmn.dates.POWER <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.avg.osv)  ,   'date' ] , d.power$date )
temp.min.cmn.dates.POWER <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.min.osv) ,   'date' ] , d.power$date )
temp.max.cmn.dates.POWER <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.max.osv) ,   'date' ] , d.power$date )
rad.cmn.dates.POWER <<- intersect( d.eddy.real[ !is.na(d.eddy.real$rg.osv) ,   'date' ] , d.power$date )
ws.cmn.dates.POWER <<- intersect( d.eddy.real[ !is.na(d.eddy.real$ws.osv) ,   'date' ] , d.power$date )
rh.cmn.dates.POWER <<- intersect( d.eddy.real[ !is.na(d.eddy.real$rh.osv) ,   'date' ] , d.power$date )


# Pearson's correlations
cor(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER,   'precip.osv' ]  ,  d.power[d.power$date %in% precip.cmn.dates.POWER , 'precip' ]  )  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.POWER,   'temp.avg.osv' ]  ,  d.power[d.power$date %in% temp.avg.cmn.dates.POWER, 'temp.mn' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.POWER  ,   'temp.min.osv' ]  ,  d.power[d.power$date %in% temp.min.cmn.dates.POWER, 'temp.min' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.POWER ,   'temp.max.osv' ]  ,  d.power[d.power$date %in% temp.max.cmn.dates.POWER, 'temp.max' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% rad.cmn.dates.POWER ,   'rg.osv' ]  ,  d.power[d.power$date %in% rad.cmn.dates.POWER , 'rg' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% ws.cmn.dates.POWER ,   'ws.osv' ]  ,  d.power[d.power$date %in% ws.cmn.dates.POWER , 'ws' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% rh.cmn.dates.POWER,   'rh.osv' ]  ,  d.power[d.power$date %in% rh.cmn.dates.POWER , 'rh' ])  


# Mean observed
mn.osv.npower.precip <- mean( d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER ,   'precip.osv' ]  )
mn.osv.npower.temp.avg <- mean( d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.POWER ,   'temp.avg.osv' ]  )
mn.osv.npower.temp.min <- mean( d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.POWER  ,   'temp.min.osv' ]  )
mn.osv.npower.temp.max <- mean( d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.POWER,   'temp.max.osv' ]  )
mn.osv.npower.rg <- mean( d.eddy.real[ d.eddy.real$date %in% rad.cmn.dates.POWER ,   'rg.osv' ]  )
mn.osv.npower.ws <- mean( d.eddy.real[ d.eddy.real$date %in% ws.cmn.dates.POWER  ,   'ws.osv' ]  )
mn.osv.npower.rh <- mean( d.eddy.real[ d.eddy.real$date %in%  rh.cmn.dates.POWER ,   'rh.osv' ]  )

# MEAN EXTERNAL
mn.series.npower.precip  <<- mean( d.power[d.power$date %in% precip.cmn.dates.POWER , 'precip' ]) 
mn.series.npower.temp.avg  <<- mean( d.power[d.power$date %in% temp.avg.cmn.dates.POWER , 'temp.mn' ]) 
mn.series.npower.temp.min  <<- mean( d.power[d.power$date %in% temp.min.cmn.dates.POWER ,  'temp.min']) 
mn.series.npower.temp.max  <<- mean( d.power[d.power$date %in%  temp.max.cmn.dates.POWER , 'temp.max'  ]) 
mn.series.npower.rg  <<- mean( d.power[d.power$date %in% rad.cmn.dates.POWER  ,  'rg']) 
mn.series.npower.ws  <<- mean( d.power[d.power$date %in% ws.cmn.dates.POWER ,  'ws' ]) 
mn.series.npower.rh  <<- mean( d.power[d.power$date %in% rh.cmn.dates.POWER , 'rh' ]) 


# Mean comparisons
mn.bias.npower.precip <<- mean( d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER ,   'precip.osv' ]  ) /   mean( d.power[d.power$date %in% precip.cmn.dates.POWER , 'precip' ]) 
mn.bias.npower.temp.avg <<- mean(  d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.POWER,   'temp.avg.osv' ]   ) /   mean( d.power[d.power$date %in% temp.avg.cmn.dates.POWER, 'temp.mn' ])  
mn.bias.npower.temp.min <<-  mean(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.POWER  ,   'temp.min.osv' ]   ) /   mean(  d.power[d.power$date %in% temp.min.cmn.dates.POWER, 'temp.min' ])  
mn.bias.npower.temp.max <<-  mean(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.POWER ,   'temp.max.osv' ]  ) /   mean(  d.power[d.power$date %in% temp.max.cmn.dates.POWER, 'temp.max' ])  
mn.bias.npower.rg <<-  mean(  d.eddy.real[ d.eddy.real$date %in% rad.cmn.dates.POWER ,   'rg.osv' ]  ) /   mean(  d.power[d.power$date %in% rad.cmn.dates.POWER , 'rg' ])  
mn.bias.npower.ws <<- mean(   d.eddy.real[ d.eddy.real$date %in% ws.cmn.dates.POWER ,   'ws.osv' ]   ) /   mean(  d.power[d.power$date %in% ws.cmn.dates.POWER , 'ws' ])  
mn.bias.npower.rh  <<-  mean(  d.eddy.real[ d.eddy.real$date %in% rh.cmn.dates.POWER ,   'rh.osv' ] ) /   mean( d.power[d.power$date %in% rh.cmn.dates.POWER , 'rh' ])  



# RATIO SD
ratio.sd.npower.precip <- sd(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER ,  'precip.osv' ]  )   / sd( d.power[d.power$date %in% precip.cmn.dates.POWER , 'precip'] ) 
ratio.sd.npower.temp.avg <- sd(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER ,  'temp.avg.osv'  ]  )   / sd( d.power[d.power$date %in% precip.cmn.dates.POWER , 'temp.mn' ] ) 
ratio.sd.npower.temp.min <- sd(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER ,  'temp.min.osv' ]  )   / sd( d.power[d.power$date %in% precip.cmn.dates.POWER , 'temp.min' ] ) 
ratio.sd.npower.temp.max <- sd(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER ,  'temp.max.osv' ]  )   / sd( d.power[d.power$date %in% precip.cmn.dates.POWER , 'temp.max' ] ) 
ratio.sd.npower.rg  <- sd(   d.eddy.real[ d.eddy.real$date %in% rad.cmn.dates.POWER  ,   'rg.osv' ]  )   / sd( d.power[d.power$date %in% rad.cmn.dates.POWER , 'rg' ] ) 
ratio.sd.npower.ws <- sd(   d.eddy.real[ d.eddy.real$date %in% ws.cmn.dates.POWER ,  'ws.osv' ]  )   / sd( d.power[d.power$date %in%  ws.cmn.dates.POWER , 'ws'] ) 
ratio.sd.npower.rh <- sd(   d.eddy.real[ d.eddy.real$date %in%  rh.cmn.dates.POWER ,  'rh.osv' ]  )   / sd( d.power[d.power$date %in%  rh.cmn.dates.POWER, 'rh' ] ) 



  
  
  #  TAHMO 621
  precip.cmn.dates.TAHMO.621 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$precip)  ,   'date' ] , d.weather.subs.621[ !is.na(d.weather.subs.621$precip)    , 'date'] )
  temp.avg.cmn.dates.TAHMO.621 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.avg.osv) ,   'date' ] ,d.weather.subs.621[ !is.na(d.weather.subs.621$temp.mn)    , 'date'] )
  temp.min.cmn.dates.TAHMO.621 <<-intersect( d.eddy.real[ !is.na(d.eddy.real$temp.min.osv) ,   'date' ] , d.weather.subs.621[ !is.na(d.weather.subs.621$temp.min)    , 'date'] )
  temp.max.cmn.dates.TAHMO.621 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.max.osv) ,   'date' ] , d.weather.subs.621[ !is.na(d.weather.subs.621$temp.max)    , 'date'] )
  
  
  # Pearson's correlations
  cor(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.621 ,  'precip.osv' ]  ,  d.weather.subs.621[d.weather.subs.621$date %in% precip.cmn.dates.TAHMO.621 , 'precip' ]  ,  method = 'pearson')  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.621 ,  'temp.avg.osv' ]  ,  d.weather.subs.621[d.weather.subs.621$date %in% temp.avg.cmn.dates.TAHMO.621 , 'temp.mn' ])  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.621,  'temp.min.osv' ]  ,  d.weather.subs.621[d.weather.subs.621$date %in% temp.min.cmn.dates.TAHMO.621, 'temp.min' ])  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.621 ,  'temp.max.osv' ]  ,  d.weather.subs.621[d.weather.subs.621$date %in% temp.max.cmn.dates.TAHMO.621  , 'temp.max' ])  
  
  # Mean comparisons
  mn.bias.ta.621.precip <<-  mean(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.621 ,  'precip.osv' ] ) /   mean(  d.weather.subs.621[d.weather.subs.621$date %in% precip.cmn.dates.TAHMO.621 , 'precip' ]  ) 
  mn.bias.ta.621.mean.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.621 ,  'temp.avg.osv' ] ) /  mean(  d.weather.subs.621[d.weather.subs.621$date %in% temp.avg.cmn.dates.TAHMO.621 , 'temp.mn' ])  
  mn.bias.ta.621.min.temp <<- mean(    d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.621,  'temp.min.osv' ]  ) /  mean(  d.weather.subs.621[d.weather.subs.621$date %in% temp.min.cmn.dates.TAHMO.621, 'temp.min' ])  
  mn.bias.ta.621.max.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.621 ,  'temp.max.osv' ] ) /  mean(  d.weather.subs.621[d.weather.subs.621$date %in% temp.max.cmn.dates.TAHMO.621  , 'temp.max' ])  
  
  
  

  #  TAHMO 678 
  d.ext <- d.weather.subs.678
  
  precip.cmn.dates.TAHMO.678 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$precip)  ,   'date' ] , d.ext[ !is.na(d.ext$precip)    , 'date'] )
  temp.avg.cmn.dates.TAHMO.678 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.avg.osv) ,   'date' ] , d.ext[ !is.na(d.ext$temp.mn)    , 'date'] )
  temp.min.cmn.dates.TAHMO.678 <<- intersect( d.eddy.real[  !is.na(d.eddy.real$temp.min.osv) ,   'date' ] , d.ext[ !is.na(d.ext$temp.min)    , 'date'] )
  temp.max.cmn.dates.TAHMO.678 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.max) ,   'date' ] , d.ext[ !is.na(d.ext$temp.max)    , 'date'] )
  
  # Mean observed
  mn.osv.TAHMO.678.precip <- mean( d.eddy.real[ d.eddy.real$date %in%   precip.cmn.dates.TAHMO.678  ,   'precip.osv' ]  )
  mn.osv.TAHMO.678.temp.avg <- mean( d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.678 ,   'temp.avg.osv' ]  )
  mn.osv.TAHMO.678.temp.min <- mean( d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.678  ,   'temp.min.osv' ]  )
  mn.osv.TAHMO.678.temp.max <- mean( d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.678,   'temp.max.osv' ]  )
  
  # Mean external
  mn.series.TAHMO.678.precip  <<- mean( d.ext[d.ext$date %in%  precip.cmn.dates.TAHMO.678, 'precip' ] ) 
  mn.series.TAHMO.678.temp.avg  <<- mean( d.ext[d.ext$date %in% temp.avg.cmn.dates.TAHMO.678 , 'temp.mn' ]) 
  mn.series.TAHMO.678.temp.min  <<- mean( d.ext[d.ext$date %in%  temp.min.cmn.dates.TAHMO.678 ,  'temp.min']) 
  mn.series.TAHMO.678.temp.max  <<- mean( d.ext[d.ext$date %in%  temp.max.cmn.dates.TAHMO.678 , 'temp.max'  ]) 
  
  # RATIO SDs
  ratio.sd.TAHMO.678.precip <- sd(   d.eddy.real[ d.eddy.real$date %in%   precip.cmn.dates.TAHMO.678  ,   'precip.osv' ]  )   / sd( d.ext[d.ext$date %in%  precip.cmn.dates.TAHMO.678, 'precip' ] ) 
  ratio.sd.TAHMO.678.temp.avg <- sd(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.678 ,   'temp.avg.osv' ]  )   / sd( d.ext[d.ext$date %in% temp.avg.cmn.dates.TAHMO.678 , 'temp.mn' ]) 
  ratio.sd.TAHMO.678.temp.min <- sd(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.678  ,   'temp.min.osv' ]  )   / sd(  d.ext[d.ext$date %in%  temp.min.cmn.dates.TAHMO.678 ,  'temp.min']) 
  ratio.sd.TAHMO.678.temp.max <- sd(  d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.678,   'temp.max.osv' ]   )   / sd( d.ext[d.ext$date %in%  temp.max.cmn.dates.TAHMO.678 , 'temp.max'  ]) 
  
  
  cor(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.678 ,  'precip.osv' ]  ,  d.ext[d.ext$date %in% precip.cmn.dates.TAHMO.678 , 'precip' ])  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.678 ,  'temp.avg.osv' ]  ,  d.ext[d.ext$date %in% temp.avg.cmn.dates.TAHMO.678 , 'temp.mn' ])  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.678,  'temp.min.osv' ]  ,  d.ext[d.ext$date %in% temp.min.cmn.dates.TAHMO.678, 'temp.min' ])  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.678 ,  'temp.max.osv' ]  ,  d.ext[d.ext$date %in% temp.max.cmn.dates.TAHMO.678  , 'temp.max' ])  
  
  # Mean comparisons
  mn.bias.ta.678.precip <<- mean(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.678  ,  'precip.osv' ] ) /   mean(  d.ext[d.ext$date %in% precip.cmn.dates.TAHMO.678  , 'precip' ]  ) 
  mn.bias.ta.678.mean.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.678  ,  'temp.avg.osv' ] ) /  mean(  d.ext[d.ext$date %in% temp.avg.cmn.dates.TAHMO.678  , 'temp.mn' ])  
  mn.bias.ta.678.min.temp <<- mean(    d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.678 ,  'temp.min.osv' ]  ) /  mean(  d.ext[d.ext$date %in% temp.min.cmn.dates.TAHMO.678 , 'temp.min' ])  
  mn.bias.ta.678.max.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.678  ,  'temp.max.osv' ] ) /  mean(  d.ext[d.ext$date %in% temp.max.cmn.dates.TAHMO.678   , 'temp.max' ])  
  
  
  #  TAHMO 814  
  d.ext <- d.weather.subs.814
  
  precip.cmn.dates.TAHMO.814 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$precip)  ,   'date' ] , d.ext[ !is.na(d.ext$precip)    , 'date'] )
  temp.avg.cmn.dates.TAHMO.814 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.avg.osv) ,   'date' ] , d.ext[ !is.na(d.ext$temp.mn)    , 'date'] )
  temp.min.cmn.dates.TAHMO.814 <<- intersect( d.eddy.real[  !is.na(d.eddy.real$temp.min.osv) ,   'date' ] , d.ext[ !is.na(d.ext$temp.min)    , 'date'] )
  temp.max.cmn.dates.TAHMO.814 <<- intersect( d.eddy.real[ !is.na(d.eddy.real$temp.max) ,   'date' ] , d.ext[ !is.na(d.ext$temp.max)    , 'date'] )
  
  # Mean observed
  mn.osv.TAHMO.814.precip <- mean( d.eddy.real[ d.eddy.real$date %in%   precip.cmn.dates.TAHMO.814  ,   'precip.osv' ]  )
  mn.osv.TAHMO.814.temp.avg <- mean( d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.814 ,   'temp.avg.osv' ]  )
  mn.osv.TAHMO.814.temp.min <- mean( d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.814  ,   'temp.min.osv' ]  )
  mn.osv.TAHMO.814.temp.max <- mean( d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.814,   'temp.max.osv' ]  )

  # Mean external
  mn.series.TAHMO.814.precip  <<- mean( d.ext[d.ext$date %in%  precip.cmn.dates.TAHMO.814, 'precip' ] ) 
  mn.series.TAHMO.814.temp.avg  <<- mean( d.ext[d.ext$date %in% temp.avg.cmn.dates.TAHMO.814 , 'temp.mn' ]) 
  mn.series.TAHMO.814.temp.min  <<- mean( d.ext[d.ext$date %in%  temp.min.cmn.dates.TAHMO.814 ,  'temp.min']) 
  mn.series.TAHMO.814.temp.max  <<- mean( d.ext[d.ext$date %in%  temp.max.cmn.dates.TAHMO.814 , 'temp.max'  ]) 
  
  # RATIO SDs
  ratio.sd.TAHMO.814.precip <- sd(   d.eddy.real[ d.eddy.real$date %in%   precip.cmn.dates.TAHMO.814  ,   'precip.osv' ]  )   / sd( d.ext[d.ext$date %in%  precip.cmn.dates.TAHMO.814, 'precip' ] ) 
  ratio.sd.TAHMO.814.temp.avg <- sd(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.814 ,   'temp.avg.osv' ]  )   / sd( d.ext[d.ext$date %in% temp.avg.cmn.dates.TAHMO.814 , 'temp.mn' ]) 
  ratio.sd.TAHMO.814.temp.min <- sd(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.814  ,   'temp.min.osv' ]  )   / sd(  d.ext[d.ext$date %in%  temp.min.cmn.dates.TAHMO.814 ,  'temp.min']) 
  ratio.sd.TAHMO.814.temp.max <- sd(  d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.814,   'temp.max.osv' ]   )   / sd( d.ext[d.ext$date %in%  temp.max.cmn.dates.TAHMO.814 , 'temp.max'  ]) 
  
  
  cor(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.814 ,  'precip.osv' ]  ,  d.ext[d.ext$date %in% precip.cmn.dates.TAHMO.814 , 'precip' ])  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.814 ,  'temp.avg.osv' ]  ,  d.ext[d.ext$date %in% temp.avg.cmn.dates.TAHMO.814 , 'temp.mn' ])  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.814,  'temp.min.osv' ]  ,  d.ext[d.ext$date %in% temp.min.cmn.dates.TAHMO.814, 'temp.min' ])  
  cor(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.814 ,  'temp.max.osv' ]  ,  d.ext[d.ext$date %in% temp.max.cmn.dates.TAHMO.814  , 'temp.max' ])  
  
  # Mean comparisons
  mn.bias.ta.814.precip <<- mean(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.814  ,  'precip.osv' ] ) /   mean(  d.ext[d.ext$date %in% precip.cmn.dates.TAHMO.814  , 'precip' ]  ) 
  mn.bias.ta.814.mean.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.814  ,  'temp.avg.osv' ] ) /  mean(  d.ext[d.ext$date %in% temp.avg.cmn.dates.TAHMO.814  , 'temp.mn' ])  
  mn.bias.ta.814.min.temp <<- mean(    d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.814 ,  'temp.min.osv' ]  ) /  mean(  d.ext[d.ext$date %in% temp.min.cmn.dates.TAHMO.814 , 'temp.min' ])  
  mn.bias.ta.814.max.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.814  ,  'temp.max.osv' ] ) /  mean(  d.ext[d.ext$date %in% temp.max.cmn.dates.TAHMO.814   , 'temp.max' ])  
  
  

} else {
  
mn.bias.npower.precip <<- 1
mn.bias.npower.temp.mean <- 1
mn.bias.npower.temp.min <- 1
mn.bias.npower.temp.max <- 1
mn.bias.npower.rg <- 1
mn.bias.npower.ws <- 1
mn.bias.npower.rh <- 1

mn.bias.ta.621.precip <- 1
mn.bias.ta.621.mean.temp  <- 1
mn.bias.ta.621.min.temp  <- 1
mn.bias.ta.621.max.temp  <- 1
  
mn.bias.ta.678.precip <- 1
mn.bias.ta.678.mean.temp  <- 1
mn.bias.ta.678.min.temp  <- 1
mn.bias.ta.678.max.temp  <- 1
  
mn.bias.ta.814.precip <- 1
mn.bias.ta.814.mean.temp  <- 1
mn.bias.ta.814.min.temp  <- 1
mn.bias.ta.814.max.temp  <- 1
  
  
} 
} # Bias calculations


for (  cv in weather.vars.eddy  ){
  
print(paste('Running variable', cv))

for (r in 1 : nrow(d.eddy.real) ){
  
 
  
if (FALSE){ # Loop testing
    
    # which( d.eddy.real$date == "2024-02-14"  ) 
    # which( d.eddy.real$date == "2024-02-14"  ) 
    
    cv <- weather.vars.eddy[1]
    r <- 700

  }
  

current.day.value <- d.eddy.real[r , cv ] 


# current.day.value <- NA
# current.day.value <- NaN

valid.value <- (!is.na(current.day.value) & (current.day.value != 'NaN')) | (current.day.value != 'NaN' & !is.na(current.day.value))   #| (((current.day.value == Inf) | (current.day.value == -Inf)))



if ( valid.value ) {  next }



print(paste('running row ', r))

current.date <- d.eddy.real[ r , "date" ]

current.subs.var <- weather.vars.subst[  which(  weather.vars.eddy == cv )   ]

current.subs.value.T621 <- NA
current.subs.value.T678 <- NA
current.subs.value.T814 <- NA
current.subs.value.NASA.P <- NA


is.NASA.P.only.var <- (current.subs.var %in% subs.vars.NASA.POWER.only)


#  TAHMO 814 (Kapiti supply)
if ( current.date %in% d.weather.subs.814$date &  !is.NASA.P.only.var  )  {  if (  !is.na( d.weather.subs.814[d.weather.subs.814$date == current.date ,current.subs.var ] )   ) { 
  
  value <- d.weather.subs.814[d.weather.subs.814$date == current.date ,current.subs.var ]
  

if (cv == 'precip.osv'){   mean <- mn.series.TAHMO.814.precip ; mean.rel.bias <- mn.bias.ta.814.precip ; ratio.sd <- ratio.sd.TAHMO.814.precip  ; mn.osv <- mn.osv.TAHMO.814.precip } 
if (cv == 'temp.avg.osv'){ mean <- mn.series.TAHMO.814.temp.avg ; mean.rel.bias <- mn.bias.ta.814.mean.temp ; ratio.sd <- ratio.sd.TAHMO.814.temp.avg  ; mn.osv <- mn.osv.TAHMO.814.temp.avg }
if (cv == 'temp.max.osv'){ mean <- mn.series.TAHMO.814.temp.max ; mean.rel.bias <- mn.bias.ta.814.max.temp ; ratio.sd <- ratio.sd.TAHMO.814.temp.max  ; mn.osv <- mn.osv.TAHMO.814.temp.max }
if (cv == 'temp.min.osv'){ mean <- mn.series.TAHMO.814.temp.min; mean.rel.bias <- mn.bias.ta.814.min.temp ; ratio.sd <- ratio.sd.TAHMO.814.temp.min ; mn.osv <- mn.osv.TAHMO.814.temp.min }
  
current.subs.value.T814 <- gen.bias.corrct(   value , mean , mn.osv  , mean.rel.bias ,    ratio.sd )

}}

# TAHMO 678
#if(   current.date %in% d.weather.subs.678$date &  !is.NASA.P.only.var & is.na(current.subs.value.T814)   ){ if(  !is.na( d.weather.subs.678[d.weather.subs.678$date == current.date ,current.subs.var ] )  ){  current.subs.value.T678 <- d.weather.subs.678[d.weather.subs.678$date == current.date ,current.subs.var ]  ; if (cv == 'precip.osv'){ current.subs.value.T678 <- current.subs.value.T678* mn.bias.ta.678.precip} ; if (cv == 'temp.avg.osv'){current.subs.value.T678 <- current.subs.value.T678  * mn.bias.ta.678.temp }  ; if (cv == 'temp.max.osv'){ current.subs.value.T678 <- current.subs.value.T678  * mn.bias.ta.678.max.temp} ; if (cv == 'temp.min.osv'){ current.subs.value.T678  <- current.subs.value.T678 * mn.bias.ta.678.min.temp }}} 



# TAHMO 678
if ( current.date %in% d.weather.subs.678$date &  !is.NASA.P.only.var  )  {  if (  !is.na( d.weather.subs.678[d.weather.subs.678$date == current.date ,current.subs.var ] )   ) { 
  
  value <- d.weather.subs.678[d.weather.subs.678$date == current.date ,current.subs.var ]
  
  
  if (cv == 'precip.osv'){   mean <- mn.series.TAHMO.678.precip ; mean.rel.bias <- mn.bias.ta.678.precip ; ratio.sd <- ratio.sd.TAHMO.678.precip  ; mn.osv <- mn.osv.TAHMO.678.precip } 
  if (cv == 'temp.avg.osv'){ mean <- mn.series.TAHMO.678.temp.avg ; mean.rel.bias <- mn.bias.ta.678.mean.temp ; ratio.sd <- ratio.sd.TAHMO.678.temp.avg  ; mn.osv <- mn.osv.TAHMO.678.temp.avg }
  if (cv == 'temp.max.osv'){ mean <- mn.series.TAHMO.678.temp.max ; mean.rel.bias <- mn.bias.ta.678.max.temp ; ratio.sd <- ratio.sd.TAHMO.678.temp.max  ; mn.osv <- mn.osv.TAHMO.678.temp.max }
  if (cv == 'temp.min.osv'){ mean <- mn.series.TAHMO.678.temp.min; mean.rel.bias <- mn.bias.ta.678.min.temp; ratio.sd <- ratio.sd.TAHMO.678.temp.min ; mn.osv <- mn.osv.TAHMO.678.temp.min }
  
  current.subs.value.T678 <- gen.bias.corrct(   value , mean , mn.osv  , mean.rel.bias ,    ratio.sd )
  
}}


# TAHMO 621 (Kapiti farmhouse)
if (  current.date %in% d.weather.subs.621$date &  !is.NASA.P.only.var & is.na(current.subs.value.T678 )  ){ if ( !is.na( d.weather.subs.621[d.weather.subs.621$date == current.date ,current.subs.var ] ) ) { current.subs.value.T621  <- d.weather.subs.621[d.weather.subs.621$date == current.date ,current.subs.var ]   ; if (cv == 'precip.osv'){ current.subs.value.T621  <- current.subs.value.T621  * mn.bias.ta.621.precip} ; if (cv == 'temp.avg.osv'){ current.subs.value.T621 <- current.subs.value.T621 * mn.bias.ta.621.temp}; if (cv == 'temp.max.osv'){ current.subs.value.T621  <- current.subs.value.T621 * mn.bias.ta.621.max.temp} ; if (cv == 'temp.min.osv'){ current.subs.value.T621 <- current.subs.value.T621  * mn.bias.ta.621.min.temp}}} 

# NASA POWER
if(   current.date %in% d.power$date & current.subs.var %in% colnames(d.power) & is.na(current.subs.value.T814 ) & is.na(current.subs.value.T678 ) & is.na(current.subs.value.T621 )   ){   
  
  value <-  d.power[ d.power$date == current.date ,current.subs.var ] 
  
  if (cv ==  "precip.osv" ){   mean <- mn.series.npower.precip ; mean.rel.bias <- mn.bias.npower.precip ; ratio.sd <- ratio.sd.npower.precip  ; mn.osv <- mn.osv.npower.precip } 
  if (cv == 'temp.avg.osv' ){   mean <- mn.series.npower.temp.avg ; mean.rel.bias <- mn.bias.npower.temp.mean ; ratio.sd <- ratio.sd.npower.temp.avg  ; mn.osv <- mn.osv.npower.temp.avg }
  if (cv ==  "temp.min.osv" ){   mean <- mn.series.npower.temp.min ; mean.rel.bias <- mn.bias.npower.temp.min  ; ratio.sd <- ratio.sd.npower.temp.min ; mn.osv <- mn.osv.npower.temp.min  } 
  if (cv ==  "temp.max.osv" ) {    mean <- mn.series.npower.temp.max ; mean.rel.bias <- mn.bias.npower.temp.max ; ratio.sd <- ratio.sd.npower.temp.max ; mn.osv <- mn.osv.npower.temp.max } 
  if (cv == "rg" ){     mean <- mn.series.npower.rg ; mean.rel.bias <- mn.bias.npower.rg ; ratio.sd <- ratio.sd.npower.rg  ; mn.osv <- mn.osv.npower.rg }
  if (cv == "rh" ){    mean <- mn.series.npower.rh ; mean.rel.bias <- mn.bias.npower.rh ; ratio.sd <- ratio.sd.npower.rh ; mn.osv <- mn.osv.npower.rh  } 
  if (cv == "ws"  ){   mean <- mn.series.npower.ws ; mean.rel.bias <- mn.bias.npower.ws ; ratio.sd <- ratio.sd.npower.ws ; mn.osv <- mn.osv.npower.ws  }
  
  
  current.subs.value.NASA.P <- gen.bias.corrct(   value , mean , mn.osv  , mean.rel.bias ,    ratio.sd )


}
  
  
print('have set correct value')



print(paste('have identifid as na for date', current.date))
  
  
if (  !is.na(current.subs.value.NASA.P )  )  {
  
  d.eddy.real[ r , cv ] <- current.subs.value.NASA.P  
  
  
  if (  cv == weather.vars.eddy[1]  ){  d.eddy.real[ r , 'variable.status.temp.avg' ] <- v.status.subs.filled.NASA.POWER 
  } else if (  cv == weather.vars.eddy[2]  ){  d.eddy.real[ r , 'variable.status.temp.min' ] <- v.status.subs.filled.NASA.POWER 
  } else if (  cv == weather.vars.eddy[3]  ){  d.eddy.real[ r , 'variable.status.temp.max' ] <- v.status.subs.filled.NASA.POWER 
  } else if (  cv == weather.vars.eddy[4]  ){  d.eddy.real[ r , 'variable.status.precip' ] <- v.status.subs.filled.NASA.POWER 
  } else if (  cv == weather.vars.eddy[5]  ){  d.eddy.real[ r , 'variable.status.rg' ] <- v.status.subs.filled.NASA.POWER 
  } else  if (  cv == weather.vars.eddy[6]  ){  d.eddy.real[ r , 'variable.status.ws' ] <- v.status.subs.filled.NASA.POWER  
  } else if (  cv == weather.vars.eddy[7]  ){  d.eddy.real[ r , 'variable.status.rh' ] <- v.status.subs.filled.NASA.POWER  }

  print('have specified status')
  
  
} else if ( !is.na( current.subs.value.T814 )  ){
  
  print(paste('for variable ', cv,'substituting', current.subs.value.2 , 'for date ', current.date ))
  
  d.eddy.real[ r , cv ] <- current.subs.value.T814 
  
  
  if (  cv == weather.vars.eddy[1]  ){  d.eddy.real[ r , 'variable.status.temp.avg' ] <- v.status.subs.filled.tahmo.814   
  } else if (  cv == weather.vars.eddy[2]  ){  d.eddy.real[ r , 'variable.status.temp.min' ] <- v.status.subs.filled.tahmo.814   
  } else if (  cv == weather.vars.eddy[3]  ){  d.eddy.real[ r , 'variable.status.temp.max' ] <- v.status.subs.filled.tahmo.814   
  } else if (  cv == weather.vars.eddy[4]  ){  d.eddy.real[ r , 'variable.status.precip' ]   <- v.status.subs.filled.tahmo.814   
  
  
  } 
  
  
} else if( !is.na(  current.subs.value.T678  )  )    {
  
  print(paste('for variable ', cv,'substituting', current.subs.value.1 , 'for date ', current.date ))
  
  d.eddy.real[ r , cv ] <- current.subs.value.T678
  
  if (  cv == weather.vars.eddy[1]  ){  d.eddy.real[ r , 'variable.status.temp.avg' ] <- v.status.subs.filled.tahmo.678  
  } else if (  cv == weather.vars.eddy[2]  ){  d.eddy.real[ r , 'variable.status.temp.min' ] <- v.status.subs.filled.tahmo.678  
  } else if (  cv == weather.vars.eddy[3]  ){  d.eddy.real[ r , 'variable.status.temp.max' ] <- v.status.subs.filled.tahmo.678  
  } else if (  cv == weather.vars.eddy[4]  ){  d.eddy.real[ r , 'variable.status.precip' ] <- v.status.subs.filled.tahmo.678  }
  
  
  
}  else if ( !is.na(current.subs.value.T621  )  ) { # IF TAHMO 621 has data
    
    
d.eddy.real[ r , cv ] <- current.subs.value.T621

if (  cv == weather.vars.eddy[1]  ){  d.eddy.real[ r , 'variable.status.temp.avg' ] <- v.status.subs.filled.tahmo.621 
} else if (  cv == weather.vars.eddy[2]  ){  d.eddy.real[ r , 'variable.status.temp.min' ] <- v.status.subs.filled.tahmo.621 
} else if (  cv == weather.vars.eddy[3]  ){  d.eddy.real[ r , 'variable.status.temp.max' ] <- v.status.subs.filled.tahmo.621  
} else if (  cv == weather.vars.eddy[4]  ){  d.eddy.real[ r , 'variable.status.precip' ] <- v.status.subs.filled.tahmo.621  }


}   


} 


}




  
}


}

d.eddy.real$true.variables <- FALSE

d.eddy.real[ 
  (
  d.eddy.real$variable.status.precip == "actual"
   & d.eddy.real$variable.status.temp.avg == "actual"
  & d.eddy.real$variable.status.temp.min == "actual"
  & d.eddy.real$variable.status.temp.max == "actual"
  & d.eddy.real$variable.status.rg == "actual"
  & d.eddy.real$variable.status.ws == "actual"
  & d.eddy.real$variable.status.rh == "actual"
  )
  
,'true.variables'] <- TRUE


d.eddy.real$variable.status <- FALSE
d.eddy.real[  d.eddy.real$true.variables == TRUE  , 'variable.status'] <- TRUE



#summary(d.eddy.real$precip.osv)
#summary(d.eddy.real$precip.not.eddy.contns)

# Systematic bias in Eddy weather obs: 1.897/1.559



# Check if any NA values remain
sum( TRUE == is.na(d.eddy.real$precip.osv)) 
sum( TRUE == is.na(d.eddy.real$temp.avg.osv))  
sum( TRUE == is.na(d.eddy.real$temp.min.osv))
sum( TRUE == is.na(d.eddy.real$temp.max.osv))

sum( TRUE == is.na(d.eddy.real$rg.osv))
sum( TRUE == is.na(d.eddy.real$rh.osv))
sum( TRUE == is.na(d.eddy.real$ws.osv))



# Data clip 
# Actual (EC tower) data
# EC tower data
{
  

d.eddy.real <- d.eddy.real[
d.eddy.real$date >= start.date.cald 
& d.eddy.real$date <= end.date.cald
,  ]


}



print('Have finished prepping Eddy data ')

stop()

# Climate data out
{
d.eddy.oc <<- d.eddy.real
d.eddy.oc$day.count <- NA
start.day.count <- 209
d.eddy.oc[1, 'day.count'] <- start.day.count
d.eddy.oc[1, 'year'] <- "2018"

for (r in 2:nrow(d.eddy.oc)) {
  
# r <- 2
  
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

, round ( d.eddy.oc$temp.avg.osv  , decimal.round ) 
,  round ( d.eddy.oc$temp.min.osv  , decimal.round ) 
,  round ( d.eddy.oc$temp.max.osv  , decimal.round ) 

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

# View(  d.eddy.clim.out)
  


}

# Merge and export climate data
{
  
d.eddy.clim.pre.sim <<- read_xlsx(   str_c( R.dat.dir , 'climate.pre.sim.data.xlsx' ) )
d.eddy.clim.pre.sim <- as.data.frame(d.eddy.clim.pre.sim )
  

row.cond <- (d.eddy.clim.pre.sim$year <= 2019 )


d.eddy.clim.pre.sim.no.bc <- d.eddy.clim.pre.sim
d.eddy.clim.pre.sim.no.bc[ row.cond , "tavg"] <- d.eddy.clim.pre.sim[ row.cond , "tavg"] #* (1 / mn.bias.npower.temp.mean)
d.eddy.clim.pre.sim.no.bc[ row.cond , "tmin"] <- d.eddy.clim.pre.sim[ row.cond , "tmin"] #* (1 / mn.bias.npower.temp.min)
d.eddy.clim.pre.sim.no.bc[ row.cond , "tmax"] <- d.eddy.clim.pre.sim[ row.cond , "tmax"] #* (1 / mn.bias.npower.temp.max)
d.eddy.clim.pre.sim.no.bc[ row.cond , "grad"] <- d.eddy.clim.pre.sim[ row.cond , "grad"] #* (1 / mn.bias.npower.rad)
d.eddy.clim.pre.sim.no.bc[ row.cond , "prec"] <- d.eddy.clim.pre.sim[ row.cond , "prec"] #* 0.945 #(1 / mn.bias.npower.precip)
d.eddy.clim.pre.sim.no.bc[ row.cond , "rh"] <- d.eddy.clim.pre.sim[ row.cond , "rh"] #* (1 / mn.bias.npower.rh)
d.eddy.clim.pre.sim.no.bc[ row.cond , "wind"] <- d.eddy.clim.pre.sim[ row.cond , "wind"] #* (1 /mn.bias.npower.ws)


d.eddy.clim.pre.sim.bc <- d.eddy.clim.pre.sim
d.eddy.clim.pre.sim.bc[ row.cond , "tavg"] <- d.eddy.clim.pre.sim[ row.cond , "tavg"] #* (1 / mn.bias.npower.temp.mean)
d.eddy.clim.pre.sim.bc[ row.cond , "tmin"] <- d.eddy.clim.pre.sim[ row.cond , "tmin"] #* (1 / mn.bias.npower.temp.min)
d.eddy.clim.pre.sim.bc[ row.cond , "tmax"] <- d.eddy.clim.pre.sim[ row.cond , "tmax"] #* (1 / mn.bias.npower.temp.max)
d.eddy.clim.pre.sim.bc[ row.cond , "grad"] <- d.eddy.clim.pre.sim[ row.cond , "grad"] * (1 / mn.bias.npower.rad)
d.eddy.clim.pre.sim.bc[ row.cond , "prec"] <- d.eddy.clim.pre.sim[ row.cond , "prec"] *  (1 / mn.bias.npower.precip)
d.eddy.clim.pre.sim.bc[ row.cond , "rh"] <- d.eddy.clim.pre.sim[ row.cond , "rh"] #* (1 / mn.bias.npower.rh)
d.eddy.clim.pre.sim.bc[ row.cond , "wind"] <- d.eddy.clim.pre.sim[ row.cond , "wind"] #* (1 /mn.bias.npower.ws)




for (r in 1:nrow(d.eddy.clim.pre.sim)){

d.eddy.clim.pre.sim.no.bc[ r , "tavg"] <- round( d.eddy.clim.pre.sim.no.bc[ r , "tavg"] ,2)
d.eddy.clim.pre.sim.no.bc[ r , "tmin"] <- round( d.eddy.clim.pre.sim.no.bc[ r , "tmin"] ,2)
d.eddy.clim.pre.sim.no.bc[ r , "tmax"] <- round( d.eddy.clim.pre.sim.no.bc[ r , "tmax"] ,2)
d.eddy.clim.pre.sim.no.bc[ r , "grad"] <- round( d.eddy.clim.pre.sim.no.bc[ r , "grad"] ,2)
d.eddy.clim.pre.sim.no.bc[ r , "prec"] <- round( d.eddy.clim.pre.sim.no.bc[ r , "prec"] ,2)
d.eddy.clim.pre.sim.no.bc[ r , "rh"] <- round( d.eddy.clim.pre.sim.no.bc[ r , "rh"] ,2)
d.eddy.clim.pre.sim.no.bc[ r , "wind"] <- round( d.eddy.clim.pre.sim.no.bc[ r , "wind"] ,2)


d.eddy.clim.pre.sim.bc[ r , "tavg"] <- round( d.eddy.clim.pre.sim.bc[ r , "tavg"] ,2)
d.eddy.clim.pre.sim.bc[ r , "tmin"] <- round( d.eddy.clim.pre.sim.bc[ r , "tmin"] ,2)
d.eddy.clim.pre.sim.bc[ r , "tmax"] <- round( d.eddy.clim.pre.sim.bc[ r , "tmax"] ,2)
d.eddy.clim.pre.sim.bc[ r , "grad"] <- round( d.eddy.clim.pre.sim.bc[ r , "grad"] ,2)
d.eddy.clim.pre.sim.bc[ r , "prec"] <- round( d.eddy.clim.pre.sim.bc[ r , "prec"] ,2)
d.eddy.clim.pre.sim.bc[ r , "rh"] <- round( d.eddy.clim.pre.sim.bc[ r , "rh"] ,2)
d.eddy.clim.pre.sim.bc[ r , "wind"] <- round( d.eddy.clim.pre.sim.bc[ r , "wind"] ,2)

}


start.year <- as.numeric(d.eddy.clim.out$`*`[1])
start.day.numeric <- as.numeric(  d.eddy.clim.out[ , c(2)  ][1]  )

d.eddy.clim.pre.sim <- d.eddy.clim.pre.sim[
  (d.eddy.clim.pre.sim$year == start.year  &  d.eddy.clim.pre.sim$day < start.day.numeric  )   
  | ( d.eddy.clim.pre.sim$year < start.year  ) 
  ,    ]

d.eddy.clim.pre.sim.bc <- d.eddy.clim.pre.sim.bc[
  (d.eddy.clim.pre.sim.bc$year == start.year  &  d.eddy.clim.pre.sim.bc$day < start.day.numeric  )   
  | ( d.eddy.clim.pre.sim.bc$year < start.year  ) 
  ,    ]

colnames(d.eddy.clim.pre.sim) <- colnames(  d.eddy.clim.out )
colnames(d.eddy.clim.pre.sim.bc) <- colnames(  d.eddy.clim.out )

full.clim.data <- rbind(  d.eddy.clim.pre.sim , d.eddy.clim.out  )
full.clim.data.bc <- rbind(  d.eddy.clim.pre.sim.bc , d.eddy.clim.out  )


write.table(  full.clim.data  , file = str_c( R.back.x2 ,"KE_Kapiti_climate_eddy_raw_no_bc.txt")  , 
              append = FALSE, sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE)


write.table(  full.clim.data.bc  , file = str_c( R.back.x2 ,"KE_Kapiti_climate_eddy_raw_bc.txt")  , 
              append = FALSE, sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE)



}




# Air chemistry data
gen.air.chem <- function(){
  
co2.dat <- as.data.frame( read_excel( str_c( R.dat.dir  , "NOAA_atmospheric_conc_ch4_and_co2.xlsx") , sheet = "NOAA_atmospheric_conc_co2"))
ch4.dat <- as.data.frame( read_excel( str_c( R.dat.dir  , "NOAA_atmospheric_conc_ch4_and_co2.xlsx") , sheet = "NOAA_atmospheric_conc_ch4"))



View(co2.dat)


# NOTE: must choose one of either BC or non BC series
d.air.chem.bc <- full.clim.data.bc[ -c(3,4,5,6,7,8,9)]
  
colnames(d.air.chem.bc  )[2] <-  "*" 

d.air.chem.bc$nh4dry <- NA
d.air.chem.bc$nh4wet <- NA
d.air.chem.bc$no2dry <- NA
d.air.chem.bc$no3dry <- NA
d.air.chem.bc$no3wet <- NA
d.air.chem.bc$co2 <- NA


# 1. Define Annual Baselines (kg N/ha/yr)
annual_nh4 <<- 1.70  # All NHx
annual_no3 <<- 1.15  # 82% of NOy
annual_no2 <<- 0.25  # 18% of NOy (Dry deposition only)

# 2. Partition Wet/Dry for NH4 and NO3 (65% wet, 35% dry)
frac_wet <<- 0.65       
frac_dry <<- 0.35       

# Calculate background daily dry deposition rates
d.air.chem.bc$nh4dry <- (annual_nh4 * frac_dry) / 365
d.air.chem.bc$no3dry <- (annual_no3 * frac_dry) / 365
d.air.chem.bc$no2dry <- annual_no2 / 365  # 100% dry, divided evenly across the year

unq.years <- unique(full.clim.data.bc$`*`)

for ( y in unq.years){
  
  
  # y <- unq.years[2]
  
  precip.ann <- sum( na.omit( full.clim.data.bc[ full.clim.data.bc$`*` == y , 'prec' ] ))
  precip.wgt <- full.clim.data.bc[ full.clim.data.bc$`*` == y , 'prec' ] /  precip.ann
  
  d.air.chem.bc[ d.air.chem.bc$`*` == y , 'nh4wet'] <- cv.depos.2.rainw * (annual_nh4 * frac_wet) *  precip.wgt
  d.air.chem.bc[ d.air.chem.bc$`*` == y , 'no3wet'] <- cv.depos.2.rainw * (annual_no3 * frac_wet) *  precip.wgt
  
  
}


write.table(  d.air.chem.bc  , file = str_c( R.back.x2 ,"KE_Kapiti_airchemistry_eddy_raw_bc.txt")  , 
              append = FALSE, sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE)



summary( d.air.chem.bc$nh4wet )
summary( d.air.chem.bc$no3wet )

summary( c( d.air.chem.bc$nh4dry +  d.air.chem.bc$nh4wet / cv.depos.2.rainw) )
summary( c( d.air.chem.bc$no3dry +  d.air.chem.bc$no3wet / cv.depos.2.rainw) )
summary( d.air.chem.bc$no2dry )


View( d.air.chem.bc  )

full.clim.data.bc$prec <- 

daily_wet_nh4 = (annual_nh4 * frac_wet) * precip_weight
daily_wet_no3 = (annual_no3 * frac_wet) * precip_weight


  

  
  
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





