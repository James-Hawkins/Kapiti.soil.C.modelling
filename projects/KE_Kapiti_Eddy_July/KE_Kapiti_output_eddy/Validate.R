

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())


save.image('L.DNDC.Validate.RData')
load('L.DNDC.Validate.RData')

# Global parameters
{
# Unit conversions
cv.MJ.to.watts <<- 11.57
  
# Running averages --  switches
r.a.switch.lai <- FALSE
r.a.switch.et <- FALSE
r.a.switch.nee <- FALSE
r.a.switch.swc.5.cm <<- TRUE
r.a.switch.swc.15.cm <<- TRUE
r.a.switch.swc.30.cm <<- TRUE
r.a.switch.gpp <<- TRUE
r.a.switch.ter <<- TRUE

r.a.perd.lai <- 1
r.a.perd.swc.5.cm <- 6
r.a.perd.swc.15 <- 6
r.a.perd.swc.30 <- 6
r.a.perd.ter <- 6
r.a.perd.gpp <- 6
r.a.perd.nee <- 1


  
# Global sets
periods <<- c('dipole' , 'drought' , 'normal')

var <- c(
  'swc.5' 
  ,'swc.15' 
  ,'swc.30' 
  , 'ter'
  , 'gpp'
  , 'nee'
  , 'lai'
)

osv.metric.vars <<- c(
  'r.a.swc.5.cm.osv' 
  , 'r.a.swc.15.cm.osv' 
  , 'r.a.swc.30.cm.osv' 
  , 'r.a.ter.osv'
  , 'r.a.gpp.osv'
  , 'r.a.nee.osv'
  , 'r.a.lai.osv'
)


sim.metric.vars <<-c(
  'r.a.swc.5.cm.sim' 
  ,'r.a.swc.15.cm.sim' 
  ,'r.a.swc.30.cm.sim' 
  , 'r.a.ter.sim'
  , 'r.a.gpp.sim'
  , 'r.a.nee.sim'
  , 'r.a.lai.sim'
)

sim.metric.vars.bc <<-c(
  'r.a.swc.5.cm.sim.bc' 
  ,  'r.a.swc.15.cm.sim.bc' 
  ,  'r.a.swc.30.cm.sim.bc' 
  , 'r.a.ter.sim.bc'
  , 'r.a.gpp.sim.bc'
  , 'r.a.nee.sim.bc'
  , 'r.a.lai.sim.bc'
)

period.label <<- c( 'Dipole' , "'20-22 drought" , "Normal")



# Conversion factors
cv.sq.m.2.ha <- 10000
cv.microml.2.kg <- 0.000000001 
cv.sec.2.yr <- 60*60*24*365
cv.sec.2.d <- 60*60*24
cv.mml.c.2.co2 <<- 12
cv.mj.2.watts <<- 1/ 0.0864
cv.secs.per.30.min <<- 1800 

parm.Lv <<- 2260000
parm.pw <<- 1000

start.date.cald <<- "2018-07-28"
end.date.cald <<- "2024-12-04"


v.status.actual <<- 'actual'
v.status.filled <<- 'filled'

v.status.mn.filled <<- 'mn.filled'
v.status.subs.filled <<- 'subs.filled'
v.status.eo.filled <<- 'eo.filled'

cor.type <<- 'spearman' 
#cor.type <<- 'pearson'

rd.decs.rmse  <<- 1

drought.period.start <<- "2020-10-01"
drought.period.mid <<- "2021-12-15"
drought.period.end <<- "2022-12-30"

dipole.period.start <<- "2019-06-01"
dipole.period.mid <<- "2019-09-15"
dipole.period.end <<- "2019-12-30"
dipole.period.valid.end <<- "2020-06-01"




}

  
library("languageR") ; library('readxl') ; library('ggplot2') ; library('stringr') ;library('stringi') ; library('chron') ; library('lubridate') ; library('ggpubr') ; source('helpers.R') ; library('tidyr')
  
  

source('Eddy_transform.R')
source('biomass.osv.R')

{

# L-DNDC modelled outputs
{
  

  

d.sl.chem <<- read.csv('KE_Kapiti_soilchemistry-daily.csv')
  
d.physio.all  <<- read.csv('KE_Kapiti_physiology-daily.csv')
  
d.watr  <<- read.csv('KE_Kapiti_watercycle-daily.csv')


# Rename columns
names(d.sl.chem)[6] <- 'emis.hetero'

names(d.physio.all)[3] <- 'date.time'
names(d.physio.all)[25] <- 'maint.resp'
names(d.physio.all)[26] <- 'transp.resp'
names(d.physio.all)[27] <- 'growth.resp'
names(d.physio.all)[28] <- 'co2.upt'

names(d.physio.all)[37] <- 'bg.biom.kg.m2'
names(d.physio.all)[38] <- 'ag.biom.kg.m2'


names(d.physio.all)[39] <- 'lai.sim'


#colnames(d.physio)
cols.2.add.physio <- c('co2.upt' ) # , 'maint.resp'  , 'transp.resp'   , 'growth.resp'  , 'emis.hetero'


unique(d.physio.all$species)

all.grass.species <- c( 
  "ANGA" 
  , "PERG" 
  , "PECL" 
  , 'CEBI' 
  , 'GRASS' 
  , 'SAFF'
  , "RED_OAT"
  , "TEPHROSIA"
  
  
  )
all.tree.species <- c(  "BUAF" , "TAPAJOS" , "ACTO" ,  "ACTO_SHRUB" , "TAPAJOS")

unique.species <- unique(d.physio.all$species )
unique.species.grass <- unique(  d.physio.all[d.physio.all$species %in% all.grass.species , 'species'])  
unique.species.trees <-  unique(  d.physio.all[d.physio.all$species %in% all.tree.species , 'species'])   
species.str.id.all <- ":ALL:" 



d.physio.grass <- d.physio.all
d.physio.trees <- d.physio.all

#d.physio.grass$co2.upt <-  d.physio.all[ d.physio.all$species == unique.species.grass[1] ,cols.2.add.physio ] + d.physio.all[ d.physio.all$species == unique.species.grass[2] ,cols.2.add.physio ] #+  d.physio.all[ d.physio.all$species == unique.species.grass[3] ,cols.2.add.physio ]
#d.physio.trees$co2.upt <- d.physio.all[ d.physio.all$species == unique.species.trees[1] ,cols.2.add.physio ] #+ d.physio.all[ d.physio.all$species == unique.species.trees[2] ,cols.2.add.physio ] 


nrow(d.physio.all)
#nrow(d.physio)
nrow(d.physio.grass)
nrow(d.physio.trees)


#d.physio.all$bg.biom.grass.kg.ha  <- d.physio$bg.biom.kg.m2 * cv.sq.m.2.ha
#d.physio.all$ag.biom.grass.kg.ha  <- d.physio$ag.biom.kg.m2 * cv.sq.m.2.ha

nrow(d.physio.all)

if (   length(unique(d.physio.all$species )  ) > 1 ) {  d.physio<- d.physio.all[ d.physio.all$species == species.str.id.all   ,]
} else { d.physio.all$species <- species.str.id.all ; d.physio <- d.physio.all }
nrow(d.physio.all)



if (   length(unique(d.physio.all$species )  ) > 1 ) {
  
for (d in d.physio$date.time)  {
  
  d.all.cond.grass <- (d.physio.all$date.time == d & d.physio.all$species %in% unique.species.grass )
  d.all.cond.trees <- (d.physio.all$date.time == d & d.physio.all$species %in% unique.species.trees)
  
  
  d.physio[d.physio$date.time == d , 'ag.biom.grass.kg.m2'] <- sum(d.physio.all[  d.all.cond.grass   ,   'ag.biom.kg.m2' ]  )
 # d.physio[d.physio$date.time == d , 'bg.biom.grass.kg.m2'] <- sum(d.physio.all[  d.all.cond.grass   ,   'bg.biom.kg.m2' ]  )
  #d.physio[d.physio$date.time == d , 'lai.sim.grass'] <- sum(d.physio.all[  d.all.cond.grass   ,   'lai.sim' ]  )
  
 # d.physio[d.physio$date.time == d , 'co2.upt.grass'] <- sum(d.physio.all[  d.all.cond.grass   ,   'co2.upt' ]  )
  
  
  d.physio[d.physio$date.time == d , 'ag.biom.trees.kg.m2'] <- sum(d.physio.all[    d.all.cond.trees   ,   'ag.biom.kg.m2' ]  )
 # d.physio[d.physio$date.time == d , 'bg.biom.trees.kg.m2'] <- sum(d.physio.all[    d.all.cond.trees  ,   'bg.biom.kg.m2' ]  )
 # d.physio[d.physio$date.time == d , 'lai.sim.trees'] <- sum(d.physio.all[  d.all.cond.trees    ,   'lai.sim' ]  )
 # d.physio[d.physio$date.time == d , 'co2.upt.trees'] <- sum(d.physio.all[  d.all.cond.trees   ,   'co2.upt' ]  )

}
}




# Convert to ha values
d.physio$ag.biom.trees.kg.ha <- d.physio$ag.biom.trees.kg.m2 * cv.sq.m.2.ha
#d.physio$bg.biom.trees.kg.ha <- d.physio$bg.biom.trees.kg.m2 * cv.sq.m.2.ha

d.physio$ag.biom.grass.kg.ha <- d.physio$ag.biom.grass.kg.m2 * cv.sq.m.2.ha
#d.physio$bg.biom.grass.kg.ha <- d.physio$bg.biom.grass.kg.m2 * cv.sq.m.2.ha


d.physio$bg.biom.kg.ha  <- d.physio$bg.biom.kg.m2 * cv.sq.m.2.ha
d.physio$ag.biom.kg.ha  <- d.physio$ag.biom.kg.m2 * cv.sq.m.2.ha

nrow(d.watr)
d.watr$date[1]
d.physio$date[1]
d.sl.chem$date[1]

d.watr$date[nrow(d.watr)]
d.physio$date[nrow(d.physio)]
d.sl.chem$date[nrow(d.sl.chem)]

#nrow(d.physio)
#nrow( d.sl.chem)
#nrow( d.watr)

names(d.watr)[3] <- 'date.time'
names(d.watr)[5] <- 'precip.sim'
names(d.watr)[7] <- 'et.sim.mm'
names(d.watr)[26] <- 'sw.5'
names(d.watr)[27] <- 'sw.10'
names(d.watr)[28] <- 'sw.15'
names(d.watr)[29] <- 'sw.20'
names(d.watr)[30] <- 'sw.30'
names(d.watr)[31] <- 'sw.40'
names(d.watr)[32] <- 'sw.50'
names(d.watr)[33] <- 'sw.60'



# Merged model data
{

d.all <- cbind( d.sl.chem$emis.hetero , d.physio)

# d.all <- cbind( d.all[d.all$date.time %in% d.watr$date , ] , d.watr[d.watr$date %in% d.all$date.time , ] )



for (d in d.all$date.time){

d.all[d == d.all$date.time , 'sw.5'] <- d.watr[d.watr$date == d, 'sw.5']
d.all[d == d.all$date.time , 'sw.15'] <- d.watr[d.watr$date == d, 'sw.15'][1] 
d.all[d == d.all$date.time , 'sw.30'] <- d.watr[d.watr$date == d, 'sw.30'][1] 


}
  


  
  names(d.all)[1] <- 'emis.hetero'
  d.all$date.time <- as.Date(d.all$date.time ,  format="%Y-%m-%d")
 
  d.all$day.cnt <- NA
  
  
  for (r in 1:nrow(d.all)  ){
    
    d.all[ r , 'day.cnt'] <- r 
    
  }
  
  
  
  final.date <- tail(d.all$date.time )[6]
  
  frst.date <- which( d.all$date.time  == start.date.cald )
  end.date <- which( d.all$date.time == end.date.cald )
  
  d.all <- d.all[d.all$day.cnt >= frst.date
                # & d.all$day.cnt <= end.date
                 ,  ]
  

  
}


}
  
   
# LAI data
d.lai <- function(){
  
#nrow(d.lai)
  
d.lai <- d.lai[
d.lai$date >= start.date.cald
&  d.lai$date <= end.date.cald
,  ]

  


d.all$lai.obs <- NA


for (d in d.lai$date){
  
  date <- as.Date( d )
  date.p.1 <- as.Date( d + 1 )
  date.p.2 <- as.Date( d + 2 )
  date.p.3 <- as.Date( d + 3 )
  date.p.4 <- as.Date( d + 4 )
  date.p.5 <- as.Date( d + 5 )
  date.p.6 <- as.Date( d + 6 )
  date.p.7 <- as.Date( d + 7 )
  
  d.all[d.all$date.time == date , 'lai.obs'] <- d.lai[ d.lai$date == date , 'lai']
  
  d.all[d.all$date.time == date.p.1 , 'lai.obs'] <- d.lai[ d.lai$date == date  , 'lai']
  d.all[d.all$date.time == date.p.2 , 'lai.obs'] <- d.lai[ d.lai$date == date , 'lai']
  d.all[d.all$date.time == date.p.3 , 'lai.obs'] <- d.lai[ d.lai$date == date , 'lai']
  d.all[d.all$date.time == date.p.4 , 'lai.obs'] <- d.lai[ d.lai$date == date , 'lai']
  d.all[d.all$date.time == date.p.5 , 'lai.obs'] <- d.lai[ d.lai$date == date , 'lai']
  d.all[d.all$date.time == date.p.6 , 'lai.obs'] <- d.lai[ d.lai$date == date , 'lai']
  d.all[d.all$date.time == date.p.7 , 'lai.obs'] <- d.lai[ d.lai$date == date , 'lai']

}
  


  
}
   
  
  

d.eddy.real <- d.eddy.real[
  d.eddy.real$date >= start.date.cald
  & d.eddy.real$date <= end.date.cald
  ,  ]




# Convert main variables to numeric
convert.numeric.list <- c(
  'transp.resp'
  , 'growth.resp'
  , 'maint.resp'
  , 'emis.hetero'
  ,'co2.upt'
  )

for (l in convert.numeric.list){
  print(paste(l))
  
  d.all[,l] <- as.numeric( d.all[,l])
  
}


d.all$year.month <- NA
d.all$biom.osv.kg.ha <- NA


for (r in 1:nrow(d.all)){
  
  curr.date <- d.all[r,'date.time']
  curr.date.month.year <- substr(  curr.date , 1,7)
  
d.all[r,'year.month'] <- curr.date.month.year 

if (curr.date.month.year %in% biom.osv.unique.months){
  
  d.all[r,'biom.osv.kg.ha'] <- mean(  biomass[biomass$Month %in% curr.date.month.year ,  'biom.osv.kg.ha'])
  
  precip.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'precip']
  precip.tah.621 <-  d.weather.subs.621[d.weather.subs.621$date == curr.date, 'precip']
  precip.tah.678 <-  d.weather.subs.678[d.weather.subs.678$date == curr.date, 'precip']
  precip.tah.814 <-  d.weather.subs.814[d.weather.subs.814$date == curr.date, 'precip']
  
  temp.min.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.min']
  temp.min.tah.621 <-  d.weather.subs.621[d.weather.subs.621$date == curr.date, 'temp.min']
  temp.min.tah.678 <-  d.weather.subs.678[d.weather.subs.678$date == curr.date, 'temp.min']
  temp.min.tah.814 <-  d.weather.subs.814[d.weather.subs.814$date == curr.date, 'temp.min']
  
  temp.avg.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.mn']
  temp.avg.tah.621 <-  d.weather.subs.621[d.weather.subs.621$date == curr.date, 'temp.mn']
  temp.avg.tah.678 <-  d.weather.subs.678[d.weather.subs.678$date == curr.date, 'temp.mn']
  temp.avg.tah.814 <-  d.weather.subs.814[d.weather.subs.814$date == curr.date, 'temp.mn']
  
  temp.max.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.max']
  temp.max.tah.621 <-  d.weather.subs.621[d.weather.subs.621$date == curr.date, 'temp.max']
  temp.max.tah.678 <-  d.weather.subs.678[d.weather.subs.678$date == curr.date, 'temp.max']
  temp.max.tah.814 <-  d.weather.subs.814[d.weather.subs.814$date == curr.date, 'temp.max']
  
  
  if (length(precip.tah.677)>0) {  d.all[r, 'TAH.677.precip'] <- precip.tah.677}
  if (length(precip.tah.621)>0) {  d.all[r, 'TAH.621.precip'] <- precip.tah.621}
  if (length(precip.tah.678)>0) {  d.all[r, 'TAH.678.precip'] <- precip.tah.678}
  if (length(precip.tah.814)>0) {  d.all[r, 'TAH.814.precip'] <- precip.tah.814}
  
  if (length(temp.min.tah.677)>0) {  d.all[r, 'TAH.677.temp.min'] <- temp.min.tah.677}
  if (length(temp.min.tah.621)>0) {  d.all[r, 'TAH.621.temp.min'] <- temp.min.tah.621}
  if (length(temp.min.tah.678)>0) {  d.all[r, 'TAH.678.temp.min'] <- temp.min.tah.678}
  if (length(temp.min.tah.814)>0) {  d.all[r, 'TAH.814.temp.min'] <- temp.min.tah.814}
  
  if (length(temp.avg.tah.677)>0) {  d.all[r, 'TAH.677.temp.avg'] <- temp.avg.tah.677}
  if (length(temp.avg.tah.621)>0) {  d.all[r, 'TAH.621.temp.avg'] <- temp.avg.tah.621}
  if (length(temp.avg.tah.678)>0) {  d.all[r, 'TAH.678.temp.avg'] <- temp.avg.tah.678}
  if (length(temp.avg.tah.814)>0) {  d.all[r, 'TAH.814.temp.avg'] <- temp.avg.tah.814}
  
  if (length(temp.max.tah.677)>0) {  d.all[r, 'TAH.677.temp.max'] <- temp.max.tah.677}
  if (length(temp.max.tah.621)>0) {  d.all[r, 'TAH.621.temp.max'] <- temp.max.tah.621}
  if (length(temp.max.tah.678)>0) {  d.all[r, 'TAH.678.temp.max'] <- temp.max.tah.678}
  if (length(temp.max.tah.814)>0) {  d.all[r, 'TAH.814.temp.max'] <- temp.max.tah.814}
  
  
}
}

d.eddy.real <- d.eddy.real[-c(2322), ]

d.all <- cbind(d.all, d.eddy.real)



  
  
 # unique(d.all$osv.biom.kg.ha.LM1)
 
}


# Computation
{
  
# Observed

d.all$gpp.osv.kg.ha <-  d.all$gpp.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 

  
d.all$reco.osv.kg.ha <-  d.all$reco.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
# convert observed eddy in mm per sq m per s to kg per ha
#d.all$NEE.obs.kg.ha <- d.all$nee.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 

# new method
d.all$NEE.obs.kg.ha <-  (-1) *d.all$reco.osv.kg.ha - d.all$gpp.osv.kg.ha   #d.all$nee.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 



d.all[   is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA
d.all[d.all$NEE.obs.kg.ha < -90 & !is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA


d.all$ET.osv <- d.all$ET.osv

# MODELLED
d.all$GPP.sim <- cv.sq.m.2.ha * d.all$co2.upt


#d.all$GPP.trees.sim <- cv.sq.m.2.ha * d.all$co2.upt.trees
#d.all$GPP.grass.sim <- cv.sq.m.2.ha * d.all$co2.upt.grass

d.all$TER.sim <- cv.sq.m.2.ha *  (d.all$maint.resp + d.all$transp.resp + d.all$growth.resp) + d.all$emis.hetero

d.all$NEE.mod <-   (-1) * d.all$TER.sim - d.all$GPP.sim 



d.all$et.sim <- d.all$et.sim.mm

}




{
d.all[,'r.a.lai.osv'] <- 0
d.all[,'r.a.lai.sim'] <- 0

d.all[,'r.a.swc.5.cm.osv'] <- 0 
d.all[,'r.a.swc.5.cm.sim'] <- 0 

d.all[,'r.a.swc.15.cm.osv'] <- 0 
d.all[,'r.a.swc.15.cm.sim'] <- 0 

d.all[,'r.a.swc.30.cm.osv'] <- 0 
d.all[,'r.a.swc.30.cm.sim'] <- 0 


d.all[,'r.a.ter.osv'] <- 0 
d.all[,'r.a.ter.sim'] <- 0 

d.all[, 'r.a.gpp.osv'] <- 0 
d.all[, 'r.a.gpp.sim'] <- 0 

d.all[,'r.a.nee.osv'] <- 0 
d.all[,'r.a.nee.sim'] <- 0 


# Compute running averages
if (r.a.switch.lai){ 
  
for (   r in (r.a.perd.lai+1):(nrow(d.all)-(r.a.perd.lai))  ){


d.all[r , 'r.a.lai.osv'] <- 0 
d.all[r , 'r.a.lai.sim'] <- 0 

for (d in r.a.perd.lai:(-r.a.perd.lai)){

d.all[r   , 'r.a.lai.osv']   <- d.all[r   , 'r.a.lai.osv'] + d.all[r - d  , 'lai.obs'] / (r.a.perd.lai * 2) 
d.all[r   , 'r.a.lai.sim']   <- d.all[r   , 'r.a.lai.sim'] + d.all[r - d  , 'lai.sim'] / (r.a.perd.lai * 2) 
}} 
  
  
} else { d.all[,'r.a.lai.osv']  <- d.all$lai.obs ; d.all[,'r.a.lai.sim'] <- d.all$lai.sim  } # LAI


if (r.a.switch.swc.5.cm){ for (   r in (r.a.perd.swc+1):(nrow(d.all)-(r.a.perd.swc))  ){
  
  
  d.all[r , 'r.a.swc.5.cm.osv'] <- 0 
  d.all[r , 'r.a.swc.5.cm.sim'] <- 0 
  
  for (d in r.a.perd.swc:(-r.a.perd.swc)){
    
    d.all[r   , 'r.a.swc.5.cm.osv']   <- d.all[r   , 'r.a.swc.5.cm.osv'] + d.all[r - d  , 'swc.3.pc.osv']  / (r.a.perd.swc * 2) 
    d.all[r   , 'r.a.swc.5.cm.sim']   <- d.all[r   , 'r.a.swc.5.cm.sim'] + d.all[r - d  , 'sw.5']/ (r.a.perd.swc * 2) 
  }}  } else { d.all[,'r.a.swc.5.cm.osv']  <- d.all$swc.3.pc.osv ; d.all[,'r.a.swc.5.cm.sim'] <- d.all$sw.5 } # SWC
  


if (r.a.switch.swc.15.cm){ for (   r in (r.a.perd.swc.15+1):(nrow(d.all)-(r.a.perd.swc.15))  ){
  
  
  d.all[r , 'r.a.swc.15.cm.osv'] <- 0 
  d.all[r , 'r.a.swc.15.cm.sim'] <- 0 
  
  for (d in r.a.perd.swc:(-r.a.perd.swc)){
    
    d.all[r   , 'r.a.swc.15.cm.osv']   <- d.all[r   , 'r.a.swc.15.cm.osv'] + d.all[r - d  , 'swc.2.pc.osv']  / (r.a.perd.swc * 2) 
    d.all[r   , 'r.a.swc.15.cm.sim']   <- d.all[r   , 'r.a.swc.15.cm.sim'] + d.all[r - d  , 'sw.15']/ (r.a.perd.swc * 2) 
  }}  } else { d.all[,'r.a.swc.15.cm.osv']  <- d.all$swc.2.pc.osv ; d.all[,'r.a.swc.15.cm.sim'] <- d.all$sw.15 } # SWC



if (r.a.switch.swc.30.cm){ for (   r in (r.a.perd.swc.30+1):(nrow(d.all)-(r.a.perd.swc.30))  ){
  
  
  d.all[r , 'r.a.swc.30.cm.osv'] <- 0 
  d.all[r , 'r.a.swc.30.cm.sim'] <- 0 
  
  for (d in r.a.perd.swc.30:(-r.a.perd.swc.30)){
    
    d.all[r   , 'r.a.swc.30.cm.osv']   <- d.all[r   , 'r.a.swc.30.cm.osv'] + d.all[r - d  , 'swc.1.pc.osv']  / (r.a.perd.swc.30 * 2) 
    d.all[r   , 'r.a.swc.30.cm.sim']   <- d.all[r   , 'r.a.swc.30.cm.sim'] + d.all[r - d  , 'sw.30']/ (r.a.perd.swc.30 * 2) 
  }}  } else { d.all[,'r.a.swc.30.cm.osv']  <- d.all$swc.1.pc.osv ; d.all[,'r.a.swc.30.cm.sim'] <- d.all$sw.30 } # SWC


if (r.a.switch.ter){for (   r in (r.a.perd.ter+1):(nrow(d.all)-(r.a.perd.ter))  ){


d.all[r , 'r.a.ter.osv'] <- 0 
d.all[r , 'r.a.ter.sim'] <- 0 

for (d in r.a.perd.ter:(- r.a.perd.ter)){

d.all[r   , 'r.a.ter.osv']   <- d.all[r   , 'r.a.ter.osv'] + d.all[r - d  , 'reco.osv.kg.ha'] / (r.a.perd.ter * 2) 
d.all[r   , 'r.a.ter.sim']   <- d.all[r   , 'r.a.ter.sim'] + d.all[r - d  , 'TER.sim'] / (r.a.perd.ter * 2)
}} } else { d.all[,'r.a.ter.osv']  <- d.all$reco.osv.kg.ha ; d.all[,'r.a.ter.sim'] <- d.all$TER.sim } #  TER

if (r.a.switch.gpp){for (   r in (r.a.perd.gpp+1):(nrow(d.all)-(r.a.perd.gpp))  ){
  
  
for (d in r.a.perd.gpp:(-r.a.perd.gpp)){

d.all[r   , 'r.a.gpp.osv']   <- d.all[r   , 'r.a.gpp.osv'] + d.all[r - d  , 'gpp.osv.kg.ha'] / (r.a.perd.gpp * 2)  
d.all[r   , 'r.a.gpp.sim']   <- d.all[r   , 'r.a.gpp.sim'] + d.all[r - d  , 'GPP.sim'] / (r.a.perd.gpp * 2)
}} } else { d.all[,'r.a.gpp.osv']  <- d.all$gpp.osv.kg.ha ; d.all[,'r.a.gpp.sim'] <- d.all$GPP.sim} #  GPP

if (r.a.switch.nee){ 

for (   r in (r.a.perd.nee+1):(nrow(d.all)-(r.a.perd.nee))  ){
  
  
  for (d in r.a.perd.nee:(-r.a.perd.nee)){
    
    d.all[r   , 'r.a.nee.osv']   <- d.all[r   , 'r.a.nee.osv'] + d.all[r - d  , 'NEE.obs.kg.ha'] / (r.a.perd.nee * 2)
    d.all[r   , 'r.a.nee.sim']   <- d.all[r   , 'r.a.nee.sim'] + d.all[r - d  , 'NEE.mod'] / (r.a.perd.nee * 2)  
  }} 
  
  
} else { d.all[,'r.a.nee.osv']  <- d.all$NEE.obs.kg.ha ; d.all[,'r.a.nee.sim'] <- d.all$NEE.mod } # NEE


d.all[ is.na(d.all$lai.obs) , 'r.a.lai.osv'] <- 0.099
d.all[ is.na(d.all$reco.osv.kg.ha) , 'r.a.ter.osv'] <- 0.099
d.all[ is.na(d.all$gpp.osv.kg.ha) , 'r.a.gpp.osv'] <- 0.099
d.all[ is.na(d.all$NEE.obs.kg.ha ) , 'r.a.nee.osv'] <- 0.099
d.all[ is.na(d.all$swc.3.pc.osv) , 'r.a.swc.5.cm.osv'] <- 0.099



summary(d.all$r.a.lai.sim)
summary(d.all$r.a.ter.sim)
summary(d.all$r.a.gpp.sim)
summary(d.all$r.a.nee.sim)
summary(d.all$r.a.swc.5.cm.sim)


summary(d.all$r.a.lai.osv)
summary(d.all$r.a.ter.osv)
summary(d.all$r.a.gpp.sim)
summary(d.all$r.a.nee.sim)
summary(d.all$r.a.swc.5.cm.sim)



d.all.n <- d.all[ 
  
  !is.na(d.all$r.a.lai.sim) 
  & !is.na(d.all$r.a.ter.sim) 
  & !is.na(d.all$r.a.gpp.sim)  
  & !is.na(d.all$r.a.nee.sim) 
  & !is.na(d.all$r.a.swc.5.cm.sim) 
  
  & d.all$r.a.lai.sim != 0
  & d.all$r.a.ter.sim != 0
  & d.all$r.a.gpp.sim != 0
  & d.all$r.a.nee.sim != 0
  & d.all$r.a.swc.5.cm.sim != 0
  
  & d.all$r.a.lai.osv != 0
  & d.all$r.a.ter.osv != 0
  & d.all$r.a.gpp.osv != 0
  & d.all$r.a.nee.osv != 0
  & d.all$r.a.swc.5.cm.osv != 0
    

    , ]
#d.all <- d.all[   !is.na(d.all$three_dra.gpp.sim) & !is.na(d.all$three_dra.gpp.osv) , ]
nrow(d.all)


} # Rolling averages


# Covid Status
{
d.all$covid <- NA

#covid.stats.pre <- 'Pre-covid'
#covid.stats.post <- 'Post-covid'


#covid.start.date <<- "2020-03-14"
#covid.end.date <<- "2022-03-07"
#covid.end.valid.date <- "2022-07-01"

d.gap.2.period.start <- "2023-01-25"
d.gap.2.period.end <- "2023-04-20"

#covid.status <- c('Pre-covid' , 'Post-covid' ,'During covid')


#d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time), 'covid'] <- covid.status[1]
#d.all[d.all$date.time >= covid.start.date & d.all$date.time <= covid.end.date   & !is.na(d.all$date.time), 'covid'] <- covid.status[3]
#d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid'] <- covid.status[2]


d.all$omit.period.2 <- FALSE
d.all[d.all$date.time > d.gap.2.period.start & d.all$date.time < d.gap.2.period.end & !is.na(d.all$date.time) , 'omit.period.2'] <- TRUE


# Periods for model validation
#d.all[ , 'covid.valid'] <- 'NA'
#d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time), 'covid.valid'] <- covid.status[1]
#d.all[d.all$date.time >  covid.end.valid.date & !is.na(d.all$date.time), 'covid.valid'] <- covid.status[2]



#nrow(d.all)
#d.all <- d.all[ !is.na(d.all$covid) , ]
#nrow(d.all)



d.all$period <- NA

period.dipole <<- 'dipole'
period.drought <<- 'drought'
period.normal <<- 'normal'
period.all <<- 'all'

d.all$period <- NA

# Dipole
d.all[d.all$date.time >= dipole.period.start &  d.all$date.time <=  dipole.period.valid.end & !is.na(d.all$date.time), 'period'] <- period.dipole

# Drought
d.all[d.all$date.time >  drought.period.start & d.all$date.time <=  drought.period.end & !is.na(d.all$date.time), 'period'] <- period.drought


# Other
d.all[ is.na(d.all$period) , 'period'] <- period.normal


unique(d.all$period)

#d.all[d.all$date.time >  dipole.period.valid.end & d.all$date.time <= drought.period.start & !is.na(d.all$date.time), 'period'] <- period.normal

#d.all[d.all$date.time >  drought.period.end & !is.na(d.all$date.time), 'period'] <- period.normal




}

# Bias detection and correction
{
  
# Systematic validation metrics
all.condition.dipole <- (d.all$period == period.dipole & d.all$variable.status == v.status.actual & !(d.all$omit.period.2) )
all.condition.drought <- (d.all$period == period.drought & d.all$variable.status == v.status.actual & !(d.all$omit.period.2))
all.condition.normal <- (d.all$period == period.normal & d.all$variable.status == v.status.actual & !(d.all$omit.period.2))
all.condition.all <- (d.all$variable.status == v.status.actual & !(d.all$omit.period.2))


# Global conditions
year.2018 <- "2018"
year.2019 <- "2019"
year.2020 <- "2020"
year.2021 <- "2021"
year.2022 <- "2022"
year.2023 <- "2023"
year.2024 <- "2024"


cond.year.2018 <- (str_detect(d.all$year.month , year.2018))
cond.year.2019 <- (str_detect(d.all$year.month , year.2019))
cond.year.2020 <- (str_detect(d.all$year.month , year.2020))
cond.year.2021 <- (str_detect(d.all$year.month , year.2021))
cond.year.2022 <- (str_detect(d.all$year.month , year.2022))
cond.year.2023 <- (str_detect(d.all$year.month , year.2023))
cond.year.2024 <- (str_detect(d.all$year.month , year.2024))

cond.year.norm.weath <- (
  
  cond.year.2019 
  | cond.year.2021
  | cond.year.2023
  | cond.year.2024
  
)

# BIAS DETECTION
# Biases


biases <- data.frame(
  osv.variable = rep(osv.metric.vars,3) 
  , sim.variable =  rep(sim.metric.vars,3) 
  , sim.variable.bc = rep(sim.metric.vars.bc,3) 
  , period =  c( rep(period.dipole,length(sim.metric.vars)) , rep(period.drought,length(sim.metric.vars))  , rep(period.normal,length(sim.metric.vars) ))
  , absolute.bias = NA
  , relative.sd = NA
)



for (r in 1:nrow(biases)) {
  
  # test: r <- 1

cur.sim.var <- biases[ r, 'sim.variable' ]
cur.period <- biases[ r, 'period' ]

osv.var <- osv.metric.vars[  which(sim.metric.vars == cur.sim.var   )  ]

if (cur.period == 'dipole'){ condition <- all.condition.dipole}
if (cur.period == 'drought'){ condition <- all.condition.drought}
if (cur.period == 'normal'){ condition <- all.condition.normal}
if (cur.period == 'all'){ condition <- all.condition.all}

# Bias is mean difference simulated minus observed
# positive bias --> simd > observed --> must reduce simulated by amount of bias
# negative bias --> simd < observed
cur.bias <- sum( na.omit(( d.all[ condition ,  cur.sim.var ] - d.all[ condition , osv.var]   )))   / sum(condition)

cur.rel.sd <-   sd(na.omit(d.all[ condition ,  cur.sim.var ])) / sd(na.omit(d.all[ condition ,  osv.var ]))

# Kobayashi and Salam method



mean.osv <- mean( na.omit(d.all[ condition , osv.var] ))
mean.sim <- mean( na.omit(d.all[ condition , cur.sim.var] ))

sd.osv <- sd( na.omit(d.all[ condition , osv.var] ))
sd.sim <- sd( na.omit(d.all[ condition , cur.sim.var] ))

Rp <- cor( d.all[ condition , osv.var]  , d.all[ condition , cur.sim.var] , method = 'pearson')

#cor( 1 , 1.5 , method = 'pearson')

# MSD = SB + SDSD + LCS


sb <- (mean.sim - mean.osv)^2 # sb
sdsd <- ( sd.sim - sd.osv)^2  # sdsd
 
lcs <- 2 * sd.sim * sd.osv * ( 1 - Rp)

msd <- sb + sdsd + lcs

biases[ r, 'squared.bias'] <- sb
biases[ r, 'sqd.diff.sd'] <- sdsd 
biases[ r, 'lcs'] <- lcs
biases[ r, 'msd'] <- msd

biases[ r, 'mean.osv'] <- mean.osv
biases[ r, 'mean.sim'] <- mean.sim

biases[ r, 'sd.osv'] <- sd.osv
biases[ r, 'sd.sim'] <- sd.sim 

biases[ r, 'absolute.bias'] <- cur.bias
biases[ r, 'relative.sd'] <- cur.rel.sd




}


# Dataframe to plot absolute biases
biases.long <- biases %>%
  pivot_longer(cols = c('squared.bias' , 'sqd.diff.sd' ,  'lcs'  ) 
               , names_to = "error.catg"
               , values_to = "error"
  )

biases.long <- as.data.frame(biases.long )

error.types <- unique( biases.long$error.catg)
biases.long[biases.long$error.catg == error.types[1], 'error.type.label'] <- 'MB'
biases.long[biases.long$error.catg == error.types[2], 'error.type.label'] <-'SDSD'
biases.long[biases.long$error.catg == error.types[3], 'error.type.label'] <- 'LCS'

biases.long$error.catg <- factor(biases.long$error.type.label , levels = c("MB" ,"SDSD" ,"LCS"  ))


periods <- unique( biases.long$period)
biases.long[biases.long$period == periods[1], 'period.label'] <- 'Dipole'
biases.long[biases.long$period  == periods[2], 'period.label'] <-'Drought'
biases.long[biases.long$period  == periods[3], 'period.label'] <- 'Normal'





bias.cond.ter <- biases.long$osv.variable == 'r.a.ter.osv' 
bias.cond.gpp <- biases.long$osv.variable == 'r.a.gpp.osv'
bias.cond.swc <- biases.long$osv.variable == 'r.a.swc.5.cm.osv' 


# BIAS CORRECTION
{
  
  
for (r in 1:nrow(d.all)){
for (v in sim.metric.vars.bc){
  

# test: v <- sim.metric.vars.bc[1]

cur.period <- d.all[r,'period'] 

raw.sim.var <- sim.metric.vars[ which(sim.metric.vars.bc == v)   ]


cur.sim.var <- d.all[ r ,raw.sim.var]

bias.df.cond <- (biases$sim.variable == raw.sim.var & biases$period == cur.period)

bias.correction.factor.mean <- biases[ bias.df.cond, 'absolute.bias']

bias.correction.factor.sd <- biases[ bias.df.cond, 'relative.sd']

mean.value <- mean( d.all[ d.all$period == cur.period  ,raw.sim.var])

#d.all[r,v] <-  1/ bias.correction.factor.sd * (d.all[ r ,raw.sim.var] - mean.value) + mean.value  - bias.correction.factor.mean 

# Kobayashi method
mean.osv <- biases[ bias.df.cond, 'mean.osv'] 
mean.sim <- biases[ bias.df.cond, 'mean.sim'] 
sd.osv <- biases[ bias.df.cond, 'sd.osv'] 
sd.sim <- biases[ bias.df.cond, 'sd.sim'] 



d.all[r,v] <- mean.osv + ( cur.sim.var - mean.sim ) * (sd.osv / sd.sim)
  
  #d.all[ r ,raw.sim.var]  - bias.correction.factor.mean 



}}
  
# Evaluate bias corrected vs. raw
mean(d.all[d.all$period == period.dipole , 'r.a.ter.sim.bc']) - mean(d.all[d.all$period == period.dipole, 'r.a.ter.sim']) 
mean(d.all[d.all$period == period.drought , 'r.a.ter.sim.bc']) - mean(d.all[d.all$period == period.drought, 'r.a.ter.sim']) 
mean(d.all[d.all$period == period.normal , 'r.a.ter.sim.bc']) - mean(d.all[d.all$period == period.normal, 'r.a.ter.sim']) 

sd(d.all[d.all$period == period.dipole , 'r.a.ter.sim.bc']) / sd(d.all[d.all$period == period.dipole, 'r.a.ter.sim']) 
sd(d.all[d.all$period == period.drought , 'r.a.ter.sim.bc']) /  sd(d.all[d.all$period == period.drought, 'r.a.ter.sim']) 
sd(d.all[d.all$period == period.normal , 'r.a.ter.sim.bc']) / sd(d.all[d.all$period == period.normal, 'r.a.ter.sim']) 

# SWC
mean(d.all[d.all$period == period.dipole , 'r.a.swc.5.cm.sim.bc']) - mean(d.all[d.all$period == period.dipole, 'r.a.swc.5.cm.sim']) 
mean(d.all[d.all$period == period.drought , 'r.a.swc.5.cm.sim.bc']) - mean(d.all[d.all$period == period.drought, 'r.a.swc.5.cm.sim']) 
mean(d.all[d.all$period == period.normal , 'r.a.swc.5.cm.sim.bc']) - mean(d.all[d.all$period == period.normal, 'r.a.swc.5.cm.sim']) 

sd(d.all[d.all$period == period.dipole , 'r.a.swc.5.cm.sim.bc']) / sd(d.all[d.all$period == period.dipole, 'r.a.swc.5.cm.sim']) 
sd(d.all[d.all$period == period.drought , 'r.a.swc.5.cm.sim.bc']) /  sd(d.all[d.all$period == period.drought, 'r.a.swc.5.cm.sim']) 
sd(d.all[d.all$period == period.normal , 'r.a.swc.5.cm.sim.bc']) / sd(d.all[d.all$period == period.normal, 'r.a.swc.5.cm.sim']) 



}

}

# Evaluation
{
  
old.eval <- function(){  

# MEAN SQUARED DEVIATION
msd.ter.osv.pre.c <- sum( na.omit((d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim']   - d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv']  )^2 ) ) /sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))



# MEAN BIASES

mb.lai.dipole <- sum( na.omit(( d.all[ all.condition.dipole , 'r.a.lai.sim'] - d.all[ all.condition.dipole , 'r.a.lai.osv']   )))   / sum( !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & all.condition.dipole)
mb.lai.drought <- sum( na.omit(( d.all[ all.condition.drought , 'r.a.lai.sim'] - d.all[ all.condition.dipole , 'r.a.lai.osv']   )))   / sum( !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & all.condition.dipole)
mb.lai.normal <- sum( na.omit(( d.all[ all.condition.normal , 'r.a.lai.sim'] - d.all[ all.condition.dipole , 'r.a.lai.osv']   )))   / sum( !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & all.condition.dipole)






mb.lai.post.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))
mb.lai.all  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))

# SQUARED BIAS
sb.ter.pre.c <-  (  mean( na.omit(d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim'] ))  - mean( na.omit(d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv'] )))^2 


# SDSD 

# TER
sd.ter.sim.pre.c <- sd( na.omit(d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim']) )
sd.ter.osv.pre.c <- sd( na.omit(d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv']) )

sdsd.ter.pre.c <- ( sd.ter.sim.pre.c - sd.ter.osv.pre.c)^2








# TER
mb.ter.pre.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
mb.ter.post.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
mb.ter.all  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))


mb.ter.2018  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2018) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2018), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2018))
mb.ter.2019  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2019) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2019), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2019))
mb.ter.2020  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2020) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2020), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2020))
mb.ter.2021  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2021) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2021), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2021))
mb.ter.2022  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2022) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2022), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2022))
mb.ter.2023  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2023) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2023), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2023))
mb.ter.2024  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2024) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2024), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2024))

mb.ter.norm.weath <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) & cond.year.norm.weath & !is.na(d.all$r.a.ter.osv) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2) &  cond.year.norm.weath & !is.na(d.all$r.a.ter.osv) , 'r.a.ter.osv']   )))   / nrow(d.all[d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &  cond.year.norm.weath,])


# GPP
mb.gpp.pre.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2))
mb.gpp.post.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2))
mb.gpp.all  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2))

                     
mb.gpp.2018  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2018) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2018), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2018))
mb.gpp.2019  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2019) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2019), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2019))
mb.gpp.2020  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2020) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2020), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2020))
mb.gpp.2021  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2021) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2021), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2021))
mb.gpp.2022  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2022) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2022), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2022))
mb.gpp.2023  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2023) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2023), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2023))
mb.gpp.2024  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2024) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2024), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2024))

mb.gpp.norm.weath <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) & cond.year.norm.weath & !is.na(d.all$r.a.gpp.osv) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2) &  cond.year.norm.weath & !is.na(d.all$r.a.gpp.osv) , 'r.a.gpp.osv']   )))   / nrow(d.all[d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &  cond.year.norm.weath,])





# RMSE
rmse.lai.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))
rmse.lai.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.lai.osv) & !(d.all$omit.period.2))
rmse.lai.all <- sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2), 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2), 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.lai.osv)& !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))

rmse.swc.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2) , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.5.cm.osv) & !is.na(d.all$r.a.swc.5.cm.sim) & !(d.all$omit.period.2))
rmse.swc.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.swc.5.cm.osv)& !is.na(d.all$r.a.swc.5.cm.sim) & !(d.all$omit.period.2))
rmse.swc.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual   & !(d.all$omit.period.2), 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.swc.5.cm.osv)& !is.na(d.all$r.a.swc.5.cm.sim) & !(d.all$omit.period.2))


rmse.ter.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
rmse.ter.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim)& !(d.all$omit.period.2))
rmse.ter.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))

rmse.ter.2018 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2018) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2018) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2018) & !(d.all$omit.period.2))
rmse.ter.2019 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2019) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2019) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2019) & !(d.all$omit.period.2))
rmse.ter.2020 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2020) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2020) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2020) & !(d.all$omit.period.2))
rmse.ter.2021 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2021) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2021) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2021) & !(d.all$omit.period.2))
rmse.ter.2022 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2022) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2022) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2022) & !(d.all$omit.period.2))
rmse.ter.2023 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2023) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2023) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2023) & !(d.all$omit.period.2))
rmse.ter.2024 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2))


rmse.gpp.pre.c <-  sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim))
rmse.gpp.post.c <-    sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) )
rmse.gpp.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2), 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2) , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.gpp.osv)& !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2))
#rmse.gpp.2024 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2), 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2024) &  !(d.all$omit.period.2)  , 'r.a.gpp.osv']   )))   / sum(!(d.all$omit.period.2)d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.gpp.osv)& !is.na(d.all$r.a.gpp.sim) & str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2))


rmse.nee.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.nee.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.nee.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.nee.osv) & !is.na(d.all$r.a.nee.sim) & !(d.all$omit.period.2))
rmse.nee.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.nee.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.nee.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.nee.osv)& !is.na(d.all$r.a.nee.sim) & !(d.all$omit.period.2))
rmse.nee.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual , 'r.a.nee.sim'] - d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.nee.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.nee.osv)& !is.na(d.all$r.a.nee.sim) & !(d.all$omit.period.2))



# NRMSE
nrmse.lai.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.lai.osv'] ))) * rmse.lai.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim))
nrmse.lai.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.lai.osv'] ))) *  rmse.lai.post.c
nrmse.lai.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2), 'r.a.lai.osv'] ))) *  rmse.lai.all


nrmse.swc.5.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.5.cm.osv'] ))) * rmse.swc.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.5.cm.osv) & !is.na(d.all$r.a.swc.5.cm.sim))
nrmse.swc.5.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.swc.5.cm.osv'] ))) *  rmse.swc.post.c
nrmse.swc.5.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.swc.5.cm.osv'] ))) *  rmse.swc.all


nrmse.swc.15.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.15.cm.osv'] ))) * rmse.swc.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.5.cm.osv) & !is.na(d.all$r.a.swc.5.cm.sim))
nrmse.swc.15.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.swc.15.cm.osv'] ))) *  rmse.swc.post.c
nrmse.swc.15.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.swc.15.cm.osv'] ))) *  rmse.swc.all


nrmse.swc.30.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.30.cm.osv'] ))) * rmse.swc.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.5.cm.osv) & !is.na(d.all$r.a.swc.5.cm.sim))
nrmse.swc.30.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.swc.30.cm.osv'] ))) *  rmse.swc.post.c
nrmse.swc.30.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.swc.30.cm.osv'] ))) *  rmse.swc.all


nrmse.ter.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.ter.osv'] ))) * rmse.ter.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim))
nrmse.ter.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post , 'r.a.ter.osv'] ))) *  rmse.ter.post.c
nrmse.ter.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.ter.osv'] ))) *  rmse.ter.all


nrmse.ter.2018  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2018)  , 'r.a.ter.osv'] ))) *  rmse.ter.2018
nrmse.ter.2019 <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2019)  , 'r.a.ter.osv'] ))) *  rmse.ter.2019
nrmse.ter.2020  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2020)  , 'r.a.ter.osv'] ))) *  rmse.ter.2020
nrmse.ter.2021  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2021)  , 'r.a.ter.osv'] ))) *  rmse.ter.2021
nrmse.ter.2022  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2022)  , 'r.a.ter.osv'] ))) *  rmse.ter.2022
nrmse.ter.2023  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2023)  , 'r.a.ter.osv'] ))) *  rmse.ter.2023
nrmse.ter.2024  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2024)  , 'r.a.ter.osv'] ))) *  rmse.ter.2024


nrmse.gpp.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid == covid.stats.pre & !is.na(d.all$r.a.gpp.osv) , 'r.a.gpp.osv'] ))) * rmse.gpp.pre.c 
nrmse.gpp.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post &  !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) , 'r.a.gpp.osv'] ))) *  rmse.gpp.post.c
nrmse.gpp.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv)  , 'r.a.gpp.osv'] ))) *  rmse.gpp.all
#nrmse.gpp.2024  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) &   str_detect(d.all$year.month, year.2024)  , 'r.a.gpp.osv'] ))) *  rmse.gpp.2024


nrmse.nee.pre.c <-  100* (1 / abs(mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.nee.osv'] )))) * rmse.nee.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.nee.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.nee.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.nee.osv) & !is.na(d.all$r.a.nee.sim))
nrmse.nee.post.c <-  100* (1 / abs(mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.nee.osv'] )))) *  rmse.nee.post.c
nrmse.nee.all  <-  100* (1 / abs(mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.nee.osv'] ))) )*  rmse.nee.all

}


metrics <- data.frame(
  osv.variable = rep(osv.metric.vars,4) 
  , sim.variable =  rep(sim.metric.vars,4) 
  , sim.variable.bc = rep(sim.metric.vars.bc,4) 
  , period =  c( rep(period.dipole,length(sim.metric.vars)) , rep(period.drought,length(sim.metric.vars))  , rep(period.normal,length(sim.metric.vars) ) , rep(period.all,length(sim.metric.vars) ) )
  
  
  ,r2 = NA
  ,rmse = NA
  ,nrmse = NA
  
  , valid.text = NA
)

d.all[, 'period.status'] <- NA


for (r in 1:nrow(metrics)){
  
  # r <-1 
  
  osv.var <- metrics[r,'osv.variable']
  sim.var  <- metrics[r,'sim.variable']
  sim.var.bc  <- metrics[r,'sim.variable.bc']
  cur.period <- metrics[r,'period']
  
  if (cur.period == period.dipole) {condition <- all.condition.dipole}
  if (cur.period == period.drought) {condition <- all.condition.drought}
  if (cur.period == period.normal) {condition <- all.condition.normal}
  if (cur.period == period.all) {condition <- all.condition.all}
  
  no.na.condition <-   !is.na(d.all[,osv.var])
  
  metrics[r , 'r2'] <- cor(  d.all[condition & no.na.condition  , osv.var ] , d.all[ condition & no.na.condition, sim.var]   , method = cor.type  )
  metrics[r , 'r2.bc'] <- cor(  d.all[condition & no.na.condition , osv.var ] , d.all[ condition & no.na.condition, sim.var.bc ]   , method = cor.type  )
  
  metrics[r , 'r2'] <- round(  metrics[r , 'r2'] , 2)
  metrics[r , 'r2.bc'] <- round(  metrics[r , 'r2.bc'] , 2)
  
  metrics[r , 'rmse'] <-   sum( na.omit(abs( d.all[ condition , osv.var ] - d.all[ condition ,  sim.var]   )))   / sum(condition & !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var]))
  metrics[r , 'rmse.bc'] <-   sum( na.omit(abs( d.all[ condition , osv.var ] - d.all[ condition ,  sim.var.bc ]   )))   / sum(condition & !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var.bc ]))
  
  metrics[r , 'nrmse'] <-  100* (1 / abs(mean(na.omit(d.all[ condition , osv.var] ))) )*    metrics[r , 'rmse'] 
  metrics[r , 'nrmse.bc'] <-  100* (1 / abs(mean(na.omit(d.all[ condition , osv.var] ))) )*    metrics[r , 'rmse.bc'] 
  
  metrics[r , 'rmse'] <-   round(  metrics[r , 'rmse'] , 1)
  metrics[r , 'rmse.bc'] <-   round(  metrics[r , 'rmse.bc'] , 1)
  
  metrics[r , 'nrmse'] <-   round(  metrics[r , 'nrmse'] , 1)
  metrics[r , 'nrmse.bc'] <-   round(  metrics[r , 'nrmse.bc'] , 1)
  
  period.status <- str_c( 'period.' , var [r] )
  
  if (cur.period == period.dipole){cur.period.label <- period.label[1] }
  if (cur.period == period.drought){cur.period.label <- period.label[2] }
  if (cur.period == period.normal){cur.period.label <- period.label[3] }
  
  
  valid.text <- str_c(  cur.period.label,': r = ',  metrics[r , 'r2'] , ' (' , metrics[r , 'r2.bc'] , ')' , ', RMSE = ', metrics[r , 'rmse'] , ' (' , metrics[r , 'rmse.bc'] , ')' , ', nRMSE = ', metrics[r , 'nrmse'] , ' (' , metrics[r , 'nrmse.bc'] , ') ' , '%')
  
  
  d.all[d.all$period == cur.period, 'period.status'] <- valid.text
  
  metrics[r , 'valid.text'] <-  valid.text
  
}

# Biomass
mean.biomass.grass.1.pre.c <- mean(  d.all[ d.all$covid == covid.stats.pre &  !is.na(d.all$ag.biom.grass.1.kg.ha) , 'ag.biom.grass.1.kg.ha']  )
mean.biomass.grass.2.pre.c <- mean(  d.all[ d.all$covid == covid.stats.pre &  !is.na(d.all$ag.biom.grass.2.kg.ha) , 'ag.biom.grass.2.kg.ha']  )

mean.biomass.grass.1.post.c <- mean(  d.all[ d.all$covid == covid.stats.post &  !is.na(d.all$ag.biom.grass.1.kg.ha) , 'ag.biom.grass.1.kg.ha']  )
mean.biomass.grass.2.post.c <- mean(  d.all[ d.all$covid == covid.stats.post &  !is.na(d.all$ag.biom.grass.2.kg.ha) , 'ag.biom.grass.2.kg.ha']  )


# Round 
mean.biomass.grass.1.pre.c <- round(mean.biomass.grass.1.pre.c ,0)
mean.biomass.grass.2.pre.c <- round(mean.biomass.grass.2.pre.c ,0)

mean.biomass.grass.1.post.c <- round(mean.biomass.grass.1.post.c ,0)
mean.biomass.grass.2.post.c <- round(mean.biomass.grass.2.post.c ,0)


}

# COVID STATUS
x <- function(){
  
  
  d.all$covid.swc <- NA
  d.all$covid.gpp <- NA
  d.all$covid.ter <- NA
  d.all$covid.nee <- NA
  d.all$covid.et <- NA
  d.all$covid.lai <- NA
  d.all$covid.climate <- NA
  
  d.all$covid.biomass.simd <- NA
  
  string.corltn <- bquote('(r[s] ')
  
  bquote(covid.status[1] ~ "\n" ~ bar(x))
  
  #bquote({R^2} [1:1] == .(rsq))
  
  #covid.status[2] , '\nr ' , 
  
  d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time) , 'covid.gpp'] <- covid.status[1] 
  d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.gpp'] <- covid.status[2] 
  
  d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time), 'covid.ter'] <- covid.status[1] 
  d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.ter'] <- covid.status[2] 
  
  d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time), 'covid.nee'] <- covid.status[1] 
  d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.nee'] <- covid.status[2] 
  
  d.all[d.all$date.time < covid.start.date & !is.na(d.all$date.time) , 'covid.swc'] <- covid.status[1] 
  d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.swc'] <-covid.status[2] 
  
  d.all[d.all$date.time < covid.start.date & !is.na(d.all$date.time) , 'covid.lai'] <- covid.status[1] 
  d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.lai'] <- covid.status[2] 
  
  d.all[d.all$date.time < covid.start.date & !is.na(d.all$date.time) , 'covid.biomass.simd'] <- covid.status[1] 
  d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time) , 'covid.biomass.simd'] <- covid.status[2] 
  
  d.all[d.all$date.time < covid.start.date & !is.na(d.all$date.time) , 'covid.climate'] <- covid.status[1] 
  d.all[d.all$date.time > covid.start.date & !is.na(d.all$date.time) , 'covid.climate'] <- covid.status[2] 
  
  
  unq.covid.gpp <- unique(d.all$covid.gpp)
  d.all$covid.gpp <- factor(  d.all$covid.gpp , levels = unq.covid.gpp)
  
  unq.covid.ter <- unique(d.all$covid.ter)
  d.all$covid.ter <- factor(  d.all$covid.ter , levels = unq.covid.ter)
  
  unq.covid.nee <- unique(d.all$covid.nee)
  d.all$covid.nee <- factor(  d.all$covid.nee , levels = unq.covid.nee)
  
  unq.covid.swc <- unique(d.all$covid.swc)
  d.all$covid.swc <- factor(  d.all$covid.swc , levels = unq.covid.swc)
  
  unq.covid.et <- unique(d.all$covid.et)
  d.all$covid.et <- factor(  d.all$covid.et , levels = unq.covid.et)
  
  unq.covid.lai <- unique(d.all$covid.lai)
  d.all$covid.lai <- factor(  d.all$covid.lai , levels = unq.covid.lai)
  
  unq.covid.climate <- unique(d.all$covid.climate )
  d.all$covid.climate  <- factor(  d.all$covid.climate  , levels = unq.covid.climate )
  
  
  unq.covid.biomass.simd <- unique(d.all$covid.biomass.simd)
  d.all$covid.biomass.simd <- factor(  d.all$covid.biomass.simd , levels = unq.covid.biomass.simd)
  
  
  
  

  
}


print(paste('nRMSE for SWC 5  is' , metrics[metrics$osv.variable == 'r.a.swc.5.cm.osv' , 'nrmse'] ))
print(paste('nRMSE for SWC 15  is' , metrics[metrics$osv.variable == 'r.a.swc.15.cm.osv' , 'nrmse'] ))
print(paste('nRMSE for SWC 30  is' , metrics[metrics$osv.variable == 'r.a.swc.30.cm.osv' , 'nrmse'] ))
print(paste('nRMSE for TER is' , metrics[metrics$osv.variable == "r.a.ter.osv" , 'nrmse'] ))
print(paste('nRMSE for GPP is' ,  metrics[metrics$osv.variable == "r.a.gpp.osv" , 'nrmse'] ))
print(paste('nRMSE for NEE is' ,  metrics[metrics$osv.variable == "r.a.nee.osv" , 'nrmse'] ))



d.all <- as.data.frame(d.all)
d.all <- d.all[,!duplicated(colnames(d.all))]

source('gg.params.R')
source('gg.seasons.R')

d.all.plot.conditions <- (!(d.all$omit.period.2)  & d.all$date >= start.date.cald  & d.all$date <= end.date.cald   )




}  # RUN ALL



# PLOT series

{
  
  
# biomass 
gg.bio.decomp <-  gg.biom( 

FALSE 
, FALSE

, TRUE
, FALSE

, FALSE
, FALSE

, FALSE
, FALSE

)

gg.bio.decomp 
  
# TER
gg.valid.ter.o <- gg.ter.labl


gg.ter.no.labl <- gen.valid.plot( 'r.a.ter.osv'  , 'r.a.ter.sim'  , 'r.a.ter.sim.bc'   , gg.valid.ter.y.ax.lab , global.valid.ter.y.cord.high , global.valid.ter.y.cord.mid  , global.valid.ter.y.cord.bottm ,'no.label' , FALSE)
gg.ter.labl <- gen.valid.plot( 'r.a.ter.osv'  , 'r.a.ter.sim'  , 'r.a.ter.sim.bc'   , gg.valid.ter.y.ax.lab , global.valid.ter.y.cord.high , global.valid.ter.y.cord.mid  , global.valid.ter.y.cord.bottm ,'label' , FALSE)


gg.kosalam.ter <- gen.gg.kaba('ter')
  
  



# GPP
gg.valid.gpp.o <- gg.ter.labl


gg.gpp.no.labl <- gen.valid.plot( 'r.a.gpp.osv'  , 'r.a.gpp.sim'  , 'r.a.gpp.sim.bc'   , gg.valid.gpp.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , FALSE)
gg.gpp.labl <- gen.valid.plot( 'r.a.gpp.osv'  , 'r.a.gpp.sim'  , 'r.a.gpp.sim.bc'   , gg.valid.gpp.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label' , FALSE)


gg.kosalam.gpp <- gen.gg.kaba('gpp')


# NEE
gg.valid.nee.o <- gg.nee.labl


gg.nee.no.labl <- gen.valid.plot( 'r.a.nee.osv'  , 'r.a.nee.sim'  , 'r.a.nee.sim.bc'   , gg.valid.nee.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE)
gg.nee.labl <- gen.valid.plot( 'r.a.nee.osv'  , 'r.a.nee.sim'  , 'r.a.nee.sim.bc'   , gg.valid.nee.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)


# SWC
gg.valid.swc.o <- gg.swc.no.labl


gg.swc.5.cm.no.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   ,  gg.valid.swc.5.cm.y.ax.lab, global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , FALSE)
gg.swc.5.cm.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   ,  gg.valid.swc.5.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)


# 15 cm layer
gg.swc.15.cm.no.labl <- gen.valid.plot( 'r.a.swc.15.cm.osv'  , 'r.a.swc.15.cm.sim'  , 'r.a.swc.15.cm.sim.bc'   , gg.valid.swc.15.cm.y.ax.lab, global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , FALSE)
gg.swc.15.cm.labl <- gen.valid.plot( 'r.a.swc.15.cm.osv'  , 'r.a.swc.15.cm.sim'  , 'r.a.swc.15.cm.sim.bc'   ,  gg.valid.swc.15.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)

# 30 cm layer
gg.swc.30.cm.no.labl <- gen.valid.plot( 'r.a.swc.30.cm.osv'  , 'r.a.swc.30.cm.sim'  , 'r.a.swc.30.cm.sim.bc'   ,   gg.valid.swc.30.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , FALSE)
gg.swc.30.cm.labl <- gen.valid.plot( 'r.a.swc.30.cm.osv'  , 'r.a.swc.30.cm.sim'  , 'r.a.swc.30.cm.sim.bc'   ,  gg.valid.swc.30.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)




gg.kosalam.swc <- gen.gg.kaba('swc')


# LAI
gg.valid.lai.o <- gg.swc.no.labl


gg.lai.no.labl <- gen.valid.plot( 'r.a.lai.osv'  , 'r.a.lai.sim'  , 'r.a.lai.sim.bc'   , gg.valid.swc.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE)
gg.lai.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   , gg.valid.swc.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label')


# Evapotranspiration
{
gg.valid.et <- gg.theme %>% + geom_line( aes(x = date.time, y = ET.osv , color = p.swc.osv.label) 
                                   , linewidth = p.ln.width * 0.6
                                   , color = p.nee.color.1
                                   
) + geom_line( aes(x = date.time, y = et.sim , color = p.swc.sim.label) 
               , linewidth = p.ln.width * 0.6 
               , color = p.nee.color.2
               
)   +
    
    
  ylab(gg.valid.et.y.lab) +
  facet_grid( ~ covid.et  , scales = 'free_x' , space = 'free') +
  theme(
    legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
    axis.title.x = element_blank() ,  
    legend.title = element_blank(),
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() , 
    axis.text.x = element_text(angle = 270 ) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
    ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  )  

# gg.valid.et
}


# Climate
{
  
  cor(d.all$precip.osv , d.all$TAH.621.precip , use = "complete.obs")
  cor(d.all$precip.osv , d.all$TAH.677.precip , use = "complete.obs")
  cor(d.all$precip.osv , d.all$TAH.678.precip , use = "complete.obs")
  cor(d.all$precip.osv , d.all$TAH.814.precip , use = "complete.obs")
  
  cor(d.all$temp.avg.osv , d.all$TAH.621.temp.avg , use = "complete.obs")
  cor(d.all$temp.avg.osv , d.all$TAH.677.temp.avg , use = "complete.obs")
  cor(d.all$temp.avg.osv , d.all$TAH.678.temp.avg , use = "complete.obs")
  cor(d.all$temp.avg.osv , d.all$TAH.814.temp.avg , use = "complete.obs")
  
  cor(d.all$temp.max.osv , d.all$TAH.621.temp.max , use = "complete.obs")
  cor(d.all$temp.max.osv , d.all$TAH.677.temp.max , use = "complete.obs")
  cor(d.all$temp.max.osv , d.all$TAH.678.temp.max , use = "complete.obs")
  cor(d.all$temp.max.osv , d.all$TAH.814.temp.max , use = "complete.obs")
  
  cor(d.all$temp.min.osv , d.all$TAH.621.temp.min , use = "complete.obs")
  cor(d.all$temp.min.osv , d.all$TAH.677.temp.min , use = "complete.obs")
  cor(d.all$temp.min.osv , d.all$TAH.678.temp.min , use = "complete.obs")
  cor(d.all$temp.min.osv , d.all$TAH.814.temp.min , use = "complete.obs")
  
  
  
  
  
  
  
gg.rain <-  gg.theme  %>%   +  
    geom_point(  data = d.all[,  ] ,
               aes( x =date.time 
                    , y = TAH.621.precip)
               , color = 'red' 
               ) +
    geom_point(  data = d.all[,  ] ,
                aes( x =date.time 
                     , y = TAH.677.precip)
                , color = 'green' 
    ) +
    geom_point(  data = d.all[,  ] ,
                aes( x =date.time 
                     , y = TAH.678.precip)
                , color = 'blue' 
    ) +
    geom_point(  data = d.all[,  ] ,
                aes( x =date.time 
                     , y = TAH.814.precip)
                , color = 'pink' 
    )
  

sec <- ggh4x::help_secondary(
  name = "",
  primary = c(10, 35), secondary = c(0, 200),
)

  
               
gg.rain <- gg.theme  %>%   +  
geom_bar(   data= d.all[ d.all$covid %in% covid.status[c(1,2)] &  !is.na(d.all$covid.climate) & ( d.all$date > covid.end.date | d.all$date < covid.start.date),  ] ,
aes( x = date.time 
, y = precip.osv
  
)  ,
, stat = 'identity'  
, width = p.br.wdth
, color = 'blue'
, alpha = p.br.alpha ) +
  ylab('Precipitation (mm/d)')


gg.rain

}

# Climate other
{
  

gg.climate.all <- ggplot( 
   d.eddy.oc
  ,   aes(x = date.time )  
) +  
  geom_line( aes(x = date, y = temp.avg.osv  ) 
             , linewidth = gg.temp.ln.width 

             , color=  'red' 
  ) + 
    
    geom_line( aes(x = date, y = rh.osv   ) 
               , linewidth = gg.temp.ln.width 
               , color=  'darkorange'
    ) +
    
    geom_line( aes(x = date, y = ws.osv  ) 
               , linewidth = gg.temp.ln.width 
               , color=  'grey'
    ) +
    
    
    geom_bar(  #data = d.all[,  ] ,
               aes( x =date
                    , y = precip.osv 
               )
               , stat = 'identity'  
               , width = p.br.wdth
               , color = p.br.clr 
               , alpha = p.br.alpha 
  )  
  
  gg.climate.all
}


# Plot 1
gg.validate.1.labels <- c('a' ,'b' , 'c' , 'd' )

gg.ter.plot.no.labl <-  gg.remv.x.lab( gg.ter.no.labl )
gg.gpp.plot.no.labl <-  gg.remv.x.lab( gg.gpp.no.labl )
gg.nee.plot.no.labl <-  gg.remv.x.lab( gg.nee.no.labl  )


gg.validate.1 <- ggarrange(
  
  
   gg.ter.plot.no.labl
  , gg.gpp.plot.no.labl
  , gg.nee.plot.no.labl
  , gg.rain
     
  , nrow = 4
  , labels = gg.validate.1.labels 
  , heights = c(1,1,1,0.75)
  , label.x = .01575
  , label.y = 0.9175
)

gg.validate.1 



gg.valid.1.dpi  <-  1000

gg.valid.1.width <- 6.45
gg.valid.1.height  <- 10.35
filename.gg.validate.1 = 'Figures.out/gg.validate.1.jpg'

ggsave(filename = filename.gg.validate.1 ,  gg.validate.1 , width = gg.valid.1.width, height = gg.valid.1.height , dpi = gg.valid.1.dpi  )


# Plot 2
gg.validate.2.labels <- c('a' ,'b' ,'c' )

gg.valid.2.heights <- c(1,1,1.275)

gg.validate.2 <- ggarrange(
  
 # gg.valid.lai
     gg.valid.swc
  ,  gg.bio.decomp
  , gg.climate
  
  , labels = gg.validate.2.labels 
  
  , heights = gg.valid.2.heights 
  
  , nrow = 3
  , label.x = .9575
  , label.y = c(0.9175, 0.9175, 0.97)
)

gg.validate.2 


gg.valid.dpi  <-  2500

gg.valid.2.width <- 7.5
gg.valid.2.height  <- 8
filename.gg.validate.2 = 'Figures.out/gg.validate.2.jpg'

ggsave(filename =    filename.gg.validate.2 ,  gg.validate.2 , width = gg.valid.2.width, height = gg.valid.2.height , dpi = gg.valid.dpi  )


}







p.et <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
) + 
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2019.rn.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2019.rn.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill =  p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.rn.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.rn.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_bar(  data = d.all[,  ] ,
             aes( x = date.time
                  , y = precip 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  # - Observed
  geom_line(  aes(x = date.time
                  , y = et.real.mm
                  , colour= 'Observed'
  )  
  ,linewidth = p.ln.width
  ) +  
  # - Modelled
  geom_line( aes(x = date.time
                 , y = et.sim.mm
                 , colour= 'Modelled'
  ) 
  , linewidth = p.ln.width 
  ) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  scale_y_continuous(
    p.et.y.ax.lab , 
    sec.axis = sec_axis(~   . , name = p.precip.sec.ax.tit )
  ) +
  scale_colour_manual(
    name = ''
    , values =   c( 
      'Observed' = p.ln.colr.obsv
      ,'Modelled'= p.ln.colr.mod
    ) 
    , breaks = c(
      'Observed'
      ,  'Modelled'
    )) +
  theme(
    legend.position = "bottom" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.y.ax.lab) #+ 
# annotate("text"
#  , x =   nee.date.x.ax.lab   , 
# , y =  p.lab.nee.r2.y.crd
# , parse = TRUE 
#, label = p.lab.nee.r2 
#  , size =p.lab.nee.tx.fs
#  , hjust = 0
# )



func <- function(){

p.swc <- ggplot( d.all[ d.all$swc.1 > 0 ,  ] ,   aes(x = date.time)  
) +  geom_rect(
  aes(xmin = as.Date( p.ssn.x.ranges.2019.rn.2.min , format = '%Y-%m-%d'),
      xmax = as.Date(p.ssn.x.ranges.2019.rn.2.max , format = '%Y-%m-%d'),
      ymin = -Inf,
      ymax = Inf), alpha = p.ssn.bg.alpha , fill =  p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.rn.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.rn.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_line( aes(x = date.time, y = swc.1   , color= p.swc.osv.label ) 
             , linewidth = p.ln.width 
             
  ) +
  geom_line( aes(x = date.time, y = sw.10  , color= p.swc.sim.label ) 
             ,
             , linewidth = p.ln.width 
             
  ) +  
  geom_bar(  data = d.all[,  ] ,
             aes( x = date.time
                  , y = precip
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  scale_y_continuous(
    p.swc.y.ax.lab, 
    sec.axis = sec_axis(~ . * 1, name = p.precip.sec.ax.tit )
  ) +
  #geom_line(    aes(x = date.time, y = nee)       ) +
  #scale_x_discrete(aes('day.cnt') , day.cnt , labels = d.all$date.time  ) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Observed" = p.nee.color.1
      , "Simulated"  = p.nee.color.2
    ) 
    , breaks = c(
      p.swc.osv.label
      ,  p.swc.sim.label
    )) +
  theme(
    legend.position = "bottom" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.swc.y.ax.lab)



p.lai <- ggplot( d.all[ !is.na(d.all$lai.real != -99.99 ),  ] ,   aes(x = date.time )  
) + 
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2019.rn.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2019.rn.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill =  p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.rn.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.rn.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_bar(  data = d.all[,  ] ,
             aes( x = date.time
                  , y = precip 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  # - Observed
  geom_line( data = d.all[ !is.na(d.all$lai.real != -99.99 ),  ] , 
             aes(x = date.time
                  , y = lai.real
                  , colour= 'Observed'
  )  
  ,linewidth = p.ln.width
  ) +  
  # - Modelled
  geom_line( aes(x = date.time
                 , y =   lai.sim
                 , colour= 'Modelled'
  ) 
  , linewidth = p.ln.width 
  ) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  scale_y_continuous(
    p.et.y.ax.lab , 
    sec.axis = sec_axis(~   . , name = p.precip.sec.ax.tit )
  ) +
  scale_colour_manual(
    name = ''
    , values =   c( 
      'Observed' = p.ln.colr.obsv
      ,'Modelled'= p.ln.colr.mod
    ) 
    , breaks = c(
      'Observed'
      ,  'Modelled'
    )) +
  theme(
    legend.position = "bottom" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.y.ax.lab) #+ 
# annotate("text"
#  , x =   nee.date.x.ax.lab   , 
# , y =  p.lab.nee.r2.y.crd
# , parse = TRUE 
#, label = p.lab.nee.r2 
#  , size =p.lab.nee.tx.fs
#  , hjust = 0
# )


p.lai
p.et
p.swc
p.nee 



filename <- 'kapiti.validate.nee.png'

p.width <- 600
p.height  <- 300
plot.dpi <- 1000

ggsave(filename =    filename ,  p.nee, width = 5 , height =4  , dpi = plot.dpi )


filename.swc <- 'kapiti.validate.swc.png'

p.width <- 600
p.height <- 300
plot.dpi <- 1000

ggsave(filename =    filename.swc ,  p.swc , width = 5 , height =4  , dpi = plot.dpi )


}


p.nee
p.swc


mean.temp.all <- mean(d.all[  ,'temp.avg.osv' ])

mean(d.all[  str_detect(d.all$year.month, year.2018) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2019) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2020) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2021) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2022) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2023) ,'temp.avg.osv' ]) /mean.temp.all


mean.precip.all <- mean(d.all[  ,'precip.osv' ]) 

mean(d.all[  str_detect(d.all$year.month, year.2018) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2019) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2020) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2021) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2022) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2023) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2024) ,'precip.osv' ]) /mean.precip.all


