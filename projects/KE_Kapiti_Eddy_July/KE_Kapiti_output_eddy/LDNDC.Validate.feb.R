

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())


save.image('L.DNDC.Validate.RData')
load('L.DNDC.Validate.RData')

# Global parameters
{
  
# Running averages --  switches
r.a.switch.lai <- FALSE
r.a.switch.et <- FALSE
r.a.switch.nee <- FALSE
r.a.switch.swc <<- FALSE
r.a.switch.gpp <<- TRUE
r.a.switch.ter <<- TRUE
  
# periods

r.a.perd.lai <- 1
r.a.perd.swc <- 1
r.a.perd.ter <- 5
r.a.perd.gpp <- 5
r.a.perd.nee <- 1
  
  
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




}

  
library("languageR") ; library('readxl') ; library('readxl') ; library('ggplot2') ; library(stringr) ;library(stringi) ; library('chron') ; library('lubridate') ; library('ggpubr')
  
  



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


all.grass.species <- c(  "ANGA" , "PERG" , "PECL" , 'CEBI' , 'GRASS' , 'SAFF')
all.tree.species <- c(  "BUAF" , "TAPAJOS" , "ACTO" )

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



if (   length(unique(d.physio$species )  ) > 1 ) {
d.physio$ag.biom.grass.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.grass[1]     ,   'ag.biom.kg.m2' ] #+ d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'ag.biom.kg.m2' ] +  d.physio.all[ d.physio.all$species == unique.species.grass[3]     ,   'ag.biom.kg.m2' ]

d.physio$ag.biom.grass.1.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'ag.biom.kg.m2' ] #+ d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'ag.biom.kg.m2' ] +  d.physio.all[ d.physio.all$species == unique.species.grass[3]     ,   'ag.biom.kg.m2' ]
d.physio$ag.biom.grass.2.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.grass[1]     ,   'ag.biom.kg.m2' ] #+ d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'ag.biom.kg.m2' ] +  d.physio.all[ d.physio.all$species == unique.species.grass[3]     ,   'ag.biom.kg.m2' ]

d.physio$bg.biom.grass.1.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'bg.biom.kg.m2' ] #+ d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'ag.biom.kg.m2' ] +  d.physio.all[ d.physio.all$species == unique.species.grass[3]     ,   'ag.biom.kg.m2' ]
d.physio$bg.biom.grass.2.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.grass[1]     ,   'bg.biom.kg.m2' ] #+ d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'ag.biom.kg.m2' ] +  d.physio.all[ d.physio.all$species == unique.species.grass[3]     ,   'ag.biom.kg.m2' ]


d.physio$bg.biom.grass.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.grass[1]     ,   'bg.biom.kg.m2' ] #+ d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'bg.biom.kg.m2' ] + d.physio.all[ d.physio.all$species == unique.species.grass[3]     ,   'bg.biom.kg.m2' ]

d.physio$ag.biom.trees.kg.m2 <- d.physio.all[   d.physio.all$species == unique.species.trees[1]   ,   'ag.biom.kg.m2' ] #+ d.physio.all[ d.physio.all$species == unique.species.trees[2]     ,   'ag.biom.kg.m2' ] 
d.physio$bg.biom.trees.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.trees[1]    ,   'bg.biom.kg.m2' ] #+ d.physio.all[ d.physio.all$species == unique.species.trees[2]     ,   'bg.biom.kg.m2' ] 

d.physio$lai.sim.grass <- d.physio.all[ d.physio.all$species == unique.species.grass[1]   ,   'lai.sim' ] #+ d.physio.all[ d.physio.all$species == unique.species.grass[2]   ,   'lai.sim' ] + d.physio.all[ d.physio.all$species == unique.species.grass[3]   ,   'lai.sim' ]
d.physio$lai.sim.trees <-  d.physio.all[ d.physio.all$species == unique.species.trees[1]    ,   'lai.sim' ]# + d.physio.all[ d.physio.all$species == unique.species.trees[2]    ,   'lai.sim' ]

d.physio$co2.upt.grass <- d.physio.all[ d.physio.all$species == unique.species.grass[1]   ,   'co2.upt' ] #+ d.physio.all[ d.physio.all$species == unique.species.grass[2]   ,   'co2.upt' ] + d.physio.all[ d.physio.all$species == unique.species.grass[3]   ,   'co2.upt' ]
d.physio$co2.upt.trees <-  d.physio.all[ d.physio.all$species ==  unique.species.trees[1]   ,   'co2.upt' ] #+ d.physio.all[ d.physio.all$species ==  unique.species.trees[2]   ,   'co2.upt' ] 

# Convert to ha values
d.physio$ag.biom.trees.kg.ha <- d.physio$ag.biom.trees.kg.m2 * cv.sq.m.2.ha
d.physio$bg.biom.trees.kg.ha <- d.physio$bg.biom.trees.kg.m2 * cv.sq.m.2.ha

d.physio$ag.biom.grass.kg.ha <- d.physio$ag.biom.grass.kg.m2 * cv.sq.m.2.ha

d.physio$ag.biom.grass.1.kg.ha <- d.physio$ag.biom.grass.1.kg.m2 * cv.sq.m.2.ha
d.physio$ag.biom.grass.2.kg.ha <- d.physio$ag.biom.grass.2.kg.m2 * cv.sq.m.2.ha

d.physio$bg.biom.grass.1.kg.ha <- d.physio$bg.biom.grass.1.kg.m2 * cv.sq.m.2.ha
d.physio$bg.biom.grass.2.kg.ha <- d.physio$bg.biom.grass.2.kg.m2 * cv.sq.m.2.ha



d.physio$bg.biom.grass.kg.ha <- d.physio$bg.biom.grass.kg.m2 * cv.sq.m.2.ha


d.physio$bg.biom.kg.ha  <- d.physio$bg.biom.kg.m2 * cv.sq.m.2.ha
d.physio$ag.biom.kg.ha  <- d.physio$ag.biom.kg.m2 * cv.sq.m.2.ha
}

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

d.all[d == d.all$date.time , 'sw.5'] <- d.watr[d.watr$date == d, 'sw.5'][1] 


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
{
  
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
  precip.tah.621 <-  d.weather.subs.2[d.weather.subs.2$date == curr.date, 'precip']
  precip.tah.678 <-  d.weather.subs.3[d.weather.subs.3$date == curr.date, 'precip']
  precip.tah.814 <-  d.weather.subs.4[d.weather.subs.4$date == curr.date, 'precip']
  
  temp.min.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.min']
  temp.min.tah.621 <-  d.weather.subs.2[d.weather.subs.2$date == curr.date, 'temp.min']
  temp.min.tah.678 <-  d.weather.subs.3[d.weather.subs.3$date == curr.date, 'temp.min']
  temp.min.tah.814 <-  d.weather.subs.4[d.weather.subs.4$date == curr.date, 'temp.min']
  
  temp.avg.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.mn']
  temp.avg.tah.621 <-  d.weather.subs.2[d.weather.subs.2$date == curr.date, 'temp.mn']
  temp.avg.tah.678 <-  d.weather.subs.3[d.weather.subs.3$date == curr.date, 'temp.mn']
  temp.avg.tah.814 <-  d.weather.subs.4[d.weather.subs.4$date == curr.date, 'temp.mn']
  
  temp.max.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.max']
  temp.max.tah.621 <-  d.weather.subs.2[d.weather.subs.2$date == curr.date, 'temp.max']
  temp.max.tah.678 <-  d.weather.subs.3[d.weather.subs.3$date == curr.date, 'temp.max']
  temp.max.tah.814 <-  d.weather.subs.4[d.weather.subs.4$date == curr.date, 'temp.max']
  
  
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

d.all$NEE.mod <-   (-1) * d.all$TER - d.all$GPP.sim 



d.all$et.sim <- d.all$et.sim.mm

}

# Rolling averages
{
d.all[,'r.a.lai.osv'] <- 0
d.all[,'r.a.lai.sim'] <- 0

d.all[,'r.a.swc.osv'] <- 0 
d.all[,'r.a.swc.sim'] <- 0 

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

if (r.a.switch.swc){ for (   r in (r.a.perd.swc+1):(nrow(d.all)-(r.a.perd.swc))  ){
  
  
  d.all[r , 'r.a.swc.osv'] <- 0 
  d.all[r , 'r.a.swc.sim'] <- 0 
  
  for (d in r.a.perd.swc:(-r.a.perd.swc)){
    
    d.all[r   , 'r.a.swc.osv']   <- d.all[r   , 'r.a.swc.osv'] + d.all[r - d  , 'swc.3.pc.osv']  / (r.a.perd.swc * 2) 
    d.all[r   , 'r.a.swc.sim']   <- d.all[r   , 'r.a.swc.sim'] + d.all[r - d  , 'sw.5']/ (r.a.perd.swc * 2) 
  }}  } else { d.all[,'r.a.swc.osv']  <- d.all$swc.3.pc.osv ; d.all[,'r.a.swc.sim'] <- d.all$sw.5 } # SWC
  
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
d.all[ is.na(d.all$swc.3.pc.osv) , 'r.a.swc.osv'] <- 0.099



summary(d.all$r.a.lai.sim)
summary(d.all$r.a.ter.sim)
summary(d.all$r.a.gpp.sim)
summary(d.all$r.a.nee.sim)
summary(d.all$r.a.swc.sim)


summary(d.all$r.a.lai.osv)
summary(d.all$r.a.ter.osv)
summary(d.all$r.a.gpp.sim)
summary(d.all$r.a.nee.sim)
summary(d.all$r.a.swc.sim)



d.all.n <- d.all[ 
  
  !is.na(d.all$r.a.lai.sim) 
  & !is.na(d.all$r.a.ter.sim) 
  & !is.na(d.all$r.a.gpp.sim)  
  & !is.na(d.all$r.a.nee.sim) 
  & !is.na(d.all$r.a.swc.sim) 
  
  & d.all$r.a.lai.sim != 0
  & d.all$r.a.ter.sim != 0
  & d.all$r.a.gpp.sim != 0
  & d.all$r.a.nee.sim != 0
  & d.all$r.a.swc.sim != 0
  
  & d.all$r.a.lai.osv != 0
  & d.all$r.a.ter.osv != 0
  & d.all$r.a.gpp.osv != 0
  & d.all$r.a.nee.osv != 0
  & d.all$r.a.swc.osv != 0
    

    , ]
#d.all <- d.all[   !is.na(d.all$three_dra.gpp.sim) & !is.na(d.all$three_dra.gpp.osv) , ]
nrow(d.all)


}

# Covid Status
{
d.all$covid <- NA

covid.stats.pre <- 'Pre-covid'
covid.stats.post <- 'Post-covid'


covid.start.date <<- "2020-03-14"
covid.end.date <<- "2022-03-07"
covid.end.valid.date <- "2022-07-01"

d.gap.2.period.start <- "2023-01-25"
d.gap.2.period.end <- "2023-04-20"

covid.status <- c('Pre-covid' , 'Post-covid' ,'During covid')


d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time), 'covid'] <- covid.status[1]
d.all[d.all$date.time >= covid.start.date & d.all$date.time <= covid.end.date   & !is.na(d.all$date.time), 'covid'] <- covid.status[3]
d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid'] <- covid.status[2]


d.all$omit.period.2 <- FALSE
d.all[d.all$date.time > d.gap.2.period.start & d.all$date.time < d.gap.2.period.end & !is.na(d.all$date.time) , 'omit.period.2'] <- TRUE


# Periods for model validation
d.all[ , 'covid.valid'] <- 'NA'
d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time), 'covid.valid'] <- covid.status[1]
d.all[d.all$date.time >  covid.end.valid.date & !is.na(d.all$date.time), 'covid.valid'] <- covid.status[2]



nrow(d.all)
#d.all <- d.all[ !is.na(d.all$covid) , ]
nrow(d.all)
}

# Evaluation
{
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


# MEAN SQUARED DEVIATION
msd.ter.osv.pre.c <- sum( na.omit((d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim']   - d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv']  )^2 ) ) /sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))



# MEAN BIASES
mb.lai.pre.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))
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

rmse.swc.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2) , 'r.a.swc.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.osv) & !is.na(d.all$r.a.swc.sim) & !(d.all$omit.period.2))
rmse.swc.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.swc.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.swc.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.swc.osv)& !is.na(d.all$r.a.swc.sim) & !(d.all$omit.period.2))
rmse.swc.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual , 'r.a.swc.sim'] - d.all[ d.all$variable.status == v.status.actual   & !(d.all$omit.period.2), 'r.a.swc.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.swc.osv)& !is.na(d.all$r.a.swc.sim) & !(d.all$omit.period.2))


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


nrmse.swc.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.osv'] ))) * rmse.swc.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.swc.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.swc.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.osv) & !is.na(d.all$r.a.swc.sim))
nrmse.swc.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.swc.osv'] ))) *  rmse.swc.post.c
nrmse.swc.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.swc.osv'] ))) *  rmse.swc.all


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




# Rounding




# RMSEs
rmse.lai.pre.c <-  round( rmse.lai.pre.c , rd.decs.rmse )
rmse.lai.post.c <-  round( rmse.lai.post.c , rd.decs.rmse)
rmse.lai.all  <-  round( rmse.lai.all , rd.decs.rmse)

rmse.swc.pre.c <-  round( rmse.swc.pre.c , rd.decs.rmse-2)
rmse.swc.post.c <-  round( rmse.swc.post.c , rd.decs.rmse-2)
rmse.swc.all  <-  round( rmse.swc.all , rd.decs.rmse-2)

rmse.ter.pre.c <-  round( rmse.ter.pre.c , rd.decs.rmse)
rmse.ter.post.c <-  round( rmse.ter.post.c , rd.decs.rmse)
rmse.ter.all  <-  round( rmse.ter.all , rd.decs.rmse)

rmse.gpp.pre.c <-  round( rmse.gpp.pre.c , rd.decs.rmse)
rmse.gpp.post.c <-  round( rmse.gpp.post.c , rd.decs.rmse)
rmse.gpp.all  <-  round( rmse.gpp.all , rd.decs.rmse)

rmse.nee.pre.c <-  round( rmse.nee.pre.c , rd.decs.rmse)
rmse.nee.post.c <-  round( rmse.nee.post.c , rd.decs.rmse)
rmse.nee.all  <-  round( rmse.nee.all , rd.decs.rmse)

# NRMSEs
nrmse.lai.pre.c <-  round( nrmse.lai.pre.c , 1)
nrmse.lai.post.c <-  round( nrmse.lai.post.c , 1)
nrmse.lai.all <-  round( nrmse.lai.all, 1)

nrmse.swc.pre.c <-  round( nrmse.swc.pre.c , 1)
nrmse.swc.post.c <-  round( nrmse.swc.post.c , 1)
nrmse.swc.all <-  round( nrmse.swc.all, 1)

nrmse.ter.pre.c <-  round( nrmse.ter.pre.c , 1)
nrmse.ter.post.c <-  round( nrmse.ter.post.c , 1)
nrmse.ter.all <-  round( nrmse.ter.all, 1)

nrmse.gpp.pre.c <-  round( nrmse.gpp.pre.c , 1)
nrmse.gpp.post.c <-  round( nrmse.gpp.post.c , 1)
nrmse.gpp.all <-  round( nrmse.gpp.all, 1)

nrmse.nee.pre.c <-  round( nrmse.nee.pre.c , 1)
nrmse.nee.post.c <-  round( nrmse.nee.post.c , 1)
nrmse.nee.all <-  round( nrmse.nee.all, 1)




# Pearsons correlations
cor.swc.pre.c <- cor(  d.all[ !is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'r.a.swc.osv'] , d.all[!is.na(d.all$variable.status) &   d.all$variable.status == v.status.actual &  d.all$covid == covid.stats.pre , 'r.a.swc.sim'] , method = cor.type  )
cor.swc.post.c <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , 'r.a.swc.osv'] , d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual &  d.all$covid == covid.stats.post, 'r.a.swc.sim']   , method = cor.type  )
cor.swc.all <- cor(  d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual  , 'r.a.swc.osv'] , d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual , 'r.a.swc.sim']   , method = cor.type  )


cor.nee.pre.c <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'r.a.nee.sim'] , d.all[ !is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual& d.all$covid == covid.stats.pre  , 'r.a.nee.osv']   , method = cor.type)
cor.nee.post.c <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'r.a.nee.sim'] , d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'r.a.nee.osv'] , method = cor.type  )
cor.nee.all <- cor(  d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual  , 'r.a.nee.osv'] , d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual , 'r.a.nee.sim']   , method = cor.type  )


cor.ter.pre.c <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'r.a.ter.sim'] , d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'r.a.ter.osv']   , method = cor.type)
cor.ter.post.c <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post & !is.na(d.all$r.a.ter.sim) & !is.na(d.all$r.a.ter.osv), 'r.a.ter.sim'] , d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post & !is.na(d.all$r.a.ter.sim) & !is.na(d.all$r.a.ter.osv), 'r.a.ter.osv'], method = cor.type   )
cor.ter.all <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) , 'r.a.ter.osv'] , d.all[ !is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) , 'r.a.ter.sim']   , method = cor.type  )


cor.gpp.pre.c <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'r.a.gpp.sim'] , d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'r.a.gpp.osv']  , method = cor.type )
cor.gpp.post.c <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post & !is.na(d.all$r.a.gpp.sim) & !is.na(d.all$r.a.gpp.osv) , 'r.a.gpp.sim'] , d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post & !is.na(d.all$r.a.gpp.sim) & !is.na(d.all$r.a.gpp.osv), 'r.a.gpp.osv']  , method = cor.type )
cor.gpp.all <- cor(  d.all[ !is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual  , 'r.a.gpp.osv'] , d.all[ !is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual , 'r.a.gpp.sim']   , method = cor.type  )



#cor.et.pre.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'et.sim'] , d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'ET.osv'] , method = cor.type  )
#cor.et.post.c <- cor(  d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'et.sim'] , d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'ET.osv']  , method = cor.type )
#cor.et.all <- cor(  d.all[ d.all$variable.status == v.status.actual  , 'r.a.et.osv'] , d.all[ d.all$variable.status == v.status.actual , 'r.a.et.sim']   , method = cor.type  )



cor.lai.pre.c <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'r.a.lai.sim'] , d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'r.a.lai.sim']  , method = cor.type)
cor.lai.post.c <- cor(  d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'r.a.lai.sim'] , d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'r.a.lai.sim'] , method = cor.type  )
cor.lai.all <- cor(  d.all[ !is.na(d.all$variable.status) & d.all$variable.status == v.status.actual & !is.na(d.all$r.a.lai.osv) , 'r.a.lai.osv'] , d.all[!is.na(d.all$variable.status) &  d.all$variable.status == v.status.actual & !is.na(d.all$r.a.lai.osv), 'r.a.lai.sim']   , method = cor.type  )



# Rounded
cor.swc.pre.c <- round( cor.swc.pre.c, 2)
cor.swc.post.c <- round( cor.swc.post.c , 2)
cor.swc.all <- round( cor.swc.all , 2)

cor.nee.pre.c <- round( cor.nee.pre.c , 2)
cor.nee.post.c <- round( cor.nee.post.c , 2)
cor.nee.all <- round( cor.nee.all , 2)

cor.ter.pre.c <- round( cor.ter.pre.c , 2)
cor.ter.post.c <- round( cor.ter.post.c , 2)
cor.ter.all <- round( cor.ter.all , 2)

cor.gpp.pre.c <- round( cor.gpp.pre.c, 2)
cor.gpp.post.c <- round( cor.gpp.post.c  , 2)
cor.gpp.all <- round( cor.gpp.all , 2)

cor.et.pre.c <- round( cor.et.pre.c, 2)
cor.et.post.c<- round( cor.et.post.c  , 2)
#cor.et.all <- round( cor.et.all , 2)

cor.lai.pre.c  <- round( cor.lai.pre.c, 2)
cor.lai.post.c  <- round( cor.lai.post.c  , 2)
cor.lai.all <- round( cor.lai.all , 2)


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

# Bias correction
{
  
d.all[,'r.a.ter.sim.bc'] <- d.all[,'r.a.ter.sim'] - mb.ter.norm.weath
  
  
d.all[,'r.a.gpp.sim.bc'] <- d.all[,'r.a.gpp.sim'] - mb.gpp.norm.weath
  
  
}

# Define evaluation results based on output type
{
  
  
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

d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time) , 'covid.gpp'] <- str_c('r ', cor.gpp.pre.c , '; rmse ', rmse.gpp.pre.c, '; nrmse ', nrmse.gpp.pre.c , '%' )
d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.gpp'] <- str_c('r ',  cor.gpp.post.c , '; rmse ', rmse.gpp.post.c, '; nrmse ', nrmse.gpp.post.c , '%')

d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time), 'covid.ter'] <- str_c('r ',   cor.ter.pre.c , '; rmse ', rmse.ter.pre.c, '; nrmse ', nrmse.ter.pre.c , '%')
d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.ter'] <- str_c('r ',   cor.ter.post.c , '; rmse ', rmse.ter.post.c, '; nrmse ', nrmse.ter.post.c , '%')

d.all[d.all$date.time < covid.start.date  & !is.na(d.all$date.time), 'covid.nee'] <- str_c( 'r ',  cor.nee.pre.c , '; rmse ', rmse.nee.pre.c, '; nrmse ', nrmse.nee.pre.c , '%')
d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.nee'] <- str_c( 'r ',  cor.nee.post.c , '; rmse ', rmse.nee.post.c, '; nrmse ', nrmse.nee.post.c , '%')

d.all[d.all$date.time < covid.start.date & !is.na(d.all$date.time) , 'covid.swc'] <- str_c('r ',   cor.swc.pre.c , '; rmse ', rmse.swc.pre.c, '; nrmse ', nrmse.swc.pre.c, '%')
d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.swc'] <- str_c( 'r ',  cor.swc.post.c , '; rmse ', rmse.swc.post.c, '; nrmse ', nrmse.swc.post.c , '%')

d.all[d.all$date.time < covid.start.date & !is.na(d.all$date.time) , 'covid.lai'] <- str_c('r ' ,  cor.lai.pre.c , '; rmse ', rmse.lai.pre.c, '; nrmse ', nrmse.lai.pre.c , '%')
d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time), 'covid.lai'] <- str_c('r ' ,  cor.lai.post.c , '; rmse ', rmse.lai.post.c, '; nrmse ', nrmse.lai.post.c , '%')

d.all[d.all$date.time < covid.start.date & !is.na(d.all$date.time) , 'covid.biomass.simd'] <- str_c(  'P ', mean.biomass.grass.1.pre.c , '; A ', mean.biomass.grass.2.pre.c  , ' kg/ha')
d.all[d.all$date.time > covid.end.date  & !is.na(d.all$date.time) , 'covid.biomass.simd'] <- str_c(  'P ', mean.biomass.grass.1.post.c , '; A ', mean.biomass.grass.2.post.c  , ' kg/ha')

d.all[d.all$date.time < covid.start.date & !is.na(d.all$date.time) , 'covid.climate'] <- "Pre-covid"
d.all[d.all$date.time > covid.start.date & !is.na(d.all$date.time) , 'covid.climate'] <- "Post-covid"


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





global.valid.text.swc <- str_c( 'Total period\nr ' , cor.swc.all , '; RMSE ', rmse.swc.all, '; NRMSE ', nrmse.swc.all , '%' )


global.valid.text.lai <- str_c( 'Period mean\nr ' , cor.lai.all , '; RMSE ', rmse.lai.all, '; NRMSE ', nrmse.lai.all , '%' )
global.valid.text.ter <- str_c( 'Period mean\nr ' , cor.ter.all , '; RMSE ', rmse.ter.all, '; NRMSE ', nrmse.ter.all , '%' )
global.valid.text.gpp <- str_c( 'Period mean\nr ' , cor.gpp.all , '; RMSE ', rmse.gpp.all, '; NRMSE ', nrmse.gpp.all , '%' )
global.valid.text.nee <- str_c( 'Period mean\nr ' , cor.nee.all , '; RMSE ', rmse.nee.all, '; NRMSE ', nrmse.nee.all , '%' )



}

# Plot parameters
{
  
gg.valid.labels <- c(
    'L-DNDC'
    ,     'Eddy flux tower'    
  )
  
gg.valid.nee.y.ax.lab <<- 'Net ecosystem exchange (kg C/ha/day)'  
gg.valid.gpp.y.ax.lab <<- 'Gross primary productivity (kg C/ha/day)'  
gg.valid.ter.y.ax.lab <<- 'Total ecosystem respiration (kg C/ha/day)'
gg.valid.agb.grass.y.ax.lab  <<- 'Grass yield (kg/ha)'
gg.valid.et.y.lab <<- 'Evapotranspiration (mm/d)'
gg.valid.lai.y.lab <<- 'Leaf area index'
gg.valid.agb.y.lab <- 'Dry matter yield (Mg/ha/yr)'

gg.valid.leg.y.crd <- 0.78
gg.valid.leg.x.crd <- 0.55

gg.valid.y.ax.tit.fs <- 9
  
  
p.x.ax.lab <<- 'Date (YY-MM-DD)'  

p.swc.y.ax.lab <- 'Soil water content (%)'
p.et.y.ax.lab  <- 'Evapotranspiration (mm/d)'
p.lai.y.ax.lab  <- 'Leaf area index'

p.precip.sec.ax.tit <- 'Precipitation (mm/day)'


global.valid.ter.y.cord <<- 42
global.valid.gpp.y.cord <<- 90
global.valid.swc.y.cord <<- 80
global.valid.nee.y.cord <<- -125


p.mrgn.main.top <- 0.2
p.mrgn.main.right <-  1.12
p.mrgn.main.bottom <- 0.05
p.mrgn.main.left <- 0.2

gg.climate.y.ax.lab.temp <<- 'Temperature (Degrees Celsius)'
gg.temp.ln.width  <<- 0.175


global.valid.sum.date <<- "2023-01-01"

global.valid.text.color <- 'black'
global.valid.text.background <- 'white'
  
p.br.wdth <<- .15

p.br.alpha <<- 0.6

p.ln.width <- 0.6

p.date.interval.x.axis <- "3 month"

gg.valid.date.r2.x.crd <<- 0.5
gg.valid.date.r2.y.crd  <<- 75

p.lab.nee.tx.fs <- 4.75

gg.climate.x.txt.fs <- 13.25


gg.valid.panel.border.line.thickness <- 1
gg.valid.facet.text.size <- 11

# NRMSE labels
#gg.valid.lab.nee.rmse <-  paste0("NRMSE:~",NEE.NRMSE.actual )
#gg.valid.lab.ter.rmse <-  paste0("NRMSE:~",TER.NRMSE.actual )
#gg.valid.lab.gpp.rmse <-  paste0("NRMSE:~",GPP.NRMSE.actual )


p.br.clr <<- '#87C0FF'
p.ln.colr.mod <- '#BBF451'
p.ln.colr.obsv  <- '#71797E'

p.colors <- c(p.ln.colr.obsv , p.ln.colr.mod  , p.br.clr)


p.nee.label.1 <- "NEE, obsd"
p.nee.label.2 <- "NEE, simd"
p.nee.label.3 <- "GPP"
p.nee.label.4 <- "TER"

p.nee.color.1 <- p.ln.colr.obsv
p.nee.color.2 <- p.ln.colr.mod
p.nee.color.3 <- 'lightblue'
p.nee.color.4 <- 'pink'

p.lai.color.grass <<- '#FDC745'
p.lai.color.trees <<- '#7BF1A8'
p.lai.color.all <<- 'black'

p.lai.color.obs <- 'black'

gg.valid.date.x.ax.lab <- as.Date("2019-03-01")


p.swc.osv.label  <- 'Observed'
p.swc.sim.label <- 'Simulated'


p.ssn.x.ranges.2019.rn.2.min <- start.date.cald
p.ssn.x.ranges.2019.rn.2.max <- "2019-12-31"


p.ssn.x.ranges.2020.dr.1.min <- "2020-01-01" 
p.ssn.x.ranges.2020.dr.1.max <- "2020-02-29" 

p.ssn.x.ranges.2020.rn.1.min <- "2020-03-01" 
p.ssn.x.ranges.2020.rn.1.max <- "2020-05-31" 

p.ssn.x.ranges.2020.dr.2.min <- "2020-06-01" 
p.ssn.x.ranges.2020.dr.2.max <- final.date


p.rn.ssn.clr <- '#eaffdf'
p.dr.ssn.clr <- '#fef2c6'

p.ssn.bg.alpha <- 0.1


}



d.all <- as.data.frame(d.all)
d.all <- d.all[,!duplicated(colnames(d.all))]

source('gg.parameters.R')
source('gg.seasons.R')

gg.theme <-   ggplot( d.all[ !(d.all$omit.period.2) & d.all$covid %in% covid.status[c(1,2)] & d.all$date >= start.date.cald  & d.all$date <= end.date.cald ,  ] ,   aes(x = date.time)) +
#  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d" , limits = c(start.date.cald , end.date.cald)) +
  scale_x_date(   breaks  = as.Date(season.cutoffs)
    
   # limits = c(as.Date(start.date.cald) , as.Date(end.date.cald)),
            #  , date_labels = "%m %Y", # Format the labels as "Mon YYYY"
              # date_breaks = "3 months"
               , expand=c(0.00025,0.00025)
               ) +
  theme(
    legend.position = "none" , #c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
    axis.title.x = element_blank() ,  
    legend.title = element_blank(),
    axis.title.y.right = element_blank() , 
    axis.title.y.left = element_text(size = gg.valid.y.ax.tit.fs ) , 
    axis.text.y.right = element_blank() , 
    axis.text.x = element_text(angle = 270 , hjust = 0.5 , vjust = 0.5) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
    ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  )  +
  geom_rect(
    data = season.df[ !is.na(season.df$xmin) & !is.na(season.df$xmax) & !is.na(season.df$covid.climate) , ]  ,
    aes(xmin =  xmin
        , xmax = xmax, 
        ymin = ymin ,
        ymax = ymax
        , fill = fill
    )
    , inherit.aes = FALSE 
    , alpha = .2
  )  +
  scale_fill_manual(values = ssn.fills) +
geom_bar(   data= d.all[ d.all$covid %in% covid.status[c(1,2)] &  !is.na(d.all$covid.climate) & ( d.all$date > covid.end.date | d.all$date < covid.start.date),  ] ,
aes( x = date.time 
, y = precip.osv

)  ,
, stat = 'identity'  
, width = p.precip.br.wdth
, color = p.precip.bar.fill
, alpha = p.precip.br.alpha ) 




} # All


# Decomposition of biomass
{
  
gg.bio.decomp <- ggplot( d.all ,   aes(x = date.time )  
) +  
  geom_line( aes(x = date, y = ag.biom.grass.1.kg.ha /1000, color=  "PERG" ) 
             , linewidth = p.ln.width 
  ) + 
  geom_line( aes(x = date, y = ag.biom.grass.2.kg.ha /1000, color= "CEBI"  ) 
             , linewidth = p.ln.width 
  )  + 
  scale_colour_manual(
    name = ''
    , values =   c( 
      "PERG"  = 'orange' 
      , "CEBI" = 'green'
    ) 
    , breaks = c(
      "PERG" 
      , "CEBI"
    ) 
  )  + 
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  facet_grid( ~ covid.biomass.simd  , scales = 'free_x' , space = 'free') +
  theme(
    plot.margin = margin( 
      
      p.mrgn.main.top
      , p.mrgn.main.right
      ,  p.mrgn.main.bottom 
      , p.mrgn.main.left
      
      , "cm"  ) , 
    
    
    legend.position = "none" ,
    legend.title = element_blank(),   
    axis.title.x = element_blank() , 
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() ,
    axis.ticks.y.right = element_blank() ,
    axis.text.x = element_blank() , 
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ) +
    ylab(gg.valid.agb.y.lab ) 
  
gg.bio.decomp
  
}

# Total ecosystem respiration
{
  
gg.valid.ter.o <- gg.valid.ter  
  
gg.valid.ter <- gg.theme  %>%   +   #ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha )  & d.all$covid %in% covid.status[c(1,2)] ,  ] ,   aes(x = date.time ) ) +  
  geom_line( aes(x = date, y = r.a.ter.sim , color= gg.valid.labels[1]   ) 
             , linewidth = p.ln.width 
  ) +   
 # geom_line( aes(x = date, y = r.a.ter.sim.bc , color= 'bias.corrected' ) 
         #    , linewidth = p.ln.width 
 # ) +
  geom_line( aes(x = date, y = r.a.ter.osv , color= gg.valid.labels[2]  ) 
             , linewidth = p.ln.width 
  ) +
    geom_label(
      data = d.all[ d.all$covid == "Post-covid"  , ],
      mapping = aes(x =  as.Date( global.valid.sum.date ), y = global.valid.ter.y.cord , label = global.valid.text.ter ),
      fill = global.valid.text.background
      , color = global.valid.text.color
      , label.size = NA
    )+
  scale_colour_manual(
    name = ''
    , values =   c( 
      "L-DNDC"  = p.nee.color.2
      , "Eddy flux tower" = p.nee.color.1
      , 'bias.corrected' = 'pink'
    ) 
    , breaks = c(
        gg.valid.labels[1]
      , gg.valid.labels[2]
      , 'bias.corrected' 
    ) 
  )  + 
 # scale_x_date(limits = c(as.Date(start.date.cald) , as.Date(end.date.cald)),
              # date_labels = "%m %Y", # Format the labels as "Mon YYYY"
              # date_breaks = "3 months"
               #, expand=c(0.00025,0.00025)
  #) +
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  facet_grid( ~ covid.ter  , scales = 'free_x' , space = 'free') +
  theme(
    plot.margin = margin( 
      
      p.mrgn.main.top
      , p.mrgn.main.right
      ,  p.mrgn.main.bottom 
      , p.mrgn.main.left
      
      , "cm"  ) , 
    legend.position = "none" , #c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ) ,
    legend.title = element_blank(),
    axis.title.x = element_blank() , 
 #   axis.text.x = element_blank() , 
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ) + 
 ylab(gg.valid.ter.y.ax.lab ) 


gg.valid.ter


}

# Gross primary productivity
{
  
gg.valid.gpp.o <- gg.valid.gpp
  
gg.valid.gpp <- gg.theme  %>%   +  
  geom_line( aes(x = date, y = r.a.gpp.sim , color= gg.valid.labels[1]  ) 
             , linewidth = p.ln.width 
  ) +   
  geom_line( aes(x = date, y = r.a.gpp.osv , color=  gg.valid.labels[2]   ) 
             , linewidth = p.ln.width 
    
  ) +  
 # geom_line( aes(x = date, y = r.a.gpp.sim.bc , color= 'bias.corrected' ) 
        #     , linewidth = p.ln.width 
 # ) +
    geom_label(
      data = d.all[ d.all$covid == "Post-covid"  , ],
      mapping = aes(x =  as.Date( global.valid.sum.date ), y = global.valid.gpp.y.cord , label = global.valid.text.gpp ),
      fill = global.valid.text.background
      , color = global.valid.text.color
      , label.size = NA
    )+
  scale_colour_manual(
    name = ''
    , values =   c( 
      "L-DNDC"  = p.nee.color.2
      , "Eddy flux tower" = p.nee.color.1
    #  , "bias.corrected" = 'pink'
    ) 
    , breaks = c(
      gg.valid.labels[1]
   #   , gg.valid.labels[2]
    ) 
  )  + 
 scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
 facet_grid( ~ covid.gpp  , scales = 'free' , space = 'free') +
  theme(
    plot.margin = margin( 
      
      p.mrgn.main.top
      , p.mrgn.main.right
      ,  p.mrgn.main.bottom 
      , p.mrgn.main.left
      
      , "cm"  ) , 
    
    
    legend.position = "none" ,
    legend.title = element_blank(),   
    axis.title.x = element_blank() , 
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() ,
    axis.ticks.y.right = element_blank() ,
  #  axis.text.x = element_blank() , 
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ) +
 ylab(gg.valid.gpp.y.ax.lab) 

gg.valid.gpp
  
}

# Leaf area index
{
  
 
gg.valid.lai <-  ggplot( d.all[ d.all$swc.3.pc.osv > 0 & d.all$covid %in% covid.status[c(1,2)]  ,  ]  ,   aes(x = date.time ) )  +
     #gg.theme %>% +
#geom_line( aes(x = date.time, y =  r.a.lai.sim , color = 'all' )
             #, linewidth = p.ln.width 
 # ) +#
  geom_line( data =  d.all[ d.all$swc.3.pc.osv > 0 & d.all$covid %in% covid.status[c(1,2)]  ,  ] , aes(x = date.time, y =  r.a.lai.osv , color = 'obs' )
             , linewidth = p.ln.width 
  ) +
   # geom_label(
    #  data = d.all[ d.all$covid == "Post-covid"  , ],
     # mapping = aes(x =  as.Date( global.valid.sum.date ), y = global.valid.lai.y.cord , label = global.valid.text.lai ),
    #  fill = global.valid.text.background
    #  , color = global.valid.text.color
    #  , label.size = NA
    #)+
  scale_colour_manual(
    name = ''
    , values =   c( 
      "grass" = p.lai.color.grass
      , "trees"  = p.lai.color.trees
      ,  "all"  = p.ln.colr.mod
      , 'obs' =  p.ln.colr.obsv
    ) 
    , breaks = c(
      p.lai.color.grass
      ,  p.lai.color.trees
      ,  p.lai.color.all
      ,p.lai.color.obs
    )) +
  facet_grid( ~ covid.lai  , scales = 'free_x' , space = 'free') +
  ylab( gg.valid.lai.y.lab ) +
  theme(
    
    plot.margin = margin( 
      
      p.mrgn.main.top
      , p.mrgn.main.right
      ,  p.mrgn.main.bottom 
      , p.mrgn.main.left
      
      , "cm"  ) , 
    
    legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd )
   # ,   axis.text.x = element_blank() , 
  )  + xlab('Date')
  
gg.valid.lai

# gg.valid.lai.o <- gg.valid.lai

} 

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

# Soil water content
{
gg.valid.swc.o <-   gg.valid.swc

gg.valid.swc <- ggplot( d.all[ d.all$swc.3.pc.osv > 0 & d.all$covid %in% covid.status[c(1,2)]  ,  ] ,   aes(x = date.time)  
) + 
  geom_line( aes(x = date.time, y =  r.a.swc.osv, color= p.swc.osv.label 
  ) 
  , linewidth = p.ln.width 
  
  ) +  
  geom_line( aes(x = date.time, y =  r.a.swc.sim, color= p.swc.sim.label ) 
             , linewidth = p.ln.width 
  ) +  
  geom_label(
    data = d.all[ d.all$covid == "Post-covid"  , ],
    mapping = aes(x =  as.Date( global.valid.sum.date ), y = global.valid.swc.y.cord , label = global.valid.text.swc ),
    fill = global.valid.text.background
    , color = global.valid.text.color
    , label.size = NA
  )+
facet_grid( ~ covid.swc  , scales = 'free_x' , space = 'free') +

  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
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
    
    plot.margin = margin( 
      
      p.mrgn.main.top
      , p.mrgn.main.right
      ,  p.mrgn.main.bottom 
      , p.mrgn.main.left
      
      , "cm"  ) , 
    
    
    legend.position = "none" # c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
    , axis.title.x = element_blank() ,  
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() , 
    axis.text.x = element_text(angle = 270 , vjust = 0.5) ,
  #  axis.text.x = element_blank() , 
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
   strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
   , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  ylab(p.swc.y.ax.lab)


gg.valid.swc


}

# Net ecosystem exchange
{
  
gg.valid.nee <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ) & d.all$covid %in% covid.status[c(1,2)] ,  ] ,   aes(x = date.time )  
) +  
  # - Observed
  geom_line(  aes(x = date.time 
                  , y = r.a.nee.osv
                  , colour= gg.valid.labels[2]
  )  
  ,linewidth = p.ln.width
  ) +  
  # - Modelled
  geom_line( aes(x = date.time
                 , y = r.a.nee.sim
                 , colour=  gg.valid.labels[1]
  ) 
  , linewidth = p.ln.width 
  ) + 
  geom_label(
      data = d.all[ d.all$covid == "Post-covid"  , ],
      mapping = aes(x =  as.Date( global.valid.sum.date ), y = global.valid.nee.y.cord , label = global.valid.text.nee ),
      fill = global.valid.text.background
      , color = global.valid.text.color
      , label.size = NA
    )+
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "L-DNDC" = p.nee.color.2  
      ,"Eddy flux tower" = p.nee.color.1
    ) 
    , breaks = c(
      gg.valid.labels[1]
      , gg.valid.labels[2]
    ) 
  )  + 
  facet_grid( ~ covid.nee  , scales = 'free_x' , space = 'free') +
  theme(
    plot.margin = margin( 
      
      p.mrgn.main.top
      , p.mrgn.main.right
      ,  p.mrgn.main.bottom 
      , p.mrgn.main.left
      , "cm"  ) , 
    
    legend.position = "none" ,
    axis.title.x = element_blank() , 
  #  axis.text.x = element_text(angle = 270) ,
    axis.text.x = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ) + 
  ylab(gg.valid.nee.y.ax.lab) 

 gg.valid.nee

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
  
  
  
  
  
  
  
gg.rain <- ggplot(d.all, aes(x = date.time  )) +
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

  
               
gg.climate <- ggplot( 
  d.all[ d.all$covid %in% covid.status[c(1,2)] &  !is.na(d.all$covid.climate) ,  ]
#  d.eddy.real
  
  ,   aes(x = date.time )  
) +  
geom_line( 
 data= d.all[ d.all$covid %in% covid.status[c(1,2)] &  !is.na(d.all$covid.climate) & ( d.all$date > covid.end.date | d.all$date < covid.start.date),  ]
, aes(x = date, y = temp.avg.osv *2.5 , color=  'pink'  ) 
, linewidth = gg.temp.ln.width 
)  + 
geom_bar(   data= d.all[ d.all$covid %in% covid.status[c(1,2)] &  !is.na(d.all$covid.climate) & ( d.all$date > covid.end.date | d.all$date < covid.start.date),  ] ,
aes( x = date.time 
, y = precip.osv
  
)  ,
, stat = 'identity'  
, width = p.br.wdth
, color = 'blue'
, alpha = p.br.alpha ) + 
 # geom_bar(  data = d.all[,  ] ,
           #  aes( x =date.time 
                #  , y = precip.not.eddy.contns
              # ) ,
#, stat = 'identity'  
#, width = p.br.wdth
#, color = 'red'
#, alpha = p.br.alpha 
#)  + 
   geom_rect(
      data = season.df[ !is.na(season.df$xmin) & !is.na(season.df$xmax) & !is.na(season.df$covid.climate) , ]  ,
   aes(xmin =  xmin
         , xmax = xmax, 
        ymin = ymin ,
          ymax = ymax
        , fill = fill
      )
    , inherit.aes = FALSE 
      , alpha = .2
    )  + 
scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
#  ylab(gg.climate.y.ax.lab) +
facet_grid( ~ covid.climate , scales = 'free_x' , space = 'free')  +
scale_y_continuous(
gg.climate.y.ax.lab.temp , 
sec.axis = sec_axis( . / 2.5 ~  .   , name = p.precip.sec.ax.tit )
, limits = c( 0,200   )
) +
  #scale_y_continuous(
   # limits = c(10, 35) ,
    #name = '',
    #sec.axis = sec)  +
  theme(
legend.position = "none" , #c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ) ,
legend.title = element_blank(),
axis.title.x = element_blank() , 
axis.text.x = element_text( size = gg.climate.x.txt.fs , angle = 270 , vjust = 0.5) ,
#  legend.title = element_blank() ,
panel.grid.major = element_blank(),
panel.background = element_blank(),
panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
, strip.background = element_blank(),
, strip.text.x = element_blank(),
) +
    scale_fill_manual(values = ssn.fills)

gg.climate

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

gg.validate.1 <- ggarrange(
  
  
   gg.valid.ter
  , gg.valid.gpp
  , gg.valid.nee
  , gg.climate
     
  , nrow = 4
  , labels = gg.validate.1.labels 
  , label.x = .9575
  , label.y = 0.9175
)

gg.validate.1 


gg.valid.1.dpi  <-  1000

gg.valid.1.width <- 8.45
gg.valid.1.height  <- 13.35
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


