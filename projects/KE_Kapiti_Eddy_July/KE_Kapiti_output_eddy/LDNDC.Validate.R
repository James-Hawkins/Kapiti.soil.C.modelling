

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())


save.image('L.DNDC.Validate.RData')
load('L.DNDC.Validate.RData')

# Global parameters
{
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


}

  
library("languageR") ; library(readxl) ; library('readxl') ; library('ggplot2') ; library(stringr) ;library(stringi) ; library('chron') ; library('lubridate') ; library('ggpubr')
  
  

'''
Main issues needing worked out


'''

source('Eddy_transform.R')
source('biomass.osv.R')

{

# L-DNDC modelled outputs
{

d.sl.chem <<- read.csv('KE_Kapiti_soilchemistry-daily.csv')
  
d.physio.all  <<- read.csv('KE_Kapiti_physiology-daily.csv')
  
d.watr  <<- read.csv('KE_Kapiti_watercycle-daily.csv')

# Rename columns
names(d.sl.chem )[6] <- 'emis.hetero'

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


all.grass.species <- c(  "ANGA" , "PERG" , "PECL" , 'BEAN' , 'GRASS')
all.tree.species <- c(  "BUAF" , "TAPAJOS" , "ACTO" )

unique.species <- unique(d.physio.all$species )
unique.species.grass <- unique(  d.physio.all[d.physio.all$species %in% all.grass.species , 'species'])  
unique.species.trees <-  unique(  d.physio.all[d.physio.all$species %in% all.tree.species , 'species'])   
species.str.id.all <- ":ALL:" 



d.physio.grass <- d.physio.all
d.physio.trees <- d.physio.all

d.physio.grass$co2.upt <-  d.physio.all[ d.physio.all$species == unique.species.grass[1] ,cols.2.add.physio ] + d.physio.all[ d.physio.all$species == unique.species.grass[2] ,cols.2.add.physio ] +  d.physio.all[ d.physio.all$species == unique.species.grass[3] ,cols.2.add.physio ]
d.physio.trees$co2.upt <- d.physio.all[ d.physio.all$species == unique.species.trees[1] ,cols.2.add.physio ] + d.physio.all[ d.physio.all$species == unique.species.trees[2] ,cols.2.add.physio ] 


nrow(d.physio.all)
#nrow(d.physio)
nrow(d.physio.grass)
nrow(d.physio.trees)


#d.physio.all$bg.biom.grass.kg.ha  <- d.physio$bg.biom.kg.m2 * cv.sq.m.2.ha
#d.physio.all$ag.biom.grass.kg.ha  <- d.physio$ag.biom.kg.m2 * cv.sq.m.2.ha

nrow(d.physio.all)
d.physio <- d.physio.all[ d.physio.all$species == species.str.id.all     , ]
nrow(d.physio)


d.physio$ag.biom.grass.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.grass[1]     ,   'ag.biom.kg.m2' ] + d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'ag.biom.kg.m2' ] +  d.physio.all[ d.physio.all$species == unique.species.grass[3]     ,   'ag.biom.kg.m2' ]
d.physio$bg.biom.grass.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.grass[1]     ,   'bg.biom.kg.m2' ] + d.physio.all[ d.physio.all$species == unique.species.grass[2]     ,   'bg.biom.kg.m2' ] + d.physio.all[ d.physio.all$species == unique.species.grass[3]     ,   'bg.biom.kg.m2' ]

d.physio$ag.biom.trees.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.trees[1]     ,   'ag.biom.kg.m2' ] + d.physio.all[ d.physio.all$species == unique.species.trees[2]     ,   'ag.biom.kg.m2' ] 
d.physio$bg.biom.trees.kg.m2 <- d.physio.all[ d.physio.all$species == unique.species.trees[1]    ,   'bg.biom.kg.m2' ] + d.physio.all[ d.physio.all$species == unique.species.trees[2]     ,   'bg.biom.kg.m2' ] 

d.physio$lai.sim.grass <- d.physio.all[ d.physio.all$species == unique.species.grass[1]   ,   'lai.sim' ] + d.physio.all[ d.physio.all$species == unique.species.grass[2]   ,   'lai.sim' ] + d.physio.all[ d.physio.all$species == unique.species.grass[3]   ,   'lai.sim' ]
d.physio$lai.sim.trees <-  d.physio.all[ d.physio.all$species == unique.species.trees[1]    ,   'lai.sim' ] + d.physio.all[ d.physio.all$species == unique.species.trees[2]    ,   'lai.sim' ]

d.physio$co2.upt.grass <- d.physio.all[ d.physio.all$species == unique.species.grass[1]   ,   'co2.upt' ] + d.physio.all[ d.physio.all$species == unique.species.grass[2]   ,   'co2.upt' ] + d.physio.all[ d.physio.all$species == unique.species.grass[3]   ,   'co2.upt' ]
d.physio$co2.upt.trees <-  d.physio.all[ d.physio.all$species ==  unique.species.trees[1]   ,   'co2.upt' ] + d.physio.all[ d.physio.all$species ==  unique.species.trees[2]   ,   'co2.upt' ] 

# Convert to ha values
d.physio$ag.biom.trees.kg.ha <- d.physio$ag.biom.trees.kg.m2 * cv.sq.m.2.ha
d.physio$bg.biom.trees.kg.ha <- d.physio$bg.biom.trees.kg.m2 * cv.sq.m.2.ha

d.physio$ag.biom.grass.kg.ha <- d.physio$ag.biom.grass.kg.m2 * cv.sq.m.2.ha
d.physio$bg.biom.grass.kg.ha <- d.physio$bg.biom.grass.kg.m2 * cv.sq.m.2.ha


d.physio$bg.biom.kg.ha  <- d.physio$bg.biom.kg.m2 * cv.sq.m.2.ha
d.physio$ag.biom.kg.ha  <- d.physio$ag.biom.kg.m2 * cv.sq.m.2.ha




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



# merged model data
{
  
  d.all <- cbind( d.sl.chem$emis.hetero , d.physio)
  
  d.all <- cbind( d.all , d.watr)
  
  names(d.all)[1] <- 'emis.hetero'
  d.all$date.time <- as.Date(d.all$date.time ,  format="%Y-%m-%d")
  #d.all$date.time <- as.Date(d.all$date.time ,  format="%m/%d/%Y")
  
  d.all$day.cnt <- NA
  
  
  for (r in 1:nrow(d.all)  ){
    
    d.all[ r , 'day.cnt'] <- r 
    
  }
  
  
  
  final.date <- tail(d.all$date.time )[6]
  
  frst.date <- which( d.all$date.time  == start.date.cald )
  end.date <- which( d.all$date.time == end.date.cald )
  
  d.all <- d.all[d.all$day.cnt >= frst.date
                 & d.all$day.cnt <= end.date
                 ,  ]
  

  
}


}
  
   



# L-DNDC raw data
d.eddy.clim  <<- read.csv('KE_Kapiti_climate_eddy.csv')

names(d.eddy.clim)[1] <- 'yr'
names(d.eddy.clim)[2] <- 'day.cnt'
names(d.eddy.clim)[7] <- 'precip'
d.eddy.clim <- d.eddy.clim[ 23:nrow(d.eddy.clim) ,  ]

# Climate data
{
  # Insert calendar date into climate data
for (r in 1:nrow(d.eddy.clim)){

day.cnt <- d.eddy.clim[ r , 'day.cnt']
year <- d.eddy.clim[ r , 'yr']
origin <- str_c(d.eddy.clim[d.eddy.clim$day.cnt ==  day.cnt & d.eddy.clim$yr == year, 'yr'],'-01-01')

day.cnt <- as.numeric(day.cnt)

d.eddy.clim[r,'date'] <-  as.Date( day.cnt ,  origin = origin)


}

  
  
d.eddy.clim <- d.eddy.clim[
d.eddy.clim$date >= start.date.cald
& d.eddy.clim$date <= end.date
,  ]

nrow(d.eddy.clim)

d.eddy.clim$precip <- as.numeric(d.eddy.clim$precip)

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



nrow(d.eddy.real)
nrow(d.eddy.clim)
nrow(d.watr)
nrow(d.physio)
nrow(d.sl.chem)
nrow(d.all)
nrow(d.lai)

# View(d.eddy.clim)
#  View(d.all)





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
  
  
}
  
}




nrow(d.eddy.real)
nrow(d.eddy.clim)
nrow(d.all)


d.all <- cbind(d.all, d.eddy.real)

#d.all <- cbind(d.all, d.eddy.clim)

covid.stats.pre <- 'Pre-covid'
covid.stats.post <- 'Post-covid'

# Computation
{
  
# Observed

d.all$gpp.osv.kg.ha <-  d.all$gpp.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 

  
d.all$reco.osv.kg.ha <-  d.all$reco.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
# convert observed eddy in mm per sq m per s to kg per ha
d.all$NEE.obs.kg.ha <- d.all$nee.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 

d.all[   is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA
d.all[d.all$NEE.obs.kg.ha < -90 & !is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA



summary(d.all$co2.upt)
summary(d.all$maint.resp)
summary(d.all$transp.resp)
summary(d.all$growth.resp)
summary(d.all$emis.hetero)

#hist(d.all$co2.upt)
#hist(d.all$maint.resp)
#hist(d.all$transp.resp)
#hist(d.all$growth.resp)
#hist(d.all$emis.hetero)

# MODELLED
d.all$GPP.sim <- cv.sq.m.2.ha * d.all$co2.upt


d.all$GPP.trees.sim <- cv.sq.m.2.ha * d.all$co2.upt.trees
d.all$GPP.grass.sim <- cv.sq.m.2.ha * d.all$co2.upt.grass

d.all$TER.sim <- cv.sq.m.2.ha *  (d.all$maint.resp + d.all$transp.resp + d.all$growth.resp) + d.all$emis.hetero

d.all$NEE.mod <-   (-1) * d.all$TER - d.all$GPP.sim 



d.all$et.sim <- d.all$et.sim.mm
#hist(d.all$GPP.sim)
##hist(d.all$TER.sim)
#hist(d.all$NEE.mod)

}



# Covid Status
{
  d.all$covid <- NA
  
  covid.start.date <- "2020-03-14"
  covid.end.date <- "2022-03-07"
  
  covid.status <- c('Pre-covid' , 'Post-covid')
  
  
  d.all[d.all$date.time < covid.start.date  , 'covid'] <- covid.status[1]
  d.all[d.all$date.time > covid.end.date  , 'covid'] <- covid.status[2]
  
  nrow(d.all)
  d.all <- d.all[ !is.na(d.all$covid) , ]
  nrow(d.all)
}

# Evaluation
{

  
d.all$sqr.dvn.ag.biomass.pst.cvd <- NA
d.all$sqr.dvn.swc.pst.cvd <- NA
d.all$sqr.dvn.nee.pst.cvd <- NA
d.all$sqr.dvn.ter.pst.cvd <- NA
d.all$sqr.dvn.gpp.pst.cvd <- NA

d.all$sqr.dvn.ag.biomass.pre.cvd <- NA
d.all$sqr.dvn.swc.pre.cvd <- NA
d.all$sqr.dvn.nee.pre.cvd <- NA
d.all$sqr.dvn.ter.pre.cvd <- NA
d.all$sqr.dvn.gpp.pre.cvd <- NA


d.all[, 'R2.swc.pre.cvd'] <- NA
d.all[, 'R2.nee.pre.cvd'] <- NA
d.all[, 'R2.ter.pre.cvd'] <- NA
d.all[, 'R2.gpp.pre.cvd'] <- NA

for (r in 1:nrow(  d.all)){


if (d.all[r , 'variable.status' ] == v.status.actual ){

if (d.all[r , 'covid' ] == covid.stats.pre ){

d.all[r, 'sqr.dvn.ag.biomass.pre.cvd'] <- sqrt((d.all[r, 'ag.biom.kg.ha'] - d.all[r, 'biom.osv.kg.ha'])^2) 

d.all[r, 'sqr.dvn.swc.pre.cvd'] <-  sqrt((d.all[r, 'sw.5'] - d.all[r, 'swc.3.pc.osv'])^2)
d.all[r, 'sqr.dvn.nee.pre.cvd'] <-  sqrt((d.all[r, 'NEE.mod'] - d.all[r, 'NEE.obs.kg.ha'])^2)
d.all[r, 'sqr.dvn.ter.pre.cvd'] <-  sqrt((d.all[r, 'TER.sim'] - d.all[r, 'reco.osv.kg.ha'])^2)
d.all[r, 'sqr.dvn.gpp.pre.cvd'] <-  sqrt((d.all[r, 'GPP.sim'] - d.all[r, 'gpp.osv.kg.ha'])^2)

d.all[r, 'R2.swc.pre.cvd'] <- abs(d.all[r, 'sqr.dvn.swc.pre.cvd'] / d.all[r, 'swc.3.pc.osv'])
d.all[r, 'R2.nee.pre.cvd'] <- abs(d.all[r, 'sqr.dvn.nee.pre.cvd'] / d.all[r, 'NEE.obs.kg.ha'])
d.all[r, 'R2.ter.pre.cvd'] <- abs(d.all[r, 'sqr.dvn.ter.pre.cvd'] / d.all[r, 'reco.osv.kg.ha'])
d.all[r, 'R2.gpp.pre.cvd'] <- abs(d.all[r, 'sqr.dvn.gpp.pre.cvd'] / d.all[r, 'gpp.osv.kg.ha'])



} else if (d.all[r , 'covid' ] == covid.stats.post ){

d.all[r, 'sqr.dvn.ag.biomass.pst.cvd'] <- sqrt((d.all[r, 'ag.biom.kg.ha'] - d.all[r, 'biom.osv.kg.ha'])^2) 

d.all[r, 'sqr.dvn.swc.pst.cvd'] <-  sqrt((d.all[r, 'sw.5'] - d.all[r, 'swc.3.pc.osv'])^2)
d.all[r, 'sqr.dvn.nee.pst.cvd'] <-  sqrt((d.all[r, 'NEE.mod'] - d.all[r, 'NEE.obs.kg.ha'])^2)
d.all[r, 'sqr.dvn.ter.pst.cvd'] <-  sqrt((d.all[r, 'TER.sim'] - d.all[r, 'reco.osv.kg.ha'])^2)
d.all[r, 'sqr.dvn.gpp.pst.cvd'] <-  sqrt((d.all[r, 'GPP.sim'] - d.all[r, 'gpp.osv.kg.ha'])^2)


d.all[r, 'R2.swc.pst.cvd'] <- abs(d.all[r, 'sqr.dvn.swc.pst.cvd'] / d.all[r, 'swc.3.pc.osv'])
d.all[r, 'R2.nee.pst.cvd'] <- abs(d.all[r, 'sqr.dvn.nee.pst.cvd'] / d.all[r, 'NEE.obs.kg.ha'])
d.all[r, 'R2.ter.pst.cvd'] <- abs(d.all[r, 'sqr.dvn.ter.pst.cvd'] / d.all[r, 'reco.osv.kg.ha'])
d.all[r, 'R2.gpp.pst.cvd'] <- abs(d.all[r, 'sqr.dvn.gpp.pst.cvd'] / d.all[r, 'gpp.osv.kg.ha'])


}
}
  
}



# Pearsons correlations
cor.swc.pre.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'swc.3.pc.osv'] , d.all[ d.all$variable.status == v.status.actual &  d.all$covid == covid.stats.pre , 'sw.5']   )
cor.swc.post.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , 'swc.3.pc.osv'] , d.all[ d.all$variable.status == v.status.actual &  d.all$covid == covid.stats.post, 'sw.5']   )

cor.nee.pre.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'NEE.mod'] , d.all[ d.all$variable.status == v.status.actual& d.all$covid == covid.stats.pre , 'NEE.obs.kg.ha']   )
cor.nee.post.c <- cor(  d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'NEE.mod'] , d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'NEE.obs.kg.ha']   )

cor.ter.pre.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'TER.sim'] , d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'reco.osv.kg.ha']   )
cor.ter.post.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , 'TER.sim'] , d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'reco.osv.kg.ha']   )

cor.gpp.pre.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'GPP.sim'] , d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'gpp.osv.kg.ha']   )
cor.gpp.post.c <- cor(  d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'GPP.sim'] , d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'gpp.osv.kg.ha']   )

cor.et.pre.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'et.sim'] , d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'ET.osv']   )
cor.et.post.c <- cor(  d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'et.sim'] , d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'ET.osv']   )

cor.lai.pre.c <- cor(  d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'lai.sim'] , d.all[ d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , 'lai.obs']  )
cor.lai.post.c <- cor(  d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'lai.sim'] , d.all[ d.all$variable.status == v.status.actual  & d.all$covid == covid.stats.post, 'lai.obs']   )

# Rounded
cor.swc.pre.c <- round( cor.swc.pre.c, 2)
cor.swc.post.c <- round( cor.swc.post.c , 2)

cor.nee.pre.c <- round( cor.nee.pre.c , 2)
cor.nee.post.c <- round( cor.nee.post.c , 2)

cor.ter.pre.c <- round( cor.ter.pre.c , 2)
cor.ter.post.c <- round( cor.ter.post.c , 2)

cor.gpp.pre.c <- round( cor.gpp.pre.c, 2)
cor.gpp.post.c <- round( cor.gpp.post.c  , 2)

cor.et.pre.c <- round( cor.et.pre.c, 2)
cor.et.post.c<- round( cor.et.post.c  , 2)

cor.lai.pre.c  <- round( cor.lai.pre.c, 2)
cor.lai.post.c  <- round( cor.lai.post.c  , 2)








RMSE.SWC.pre.c <- sum(na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.swc.pre.cvd'])) / length((na.omit(d.all[ d.all$variable.status == v.status.actual  , 'sqr.dvn.swc.pre.cvd'])))
NRMSE.SWC.pre.c <- 100* RMSE.SWC.pre.c / sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.pre & d.all$variable.status == v.status.actual , 'swc.3.pc.osv']))^2)

RMSE.SWC.post.c <- sum(na.omit(d.all[, 'sqr.dvn.swc.pst.cvd'])) / length((na.omit(d.all[, 'sqr.dvn.swc.pst.cvd'])))
NRMSE.SWC.post.c <- 100* RMSE.SWC.post.c / sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.post , 'swc.3.pc.osv']))^2)

# AGB
RMSE.AGB.pre.c <- sum(na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.ag.biomass.pre.cvd'])) / length((na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.ag.biomass.pre.cvd'])))
NRMSE.AGB.pre.c <- 100* RMSE.AGB.pre.c / sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.pre & d.all$variable.status == v.status.actual , 'biom.osv.kg.ha']))^2)

RMSE.AGB.post.c <- sum(na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.ag.biomass.pst.cvd'])) / length((na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.ag.biomass.pst.cvd'])))
NRMSE.AGB.post.c <- 100* RMSE.AGB.post.c / sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.post & d.all$variable.status == v.status.actual , 'biom.osv.kg.ha']))^2)


# NEE
RMSE.NEE.pre.c <- sum(na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.nee.pre.cvd'])) / length((na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.nee.pre.cvd'])))
NRMSE.NEE.pre.c <- 100* RMSE.NEE.pre.c/ sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.pre & d.all$variable.status == v.status.actual, 'NEE.obs.kg.ha']))^2)

RMSE.NEE.post.c <- sum(na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.nee.pst.cvd'])) / length((na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.nee.pst.cvd'])))
NRMSE.NEE.post.c <- 100*  RMSE.NEE.post.c / sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.post & d.all$variable.status == v.status.actual, 'NEE.obs.kg.ha']))^2)

# TER
RMSE.TER.pre.c <- sum(na.omit(d.all[ d.all$variable.status == v.status.actual, 'sqr.dvn.ter.pre.cvd'])) / length((na.omit(d.all[ d.all$variable.status == v.status.actual, 'sqr.dvn.ter.pre.cvd'])))
NRMSE.TER.pre.c <- 100* RMSE.TER.pre.c/ sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.pre & d.all$variable.status == v.status.actual, 'reco.osv.kg.ha']))^2)

RMSE.TER.post.c <- sum(na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.ter.pst.cvd'])) / length((na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.ter.pst.cvd'])))
NRMSE.TER.post.c <- 100*  RMSE.TER.post.c / sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.post & d.all$variable.status == v.status.actual, 'reco.osv.kg.ha']))^2)

# GPP
RMSE.GPP.pre.c <- sum(na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.gpp.pre.cvd'])) / length((na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.gpp.pre.cvd'])))
NRMSE.GPP.pre.c <- 100* RMSE.GPP.pre.c/ sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.pre & d.all$variable.status == v.status.actual, 'gpp.osv.kg.ha']))^2)

RMSE.GPP.post.c <- sum(na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.gpp.pst.cvd'])) / length((na.omit(d.all[d.all$variable.status == v.status.actual, 'sqr.dvn.gpp.pst.cvd'])))
NRMSE.GPP.post.c <- 100*  RMSE.GPP.post.c / sqrt(  mean(na.omit(d.all[d.all$covid == covid.stats.post & d.all$variable.status == v.status.actual, 'gpp.osv.kg.ha']))^2)


NRMSE.SWC.post.c <- round(  NRMSE.SWC.post.c , 1)
NRMSE.NEE.post.c <- round(  NRMSE.NEE.post.c , 1)
NRMSE.GPP.post.c <- round(  NRMSE.GPP.post.c , 1)
NRMSE.TER.post.c <- round(  NRMSE.TER.post.c , 1)
NRMSE.AGB.post.c <- round(  NRMSE.AGB.post.c , 1)

NRMSE.SWC.pre.c <- round(  NRMSE.SWC.pre.c , 1)
NRMSE.NEE.pre.c <- round(  NRMSE.NEE.pre.c , 1)
NRMSE.GPP.pre.c <- round(  NRMSE.GPP.pre.c , 1)
NRMSE.TER.pre.c <- round(  NRMSE.TER.pre.c , 1)
NRMSE.AGB.pre.c <- round(  NRMSE.AGB.pre.c , 1)

# R2s
# SWC
RSS.SWC.pre.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'sqr.dvn.swc.pre.cvd']))
TSS.SWC.pre.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'swc.3.pc.osv']))
R2.SWC.pre.c <- 1 -  RSS.SWC.pre.c / TSS.SWC.pre.c

RSS.SWC.pst.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'sqr.dvn.swc.pst.cvd']))
TSS.SWC.pst.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'swc.3.pc.osv']))
R2.SWC.pst.c <- 1 -  RSS.SWC.pst.c / TSS.SWC.pst.c

# TER
RSS.TER.pre.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'sqr.dvn.ter.pre.cvd']))
TSS.TER.pre.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'reco.osv.kg.ha']))
R2.TER.pre.c <- 1 -  RSS.TER.pre.c / TSS.TER.pre.c

RSS.TER.pst.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'sqr.dvn.ter.pst.cvd']))
TSS.TER.pst.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'reco.osv.kg.ha']))
R2.TER.pst.c <- 1 -  RSS.TER.pst.c / TSS.TER.pst.c

# GPP
RSS.GPP.pre.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'sqr.dvn.gpp.pre.cvd']))
TSS.GPP.pre.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'gpp.osv.kg.ha']))
R2.GPP.pre.c <- 1 -  RSS.GPP.pre.c / TSS.GPP.pre.c

RSS.GPP.pst.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'sqr.dvn.gpp.pst.cvd']))
TSS.GPP.pst.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'gpp.osv.kg.ha']))
R2.GPP.pst.c <- 1 -  RSS.GPP.pst.c / TSS.GPP.pst.c


# New method
R2.SWC.pre.c <-  mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'R2.swc.pre.cvd']))
R2.SWC.pst.c <-  mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'R2.swc.pst.cvd']))

R2.GPP.pre.c <-  mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'R2.gpp.pre.cvd']))
R2.GPP.pst.c <-  mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'R2.gpp.pst.cvd']))

R2.TER.pre.c <-  mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'R2.ter.pre.cvd']))
R2.TER.pst.c <-  mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'R2.ter.pst.cvd']))

R2.NEE.pre.c <- mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'R2.nee.pre.cvd']))
R2.NEE.pst.c <-  mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'R2.nee.pst.cvd']))


# NEE
RSS.NEE.pre.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'sqr.dvn.nee.pre.cvd']))
mean.observed <-  mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'NEE.obs.kg.ha'] ))
TSS.NEE.pre.c <- sum(    na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'NEE.obs.kg.ha'] - mean(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre, 'NEE.obs.kg.ha'] ))))^2)
R2.NEE.pre.c <- 1 -  RSS.NEE.pre.c / TSS.NEE.pre.c

RSS.NEE.pst.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'sqr.dvn.nee.pst.cvd']))
TSS.NEE.pst.c <- sum(  na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post, 'NEE.obs.kg.ha']))
R2.NEE.pst.c <- 1 -  RSS.NEE.pst.c / TSS.NEE.pst.c


sim.var <- 'ag.biom.kg.ha'
observed.var <- 'biom.osv.kg.ha'
  
  NEE.RMSE.actual.pre.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var]))
  NRMSE.NEE.actual.pre.c <- 100 * NEE.RMSE.actual.pre.c/ sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ]^2))))
  NRMSE.NEE.actual.pre.c <- round( NRMSE.NEE.actual.pre.c , 1)
  
  NEE.RMSE.actual.post.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var]))
  NRMSE.NEE.actual.post.c <- 100 * NEE.RMSE.actual.post.c / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ]^2))))
  NRMSE.NEE.actual.post.c <- round(  NRMSE.NEE.actual.post.c  , 1)
  
  print(paste('NRMSE for NEE ' ,  NRMSE.NEE.actual.pre.c))
  print(paste('NRMSE for NEE ' ,  NRMSE.NEE.actual.post.c))
  
  
  # NEE

  # Actual  data
  sim.var <- 'NEE.mod'
  observed.var <- 'NEE.obs.kg.ha'
  
  NEE.RMSE.actual.pre.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var]))
  NRMSE.NEE.actual.pre.c <- 100 * NEE.RMSE.actual.pre.c/ sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ]^2))))
  NRMSE.NEE.actual.pre.c <- round( NRMSE.NEE.actual.pre.c , 1)
  
  NEE.RMSE.actual.post.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var]))
  NRMSE.NEE.actual.post.c <- 100 * NEE.RMSE.actual.post.c / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ]^2))))
  NRMSE.NEE.actual.post.c <- round(  NRMSE.NEE.actual.post.c  , 1)
  
  print(paste('NRMSE for NEE ' ,  NRMSE.NEE.actual.pre.c))
  print(paste('NRMSE for NEE ' ,  NRMSE.NEE.actual.post.c))
  
  #  TER
  # R2
  
  sim.var <- 'TER.sim'
  observed.var <- 'reco.osv.kg.ha' 
  

  TER.RMSE.actual.pre.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var]))
  NRMSE.TER.actual.pre.c <- 100 * TER.RMSE.actual.pre.c / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ]^2))))
  NRMSE.TER.actual.pre.c <- round( NRMSE.TER.actual.pre.c , 1)
  
  TER.RMSE.actual.post.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var]))
  NRMSE.TER.actual.post.c <- 100 * TER.RMSE.actual.post.c / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ]^2))))
  NRMSE.TER.actual.post.c <- round(  NRMSE.TER.actual.post.c  , 1)
  
  print(paste('NRMSE for NEE ' ,  NRMSE.TER.actual.pre.c))
  print(paste('NRMSE for NEE ' ,  NRMSE.TER.actual.post.c))
  
  #  GPP
  sim.var <- 'GPP.sim'
  observed.var <- 'gpp.osv.kg.ha'
  
  GPP.RMSE.actual.pre.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var]))
  NRMSE.GPP.actual.pre.c <- 100 * GPP.RMSE.actual.pre.c / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ]^2))))
  NRMSE.GPP.actual.pre.c <- round( NRMSE.GPP.actual.pre.c , 1)
  
  GPP.RMSE.actual.post.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var]))
  NRMSE.GPP.actual.post.c <- 100 * GPP.RMSE.actual.post.c / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ]^2))))
  NRMSE.GPP.actual.post.c <- round(  NRMSE.GPP.actual.post.c  , 1)
  
  print(paste('NRMSE for NEE ' ,  NRMSE.GPP.actual.pre.c))
  print(paste('NRMSE for NEE ' ,  NRMSE.GPP.actual.post.c))
  
  

  # SWC - 5 cm layer
  sim.var <- 'sw.5'
  observed.var <- 'swc.3.pc.osv'
  
  SWC.RMSE.actual.pre.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var]))
  NRMSE.SWC.actual.pre.c <- 100 * SWC.RMSE.actual.pre.c / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.pre , observed.var ]^2))))
  NRMSE.SWC.actual.pre.c <- round( NRMSE.SWC.actual.pre.c , 1)
  
  SWC.RMSE.actual.post.c <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ] - d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var]))
  NRMSE.SWC.actual.post.c <- 100 * SWC.RMSE.actual.post.c / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual & d.all$covid == covid.stats.post , observed.var ]^2))))
  NRMSE.SWC.actual.post.c <- round(  NRMSE.SWC.actual.post.c  , 1)
  
  print(paste('NRMSE for NEE ' ,  NRMSE.SWC.actual.pre.c))
  print(paste('NRMSE for NEE ' ,  NRMSE.SWC.actual.post.c))
  

  head(d.all$precip.sim)
  head(d.all$precip.osv)
  
  tail(d.all$precip.sim)
  tail(d.all$precip.osv)
  
precip.compare <-  c()
  
for (r in 3:nrow(d.all)){

precip.compare[r] <- (d.all[r,'precip.sim'] - d.all[r-2,'precip.osv'] )/d.all[r-2,'precip.osv']

d.all[r,'precip.dev'] <- precip.compare[r]

}


summary(na.omit(precip.compare))
  
  precip.compare <- na.omit(precip.compare)
  #cor(precip.compare)
  
}



# Define NRMSEs based on output type
{
d.all$covid.swc <- NA
d.all$covid.gpp <- NA
d.all$covid.ter <- NA
d.all$covid.nee <- NA
d.all$covid.et <- NA
d.all$covid.lai <- NA

d.all[d.all$date.time < covid.start.date  , 'covid.gpp'] <- str_c( covid.status[1] , ' (r: ' , cor.gpp.pre.c , ')')
d.all[d.all$date.time > covid.end.date  , 'covid.gpp'] <- str_c( covid.status[2] , ' (r: ' , cor.gpp.post.c , ')')

d.all[d.all$date.time < covid.start.date  , 'covid.ter'] <- str_c( covid.status[1] , ' (r: ' , cor.ter.pre.c, ')')
d.all[d.all$date.time > covid.end.date  , 'covid.ter'] <- str_c( covid.status[2] , ' (r: ' , cor.ter.post.c  , ')')

d.all[d.all$date.time < covid.start.date  , 'covid.nee'] <- str_c( covid.status[1] , ' (r: ' , cor.nee.pre.c , ')')
d.all[d.all$date.time > covid.end.date  , 'covid.nee'] <- str_c( covid.status[2] , ' (r: ' , cor.nee.post.c  , ')')

d.all[d.all$date.time < covid.start.date  , 'covid.swc'] <- str_c( covid.status[1] , ' (r: ' , cor.swc.pre.c , ')')
d.all[d.all$date.time > covid.end.date  , 'covid.swc'] <- str_c( covid.status[2] , ' (r: ' , cor.swc.post.c , ')')

d.all[d.all$date.time < covid.start.date  , 'covid.et'] <- str_c( covid.status[1] , ' (r: ' , cor.et.pre.c , ')')
d.all[d.all$date.time > covid.end.date  , 'covid.et'] <- str_c( covid.status[2] , ' (r: ' , cor.et.post.c , ')')

d.all[d.all$date.time < covid.start.date  , 'covid.lai'] <- str_c( covid.status[1] , ' (r: ' , cor.lai.pre.c , ')')
d.all[d.all$date.time > covid.end.date  , 'covid.lai'] <- str_c( covid.status[2] , ' (r: ' , cor.lai.post.c , ')')


unq.covid.gpp <- unique(d.all$covid.gpp)
d.all$covid.gpp <- factor(  d.all$covid.gpp, levels = unq.covid.gpp)

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


}

# plot params
{
  
gg.valid.labels <- c(
    'Simulated'
    ,     'Observed'    
  )
  
gg.valid.nee.y.ax.lab <<- 'Net ecosystem exchange (kg C/ha/day)'  
gg.valid.gpp.y.ax.lab <<- 'Gross primary productivity (kg C/ha/day)'  
gg.valid.ter.y.ax.lab <<- 'Total ecosystem respiration (kg C/ha/day)'
gg.valid.agb.grass.y.ax.lab  <<- 'Grass yield (kg/ha)'
gg.valid.et.y.lab <<- 'Evapotranspiration (mm/d)'
gg.valid.lai.y.lab <<- 'Leaf area index'

gg.valid.leg.y.crd <- 0.78
gg.valid.leg.x.crd <- 0.55
  
  
p.x.ax.lab <<- 'Date (YY-MM-DD)'  

p.swc.y.ax.lab <- 'Soil water content (%)'
p.et.y.ax.lab  <- 'Evapotranspiration (mm/d)'
p.lai.y.ax.lab  <- 'Leaf area index'

p.precip.sec.ax.tit <- 'Precipitation (mm/day)'
  
p.br.wdth <<- .15

p.br.alpha <<- 0.6

p.ln.width <- 0.6

p.date.interval.x.axis <- "3 month"

gg.valid.date.r2.x.crd <<- 0.5
gg.valid.date.r2.y.crd  <<- 75

p.lab.nee.tx.fs <- 4.75


gg.valid.panel.border.line.thickness <- 1
gg.valid.facet.text.size <- 11

# NRMSE labels
#gg.valid.lab.nee.rmse <-  paste0("NRMSE:~",NEE.NRMSE.actual )
#gg.valid.lab.ter.rmse <-  paste0("NRMSE:~",TER.NRMSE.actual )
#gg.valid.lab.gpp.rmse <-  paste0("NRMSE:~",GPP.NRMSE.actual )


p.br.clr <<- '#87C0FF'
p.ln.colr.mod <- 'lightgreen'
p.ln.clr.obsv  <- 'black'

p.colors <- c(p.ln.clr.obsv , p.ln.colr.mod  , p.br.clr)


p.nee.label.1 <- "NEE, obsd"
p.nee.label.2 <- "NEE, simd"
p.nee.label.3 <- "GPP"
p.nee.label.4 <- "TER"

p.nee.color.1 <- p.ln.clr.obsv
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



}

d.all <- as.data.frame(d.all)
d.all <- d.all[,!duplicated(colnames(d.all))]

gg.theme <-   ggplot( d.all ,   aes(x = date.time)) +
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  theme(
    legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
    axis.title.x = element_blank() ,  
    legend.title = element_blank(),
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() , 
    axis.text.x = element_text(angle = 270 , hjust = 0.5 , vjust = 0.5) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
    ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  )  



# Biomass & C dynamics
gg.valid.lai <- gg.theme %>% +
  #geom_line( aes(x = date.time, y = lai.sim.grass  , color = 'grass' ) 
  #, linewidth = p.ln.width 
  #) +
  #( aes(x = date.time, y =  lai.sim.trees , color = 'trees' )
  #, linewidth = p.ln.width 
  #) +
  geom_line( aes(x = date.time, y =  lai.sim , color = 'all' )
             , linewidth = p.ln.width 
  ) +
  geom_line( aes(x = date.time, y =  lai.obs , color = 'obs' )
             , linewidth = p.ln.width 
  ) +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "grass" = p.lai.color.grass
      , "trees"  = p.lai.color.trees
      ,  "all"  = p.lai.color.all 
      , 'obs' = p.lai.color.obs
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
    legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd )
  ) 

gg.valid.lai

 # ggplot with legend for different line aesthetics
# https://stackoverflow.com/questions/65929800/ggplot2-separate-legend-for-multiple-geom-lines

gg.precip <- gg.theme %>% + 

  # geom_line( aes(x = date.time, y = precip.osv , color= p.swc.osv.label) 
               #  , linewidth = p.ln.width 
#) +
  geom_line( aes(x = date.time, y = precip.sim , color=  p.swc.sim.label)
             , linewidth = p.ln.width 
  ) +
#geom_line( aes(x = date.time, y = precip.dev , color= p.swc.osv.label )
   #               , linewidth = p.ln.width 
 # ) + 
 # scale_y_continuous(limits = c(0,1)) +
  facet_grid( ~ covid.swc  , scales = 'free_x') +
  theme(
    legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
    axis.title.x = element_blank() ,  
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
    ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  )  
  



gg.bioma <- ggplot( d.all[d.all$year.month >=  biomass.period.start & d.all$year.month <=  biomass.period.end, ] ,   aes(x = date.time)  
) +   geom_line( aes(x = date.time, y = ag.biom.grass.kg.ha, color= p.swc.sim.label ) 
                 , linewidth = p.ln.width 
                 
) +
geom_line( aes(x = date.time, y = biom.osv.kg.ha , color= p.swc.osv.label )
, linewidth = p.ln.width 
) +
ylab( gg.valid.agb.grass.y.ax.lab ) +
  theme(
  legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
  axis.title.x = element_blank() ,  
  legend.title = element_blank(),
  axis.title.y.right = element_blank() , 
  axis.text.y.right = element_blank() , 
  axis.text.x = element_text(angle = 270) ,
  #  legend.title = element_blank() ,
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
  , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
)  
  
gg.bioma
  




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

gg.valid.et


gg.valid.swc <- ggplot( d.all[ d.all$swc.3.pc.osv > 0 ,  ] ,   aes(x = date.time)  
) + 
  geom_line( aes(x = date.time, y = swc.3.pc.osv , color= p.swc.osv.label 
                                                                 ) 
             , linewidth = p.ln.width 
             
  ) +  
  geom_line( aes(x = date.time, y = sw.5  , color= p.swc.sim.label ) 
             , linewidth = p.ln.width 
  ) +  
  facet_grid( ~ covid.swc  , scales = 'free_x' , space = 'free') +
  geom_bar(  data = d.all,
             aes( x = date.time
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
            , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  scale_y_continuous(
    p.swc.y.ax.lab, 
    sec.axis = sec_axis(~   ., name = p.precip.sec.ax.tit )
  ) +
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
    legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
    axis.title.x = element_blank() ,  
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() , 
    axis.text.x = element_text(angle = 270 , vjust = 0.5) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
   strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
   , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.swc.y.ax.lab)

gg.valid.swc


summary(d.all$lai.obs)
typeof(d.all$lai.sim)



# gg.valid.lai.p  <- gg.valid.lai

gg.valid.ter <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
) +  
  geom_line( aes(x = date, y = reco.osv.kg.ha , color= gg.valid.labels[2]   ) 
             , linewidth = p.ln.width 
             
  ) +   
  geom_line( aes(x = date, y = TER.sim  , color= gg.valid.labels[1]  ) 
             , linewidth = p.ln.width 
             
  ) +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Simulated" = p.nee.color.2
      ,"Observed"  = p.nee.color.1
    ) 
    , breaks = c(
      gg.valid.labels[1]
      , gg.valid.labels[2]
    ) 
  )  + 
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  scale_y_continuous(
    gg.valid.ter.y.ax.lab , 
    sec.axis = sec_axis(~   . , name = p.precip.sec.ax.tit )
  ) +
  facet_grid( ~ covid.ter  , scales = 'free_x' , space = 'free') +
  theme(
    legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ) , 
    legend.title = element_blank(),
    axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
   , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ) #+ 
 # xlab(p.x.ax.lab) +
 # ylab(p.y.ax.lab) + 
 # geom_bar(  data = d.all[,  ] ,
             #aes( x =date.time 
             #     , y = precip.osv 
          #   )
            # , stat = 'identity'  
             #, width = p.br.wdth
            # , color = p.br.clr 
            # , alpha = p.br.alpha 
  #) 

gg.valid.ter



gg.valid.gpp <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
) +  
  geom_line( aes(x = date, y = GPP.sim  , color= gg.valid.labels[1]  ) 
             , linewidth = p.ln.width 
             
  ) +   
  geom_line( aes(x = date, y = gpp.osv.kg.ha , color=  gg.valid.labels[2]   ) 
             , linewidth = p.ln.width 
             
  ) +   
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Simulated" = p.nee.color.2
      ,"Observed"  = p.nee.color.1
    ) 
    , breaks = c(
      gg.valid.labels[1]
      , gg.valid.labels[2]
    ) 
  )  + 
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  scale_y_continuous(
    gg.valid.gpp.y.ax.lab , 
    sec.axis = sec_axis(~   . , name = p.precip.sec.ax.tit )
  ) +
  facet_grid( ~ covid.gpp  , scales = 'free' , space = 'free') +
  theme(
    legend.position = "none" ,
    legend.title = element_blank(),   
    axis.title.x = element_blank() , 
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() ,
    axis.ticks.y.right = element_blank() ,
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ) + 
#  xlab(p.x.ax.lab) +
 # ylab(p.y.ax.lab) + 
  geom_bar(  data = d.all[,  ] ,
             aes( x =date.time 
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) 

gg.valid.gpp

gg.valid.nee <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
) +  
  # - Observed
  geom_line(  aes(x = date.time 
                  , y = NEE.obs.kg.ha 
                  , colour= gg.valid.labels[2]
  )  
  ,linewidth = p.ln.width
  ) +  
  # - Modelled
  geom_line( aes(x = date.time
                 , y = NEE.mod 
                 , colour=  gg.valid.labels[1]
  ) 
  , linewidth = p.ln.width 
  
  ) + 
  geom_bar(  data = d.all,
             aes( x = date.time
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Simulated" = p.nee.color.2  
      ,"Observed"  = p.nee.color.1
    ) 
    , breaks = c(
      gg.valid.labels[1]
      , gg.valid.labels[2]
    ) 
  )  + 
  facet_grid( ~ covid.nee  , scales = 'free_x' , space = 'free') +
  scale_y_continuous(
   # p.y.ax.lab , 
    sec.axis = sec_axis(~   . , name = p.precip.sec.ax.tit )
  ) +
  theme(
    legend.position = "none" ,
    axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ) + 
  #xlab(p.x.ax.lab) +
  ylab(gg.valid.nee.y.ax.lab) + 
  geom_bar(  data = d.all[,  ] ,
             aes( x =date.time 
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  )

gg.valid.nee


# Plot 1
gg.validate.1.labels <- c('a' ,'b' )

gg.validate.1 <- ggarrange(
  
  gg.valid.swc
  , gg.valid.et
     
  , nrow = 2
  , labels = gg.validate.1.labels 
)

gg.validate.1 


gg.valid.1.dpi  <-  2500

gg.valid.1.width <- 5.65
gg.valid.1.height  <- 7.35
filename.gg.validate.1 = 'Figures.out/gg.validate.1.jpg'

ggsave(filename =    filename.gg.validate.1 ,  gg.validate.1 , width = gg.valid.1.width, height = gg.valid.1.height , dpi = gg.valid.dpi  )


# Plot 2
gg.validate.2.labels <- c('a' ,'b' ,'c' , 'd')

gg.validate.2 <- ggarrange(
  
  gg.valid.lai
  ,   gg.valid.ter 
  ,   gg.valid.gpp 
  ,  gg.valid.nee 
  
  , labels = gg.validate.labels 
)

gg.validate.2 


gg.valid.dpi  <-  2500

gg.valid.width <- 10.5
gg.valid.height  <- 8
filename.gg.validate = 'Figures.out/gg.validate.2.jpg'

ggsave(filename =    filename.gg.validate ,  gg.validate.2 , width = gg.valid.width, height = gg.valid.height , dpi = gg.valid.dpi  )



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
      'Observed' = p.ln.clr.obsv
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
      'Observed' = p.ln.clr.obsv
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
