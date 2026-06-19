
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

names(d.physio.all)[16] <- 'fol.grow'
names(d.physio.all)[17] <- 'fru.grow'
names(d.physio.all)[18] <- 'fn.rt.grow'
names(d.physio.all)[19] <- 'lst.grow'
names(d.physio.all)[20] <- 'fac.grow'

names(d.physio.all)[21] <- 'fol.resp'
names(d.physio.all)[22] <- 'frt.resp'
names(d.physio.all)[23] <- 'fn.rt.resp'
names(d.physio.all)[24] <- 'lst.resp'

names(d.physio.all)[37] <- 'bg.biom.kg.m2'
names(d.physio.all)[38] <- 'ag.biom.kg.m2'


names(d.physio.all)[39] <- 'lai.sim'


#colnames(d.physio)
cols.2.add.physio <- c('co2.upt' ) # , 'maint.resp'  , 'transp.resp'   , 'growth.resp'  , 'emis.hetero'


#unique(d.physio.all$species)

all.grass.species <- c( 
  "ANGA" 
  , "PERG" 
  , "PECL" 
  , 'CEBI' 
  , 'GRASS' 
  , 'SAFF'
  , "RED_OAT"
  , "INDIGO"
  , "TEPHROSIA"
  
  , 'GRASS.HYBRID.1'
  , 'GRASS.HYBRID.2'
  , 'GRASS.HYBRID.3'
  , 'GRASS.HYBRID.4'
  
  , 'FORB.HYBRID.1'
  , 'FORB.HYBRID.2'
  , 'FORB.HYBRID.3'
  , 'FORB.HYBRID.4'
  
)
all.tree.species <- c(  "BUAF" , "TAPAJOS" , "ACTO" ,  "ACTO_SHRUB" , "TAPAJOS" , 'WHISTL_THORN' , 'WHISTL_THORN2')

unique.species <- unique(d.physio.all$species )
unique.species.grass <- unique(  d.physio.all[d.physio.all$species %in% all.grass.species , 'species'])  
unique.species.trees <-  unique(  d.physio.all[d.physio.all$species %in% all.tree.species , 'species'])   
species.str.id.all <- ":ALL:" 



d.physio.grass <- d.physio.all
d.physio.trees <- d.physio.all

#d.physio.grass$co2.upt <-  d.physio.all[ d.physio.all$species == unique.species.grass[1] ,cols.2.add.physio ] + d.physio.all[ d.physio.all$species == unique.species.grass[2] ,cols.2.add.physio ] #+  d.physio.all[ d.physio.all$species == unique.species.grass[3] ,cols.2.add.physio ]
#d.physio.trees$co2.upt <- d.physio.all[ d.physio.all$species == unique.species.trees[1] ,cols.2.add.physio ] #+ d.physio.all[ d.physio.all$species == unique.species.trees[2] ,cols.2.add.physio ] 


#nrow(d.physio.all)
#nrow(d.physio)
#nrow(d.physio.grass)
#nrow(d.physio.trees)



#nrow(d.physio.all)

if (   length(unique(d.physio.all$species )  ) > 1 ) {  d.physio<- d.physio.all[ d.physio.all$species == species.str.id.all   ,]
} else { d.physio.all$species <- species.str.id.all ; d.physio <- d.physio.all }
#nrow(d.physio.all)



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


d.physio$ag.biom.grass.Mg.ha <- d.physio$ag.biom.grass.kg.ha /1000

d.physio$bg.biom.kg.ha  <- d.physio$bg.biom.kg.m2 * cv.sq.m.2.ha
d.physio$ag.biom.kg.ha  <- d.physio$ag.biom.kg.m2 * cv.sq.m.2.ha

d.physio$bg.biom.Mg.ha  <- d.physio$bg.biom.kg.ha / 1000
d.physio$ag.biom.Mg.ha  <- d.physio$ag.biom.kg.ha / 1000




nrow(d.watr)
d.watr$date[1]
d.physio$date[1]
d.sl.chem$date[1]

d.watr$date[nrow(d.watr)]
d.physio$date[nrow(d.physio)]
d.sl.chem$date[nrow(d.sl.chem)]

nrow(d.physio)
nrow( d.sl.chem)
nrow( d.watr)

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



# Merged LDNDC data
{
  
  d.sim <- cbind( d.sl.chem$emis.hetero , d.physio)
  
  # d.all <- cbind( d.all[d.all$date.time %in% d.watr$date , ] , d.watr[d.watr$date %in% d.all$date.time , ] )
  
  
  
  nrow(d.watr)
  nrow(d.sim)
  
  
  d.sim[ , 'sw.5'] <- d.watr[, 'sw.5']
  d.sim[ , 'sw.15'] <- d.watr[, 'sw.15']
  d.sim[ , 'sw.30'] <- d.watr[, 'sw.30']
  d.sim[ , 'et.sim.mm'] <- d.watr[, 'et.sim.mm']
  
 # for (d in d.all$date.time){
    
    #d.all[d == d.all$date.time , 'sw.5'] <- d.watr[d.watr$date == d, 'sw.5']
   # d.all[d == d.all$date.time , 'sw.15'] <- d.watr[d.watr$date == d, 'sw.15'][1] 
   # d.all[d == d.all$date.time , 'sw.30'] <- d.watr[d.watr$date == d, 'sw.30'][1] 
   # d.all[d == d.all$date.time , 'et.sim.mm'] <- d.watr[d.watr$date == d, 'et.sim.mm'][1] 
    
    
 # }
  
  
  
  
  names(d.sim)[1] <- 'emis.hetero'
  d.sim$date.time <- as.Date(d.sim$date.time ,  format="%Y-%m-%d")
  #d.sim$date.time <- as.Date(d.sim$date.time ,  format="%d/%m/%Y")
 
  
  
  d.sim$day.cnt <- NA
  
  
  for (r in 1:nrow(d.sim)  ){
    
    d.sim[ r , 'day.cnt'] <- r 
    
  }
  
  
  
  
  
  final.date <- tail(d.sim$date.time )[6]
  
  frst.date <- which( d.sim$date.time  == start.date.cald )
  end.date <- which( d.sim$date.time == end.date.cald )
  
  d.sim <- d.sim[d.sim$day.cnt >= frst.date
                 # & d.all$day.cnt <= end.date
                 ,  ]
  
  
  
}


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
  
  d.sim[,l] <- as.numeric( d.sim[,l])
  
}



d.sim <<- d.sim

}
