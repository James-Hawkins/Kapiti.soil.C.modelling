# L-DNDC modelled outputs
{
  
  
  
  d.sl.chem <<- read.csv('KE_Kapiti_soilchemistry-daily.csv')
  
  d.physio.all  <<- read.csv('KE_Kapiti_physiology-daily.csv')
  
  d.watr  <<- read.csv('KE_Kapiti_watercycle-daily.csv')
  
  
  # names(d.all)[1] <- 'emis.hetero'
  d.sl.chem$date.time <- as.Date( d.sl.chem$datetime,  format="%Y-%m-%d")
  d.physio.all$date.time <- as.Date( d.physio.all$datetime ,  format="%Y-%m-%d")
  d.watr$date.time <- as.Date( d.watr$datetime ,  format="%Y-%m-%d")
  
  
  frst.date.sl.chem <- which(  d.sl.chem$date.time  == start.date.cald )
  frst.date.physio <-  which(   d.physio.all$date.time  == start.date.cald )[1]/4
  #frst.date.sl.chem * length(unique(d.physio.all$species)) 
  frst.date.watr <- which(  d.watr$date.time  == start.date.cald )
  
  
  end.date <- which(  d.sl.chem$date.time == end.date.cald )
  

  d.sl.chem$day.cnt <- NA
  d.physio.all$day.cnt <- NA
  d.watr$day.cnt <- NA
  
  r <- 1
  
  for (r in 1:nrow(d.sl.chem)  ){
    
    d.sl.chem[ r , 'day.cnt'] <- r 
    d.watr[ r , 'day.cnt'] <- r 
  }
  

#for (r in 1:nrow( d.physio.all)  ){

#cur.day <- ceiling(seq_len(nrow(d.physio.all)) / 4)

#d.physio.all[ r , 'day.cnt'] <-  cur.day 

#}
  
d.physio.all[  , 'day.cnt'] <- ceiling(seq_len(nrow(d.physio.all)) / 4)
  
  
  
  d.sl.chem <- d.sl.chem[d.sl.chem$day.cnt >=   frst.date.sl.chem 
                 # & d.all$day.cnt <= end.date
                 ,  ]
  

  d.physio.all  <-  d.physio.all[ d.physio.all$day.cnt >=   frst.date.physio
                         # & d.all$day.cnt <= end.date
                         ,  ]
  d.watr <-  d.watr[ d.watr$day.cnt >=   frst.date.watr
                         # & d.all$day.cnt <= end.date
                         ,  ]
  

  head(d.watr$date.time)
  head( d.physio.all$date.time)
  
  # Rename columns
  names(d.sl.chem)[5] <- 'emis.auto'
  names(d.sl.chem)[6] <- 'emis.hetero'
  
  names(d.physio.all)[3] <- 'date.time'
  
  

  names(d.physio.all)[16] <- 'fol.grow'
  names(d.physio.all)[17] <- 'fru.grow'
  names(d.physio.all)[18] <- 'fn.rt.grow'
  names(d.physio.all)[19] <- 'lst.grow'
  names(d.physio.all)[20] <- 'fac.grow'
  
  names(d.physio.all)[21] <- 'fol.resp'
  names(d.physio.all)[22] <- 'frt.resp'
  names(d.physio.all)[23] <- 'fn.rt.resp'
  names(d.physio.all)[24] <- 'lst.resp'
  
  names(d.physio.all)[25] <- 'maint.resp'
  names(d.physio.all)[26] <- 'transp.resp'
  names(d.physio.all)[27] <- 'growth.resp'
  names(d.physio.all)[28] <- 'co2.upt'
  
  names(d.physio.all)[37] <- 'bg.biom.kg.m2'
  names(d.physio.all)[38] <- 'ag.biom.kg.m2'
  
  
  names(d.physio.all)[39] <- 'lai.sim'
  
  
  #colnames(d.physio)
  cols.2.add.physio <- c('co2.upt' ) # , 'maint.resp'  , 'transp.resp'   , 'growth.resp'  , 'emis.hetero'
  

  all.grass.species <- c( 
  #  "ANGA" 
  #  , "PERG" 
   # , "PECL" 
   # , 'CEBI' 
  # , 'GRASS' 
  #  , 'SAFF'
    "RED_OAT"
    , "INDIGO"
    
    
  )
  all.tree.species <- c( 
    "BUAF" 
    , "TAPAJOS"
    , "ACTO" 
    ,  "ACTO_SHRUB"
    , "TAPAJOS"
    , 'WHISTL_THORN'
    , 'WHISTL_THORN2'
    )
  
  unique.species <- unique(d.physio.all$species )
  unique.species.grass <- unique(  d.physio.all[d.physio.all$species %in% all.grass.species , 'species'])  
  unique.species.trees <-  unique(  d.physio.all[d.physio.all$species %in% all.tree.species , 'species'])   
  species.str.id.all <- ":ALL:" 
  
  
  
  d.physio.grass <- d.physio.all
  d.physio.trees <- d.physio.all
  

  if (   length(unique(d.physio.all$species )  ) > 1 ) {  d.physio<- d.physio.all[ d.physio.all$species == species.str.id.all   ,]
  } else { d.physio.all$species <- species.str.id.all ; d.physio <- d.physio.all }

  
  
  
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
      
      
      # Yield variables
    #  if ( d != d.physio$date.time[1] ){
        
     # prev.day <- as.Date(d) - 1
      
      #d.physio[d.physio$date.time == d , 'ag.biom.grass.yield.kg.m2.yr'] <-   ( d.physio[d.physio$date.time == d , 'ag.biom.grass.kg.m2'] - d.physio[d.physio$date.time ==  prev.day , 'ag.biom.grass.kg.m2'])
      
      
      
      
     # }
    }
  }
  
  
  
  
  # Convert to ha values
  d.physio$ag.biom.trees.kg.ha <- d.physio$ag.biom.trees.kg.m2 * cv.sq.m.2.ha
  #d.physio$bg.biom.trees.kg.ha <- d.physio$bg.biom.trees.kg.m2 * cv.sq.m.2.ha
  
  d.physio$ag.biom.grass.kg.ha <- d.physio$ag.biom.grass.kg.m2 * cv.sq.m.2.ha
  #d.physio$bg.biom.grass.kg.ha <- d.physio$bg.biom.grass.kg.m2 * cv.sq.m.2.ha
  
  
  d.physio$bg.biom.kg.ha  <- d.physio$bg.biom.kg.m2 * cv.sq.m.2.ha
  d.physio$ag.biom.kg.ha  <- d.physio$ag.biom.kg.m2 * cv.sq.m.2.ha
  
  
  
 # d.physio$ag.biom.grass.yield.kg.ha.yr <-  d.physio$ag.biom.grass.yield.kg.m2 * cv.sq.m.2.ha  * 365
  
  
  
 # d.physio[ !is.na(d.physio$ag.biom.grass.yield.kg.ha.yr)  & d.physio$ag.biom.grass.yield.kg.ha.yr <= 0 , 'ag.biom.grass.yield.kg.ha.yr' ]  <-  0
  
  
  
 # nrow(d.watr)
#  d.watr$date[1]
 # d.physio$date[1]
  #d.sl.chem$date[1]
  
 # d.watr$date[nrow(d.watr)]
#  d.physio$date[nrow(d.physio)]
  #d.sl.chem$date[nrow(d.sl.chem)]
  
 # nrow(d.physio)
  #nrow( d.sl.chem)
  #nrow( d.watr)
  
  names(d.watr)[3] <- 'date.time'
  names(d.watr)[5] <- 'precip.sim'
  names(d.watr)[7] <- 'et.sim.mm'
  names(d.watr)[26] <- 'sw.5'
  #names(d.watr)[27] <- 'sw.10'
  names(d.watr)[28] <- 'sw.15'
 # names(d.watr)[29] <- 'sw.20'
  names(d.watr)[30] <- 'sw.30'
#  names(d.watr)[31] <- 'sw.40'
#  names(d.watr)[32] <- 'sw.50'
 # names(d.watr)[33] <- 'sw.60'
  
  
  
  # Merged LDNDC data
  {
    
    d.all <- cbind( emis.auto = d.sl.chem$emis.auto , emis.hetero = d.sl.chem$emis.hetero , d.physio)
    
  
    

    # d.all <- cbind( d.all[d.all$date.time %in% d.watr$date , ] , d.watr[d.watr$date %in% d.all$date.time , ] )
    
    
    
    for (d in d.all$date.time){
      
      d.all[d == d.all$date.time , 'sw.5'] <- d.watr[d.watr$date.time == d, 'sw.5']
      d.all[d == d.all$date.time , 'sw.15'] <- d.watr[d.watr$date.time == d, 'sw.15'][1] 
      d.all[d == d.all$date.time , 'sw.30'] <- d.watr[d.watr$date.time == d, 'sw.30'][1] 
      
      
    }
    
    
    
    
   # names(d.all)[1] <- 'emis.hetero'
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
  
  
  
  d.all <<- d.all
  
}

