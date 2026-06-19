
ect.biomass <- read_excel( 'Kapiti.Biomass.Clippings.xlsx' , sheet = 'KAPITI_BIOMASS_CAGES_RAW_REVD')

ect.biomass <- as.data.frame(ect.biomass)


ect.biomass <- ect.biomass[ !is.na(ect.biomass$Biomass.weight.kg.harm) , ]
ect.biomass <- ect.biomass[ !is.na(ect.biomass$Date) , ]


ect.biomass$Date <- as.Date(ect.biomass$Date ,  format = "%Y-%m-%d")



# Biomass data
for (d in 1 : nrow(ect.biomass)  ){
  
  # d <- 100
  
  date <- ect.biomass[d, 'Date']
  
  c.cage <- ect.biomass[d, 'cage.ID']
  
  if ( is.na(c.cage) ){ next }
  
  series.labl <- str_c(  'osv.biom.Mg.ha.' , c.cage )
  series.labl.delta <- str_c(  'osv.biom.delta.Mg.ha.' , c.cage )
  
  
  cur.biom.Mg.ha.yr <- mean(na.omit(ect.biomass[ect.biomass$Date ==  date & ect.biomass$cage.ID == c.cage, 'biomass.dry.weight.Mg.ha']))
  
  d.all[  d.all$date ==  date ,   series.labl] <-   cur.biom.Mg.ha.yr
  
  all.preceding.dates <- d.all[d.all$date < date & !is.na(d.all[,series.labl])  ,   'date']
  nearest.date <- all.preceding.dates[length(all.preceding.dates)]
  
  
  cond.prev.date <- ( ect.biomass$Date == nearest.date & ect.biomass$cage.ID == c.cage )
  
  prev.date.biom.Mg.ha.yr <- mean(na.omit(ect.biomass[ cond.prev.date, 'biomass.dry.weight.Mg.ha']))
  
  
  delta.biom.Mg.ha.yr <-  cur.biom.Mg.ha.yr  - prev.date.biom.Mg.ha.yr 
  
  
  
  
  if (  !is.na(delta.biom.Mg.ha.yr )) {
    
    str.c.id.t.mns.1 <- str_c(  'date.t.mns.1.' , c.cage )
    d.all[d.all$date ==  date ,  str.c.id.t.mns.1 ]  <-  nearest.date 
 
       if (delta.biom.Mg.ha.yr <=0 ) {  d.all[d.all$date ==  date ,    series.labl.delta] <- 0
  } else{d.all[d.all$date ==  date ,    series.labl.delta] <-    delta.biom.Mg.ha.yr
  }
  } else { d.all[d.all$date ==  date ,    series.labl.delta] <-  NA}

  
  
 
}



for (d in d.all$date) {
  
# d <- d.all$date[1]
  
  values <- c()
  
  lm1 <- na.omit(d.all[d.all$date ==  d &  !is.na(d.all$osv.biom.delta.Mg.ha.LM1) ,  'osv.biom.delta.Mg.ha.LM1' ])
  ls1 <- na.omit(d.all[d.all$date ==  d &  !is.na(d.all$osv.biom.delta.Mg.ha.LS1) ,  'osv.biom.delta.Mg.ha.LS1' ])
  
  lm2 <- na.omit(d.all[d.all$date ==  d &  !is.na(d.all$osv.biom.delta.Mg.ha.LM2) ,  'osv.biom.delta.Mg.ha.LM2' ])
  ls2 <- na.omit(d.all[d.all$date ==  d &  !is.na(d.all$osv.biom.delta.Mg.ha.LS2) ,  'osv.biom.delta.Mg.ha.LS2' ])
  
  lm3 <- na.omit(d.all[d.all$date ==  d &  !is.na(d.all$osv.biom.delta.Mg.ha.LM3) ,  'osv.biom.delta.Mg.ha.LM3' ])
  ls3 <- na.omit(d.all[d.all$date ==  d &  !is.na(d.all$osv.biom.delta.Mg.ha.LS3) ,  'osv.biom.delta.Mg.ha.LS3' ])
  
  lm4 <- na.omit(d.all[d.all$date ==  d &  !is.na(d.all$osv.biom.delta.Mg.ha.LM4) ,  'osv.biom.delta.Mg.ha.LM4' ])
  ls4 <- na.omit(d.all[d.all$date ==  d &  !is.na(d.all$osv.biom.delta.Mg.ha.LS4) ,  'osv.biom.delta.Mg.ha.LS4' ])
  
      
  if(  length(lm1) >0  ) { values <- append(values ,   lm1  )}
  if(  length(ls1) >0  ) { values <- append(values ,   ls1  )}
  
  if(  length(lm2) >0  ) { values <- append(values ,   lm2  )}
  if(  length(ls2) >0  ) { values <- append(values ,   ls2  )}
  
  if(  length(lm3) >0  ) { values <- append(values ,   lm3  )}
  if(  length(ls3) >0  ) { values <- append(values ,   ls3  )}
  
  if(  length(lm4) >0  ) { values <- append(values ,   lm4  )}
  if(  length(ls4) >0  ) { values <- append(values ,   ls4  )}
  
  if (!is.null(values)) { 
    
    mean <- mean(values)
    d.all[d.all$date ==  d ,    'biom.delta.mean.Mg.ha.yr'] <-  mean
    
  } else {next}

  
}


#  unique(d.all[ ,    'biom.delta.mean.Mg.ha.yr'])

#unique(d.all$osv.biom.delta.Mg.ha.LM2)

d.all <<-   d.all
