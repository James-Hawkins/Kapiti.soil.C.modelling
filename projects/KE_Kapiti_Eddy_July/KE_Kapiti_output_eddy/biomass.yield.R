

# For each cage specifically

ect.biom.lm4 <- read_excel( 'Kapiti.Biomass.Clippings.xlsx' , sheet = 'LM4_BIOM_DELTA')
ect.biom.lm2 <- read_excel( 'Kapiti.Biomass.Clippings.xlsx' , sheet = 'LM2_BIOM_DELTA')

ect.biom.lm4 <- as.data.frame(ect.biom.lm4 )
ect.biom.lm2 <- as.data.frame(ect.biom.lm2)

ect.biom.lm4  <- ect.biom.lm4[ !is.na(ect.biom.lm4$delta.biomass.dry.weight.Mg.ha.harm) , ]
ect.biom.lm4  <- ect.biom.lm4[ !is.na(ect.biom.lm4$Date) , ]

ect.biom.lm2  <- ect.biom.lm2[ !is.na(ect.biom.lm2$delta.biomass.dry.weight.Mg.ha.harm) , ]
ect.biom.lm2  <- ect.biom.lm2[ !is.na(ect.biom.lm2$Date) , ]

ect.biom.lm4$Date <- as.Date(ect.biom.lm4$Date ,  format = "%Y-%m-%d")
ect.biom.lm2$Date <- as.Date(ect.biom.lm2$Date ,  format = "%Y-%m-%d")

# 
LM2.dates <- ect.biom.lm2$Date
LM4.dates <- ect.biom.lm4$Date

for (d in d.all$date){
  
  # d <- "2024-03-26"
  
  if ( d > LM4.dates[1] ){
    
    dates.less <- LM4.dates[  (d > (LM4.dates))  ]
    date.floor <- LM4.dates[  (d > (LM4.dates))  ][length( dates.less)]
    date.ceiling <- LM4.dates[  (d < (LM4.dates))  ][1]
    
    if (!is.na(date.floor) && !is.na(date.ceiling)) {
      yield <- ect.biom.lm4[  ect.biom.lm4$Date == date.floor  , 'delta.biomass.dry.weight.Mg.ha.harm']
      
      idx <- which(d.all$date > date.floor & d.all$date < date.ceiling)
      if (length(idx) > 0) {
        d.all[idx, 'biom.yield.Mg.ha.yr.LM4'] <- yield
        d.all[idx, 'sim.biom.yield.Mg.ha.yr.LM4'] <- mean(d.all[idx, 'ag.biom.grass.yield.kg.ha.yr'] / 1000, na.rm = TRUE)
      }
    }
    
  }
  
  if ( d > LM2.dates[1] ){
    
    dates.less <- LM2.dates[  (d > (LM2.dates))  ]
    date.floor <- LM2.dates[  (d > (LM2.dates))  ][length( dates.less)]
    date.ceiling <- LM2.dates[  (d < (LM2.dates))  ][1]
    
    if (!is.na(date.floor) && !is.na(date.ceiling)) {
      yield <- ect.biom.lm2[  ect.biom.lm2$Date == date.floor  , 'delta.biomass.dry.weight.Mg.ha.harm']
      
      idx <- which(d.all$date > date.floor & d.all$date < date.ceiling)
      if (length(idx) > 0) {
        d.all[idx, 'biom.yield.Mg.ha.yr.LM2'] <- yield
        d.all[idx, 'sim.biom.yield.Mg.ha.yr.LM2'] <- mean(d.all[idx, 'ag.biom.grass.yield.kg.ha.yr'] / 1000, na.rm = TRUE)
      }
    }
  }
  
  
  
}


# unique(d.all$biom.yield.Mg.ha.yr.LM4)
unique(d.all$biom.yield.Mg.ha.yr.LM2)

