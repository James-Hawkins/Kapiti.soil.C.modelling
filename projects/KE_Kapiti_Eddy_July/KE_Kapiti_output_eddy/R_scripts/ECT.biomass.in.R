
ect.biomass <- read_excel( 'Kapiti.Biomass.Clippings.xlsx' , sheet = 'KAPITI_BIOMASS_CAGES_RAW_REVD')

ect.biomass <- as.data.frame(ect.biomass)


ect.biomass <- ect.biomass[ !is.na(ect.biomass$Biomass.weight.kg.harm) , ]
ect.biomass <- ect.biomass[ !is.na(ect.biomass$Date) , ]


colnames(ect.biomass)


# Biomass data
for (d in 1 : nrow(ect.biomass)  ){
  
  # d <- 1
  
  date <- ect.biomass[d, 'Date']
  
  c.cage <- ect.biomass[d, 'cage.ID']
  
  if ( is.na(c.cage) ){ next }
  
  series.labl <- str_c(  'osv.biom.Mg.ha.' , c.cage )
  
  d.all[d.all$date ==  date ,   series.labl] <- mean(na.omit(ect.biomass[ect.biomass$Date ==  date & ect.biomass$cage.ID == c.cage, 'biomass.dry.weight.Mg.ha']))
  
  
}
