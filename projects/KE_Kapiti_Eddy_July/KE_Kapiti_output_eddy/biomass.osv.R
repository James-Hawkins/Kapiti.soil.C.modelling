

biomass <-  read_excel("RAMONA_biomass_TOTAL+JH.xlsx", sheet = 'Circle_9')


colnames(biomass)[8] <- 'biom.osv.kg.ha'


biom.osv.unique.months <- unique(biomass$Month)


biomass <- as.data.frame(biomass)
biomass$biom.osv.kg.ha


biomass.period.start <- biom.osv.unique.months[1]
biomass.period.end <-  biom.osv.unique.months[length(biom.osv.unique.months)]
