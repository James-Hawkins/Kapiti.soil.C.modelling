# Period subsetting
{
  d.all$covid <- NA
  

  covid.stats.pre <- 'Pre-covid'
  covid.stats.post <- 'Post-covid'
  
  
  covid.start.date <<- "2020-03-14"
  covid.end.date <<- "2022-03-07"
  covid.end.valid.date <- "2022-07-01"
  
  d.gap.2.period.start <- "2023-01-25"
  d.gap.2.period.end <- "2023-04-20"
  
  
  el.nino.period.start <- "2023-06-15"
  el.nino.period.end <- "2024-04-30"
  el.nino.period.mid <- "2023-12-30"
  
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
  
  
  
  
  d.gap.2.period.start <- "2023-01-25"
  d.gap.2.period.end <- "2023-04-20"
  
  
  boma.period.start <<- "2021-01-01"
  boma.period.end <<- "2022-01-01"
  boma.period.mid <<-  "2021-06-15"
  
  d.all$omit.period.2 <- FALSE
  d.all[d.all$date.time > d.gap.2.period.start & d.all$date.time < d.gap.2.period.end & !is.na(d.all$date.time) , 'omit.period.2'] <- TRUE
  
  
  
  
  

  period.dipole <<- 'pluvial'
  period.drought <<- 'drought'
  period.normal <<- 'normal'
  period.all <<- 'all'
  
  d.all$period <-   period.normal 
  
  # Dipole
  d.all[d.all$date.time >= dipole.period.start &  d.all$date.time <=  dipole.period.valid.end & !is.na(d.all$date.time), 'period'] <- period.dipole
  
  # Drought
  d.all[d.all$date.time >  drought.period.start & d.all$date.time <=  drought.period.end & !is.na(d.all$date.time), 'period'] <- period.drought
  
  
  # Other
  d.all[ d.all$period  !="drought"  & d.all$period  !="dipole"  &  d.all$date.time <=  drought.period.start & d.all$date.time >=  post.drought.period.end, 'period'] <- period.normal
  
  d.all[  d.all$date.time >=  drought.period.end & d.all$date.time <=  post.drought.period.end, 'period'] <- NA
  
  unique(d.all$period)
  
  #d.all[d.all$date.time >  dipole.period.valid.end & d.all$date.time <= drought.period.start & !is.na(d.all$date.time), 'period'] <- period.normal
  
  #d.all[d.all$date.time >  drought.period.end & !is.na(d.all$date.time), 'period'] <- period.normal
  
  
  # Agricultural drought method
  
 
  d.all$period.ag.drt <- periods.ag.drought[3] 
  
  d.all[  d.all$r.a.swc.15.30d.cm.sim <= 15  , 'period.ag.drt'] <-  periods.ag.drought[1] 
  d.all[  d.all$r.a.swc.15.30d.cm.sim  > 19  , 'period.ag.drt'] <-  periods.ag.drought[2] 
  #d.all[  d.all$r.a.swc.15.cm.sim <= 27  , 'period.ag.drt'] <-  periods.ag.drought[3] 
  
  
 # unique(d.all$period.ag.drt)
  
  
}



#summary(d.all[d.all$period.ag.drt == periods.ag.drought[1]  , 'r.a.swc.15.30d.cm.sim'])
#summary(d.all[d.all$period.ag.drt == periods.ag.drought[2]  , 'r.a.swc.15.30d.cm.sim'])
#summary(d.all[d.all$period.ag.drt == periods.ag.drought[3]  , 'r.a.swc.15.30d.cm.sim'])

#summary(d.all[d.all$period.ag.drt == periods.ag.drought[1]  , 'precip.osv'])
#summary(d.all[d.all$period.ag.drt == periods.ag.drought[2]  , 'precip.osv'])
#summary(d.all[d.all$period.ag.drt == periods.ag.drought[3]  , 'precip.osv'])




d.all <<- d.all