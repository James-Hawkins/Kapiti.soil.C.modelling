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
  
  
  
  d.all$omit.period.2 <- FALSE
  d.all[d.all$date.time > d.gap.2.period.start & d.all$date.time < d.gap.2.period.end & !is.na(d.all$date.time) , 'omit.period.2'] <- TRUE
  
  
  
  
  
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
