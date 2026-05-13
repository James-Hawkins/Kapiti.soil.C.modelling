
d.all$year.month <- NA
d.all$biom.osv.kg.ha <- NA


for (r in 1:nrow(d.all)){
  
  curr.date <- d.all[r,'date.time']
  curr.date.month.year <- substr(  curr.date , 1,7)
  
  d.all[r,'year.month'] <- curr.date.month.year 
  
  if (curr.date.month.year %in% biom.osv.unique.months){
    
    d.all[r,'biom.osv.kg.ha'] <- mean(  biomass[biomass$Month %in% curr.date.month.year ,  'biom.osv.kg.ha'])
    
    precip.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'precip']
    precip.tah.621 <-  d.weather.subs.621[d.weather.subs.621$date == curr.date, 'precip']
    precip.tah.678 <-  d.weather.subs.678[d.weather.subs.678$date == curr.date, 'precip']
    precip.tah.814 <-  d.weather.subs.814[d.weather.subs.814$date == curr.date, 'precip']
    
    temp.min.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.min']
    temp.min.tah.621 <-  d.weather.subs.621[d.weather.subs.621$date == curr.date, 'temp.min']
    temp.min.tah.678 <-  d.weather.subs.678[d.weather.subs.678$date == curr.date, 'temp.min']
    temp.min.tah.814 <-  d.weather.subs.814[d.weather.subs.814$date == curr.date, 'temp.min']
    
    temp.avg.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.mn']
    temp.avg.tah.621 <-  d.weather.subs.621[d.weather.subs.621$date == curr.date, 'temp.mn']
    temp.avg.tah.678 <-  d.weather.subs.678[d.weather.subs.678$date == curr.date, 'temp.mn']
    temp.avg.tah.814 <-  d.weather.subs.814[d.weather.subs.814$date == curr.date, 'temp.mn']
    
    temp.max.tah.677 <-  d.weather.subs[d.weather.subs$date == curr.date, 'temp.max']
    temp.max.tah.621 <-  d.weather.subs.621[d.weather.subs.621$date == curr.date, 'temp.max']
    temp.max.tah.678 <-  d.weather.subs.678[d.weather.subs.678$date == curr.date, 'temp.max']
    temp.max.tah.814 <-  d.weather.subs.814[d.weather.subs.814$date == curr.date, 'temp.max']
    
    
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