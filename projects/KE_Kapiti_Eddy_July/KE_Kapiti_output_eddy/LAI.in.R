# LAI data
d.lai <- function(){
  
  # nrow(d.lai)
  
  d.lai <- d.lai[
    d.lai$date >= start.date.cald
    &  d.lai$date <= end.date.cald
    ,  ]
  
  nrow(d.all)
  nrow(d.lai)
  
  
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

