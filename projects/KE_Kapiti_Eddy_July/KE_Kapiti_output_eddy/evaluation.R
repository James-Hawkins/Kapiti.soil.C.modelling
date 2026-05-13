# Evaluation
{
  
  old.eval <- function(){  
    
    # MEAN SQUARED DEVIATION
    msd.ter.osv.pre.c <- sum( na.omit((d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim']   - d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv']  )^2 ) ) /sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
    
    
    
    # MEAN BIASES
    
    mb.lai.dipole <- sum( na.omit(( d.all[ all.condition.dipole , 'r.a.lai.sim'] - d.all[ all.condition.dipole , 'r.a.lai.osv']   )))   / sum( !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & all.condition.dipole)
    mb.lai.drought <- sum( na.omit(( d.all[ all.condition.drought , 'r.a.lai.sim'] - d.all[ all.condition.dipole , 'r.a.lai.osv']   )))   / sum( !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & all.condition.dipole)
    mb.lai.normal <- sum( na.omit(( d.all[ all.condition.normal , 'r.a.lai.sim'] - d.all[ all.condition.dipole , 'r.a.lai.osv']   )))   / sum( !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & all.condition.dipole)
    
    
    
    
    
    
    mb.lai.post.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))
    mb.lai.all  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))
    
    # SQUARED BIAS
    sb.ter.pre.c <-  (  mean( na.omit(d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim'] ))  - mean( na.omit(d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv'] )))^2 
    
    
    # SDSD 
    
    # TER
    sd.ter.sim.pre.c <- sd( na.omit(d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim']) )
    sd.ter.osv.pre.c <- sd( na.omit(d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv']) )
    
    sdsd.ter.pre.c <- ( sd.ter.sim.pre.c - sd.ter.osv.pre.c)^2
    
    
    
    
    
    
    
    
    # TER
    mb.ter.pre.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
    mb.ter.post.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
    mb.ter.all  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
    
    
    mb.ter.2018  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2018) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2018), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2018))
    mb.ter.2019  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2019) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2019), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2019))
    mb.ter.2020  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2020) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2020), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2020))
    mb.ter.2021  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2021) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2021), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2021))
    mb.ter.2022  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2022) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2022), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2022))
    mb.ter.2023  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2023) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2023), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2023))
    mb.ter.2024  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2024) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2024), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2024))
    
    mb.ter.norm.weath <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) & cond.year.norm.weath & !is.na(d.all$r.a.ter.osv) , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2) &  cond.year.norm.weath & !is.na(d.all$r.a.ter.osv) , 'r.a.ter.osv']   )))   / nrow(d.all[d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2) &  cond.year.norm.weath,])
    
    
    # GPP
    mb.gpp.pre.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2))
    mb.gpp.post.c <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2)  , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2))
    mb.gpp.all  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2))
    
    
    mb.gpp.2018  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2018) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2018), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2018))
    mb.gpp.2019  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2019) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2019), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2019))
    mb.gpp.2020  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2020) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2020), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2020))
    mb.gpp.2021  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2021) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2021), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2021))
    mb.gpp.2022  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2022) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2022), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2022))
    mb.gpp.2023  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2023) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2023), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2023))
    mb.gpp.2024  <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2024) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2)  &   str_detect(d.all$year.month, year.2024), 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &   str_detect(d.all$year.month, year.2024))
    
    mb.gpp.norm.weath <- sum( na.omit(( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2) & cond.year.norm.weath & !is.na(d.all$r.a.gpp.osv) , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  &  !(d.all$omit.period.2) &  cond.year.norm.weath & !is.na(d.all$r.a.gpp.osv) , 'r.a.gpp.osv']   )))   / nrow(d.all[d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) &  cond.year.norm.weath,])
    
    
    
    
    
    # RMSE
    rmse.lai.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2)  , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))
    rmse.lai.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.lai.osv) & !(d.all$omit.period.2))
    rmse.lai.all <- sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2), 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2), 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & !is.na(d.all$r.a.lai.osv)& !is.na(d.all$r.a.lai.sim) & !(d.all$omit.period.2))
    
    rmse.swc.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2) , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.5.cm.osv) & !is.na(d.all$r.a.swc.5.cm.sim) & !(d.all$omit.period.2))
    rmse.swc.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.swc.5.cm.osv)& !is.na(d.all$r.a.swc.5.cm.sim) & !(d.all$omit.period.2))
    rmse.swc.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual   & !(d.all$omit.period.2), 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.swc.5.cm.osv)& !is.na(d.all$r.a.swc.5.cm.sim) & !(d.all$omit.period.2))
    
    
    rmse.ter.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
    rmse.ter.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim)& !(d.all$omit.period.2))
    rmse.ter.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & !(d.all$omit.period.2))
    
    rmse.ter.2018 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2018) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2018) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2018) & !(d.all$omit.period.2))
    rmse.ter.2019 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2019) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2019) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2019) & !(d.all$omit.period.2))
    rmse.ter.2020 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2020) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2020) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2020) & !(d.all$omit.period.2))
    rmse.ter.2021 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2021) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2021) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2021) & !(d.all$omit.period.2))
    rmse.ter.2022 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2022) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2022) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2022) & !(d.all$omit.period.2))
    rmse.ter.2023 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2023) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2023) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2023) & !(d.all$omit.period.2))
    rmse.ter.2024 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2), 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2) , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.ter.osv)& !is.na(d.all$r.a.ter.sim) & str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2))
    
    
    rmse.gpp.pre.c <-  sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim))
    rmse.gpp.post.c <-    sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2) )
    rmse.gpp.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2), 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2) , 'r.a.gpp.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.gpp.osv)& !is.na(d.all$r.a.gpp.sim) & !(d.all$omit.period.2))
    #rmse.gpp.2024 <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual &   str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2), 'r.a.gpp.sim'] - d.all[ d.all$variable.status == v.status.actual & str_detect(d.all$year.month, year.2024) &  !(d.all$omit.period.2)  , 'r.a.gpp.osv']   )))   / sum(!(d.all$omit.period.2)d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.gpp.osv)& !is.na(d.all$r.a.gpp.sim) & str_detect(d.all$year.month, year.2024) & !(d.all$omit.period.2))
    
    
    rmse.nee.pre.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.nee.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.nee.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.nee.osv) & !is.na(d.all$r.a.nee.sim) & !(d.all$omit.period.2))
    rmse.nee.post.c <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.nee.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.nee.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.post & !is.na(d.all$r.a.nee.osv)& !is.na(d.all$r.a.nee.sim) & !(d.all$omit.period.2))
    rmse.nee.all <-   sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual , 'r.a.nee.sim'] - d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.nee.osv']   )))   / sum(d.all$variable.status == v.status.actual  & !is.na(d.all$r.a.nee.osv)& !is.na(d.all$r.a.nee.sim) & !(d.all$omit.period.2))
    
    
    
    # NRMSE
    nrmse.lai.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.lai.osv'] ))) * rmse.lai.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.lai.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.lai.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.lai.osv) & !is.na(d.all$r.a.lai.sim))
    nrmse.lai.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.lai.osv'] ))) *  rmse.lai.post.c
    nrmse.lai.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & !(d.all$omit.period.2), 'r.a.lai.osv'] ))) *  rmse.lai.all
    
    
    nrmse.swc.5.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.5.cm.osv'] ))) * rmse.swc.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.5.cm.osv) & !is.na(d.all$r.a.swc.5.cm.sim))
    nrmse.swc.5.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.swc.5.cm.osv'] ))) *  rmse.swc.post.c
    nrmse.swc.5.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.swc.5.cm.osv'] ))) *  rmse.swc.all
    
    
    nrmse.swc.15.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.15.cm.osv'] ))) * rmse.swc.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.5.cm.osv) & !is.na(d.all$r.a.swc.5.cm.sim))
    nrmse.swc.15.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.swc.15.cm.osv'] ))) *  rmse.swc.post.c
    nrmse.swc.15.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.swc.15.cm.osv'] ))) *  rmse.swc.all
    
    
    nrmse.swc.30.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.swc.30.cm.osv'] ))) * rmse.swc.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.swc.5.cm.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.swc.5.cm.osv) & !is.na(d.all$r.a.swc.5.cm.sim))
    nrmse.swc.30.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2) , 'r.a.swc.30.cm.osv'] ))) *  rmse.swc.post.c
    nrmse.swc.30.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.swc.30.cm.osv'] ))) *  rmse.swc.all
    
    
    nrmse.ter.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.ter.osv'] ))) * rmse.ter.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.ter.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.ter.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.ter.osv) & !is.na(d.all$r.a.ter.sim))
    nrmse.ter.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post , 'r.a.ter.osv'] ))) *  rmse.ter.post.c
    nrmse.ter.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  , 'r.a.ter.osv'] ))) *  rmse.ter.all
    
    
    nrmse.ter.2018  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2018)  , 'r.a.ter.osv'] ))) *  rmse.ter.2018
    nrmse.ter.2019 <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2019)  , 'r.a.ter.osv'] ))) *  rmse.ter.2019
    nrmse.ter.2020  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2020)  , 'r.a.ter.osv'] ))) *  rmse.ter.2020
    nrmse.ter.2021  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2021)  , 'r.a.ter.osv'] ))) *  rmse.ter.2021
    nrmse.ter.2022  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2022)  , 'r.a.ter.osv'] ))) *  rmse.ter.2022
    nrmse.ter.2023  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2023)  , 'r.a.ter.osv'] ))) *  rmse.ter.2023
    nrmse.ter.2024  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.ter.osv) &   str_detect(d.all$year.month, year.2024)  , 'r.a.ter.osv'] ))) *  rmse.ter.2024
    
    
    nrmse.gpp.pre.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid == covid.stats.pre & !is.na(d.all$r.a.gpp.osv) , 'r.a.gpp.osv'] ))) * rmse.gpp.pre.c 
    nrmse.gpp.post.c <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post &  !is.na(d.all$r.a.gpp.osv) & !is.na(d.all$r.a.gpp.sim) , 'r.a.gpp.osv'] ))) *  rmse.gpp.post.c
    nrmse.gpp.all  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv)  , 'r.a.gpp.osv'] ))) *  rmse.gpp.all
    #nrmse.gpp.2024  <-  100* (1 / mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !is.na(d.all$r.a.gpp.osv) &   str_detect(d.all$year.month, year.2024)  , 'r.a.gpp.osv'] ))) *  rmse.gpp.2024
    
    
    nrmse.nee.pre.c <-  100* (1 / abs(mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre & !(d.all$omit.period.2), 'r.a.nee.osv'] )))) * rmse.nee.pre.c # sum( na.omit(abs( d.all[ d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre , 'r.a.nee.sim'] - d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.pre , 'r.a.nee.osv']   )))   / sum(d.all$variable.status == v.status.actual & d.all$covid.valid== covid.stats.pre & !is.na(d.all$r.a.nee.osv) & !is.na(d.all$r.a.nee.sim))
    nrmse.nee.post.c <-  100* (1 / abs(mean(na.omit(d.all[ d.all$variable.status == v.status.actual  & d.all$covid.valid== covid.stats.post & !(d.all$omit.period.2), 'r.a.nee.osv'] )))) *  rmse.nee.post.c
    nrmse.nee.all  <-  100* (1 / abs(mean(na.omit(d.all[ d.all$variable.status == v.status.actual & !(d.all$omit.period.2)  , 'r.a.nee.osv'] ))) )*  rmse.nee.all
    
  }
  
  
  metrics <- data.frame(
    osv.variable = rep(osv.metric.vars,4) 
    , sim.variable =  rep(sim.metric.vars,4) 
    , sim.variable.bc = rep(sim.metric.vars.bc,4) 
    , period =  c( rep(period.dipole,length(sim.metric.vars)) , rep(period.drought,length(sim.metric.vars))  , rep(period.normal,length(sim.metric.vars) ) , rep(period.all,length(sim.metric.vars) ) )
    
    
    ,r2 = NA
    ,rmse = NA
    ,nrmse = NA
    
    , valid.text = NA
  )
  
  d.all[, 'period.status'] <- NA
  
  
  for (r in 1:nrow(metrics)){
    
    # r <-1 
    
    osv.var <- metrics[r,'osv.variable']
    sim.var  <- metrics[r,'sim.variable']
    sim.var.bc  <- metrics[r,'sim.variable.bc']
    cur.period <- metrics[r,'period']
    
    if (cur.period == period.dipole) {condition <- all.condition.dipole}
    if (cur.period == period.drought) {condition <- all.condition.drought}
    if (cur.period == period.normal) {condition <- all.condition.normal}
    if (cur.period == period.all) {condition <- all.condition.all}
    
    no.na.condition <-   !is.na(d.all[,osv.var])
    
    metrics[r , 'r2'] <- cor(  d.all[condition & no.na.condition  , osv.var ] , d.all[ condition & no.na.condition, sim.var]   , method = cor.type  )
    metrics[r , 'r2.bc'] <- cor(  d.all[condition & no.na.condition , osv.var ] , d.all[ condition & no.na.condition, sim.var.bc ]   , method = cor.type  )
    
    metrics[r , 'r2'] <- round(  metrics[r , 'r2'] , 2)
    metrics[r , 'r2.bc'] <- round(  metrics[r , 'r2.bc'] , 2)
    
    metrics[r , 'rmse'] <-   sum( na.omit(abs( d.all[ condition , osv.var ] - d.all[ condition ,  sim.var]   )))   / sum(condition & !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var]))
    metrics[r , 'rmse.bc'] <-   sum( na.omit(abs( d.all[ condition , osv.var ] - d.all[ condition ,  sim.var.bc ]   )))   / sum(condition & !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var.bc ]))
    
    metrics[r , 'nrmse'] <-  100* (1 / abs(mean(na.omit(d.all[ condition , osv.var] ))) )*    metrics[r , 'rmse'] 
    metrics[r , 'nrmse.bc'] <-  100* (1 / abs(mean(na.omit(d.all[ condition , osv.var] ))) )*    metrics[r , 'rmse.bc'] 
    
    metrics[r , 'rmse'] <-   round(  metrics[r , 'rmse'] , 1)
    metrics[r , 'rmse.bc'] <-   round(  metrics[r , 'rmse.bc'] , 1)
    
    metrics[r , 'nrmse'] <-   round(  metrics[r , 'nrmse'] , 1)
    metrics[r , 'nrmse.bc'] <-   round(  metrics[r , 'nrmse.bc'] , 1)
    
    period.status <- str_c( 'period.' , var [r] )
    
    if (cur.period == period.dipole){cur.period.label <- period.label[1] }
    if (cur.period == period.drought){cur.period.label <- period.label[2] }
    if (cur.period == period.normal){cur.period.label <- period.label[3] }
    
    
    valid.text <- str_c(  cur.period.label,': r = ',  metrics[r , 'r2'] , ' (' , metrics[r , 'r2.bc'] , ')' , ', RMSE = ', metrics[r , 'rmse'] , ' (' , metrics[r , 'rmse.bc'] , ')' , ', nRMSE = ', metrics[r , 'nrmse'] , ' (' , metrics[r , 'nrmse.bc'] , ') ' , '%')
    
    
    d.all[d.all$period == cur.period, 'period.status'] <- valid.text
    
    metrics[r , 'valid.text'] <-  valid.text
    
  }
  
  # Biomass
  mean.biomass.grass.1.pre.c <- mean(  d.all[ d.all$covid == covid.stats.pre &  !is.na(d.all$ag.biom.grass.1.kg.ha) , 'ag.biom.grass.1.kg.ha']  )
  mean.biomass.grass.2.pre.c <- mean(  d.all[ d.all$covid == covid.stats.pre &  !is.na(d.all$ag.biom.grass.2.kg.ha) , 'ag.biom.grass.2.kg.ha']  )
  
  mean.biomass.grass.1.post.c <- mean(  d.all[ d.all$covid == covid.stats.post &  !is.na(d.all$ag.biom.grass.1.kg.ha) , 'ag.biom.grass.1.kg.ha']  )
  mean.biomass.grass.2.post.c <- mean(  d.all[ d.all$covid == covid.stats.post &  !is.na(d.all$ag.biom.grass.2.kg.ha) , 'ag.biom.grass.2.kg.ha']  )
  
  
  # Round 
  mean.biomass.grass.1.pre.c <- round(mean.biomass.grass.1.pre.c ,0)
  mean.biomass.grass.2.pre.c <- round(mean.biomass.grass.2.pre.c ,0)
  
  mean.biomass.grass.1.post.c <- round(mean.biomass.grass.1.post.c ,0)
  mean.biomass.grass.2.post.c <- round(mean.biomass.grass.2.post.c ,0)
  
  
}