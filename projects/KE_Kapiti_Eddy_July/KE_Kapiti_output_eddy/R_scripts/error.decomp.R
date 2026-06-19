# Bias detection and correction
{
  
  
 
  
  # Systematic validation metrics
  all.condition.pluvial <<- (d.all$period.ag.drt == periods.ag.drought.high & d.all$variable.status == v.status.actual & !(d.all$omit.period.2) & !is.na(d.all$period ))
  all.condition.drought <<- (d.all$period.ag.drt ==   periods.ag.drought.low & d.all$variable.status == v.status.actual & !(d.all$omit.period.2) & !is.na(d.all$period ))
  all.condition.drought.plus.covid <<- (d.all$period.ag.drt == periods.ag.drought.low  & d.all$variable.status == v.status.actual & !is.na(d.all$period ))
  
  all.condition.normal <<- (d.all$period.ag.drt == periods.ag.drought.norm & d.all$variable.status == v.status.actual & !(d.all$omit.period.2) & !is.na(d.all$period ))
  all.condition.all <<- (d.all$variable.status == v.status.actual & !(d.all$omit.period.2) & !is.na(d.all$period ))
  
  

  
  # Global conditions
  year.2018 <- "2018"
  year.2019 <- "2019"
  year.2020 <- "2020"
  year.2021 <- "2021"
  year.2022 <- "2022"
  year.2023 <- "2023"
  year.2024 <- "2024"
  
  
  cond.year.2018 <- (str_detect(d.all$year.month , year.2018))
  cond.year.2019 <- (str_detect(d.all$year.month , year.2019))
  cond.year.2020 <- (str_detect(d.all$year.month , year.2020))
  cond.year.2021 <- (str_detect(d.all$year.month , year.2021))
  cond.year.2022 <- (str_detect(d.all$year.month , year.2022))
  cond.year.2023 <- (str_detect(d.all$year.month , year.2023))
  cond.year.2024 <- (str_detect(d.all$year.month , year.2024))
  
  cond.year.norm.weath <- (
    
    cond.year.2019 
    | cond.year.2021
    | cond.year.2023
    | cond.year.2024
    
  )
  
  # BIAS DETECTION
  # Biases
  
  
  biases <- data.frame(
    osv.variable = rep(osv.metric.vars,4) 
    , sim.variable =  rep(sim.metric.vars,4) 
    , sim.variable.bc = rep(sim.metric.vars.bc,4) 
    , period =  c( rep(periods.ag.drought.high ,length(sim.metric.vars)) , rep(periods.ag.drought.low,length(sim.metric.vars))  , rep(periods.ag.drought.norm ,length(sim.metric.vars) ) , rep(period.all,length(sim.metric.vars) )  )
    , absolute.bias = NA
    , relative.sd = NA
  )
  
  
  
  for (r in 1:nrow(biases)  ) {
    
    # test: r <- 9
    
    cur.sim.var <- biases[ r, 'sim.variable' ]
    cur.period <- biases[ r, 'period' ]
    
    osv.var <- osv.metric.vars[  which(sim.metric.vars == cur.sim.var   )  ]
    
    if (cur.period == periods.ag.drought.high & osv.var != "r.a.herb.agb.osv"  ){ condition <- all.condition.pluvial} else if (cur.period == 'dipole' & osv.var == "r.a.herb.agb.osv"  ) { next}
    if (cur.period == periods.ag.drought.low & osv.var != "r.a.herb.agb.osv" ){ condition <- all.condition.drought}
    if (cur.period == periods.ag.drought.low & osv.var == "r.a.herb.agb.osv" ){ condition <- all.condition.drought.plus.covid  }
    
    
    if (cur.period == 'normal'){ condition <- all.condition.normal}
    if (cur.period == 'all'){ condition <- all.condition.all}
    
    # Bias is mean difference simulated minus observed
    # positive bias --> simd > observed --> must reduce simulated by amount of bias
    # negative bias --> simd < observed
    cur.bias <- sum( na.omit(( d.all[ condition ,  cur.sim.var ] - d.all[ condition , osv.var]   )))   / sum(condition)
    
    cur.rel.sd <-   sd(na.omit(d.all[ condition ,  cur.sim.var ])) / sd(na.omit(d.all[ condition ,  osv.var ]))
    
    # Kobayashi and Salam method
    
    mean.osv <- mean( na.omit(d.all[ condition , osv.var] ))
    mean.sim <- mean( na.omit(d.all[ condition , cur.sim.var] ))
    
    sd.osv <- sd( na.omit(d.all[ condition , osv.var] ))
    sd.sim <- sd( na.omit(d.all[ condition , cur.sim.var] ))
    
    Rp <- cor( d.all[ condition , osv.var]  , d.all[ condition , cur.sim.var] , method = 'pearson')
    
    #cor( 1 , 1.5 , method = 'pearson')
    
    # MSD = SB + SDSD + LCS
    
    
    sb <- (mean.sim - mean.osv)^2 # sb
    sdsd <- ( sd.sim - sd.osv)^2  # sdsd
    
    lcs <- 2 * sd.sim * sd.osv * ( 1 - Rp)
    
    msd <- sb + sdsd + lcs
    
    biases[ r, 'squared.bias'] <- sb
    biases[ r, 'sqd.diff.sd'] <- sdsd 
    biases[ r, 'lcs'] <- lcs
    biases[ r, 'msd'] <- msd
    
    biases[ r, 'mean.osv'] <- mean.osv
    biases[ r, 'mean.sim'] <- mean.sim
    
    biases[ r, 'sd.osv'] <- sd.osv
    biases[ r, 'sd.sim'] <- sd.sim 
    
    biases[ r, 'absolute.bias'] <- cur.bias
    biases[ r, 'relative.sd'] <- cur.rel.sd
    
    
    
    
  }
  
  
  # Dataframe to plot absolute biases
  biases.long <- biases %>%
    pivot_longer(cols = c('squared.bias' , 'sqd.diff.sd' ,  'lcs'  ) 
                 , names_to = "error.catg"
                 , values_to = "error"
    )
  
  biases.long <- as.data.frame(biases.long )
  
  error.types <- unique( biases.long$error.catg)
  biases.long[biases.long$error.catg == error.types[1], 'error.type.label'] <- 'MB'
  biases.long[biases.long$error.catg == error.types[2], 'error.type.label'] <-'SDSD'
  biases.long[biases.long$error.catg == error.types[3], 'error.type.label'] <- 'LCS'
  
  biases.long$error.catg <- factor(biases.long$error.type.label , levels = c("MB" ,"SDSD" ,"LCS"  ))
  
  
  periods <- unique( biases.long$period)
  biases.long[biases.long$period == periods.ag.drought[2] , 'period.label'] <- 'Dipole'
  biases.long[biases.long$period  == periods.ag.drought[1], 'period.label'] <-'Drought'
  biases.long[biases.long$period  == periods.ag.drought[3], 'period.label'] <- 'Normal'
  biases.long[biases.long$period  == period.all, 'period.label'] <- 'All'
  
  
  
  bias.cond.ter <- biases.long$osv.variable == 'r.a.ter.osv' 
  bias.cond.gpp <- biases.long$osv.variable == 'r.a.gpp.osv'
  bias.cond.swc <- biases.long$osv.variable == 'r.a.swc.5.cm.osv' 
  
  
  # BIAS CORRECTION
  {
    
    
    for (r in 1:nrow(d.all)){
      for (v in sim.metric.vars.bc){
        
        
        # test: v <- sim.metric.vars.bc[8]
        
        cur.period <- d.all[r,'period.ag.drt'] 
        
        if ( is.na(cur.period)) { next}
        
        raw.sim.var <- sim.metric.vars[ which(sim.metric.vars.bc == v)   ]
        
        
        cur.sim.var <- d.all[ r ,raw.sim.var]
        
        bias.df.cond <- (biases$sim.variable == raw.sim.var & biases$period == cur.period)
        
        bias.correction.factor.mean <- biases[ bias.df.cond, 'absolute.bias']
        
        bias.correction.factor.sd <- biases[ bias.df.cond, 'relative.sd']
        
        mean.value <- mean( d.all[ d.all$period == cur.period  ,raw.sim.var])
        
        #d.all[r,v] <-  1/ bias.correction.factor.sd * (d.all[ r ,raw.sim.var] - mean.value) + mean.value  - bias.correction.factor.mean 
        
        # Kobayashi method
        mean.osv <- biases[ bias.df.cond, 'mean.osv'] 
        mean.sim <- biases[ bias.df.cond, 'mean.sim'] 
        sd.osv <- biases[ bias.df.cond, 'sd.osv'] 
        sd.sim <- biases[ bias.df.cond, 'sd.sim'] 
        
        
        
        d.all[r,v] <- mean.osv + ( cur.sim.var - mean.sim ) * (sd.osv / sd.sim)
        
        #d.all[ r ,raw.sim.var]  - bias.correction.factor.mean 
        
        
        
      }}
    
    # Evaluate bias corrected vs. raw
    mean(d.all[ !is.na(d.all$period ) & d.all$period == period.dipole , 'r.a.ter.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.dipole, 'r.a.ter.sim']) 
    mean(d.all[ !is.na(d.all$period ) & d.all$period == period.drought , 'r.a.ter.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.drought, 'r.a.ter.sim']) 
    mean(d.all[!is.na(d.all$period ) &  d.all$period == period.normal , 'r.a.ter.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.normal, 'r.a.ter.sim']) 
    
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.dipole , 'r.a.ter.sim.bc']) / sd(d.all[ !is.na(d.all$period ) & d.all$period == period.dipole, 'r.a.ter.sim']) 
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.drought , 'r.a.ter.sim.bc']) /  sd(d.all[ !is.na(d.all$period ) & d.all$period == period.drought, 'r.a.ter.sim']) 
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.normal , 'r.a.ter.sim.bc']) / sd(d.all[!is.na(d.all$period ) &  d.all$period == period.normal, 'r.a.ter.sim']) 
    
    # SWC
    mean(d.all[!is.na(d.all$period ) & d.all$period == period.dipole , 'r.a.swc.5.cm.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.dipole, 'r.a.swc.5.cm.sim']) 
    mean(d.all[!is.na(d.all$period ) & d.all$period == period.drought , 'r.a.swc.5.cm.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.drought, 'r.a.swc.5.cm.sim']) 
    mean(d.all[!is.na(d.all$period ) & d.all$period == period.normal , 'r.a.swc.5.cm.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.normal, 'r.a.swc.5.cm.sim']) 
    
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.dipole , 'r.a.swc.5.cm.sim.bc']) / sd(d.all[!is.na(d.all$period ) & d.all$period == period.dipole, 'r.a.swc.5.cm.sim']) 
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.drought , 'r.a.swc.5.cm.sim.bc']) /  sd(d.all[!is.na(d.all$period ) & d.all$period == period.drought, 'r.a.swc.5.cm.sim']) 
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.normal , 'r.a.swc.5.cm.sim.bc']) / sd(d.all[!is.na(d.all$period ) & d.all$period == period.normal, 'r.a.swc.5.cm.sim']) 
    
    # SWC
   # mean(d.all[!is.na(d.all$period ) & d.all$period == period.dipole , 'r.a.herb.agb.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.dipole, 'r.a.herb.agb.sim']) 
    mean(d.all[!is.na(d.all$period ) & d.all$period == period.drought , 'r.a.herb.agb.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.drought, 'r.a.herb.agb.sim']) 
  #  mean(d.all[!is.na(d.all$period ) & d.all$period == period.normal , 'r.a.swc.5.cm.sim.bc']) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.normal, 'r.a.herb.agb.sim']) 
    
  #  sd(d.all[!is.na(d.all$period ) & d.all$period == period.dipole , 'r.a.herb.agb.sim.bc']) / sd(d.all[!is.na(d.all$period ) & d.all$period == period.dipole, 'r.a.herb.agb.sim']) 
   # sd(d.all[!is.na(d.all$period ) & d.all$period == period.drought , 'r.a.herb.agb.sim.bc']) /  sd(d.all[!is.na(d.all$period ) & d.all$period == period.drought, 'r.a.herb.agb.sim']) 
   # sd(d.all[!is.na(d.all$period ) & d.all$period == period.normal , 'r.a.herb.agb.sim.bc']) / sd(d.all[!is.na(d.all$period ) & d.all$period == period.normal, 'r.a.herb.agb.sim']) 
    
    
  }
  
}

d.all <<- d.all
biases.long <<- biases.long
biases <<- biases
