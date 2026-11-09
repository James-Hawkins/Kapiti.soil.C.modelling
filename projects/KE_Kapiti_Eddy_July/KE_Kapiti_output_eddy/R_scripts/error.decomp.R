# Bias detection and correction

#  source('error.decomp.R')

{
  
  

  # Systematic validation metrics
  all.condition.deluge <<- (  d.all$period.ag.drt == periods.ag.drought.high & d.all$variable.status  & !(d.all$omit.period.2) & !is.na(d.all$period ))
  all.condition.drought <<- (  d.all$period.ag.drt ==   periods.ag.drought.low & d.all$variable.status & !(d.all$omit.period.2) & !is.na(d.all$period ))
  all.condition.drought.plus.covid <<- (d.all$period.ag.drt == periods.ag.drought.low  & d.all$variable.status  & !is.na(d.all$period ))
  
  all.condition.normal <<- (d.all$period.ag.drt == periods.ag.drought.norm & d.all$variable.status & !(d.all$omit.period.2) & !is.na(d.all$period ))
  all.condition.all <<- (d.all$variable.status  & !(d.all$omit.period.2) & !is.na(d.all$period ))
  
  

  # BIAS DETECTION
  # Biases
  
  
  biases <- data.frame(
    osv.variable = rep(osv.metric.vars,4) 
    , sim.variable =  rep(sim.metric.vars,4) 
    , sim.variable.bc = rep(sim.metric.vars.bc,4) 
    , period =  c( rep(periods.ag.drought.high ,length(sim.metric.vars)) , rep(periods.ag.drought.low,length(sim.metric.vars))  , rep(periods.ag.drought.norm ,length(sim.metric.vars) ) , rep(period.all,length(sim.metric.vars) )  )
  )
  
  
  # View(biases)
  
  for (r in 1:nrow(biases)  ) {
    
    # test: r <- 4
    
    cur.sim.var <- biases[ r, 'sim.variable' ]
    cur.period <- biases[ r, 'period' ]
    
    osv.var <- osv.metric.vars[  which(sim.metric.vars == cur.sim.var   )  ]
    
    if (cur.period == periods.ag.drought.high & osv.var != "r.a.herb.agb.osv"  ){ condition <- all.condition.deluge 
    } else if (cur.period == periods.ag.drought.high & osv.var == "r.a.herb.agb.osv"  ) {  next 
      
    } else if (cur.period == periods.ag.drought.low & osv.var != "r.a.herb.agb.osv" ){ condition <- all.condition.drought 
    } else if (cur.period == periods.ag.drought.low & osv.var == "r.a.herb.agb.osv" ){ condition <- all.condition.drought.plus.covid  
    
    
    } else if (cur.period == 'normal'){ condition <- all.condition.normal 
    } else if (cur.period == 'all'){ condition <- all.condition.all }
    
    dates.no.na <- intersect(  d.all[ condition &  !is.na(d.all[,osv.var]) , 'date'] , d.all[ condition &  !is.na(d.all[,sim.var]) , 'date']  )
    condition.no.na <- d.all$date %in%  dates.no.na
    
    condition.all <- condition.no.na & condition
    
    
    
    mean.osv <- mean( na.omit(d.all[ condition.all , osv.var] ))
   mean.sim <- mean( na.omit(d.all[ condition.all , cur.sim.var] ))
    
      sd.osv <- sd( na.omit(d.all[ condition.all, osv.var] ))
      sd.sim <- sd( na.omit(d.all[ condition.all , cur.sim.var] ))
      
      biases[ r, 'mean.osv'] <- mean.osv
      biases[ r, 'mean.sim'] <- mean.sim
      
      biases[ r, 'sd.osv'] <- sd.osv
      biases[ r, 'sd.sim'] <- sd.sim 
    
    # KLING-GUPTA METHOD
    kge.r <-  cor( d.all[ condition.all , osv.var]  , d.all[ condition.all, cur.sim.var] , method = 'pearson')
    kge.a <-  sd.sim  / sd.osv 
    kge.b <- mean.sim / mean.osv
  
    kge <- 1 - sqrt(   (  kge.r - 1)^2 +  (  kge.a - 1)^2 + (   kge.b- 1)^2 )
    
    
    biases[ r, 'kge.r'] <-   kge.r
    biases[ r, 'kge.a'] <-    kge.a 
    biases[ r, 'kge.b'] <-  kge.b
    
    biases[ r, 'kge'] <-   kge
    
  
  }
  
  #  View(biases)
  
  
  # Dataframe to plot absolute biases
  biases.long <- biases %>%
    pivot_longer(cols = c('kge.b' , 'kge.a' ,  'kge.r'  ) 
                 , names_to = "error.catg"
                 , values_to = "error"
    )
  
  biases.long <- as.data.frame(biases.long )
  
  #error.types <- unique( biases.long$error.catg)
  biases.long[biases.long$error.catg == error.types[1], 'error.type.label'] <-   error.type.labels[1]
  biases.long[biases.long$error.catg == error.types[2], 'error.type.label'] <- error.type.labels[2]
  biases.long[biases.long$error.catg == error.types[3], 'error.type.label'] <- error.type.labels[3]
  
  biases.long$error.catg <- factor(  biases.long$error.catg  , levels = c("kge.b" ,"kge.a" ,"kge.r"  ))
  biases.long$error.type.label <- factor(  biases.long$error.type.label , levels = error.type.labels)
  
  
 
  
  periods <- unique( biases.long$period)
  biases.long[biases.long$period  == periods.ag.drought[1], 'period.label'] <-  periods.ag.drought.labels[1]
  biases.long[biases.long$period == periods.ag.drought[2] , 'period.label'] <-  periods.ag.drought.labels[2]
  biases.long[biases.long$period  == periods.ag.drought[3], 'period.label'] <-  periods.ag.drought.labels[3]
  biases.long[biases.long$period  == period.all, 'period.label'] <-  periods.ag.drought.labels[4]
  
  
  
  bias.cond.ter <<- biases.long$osv.variable == 'r.a.ter.osv' 
  bias.cond.gpp <<- biases.long$osv.variable == 'r.a.gpp.osv'
  bias.cond.swc <<- biases.long$osv.variable == 'r.a.swc.5.cm.osv' 
  
  
  # BIAS CORRECTION
  {
    
    
    for (r in 1:nrow(d.all)){
      for (v in sim.metric.vars.bc){
        
        
        # test: v <- sim.metric.vars.bc[4]
        
        cur.period <- d.all[r,'period.ag.drt'] 
        
        if ( is.na(cur.period)) { next}
        
        raw.sim.var <- sim.metric.vars[ which(sim.metric.vars.bc == v)   ]
        
        
        cur.sim.var <- d.all[ r ,raw.sim.var]
        
         bias.df.cond <- (biases$sim.variable == raw.sim.var & biases$period == cur.period)
        


        mean.osv <- biases[ bias.df.cond, 'mean.osv'] 
        mean.sim <- biases[ bias.df.cond, 'mean.sim'] 
        sd.osv <- biases[ bias.df.cond, 'sd.osv'] 
        sd.sim <- biases[ bias.df.cond, 'sd.sim'] 
        
        
        sim.bc <- mean.osv + ( cur.sim.var - mean.sim ) * (sd.osv / sd.sim)
        
        if (  v == "r.a.gpp.sim.bc" &   sim.bc < 0 ){  sim.bc <- 0
        } else  if (  v == "r.a.ter.sim.bc" &   sim.bc < 0 ){  sim.bc <- 0}
        
        d.all[r,v] <-  sim.bc 
        
   
        
      }}
    
    # Evaluate bias corrected vs. raw
    indicator.bc  <- 'r.a.ter.sim.bc' ; indicator <- 'r.a.ter.sim'
    indicator.bc <-'r.a.swc.5.cm.sim.bc' ;  indicator <- 'r.a.swc.5.cm.sim'
    indicator.bc <-'r.a.gpp.sim.bc' ;  indicator <- 'r.a.gpp.sim'
    
    
    
    mean(d.all[ !is.na(d.all$period ) & d.all$period == period.dipole , indicator.bc ]) / mean(d.all[!is.na(d.all$period ) & d.all$period == period.dipole, indicator ]) 
    mean(d.all[ !is.na(d.all$period ) & d.all$period == period.drought , indicator.bc ]) / mean(d.all[!is.na(d.all$period ) & d.all$period == period.drought, indicator ]) 
    mean(d.all[!is.na(d.all$period ) &  d.all$period == period.normal , indicator.bc ]) / mean(d.all[!is.na(d.all$period ) & d.all$period == period.normal, indicator ]) 
    
    
    mean(d.all[ !is.na(d.all$period ) & d.all$period == period.dipole , indicator.bc ]) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.dipole, indicator ]) 
    mean(d.all[ !is.na(d.all$period ) & d.all$period == period.drought , indicator.bc ]) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.drought, indicator ]) 
    mean(d.all[!is.na(d.all$period ) &  d.all$period == period.normal , indicator.bc ]) - mean(d.all[!is.na(d.all$period ) & d.all$period == period.normal, indicator ]) 
    
  
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.dipole , indicator.bc ]) / sd(d.all[ !is.na(d.all$period ) & d.all$period == period.dipole, indicator ]) 
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.drought , indicator.bc ]) /  sd(d.all[ !is.na(d.all$period ) & d.all$period == period.drought, indicator ]) 
    sd(d.all[!is.na(d.all$period ) & d.all$period == period.normal , indicator.bc]) / sd(d.all[!is.na(d.all$period ) &  d.all$period == period.normal, indicator ]) 
    
 
    
  }
  
  
  #  View(biases.long)
  
}

d.all <<- d.all
biases.long <<- biases.long
biases <<- biases
