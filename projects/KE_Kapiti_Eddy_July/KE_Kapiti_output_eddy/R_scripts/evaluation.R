# Evaluation


{

  
  metrics <- data.frame(
    
    osv.variable = rep(osv.metric.vars,4) 
    , sim.variable =  rep(sim.metric.vars,4) 
    , sim.variable.bc = rep(sim.metric.vars.bc,4) 
    , period =  c( rep(periods.ag.drought.high ,length(sim.metric.vars)) , rep(periods.ag.drought.low,length(sim.metric.vars))  , rep(periods.ag.drought.norm,length(sim.metric.vars) ) , rep(period.all,length(sim.metric.vars) ) )
    
    
    ,r2 = NA
    ,rmse = NA
    ,nrmse = NA
    
    , valid.text = NA
  )
  
  d.all[, 'period.status'] <- NA
  
  condition.all.data <- d.all$true.variables

  for (  r in 1:nrow(metrics)  ){
    
    # r <- 4
    
    osv.var <- metrics[r,'osv.variable']
    sim.var  <- metrics[r,'sim.variable']
    sim.var.bc  <- metrics[r,'sim.variable.bc']
    cur.period <- metrics[r,'period']
    
    if ( osv.var == "r.a.herb.agb.osv" & cur.period == periods.ag.drought.high){ next }
    
    
    # cur.lcs <- biases[ biases$period ==  cur.period & biases$osv.variable ==    osv.var  ,  'lcs']
    #  cur.sb <- biases[ biases$period ==  cur.period & biases$osv.variable ==    osv.var  ,  'squared.bias']
    # cur.sdsd <- biases[ biases$period ==  cur.period & biases$osv.variable ==    osv.var  ,  'sqd.diff.sd']
    
    # if (   is.na(cur.lcs) ) {next }
    
    
    #  if ( !is.na(cur.lcs) & cur.lcs > mean(cur.sb , cur.sdsd  ) ){ metrics[r , 'error.phenological'] <- TRUE 
    #  } else { metrics[r , 'error.phenological'] <- FALSE }
    
    if (cur.period == periods.ag.drought.high ) {  rain.condition <- all.condition.pluvial }
    if (cur.period == periods.ag.drought.low) {rain.condition <- all.condition.drought} 
    if (cur.period == periods.ag.drought.norm) {rain.condition<- all.condition.normal }
    if (cur.period == period.all) {rain.condition <- all.condition.all }
    
    no.na.condition <-  !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var]) & !is.na(d.all[,sim.var.bc])  & !d.all$omit.period.2 & d.all$covid != covid.status[3]
    
    # sum( no.na.condition )
    
    
    condition.all <- rain.condition &  no.na.condition
    

    series.osv <- d.all[   condition.all , osv.var ] 
    series.sim <- d.all[   condition.all , sim.var] 
    series.sim.bc <- d.all[   condition.all , sim.var.bc] 
  
    

    metrics[r , 'rmse'] <-   sum( na.omit(abs( d.all[ condition , osv.var ] - d.all[ condition ,  sim.var]   )))   / sum(condition  )
    metrics[r , 'rmse.bc'] <-   sum( na.omit(abs( d.all[ condition , osv.var ] - d.all[ condition ,  sim.var.bc ]   )))   / sum(condition  )
    
    
    #  metrics[r , 'nrmse'] <-  100* (1 / abs(mean(na.omit(d.all[ condition , osv.var] ))) )*    metrics[r , 'rmse'] 
    #  metrics[r , 'nrmse.bc'] <-  100* (1 / abs(mean(na.omit(d.all[ condition , osv.var] ))) )*    metrics[r , 'rmse.bc'] 
    

    # KGE
    sd.osv <- sd( d.all[  condition.all , osv.var]  )
    sd.sim <- sd( d.all[  condition.all , sim.var]  )
    sd.sim.bc <- sd( d.all[  condition.all , sim.var.bc]  )
    
    avg.osv <- mean( d.all[  condition.all , osv.var]  )
    avg.sim <- mean( d.all[  condition.all , sim.var]  )
    avg.sim.bc <- mean( d.all[  condition.all , sim.var.bc]  )
  
    
    # Non BC
    kge.a <-  sd.sim /    sd.osv
    kge.b <-   avg.sim/     avg.osv
    kge.r <-  cor( d.all[  condition.all , osv.var] , d.all[  condition.all , sim.var]  )
    
    
  
    # Bias corrected
    kge.bc.a <-   avg.sim.bc /    sd.osv
    kge.bc.b <-    avg.sim.bc  /     avg.osv
    kge.bc.r <-  cor( d.all[  condition.all , osv.var] , d.all[  condition.all , sim.var.bc]  )
    
  

    if (   osv.var == 'NEE.obs.kg.ha' ) {   
      
      # For NEE, use modified bias term
      
      kge.b.mod <-   ( avg.sim - avg.osv ) /  sd.osv 
      kge.bc.b.mod <-   ( avg.sim.bc - avg.osv ) /  sd.osv 
      
      
      kge <- 1 - sqrt(   (  kge.r - 1)^2 +  (  kge.a - 1)^2 + (    kge.b.mod  - 0)^2 )
      kge.bc <- 1 - sqrt(   (  kge.bc.r - 1)^2 +  (  kge.bc.a - 1)^2 + (   kge.bc.b.mod - 0)^2 )
      
      # 
      kge.b <- kge.b.mod
      kge.bc.b.mod <-  kge.bc.b
        
    } else{
      
      kge <- 1 - sqrt(   (  kge.r - 1)^2 +  (  kge.a - 1)^2 + (   kge.b- 1)^2 )
      kge.bc <- 1 - sqrt(   (  kge.bc.r - 1)^2 +  (  kge.bc.a - 1)^2 + (   kge.bc.b- 1)^2 )
      
    }
    
    # Report un-biased corrected metrics
    metrics[r , 'kge.a'] <-     kge.a 
    metrics[r , 'kge.b'] <-      kge.b
    metrics[r , 'kge.r'] <-       kge.r
    metrics[r , 'kge'] <-       kge
    
    # Report  bias corrected metrics
    metrics[r , 'kge.bc.a'] <-     kge.bc.a 
    metrics[r , 'kge.bc.b'] <-      kge.bc.b
    metrics[r , 'kge.bc.r'] <-   kge.bc.r
    metrics[r , 'kge.bc'] <-  kge.bc
    
    
   
    
    # Rounded
    metrics[r , 'rmse'] <-   round(  metrics[r , 'rmse'] , 1)
    metrics[r , 'rmse.bc'] <-   round(  metrics[r , 'rmse.bc'] , 1)
    
    #  metrics[r , 'nrmse'] <-   round(  metrics[r , 'nrmse'] , 3)
    #   metrics[r , 'nrmse.bc'] <-   round(  metrics[r , 'nrmse.bc'] , 3)
    

    
    
    period.status <- str_c( 'period.' , var [r] )
    
    if (cur.period == period.dipole){cur.period.label <- period.label[1] }
    if (cur.period == period.drought){cur.period.label <- period.label[2] }
    if (cur.period == period.normal){cur.period.label <- period.label[3] }
    
    
    valid.text <- str_c(  cur.period.label,': r = ',  metrics[r , 'r2'] , ' (' , metrics[r , 'r2.bc'] , ')' , ', RMSE = ', metrics[r , 'rmse'] , ' (' , metrics[r , 'rmse.bc'] , ')' , ', nRMSE = ', metrics[r , 'nrmse'] , ' (' , metrics[r , 'nrmse.bc'] , ') ' , '%')
    
    
    d.all[ !is.na(d.all$period) & d.all$period == cur.period, 'period.status'] <- valid.text
    
    metrics[r , 'valid.text'] <-  valid.text
    
  }
  
  # View(metrics)
  
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

#View(metrics)


d.all <<- d.all
metrics <<- metrics
