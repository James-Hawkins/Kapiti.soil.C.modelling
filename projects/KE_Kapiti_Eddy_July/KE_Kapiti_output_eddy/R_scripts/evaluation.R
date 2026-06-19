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
    
    # r <- 1
    
    osv.var <- metrics[r,'osv.variable']
    sim.var  <- metrics[r,'sim.variable']
    sim.var.bc  <- metrics[r,'sim.variable.bc']
    cur.period <- metrics[r,'period']
    
    if ( osv.var == "r.a.herb.agb.osv" & cur.period == periods.ag.drought.high){next}
    
    
    cur.lcs <- biases[ biases$period ==  cur.period & biases$osv.variable ==    osv.var  ,  'lcs']
    cur.sb <- biases[ biases$period ==  cur.period & biases$osv.variable ==    osv.var  ,  'squared.bias']
    cur.sdsd <- biases[ biases$period ==  cur.period & biases$osv.variable ==    osv.var  ,  'sqd.diff.sd']
    
    if ( !is.na(cur.lcs) & cur.lcs > mean(cur.sb , cur.sdsd  ) ){ metrics[r , 'error.phenological'] <- TRUE 
    } else { metrics[r , 'error.phenological'] <- FALSE }
    
    if (cur.period == periods.ag.drought.high ) {condition <- all.condition.pluvial }
    if (cur.period == periods.ag.drought.low) {condition <- all.condition.drought} 
    if (cur.period == periods.ag.drought.norm) {condition <- all.condition.normal }
    if (cur.period == period.all) {condition <- all.condition.all }
    
    no.na.condition <-   !is.na(d.all[,osv.var])
    
    metrics[r , 'r2'] <- cor(  d.all[condition & no.na.condition  , osv.var ] , d.all[ condition & no.na.condition, sim.var]   , method = cor.type  )
    metrics[r , 'r2.bc'] <- cor(  d.all[condition & no.na.condition , osv.var ] , d.all[ condition & no.na.condition, sim.var.bc ]   , method = cor.type  )
    
    metrics[r , 'r2'] <- round(  metrics[r , 'r2'] , 2)
    metrics[r , 'r2.bc'] <- round(  metrics[r , 'r2.bc'] , 2)
    
  
     
    metrics[r , 'log.rmse'] <-   sum( na.omit(abs( log( d.all[ condition , osv.var ] + e ) - log(d.all[ condition ,  sim.var] + e)  )))   / sum(condition & !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var]))
    metrics[r , 'log.rmse.bc'] <-   sum( na.omit(abs( log( d.all[ condition , osv.var ] + e )- log(d.all[ condition , sim.var.bc] + e)   )))   / sum(condition & !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var.bc ]))
    
    metrics[r , 'pe'] <-   (e.exp^(metrics[r , 'log.rmse']) -1 ) * 100
    metrics[r , 'pe.bc'] <-     (e.exp^(metrics[r , 'log.rmse.bc']) -1 ) * 100
    
    
    

    

    metrics[r , 'rmse'] <-   sum( na.omit(abs( d.all[ condition , osv.var ] - d.all[ condition ,  sim.var]   )))   / sum(condition & !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var]))
    metrics[r , 'rmse.bc'] <-   sum( na.omit(abs( d.all[ condition , osv.var ] - d.all[ condition ,  sim.var.bc ]   )))   / sum(condition & !is.na(d.all[,osv.var]) & !is.na(d.all[,sim.var.bc ]))
    
    
    metrics[r , 'nrmse'] <-  100* (1 / abs(mean(na.omit(d.all[ condition , osv.var] ))) )*    metrics[r , 'rmse'] 
    metrics[r , 'nrmse.bc'] <-  100* (1 / abs(mean(na.omit(d.all[ condition , osv.var] ))) )*    metrics[r , 'rmse.bc'] 
    
    metrics[r , 'nrmse.r'] <-  100* (1 / abs(   max(   na.omit(d.all[ condition , osv.var] )   ) - min(   na.omit(d.all[ condition , osv.var] )   )  ) )*    metrics[r , 'rmse'] 
    metrics[r , 'nrmse.r.bc'] <-  100* (1 / abs(   max(   na.omit(d.all[ condition , osv.var] )   ) - min(   na.omit(d.all[ condition , osv.var] )   )  ) )*    metrics[r , 'rmse.bc'] 
    
    metrics[r , 'nrmse.sd'] <-  100* (1 / abs(   sd(   na.omit(d.all[ condition , osv.var] )   )  ) )*    metrics[r , 'rmse'] 
    metrics[r , 'nrmse.sd.bc'] <-  100* (1 / abs(   sd(   na.omit(d.all[ condition , osv.var] )   )   ) )*    metrics[r , 'rmse.bc'] 
    
    
    
    
    # Rounded
    metrics[r , 'rmse'] <-   round(  metrics[r , 'rmse'] , 1)
    metrics[r , 'rmse.bc'] <-   round(  metrics[r , 'rmse.bc'] , 1)
    
    metrics[r , 'nrmse'] <-   round(  metrics[r , 'nrmse'] , 3)
    metrics[r , 'nrmse.bc'] <-   round(  metrics[r , 'nrmse.bc'] , 3)
    
    metrics[r , 'nrmse.sd'] <-   round(  metrics[r , 'nrmse.sd'], 3)
    metrics[r , 'nrmse.sd.bc']  <-   round(  metrics[r , 'nrmse.sd.bc'] , 3)
    
    
    
    
    metrics[r , 'rmse'] <-   round(  metrics[r , 'rmse'] , 1)
    

    
    
    period.status <- str_c( 'period.' , var [r] )
    
    if (cur.period == period.dipole){cur.period.label <- period.label[1] }
    if (cur.period == period.drought){cur.period.label <- period.label[2] }
    if (cur.period == period.normal){cur.period.label <- period.label[3] }
    
    
    valid.text <- str_c(  cur.period.label,': r = ',  metrics[r , 'r2'] , ' (' , metrics[r , 'r2.bc'] , ')' , ', RMSE = ', metrics[r , 'rmse'] , ' (' , metrics[r , 'rmse.bc'] , ')' , ', nRMSE = ', metrics[r , 'nrmse'] , ' (' , metrics[r , 'nrmse.bc'] , ') ' , '%')
    
    
    d.all[ !is.na(d.all$period) & d.all$period == cur.period, 'period.status'] <- valid.text
    
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


d.all <<- d.all
metrics <<- metrics
