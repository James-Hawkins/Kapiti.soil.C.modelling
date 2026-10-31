# Global parameters
{
  
  options(max.print = 50000)
  
  
  # Unit conversions
  cv.MJ.to.watts <<- 11.57
  
  
  e <<- 0.000000000001
  e.exp <<- 2.71828
  
  # Running averages --  switches
  r.avg <<-  TRUE
  
  r.a.switch.swc.30.d.15.cm <<-FALSE
  r.a.switch.herb.agb <<- FALSE
  r.a.switch.lai <- FALSE
  r.a.switch.et <- FALSE
  r.a.switch.nee <- FALSE
  r.a.switch.swc.5.cm <<- FALSE
  r.a.switch.swc.15.cm <<- FALSE
  r.a.switch.swc.30.cm <<- FALSE
  r.a.switch.gpp <<- FALSE
  r.a.switch.ter <<-FALSE
  
  
  
  r.a.perd.herb.agb <- 9
  r.a.perd.lai <- 9
  r.a.perd.et <- 9
  r.a.perd.swc.5.cm <- 9
  r.a.perd.swc.15 <- 9
  r.a.perd.swc.30 <- 9
  r.a.perd.ter <- 9
  r.a.perd.gpp <- 9
  r.a.perd.nee <- 9
  
  # Drought related metrics
  r.a.perd.swc.30.d.15 <<- 120/2
  r.a.perd.et.30.d.15 <<- 120/2
  
  
  MAM.rnd.length.days <<- 92 
  OND.rnd.length.days <<- 92 
  
  # Thresholds in percent
  ag.drt.thres.drought <<- 16.5
  ag.drt.thres.pluvial <<- 19
  
  
  
  # NEE computation methods
  
  cm.ter.1 <<- TRUE ; cm.ter.2  <<- FALSE
  cm.gpp.1 <<- TRUE; cm.gpp.2  <<- FALSE
  
  
  
  # Global sets
  periods <<- c('dipole' , 'drought' , 'normal')
  periods.ag.drought <<- c( 'drought' , 'pluvial' , 'other')
  
  periods.ag.drought.high <<- periods.ag.drought[2]
  periods.ag.drought.low <<- periods.ag.drought[1]
  periods.ag.drought.norm <<- periods.ag.drought[3]
  
  periods.ag.drought.labels <<- c( 'Drought' , 'Pluvial' , 'Other' , 'All')
  periods.ag.drought.labels.ordered <<- periods.ag.drought.labels
  
  
  var <- c(
    'swc.5' 
    ,'swc.15' 
    ,'swc.30' 
    , 'ter'
    , 'gpp'
    , 'nee'
    , 'et'
    , 'ag.biom.grass.Mg.ha'
  )
  
  
  osv.metric.vars <<- c(
    'r.a.swc.5.cm.osv' 
    , 'r.a.swc.15.cm.osv' 
    , 'r.a.swc.30.cm.osv' 
    , 'r.a.ter.osv'
    , 'r.a.gpp.osv'
    , 'r.a.nee.osv'
    , 'r.a.et.osv'
    , 'r.a.herb.agb.osv'
  )
  
  
  sim.metric.vars <<-c(
    'r.a.swc.5.cm.sim' 
    ,'r.a.swc.15.cm.sim' 
    ,'r.a.swc.30.cm.sim' 
    , 'r.a.ter.sim'
    , 'r.a.gpp.sim'
    , 'r.a.nee.sim'
    , 'r.a.et.sim'
    , 'r.a.herb.agb.sim'
  )
  
  sim.metric.vars.bc <<-c(
    'r.a.swc.5.cm.sim.bc' 
    ,  'r.a.swc.15.cm.sim.bc' 
    ,  'r.a.swc.30.cm.sim.bc' 
    , 'r.a.ter.sim.bc'
    , 'r.a.gpp.sim.bc'
    , 'r.a.nee.sim.bc'
    , 'r.a.et.sim.bc'
    , 'r.a.herb.agb.sim.bc'
  )
  
  period.label <<- c( 'Pluvial' , "'20-22 drought" , "Normal")
  
  
  ssn.labels.ordered <- c(
    
    'Jan-Feb\n(short) dry'
    ,  'March-May\n(long) rains'
    ,  'June-Sept\n(long) dry'
    ,'Oct-Dec\n(short) rains'
    
  )
  
  ssn.label.SD <<- ssn.labels.ordered[1]
  ssn.label.LR <<- ssn.labels.ordered[2]
  ssn.label.LD <<- ssn.labels.ordered[3]
  ssn.label.SR <<- ssn.labels.ordered[4]
  
  
  # ECT cages summation inclusion
  
  ect.biom.include.LM1 <<- FALSE
  ect.biom.include.LS1 <<-  FALSE
  
  ect.biom.include.LM2 <<-  TRUE
  ect.biom.include.LS2 <<-  FALSE
  
  ect.biom.include.LM3 <<- TRUE
  ect.biom.include.LS3 <<- TRUE
  
  ect.biom.include.LM4 <<-  FALSE
  ect.biom.include.LS4 <<- FALSE
  
  biom.cgs.weights <<- c(
    
    0
    ,0
    
    ,.25
    ,.25
    
    ,.25
    ,.25
    
    ,0
    ,0
    
  )
  
  
  
  
  # Conversion factors
  cv.Mg.2.kg <<- 1000
  cv.sq.m.2.ha <<- 10000
  cv.microml.2.kg  <<- 0.000000001 
  cv.sec.2.yr <<- 60*60*24*365
  cv.sec.2.d  <<- 60*60*24
  cv.mml.c.2.co2 <<- 12
  cv.mj.2.watts <<- 1/ 0.0864
  cv.secs.per.30.min <<- 1800 
  
  parm.Lv <<- 2260000
  parm.pw <<- 1000
  
  start.date.cald <<- "2018-07-28"
  end.date.cald <<- "2024-12-04"
  
  
  v.status.actual <<- 'actual'
  v.status.filled <<- 'filled'
  
  v.status.mn.filled <<- 'mn.filled'
  v.status.subs.filled <<- 'subs.filled'
  v.status.eo.filled <<- 'eo.filled'
  
  #cor.type <<- 'spearman' 
  cor.type <<- 'pearson'
  
  rd.decs.rmse  <<- 1
  
  rd.decs.rain.day.mm <<- 1    ; 
  rd.decs.quant.days <<- 1    ; 
  
  drought.period.start <<- "2020-10-01"
  drought.period.mid <<- "2021-12-15"
  drought.period.end <<- "2022-12-30"
  post.drought.period.end <<- "2023-06-01"
  
  
  dipole.period.start <<- "2019-06-01"
  dipole.period.mid <<- "2019-09-15"
  dipole.period.end <<- "2019-12-30"
  dipole.period.valid.end <<- "2020-06-01"
  
  
  
}


