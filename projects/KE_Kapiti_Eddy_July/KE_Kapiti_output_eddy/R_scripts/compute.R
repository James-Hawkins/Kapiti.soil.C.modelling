# Computation
{
  
  # Observed
  
  d.all$gpp.osv.kg.ha <-  d.all$gpp.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
  d.all$reco.osv.kg.ha <-  d.all$reco.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
  # convert observed eddy in mm per sq m per s to kg per ha
  #d.all$NEE.obs.kg.ha <- d.all$nee.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  # new method
  d.all$NEE.obs.kg.ha <-  (1) *d.all$reco.osv.kg.ha - d.all$gpp.osv.kg.ha   #d.all$nee.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
  
  d.all[   is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA
  #d.all[d.all$NEE.obs.kg.ha < -90 & !is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA
  
  
  d.all$ET.osv <- d.all$ET.osv
  
  # Landscape DNDC
  
  if (  cm.gpp.1  ) {
  d.all$GPP.sim <- cv.sq.m.2.ha * ( d.all$co2.upt )
  
  }
  
  if (  cm.gpp.2 ) {
  # C balance method
  d.all$NPP.mod <-  cv.sq.m.2.ha *  (
    d.all$fol.grow
    + d.all$fru.grow
    + d.all$fn.rt.grow 
    + d.all$lst.grow 
    + d.all$fac.grow
  ) 
  
  d.all$R.a.mod <-  cv.sq.m.2.ha *  (
    d.all$fol.resp 
    + d.all$frt.resp 
    + d.all$fn.rt.resp 
    + d.all$lst.resp) #+ d.all$emis.auto
  
  
   d.all$GPP.sim <-   d.all$NPP.mod +   d.all$R.a.mod 
  }
 
  
  #d.all$GPP.trees.sim <- cv.sq.m.2.ha * d.all$co2.upt.trees
  #d.all$GPP.grass.sim <- cv.sq.m.2.ha * d.all$co2.upt.grass
  
  # Reco
  if (  cm.ter.1 ) {
  d.all$TER.sim <- cv.sq.m.2.ha *  (d.all$maint.resp + d.all$transp.resp + d.all$growth.resp) + d.all$emis.hetero
  }
  
  if (  cm.ter.2) {
  d.all$TER.sim <- cv.sq.m.2.ha *  (d.all$fol.resp + d.all$frt.resp + d.all$fn.rt.resp + d.all$lst.resp) + d.all$emis.hetero
  }
  

  
  d.all$NEE.mod <-   (1) * d.all$TER.sim - d.all$GPP.sim 
  
  
  
  d.all$et.sim <- d.all$et.sim.mm
  
  
  
  # Log errors
  
 # d.all$log.error.ter.bc <- e.exp^( log(1+ d.all$r.a.ter.sim.bc)  - log(1+d.all$r.a.ter.osv))
#  d.all$pe.ter.bc <-  (d.all$log.error.ter.bc - 1)*100
  
 # d.all$log.error.nee.bc <- e.exp^( log(1+ d.all$r.a.nee.sim.bc)  - log(1+d.all$r.a.nee.osv))
#  d.all$pe.nee.bc <-  (d.all$log.error.nee.bc - 1)*100
  
  
  
 # mean( na.omit(  d.all$pe.nee.bc  ))
  
  
 # mean( na.omit(d.all$pe.ter.bc ))
  

}

d.all <<- d.all


