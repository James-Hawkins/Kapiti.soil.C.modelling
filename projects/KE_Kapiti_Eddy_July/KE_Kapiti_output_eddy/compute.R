# Computation
{
  
  # Observed
  
  d.all$gpp.osv.kg.ha <-  d.all$gpp.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
  d.all$reco.osv.kg.ha <-  d.all$reco.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
  # convert observed eddy in mm per sq m per s to kg per ha
  #d.all$NEE.obs.kg.ha <- d.all$nee.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  # new method
  d.all$NEE.obs.kg.ha <-  (-1) *d.all$reco.osv.kg.ha - d.all$gpp.osv.kg.ha   #d.all$nee.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
  
  d.all[   is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA
  d.all[d.all$NEE.obs.kg.ha < -90 & !is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA
  
  
  d.all$ET.osv <- d.all$ET.osv
  
  # MODELLED
  d.all$GPP.sim <- cv.sq.m.2.ha * d.all$co2.upt
  
  
  #d.all$GPP.trees.sim <- cv.sq.m.2.ha * d.all$co2.upt.trees
  #d.all$GPP.grass.sim <- cv.sq.m.2.ha * d.all$co2.upt.grass
  
  d.all$TER.sim <- cv.sq.m.2.ha *  (d.all$maint.resp + d.all$transp.resp + d.all$growth.resp) + d.all$emis.hetero
  
  d.all$NEE.mod <-   (-1) * d.all$TER.sim - d.all$GPP.sim 
  
  
  
  d.all$et.sim <- d.all$et.sim.mm
  
}