{
  
  
  if (  r.avg  ){ 
    
  d.all[,'r.a.lai.osv'] <- 0
  d.all[,'r.a.lai.sim'] <- 0
  
  d.all[,'r.a.swc.5.cm.osv'] <- 0 
  d.all[,'r.a.swc.5.cm.sim'] <- 0 
  
  d.all[,'r.a.swc.15.cm.osv'] <- 0 
  d.all[,'r.a.swc.15.cm.sim'] <- 0 
  
  d.all[,'r.a.swc.30.cm.osv'] <- 0 
  d.all[,'r.a.swc.30.cm.sim'] <- 0 
  
  
  d.all[,'r.a.swc.15.30d.cm.osv'] <- 0 
  d.all[,'r.a.swc.15.30d.cm.sim'] <- 0 
  
  
  
  d.all[,'r.a.ter.osv'] <- 0 
  d.all[,'r.a.ter.sim'] <- 0 
  
  d.all[, 'r.a.gpp.osv'] <- 0 
  d.all[, 'r.a.gpp.sim'] <- 0 
  
  d.all[,'r.a.nee.osv'] <- 0 
  d.all[,'r.a.nee.sim'] <- 0 
  
  
  d.all[,'r.a.lai.osv'] <- 0
  d.all[,'r.a.lai.sim'] <- 0
  
  d.all[,'r.a.et.osv'] <- 0
  d.all[,'r.a.et.sim'] <- 0

  d.all[,'r.a.herb.agb.osv'] <- 0
  d.all[,'r.a.herb.agb.sim'] <- 0
  

  
  # Compute running averages
  if (  r.a.switch.herb.agb  ){ 
    
    for (   r in ( r.a.perd.herb.agb +1):(nrow(d.all)-(r.a.perd.herb.agb ))  ){
      
      
      for (d in r.a.perd.herb.agb :(-r.a.perd.herb.agb )){
        
        d.all[r   , 'r.a.herb.agb.osv']   <- d.all[r   , 'r.a.herb.agb.osv'] + d.all[r - d  ,  'osv.biom.Mg.ha.ALL'] / (r.a.perd.herb.agb * 2) 
        d.all[r   , 'r.a.herb.agb.sim']   <- d.all[r   , 'r.a.herb.agb.sim'] + d.all[r - d  ,  'ag.biom.grass.Mg.ha'] / (r.a.perd.herb.agb * 2) 
      }} 
    
    
  } 
 
  if (r.a.switch.et){ 
    
    for (   r in (r.a.perd.et+1):(nrow(d.all)-(r.a.perd.et))  ){
    
      
      for (d in r.a.perd.et:(-r.a.perd.et)){
        
        d.all[r   , 'r.a.et.osv']   <- d.all[r   , 'r.a.et.osv'] + d.all[r - d  , 'ET.osv'] / (r.a.perd.et * 2) 
        d.all[r   , 'r.a.et.sim']   <- d.all[r   , 'r.a.et.sim'] + d.all[r - d  , 'et.sim'] / (r.a.perd.et * 2) 
      }} 
    
    
  } else { d.all[,'r.a.et.osv']  <- d.all$ET.osv ; d.all[,'r.a.et.sim'] <- d.all$et.sim  } # ET
  

  if (r.a.switch.lai){ 
    
    for (   r in (r.a.perd.lai+1):(nrow(d.all)-(r.a.perd.lai))  ){
      
      
      d.all[r , 'r.a.lai.osv'] <- 0 
      d.all[r , 'r.a.lai.sim'] <- 0 
      
      for (d in r.a.perd.lai:(-r.a.perd.lai)){
        
        d.all[r   , 'r.a.lai.osv']   <- d.all[r   , 'r.a.lai.osv'] + d.all[r - d  , 'lai.obs'] / (r.a.perd.lai * 2) 
        d.all[r   , 'r.a.lai.sim']   <- d.all[r   , 'r.a.lai.sim'] + d.all[r - d  , 'lai.sim'] / (r.a.perd.lai * 2) 
      }} 
    
    
  } else { d.all[,'r.a.lai.osv']  <- d.all$lai.obs ; d.all[,'r.a.lai.sim'] <- d.all$lai.sim  } # LAI
  
  
  if (r.a.switch.swc.5.cm){ for (   r in (r.a.perd.swc+1):(nrow(d.all)-(r.a.perd.swc))  ){
    
    
    d.all[r , 'r.a.swc.5.cm.osv'] <- 0 
    d.all[r , 'r.a.swc.5.cm.sim'] <- 0 
    
    for (d in r.a.perd.swc:(-r.a.perd.swc)){
      
      d.all[r   , 'r.a.swc.5.cm.osv']   <- d.all[r   , 'r.a.swc.5.cm.osv'] + d.all[r - d  , 'swc.3.pc.osv']  / (r.a.perd.swc * 2) 
      d.all[r   , 'r.a.swc.5.cm.sim']   <- d.all[r   , 'r.a.swc.5.cm.sim'] + d.all[r - d  , 'sw.5']/ (r.a.perd.swc * 2) 
    }}  } else { d.all[,'r.a.swc.5.cm.osv']  <- d.all$swc.3.pc.osv ; d.all[,'r.a.swc.5.cm.sim'] <- d.all$sw.5 } # SWC
  
  
  if (r.a.switch.swc.15.cm){ for (   r in (r.a.perd.swc.15+1):(nrow(d.all)-(r.a.perd.swc.15))  ){
    
    
    d.all[r , 'r.a.swc.15.cm.osv'] <- 0 
    d.all[r , 'r.a.swc.15.cm.sim'] <- 0 
    
    for (d in r.a.perd.swc:(-r.a.perd.swc)){
      
      d.all[r   , 'r.a.swc.15.cm.osv']   <- d.all[r   , 'r.a.swc.15.cm.osv'] + d.all[r - d  , 'swc.2.pc.osv']  / (r.a.perd.swc * 2) 
      d.all[r   , 'r.a.swc.15.cm.sim']   <- d.all[r   , 'r.a.swc.15.cm.sim'] + d.all[r - d  , 'sw.15']/ (r.a.perd.swc * 2) 
    }}  } else { d.all[,'r.a.swc.15.cm.osv']  <- d.all$swc.2.pc.osv ; d.all[,'r.a.swc.15.cm.sim'] <- d.all$sw.15 } # SWC
  
  
  if (r.a.switch.swc.30.d.15.cm){ for (   r in (  r.a.perd.swc.30.d.15 +1):(nrow(d.all)-(  r.a.perd.swc.30.d.15 ))  ){
    
    
   # d.all[r , 'r.a.swc.15.30d.cm.osv'] <- 0 
   # d.all[r , 'r.a.swc.15.30d.cm.sim'] <- 0 
    
    for (d in r.a.perd.swc.30.d.15:(-r.a.perd.swc.30.d.15)){
      
      d.all[r   , 'r.a.swc.15.30d.cm.osv']   <- d.all[r   , 'r.a.swc.15.30d.cm.osv'] + d.all[r - d  , 'swc.2.pc.osv']  / (r.a.perd.swc.30.d.15* 2) 
      d.all[r   , 'r.a.swc.15.30d.cm.sim']   <- d.all[r   , 'r.a.swc.15.30d.cm.sim'] + d.all[r - d  , 'sw.15']/ (r.a.perd.swc.30.d.15 * 2) 
    }}  } else { d.all[,'r.a.swc.15.cm.osv']  <- d.all$swc.2.pc.osv ; d.all[,'r.a.swc.15.30d.cm.sim'] <- d.all$sw.15 } # SWC
  
  
  
  
  if (r.a.switch.swc.30.cm){ for (   r in (r.a.perd.swc.30+1):(nrow(d.all)-(r.a.perd.swc.30))  ){
    
    
    d.all[r , 'r.a.swc.30.cm.osv'] <- 0 
    d.all[r , 'r.a.swc.30.cm.sim'] <- 0 
    
    for (d in r.a.perd.swc.30:(-r.a.perd.swc.30)){
      
      d.all[r   , 'r.a.swc.30.cm.osv']   <- d.all[r   , 'r.a.swc.30.cm.osv'] + d.all[r - d  , 'swc.1.pc.osv']  / (r.a.perd.swc.30 * 2) 
      d.all[r   , 'r.a.swc.30.cm.sim']   <- d.all[r   , 'r.a.swc.30.cm.sim'] + d.all[r - d  , 'sw.30']/ (r.a.perd.swc.30 * 2) 
    }}  } else { d.all[,'r.a.swc.30.cm.osv']  <- d.all$swc.1.pc.osv ; d.all[,'r.a.swc.30.cm.sim'] <- d.all$sw.30 } # SWC
  
  
  
  if (r.a.switch.ter){for (   r in (r.a.perd.ter+1):(nrow(d.all)-(r.a.perd.ter))  ){
    
    
    d.all[r , 'r.a.ter.osv'] <- 0 
    d.all[r , 'r.a.ter.sim'] <- 0 
    
    for (d in r.a.perd.ter:(- r.a.perd.ter)){
      
      d.all[r   , 'r.a.ter.osv']   <- d.all[r   , 'r.a.ter.osv'] + d.all[r - d  , 'reco.osv.kg.ha'] / (r.a.perd.ter * 2) 
      d.all[r   , 'r.a.ter.sim']   <- d.all[r   , 'r.a.ter.sim'] + d.all[r - d  , 'TER.sim'] / (r.a.perd.ter * 2)
    }} } else { d.all[,'r.a.ter.osv']  <- d.all$reco.osv.kg.ha ; d.all[,'r.a.ter.sim'] <- d.all$TER.sim } #  TER
  
  
  if (r.a.switch.gpp){for (   r in (r.a.perd.gpp+1):(nrow(d.all)-(r.a.perd.gpp))  ){
    
    
    for (d in r.a.perd.gpp:(-r.a.perd.gpp)){
      
      d.all[r   , 'r.a.gpp.osv']   <- d.all[r   , 'r.a.gpp.osv'] + d.all[r - d  , 'gpp.osv.kg.ha'] / (r.a.perd.gpp * 2)  
      d.all[r   , 'r.a.gpp.sim']   <- d.all[r   , 'r.a.gpp.sim'] + d.all[r - d  , 'GPP.sim'] / (r.a.perd.gpp * 2)
    }} } else { d.all[,'r.a.gpp.osv']  <- d.all$gpp.osv.kg.ha ; d.all[,'r.a.gpp.sim'] <- d.all$GPP.sim} #  GPP
  
  
  if (r.a.switch.nee){ 
    
    for (   r in (r.a.perd.nee+1):(nrow(d.all)-(r.a.perd.nee))  ){
      
      
      for (d in r.a.perd.nee:(-r.a.perd.nee)){
        
        d.all[r   , 'r.a.nee.osv']   <- d.all[r   , 'r.a.nee.osv'] + d.all[r - d  , 'NEE.obs.kg.ha'] / (r.a.perd.nee * 2)
        d.all[r   , 'r.a.nee.sim']   <- d.all[r   , 'r.a.nee.sim'] + d.all[r - d  , 'NEE.mod'] / (r.a.perd.nee * 2)  
      }} 
    
    
  } else { d.all[,'r.a.nee.osv']  <- d.all$NEE.obs.kg.ha ; d.all[,'r.a.nee.sim'] <- d.all$NEE.mod } # NEE
  
  
  d.all[ is.na(d.all$lai.obs) , 'r.a.lai.osv'] <- 0.099
  d.all[ is.na(d.all$reco.osv.kg.ha) , 'r.a.ter.osv'] <- 0.099
  d.all[ is.na(d.all$gpp.osv.kg.ha) , 'r.a.gpp.osv'] <- 0.099
  d.all[ is.na(d.all$NEE.obs.kg.ha ) , 'r.a.nee.osv'] <- 0.099
  d.all[ is.na(d.all$swc.3.pc.osv) , 'r.a.swc.5.cm.osv'] <- 0.099
  
  
  
  
  } else {
    
    
    d.all[,'r.a.swc.15.30d.cm.osv'] <- d.all[,'swc.2.pc.osv'] 
    d.all[,'r.a.swc.15.30d.cm.sim'] <- d.all[,'sw.15']  
    
    d.all[,'r.a.lai.osv'] <- 0
    d.all[,'r.a.lai.sim'] <- 0
    
    d.all[,'r.a.et.osv'] <- 0
    d.all[,'r.a.et.sim'] <- 0
    
    d.all[,'r.a.herb.agb.osv'] <- 0
    d.all[,'r.a.herb.agb.sim'] <- 0
    
    d.all[,'r.a.swc.5.cm.osv'] <- d.all[,'swc.3.pc.osv'] 
    d.all[,'r.a.swc.5.cm.sim'] <- d.all[,'sw.5'] 
    
    d.all[,'r.a.swc.15.cm.osv'] <- d.all[,'swc.2.pc.osv']
    d.all[,'r.a.swc.15.cm.sim'] <- d.all[,'sw.15']  
    
    d.all[,'r.a.swc.30.cm.osv'] <-   d.all[,'swc.1.pc.osv']
    d.all[,'r.a.swc.30.cm.sim'] <-  d.all[,'sw.30'] 
    
    d.all[,'r.a.ter.osv'] <- d.all[,'reco.osv.kg.ha']   
    d.all[,'r.a.ter.sim'] <- d.all[,'TER.sim']
    
    d.all[, 'r.a.gpp.osv'] <-  d.all[, 'gpp.osv.kg.ha']
    d.all[, 'r.a.gpp.sim'] <-  d.all[, 'GPP.sim']
    
    d.all[,'r.a.nee.osv'] <-  d.all[,'NEE.obs.kg.ha']
    d.all[,'r.a.nee.sim'] <-  d.all[,'NEE.mod']
    
  
  }
  
  
  d.all.n <- d.all[ 
    
    !is.na(d.all$r.a.lai.sim) 
    & !is.na(d.all$r.a.ter.sim) 
    & !is.na(d.all$r.a.gpp.sim)  
    & !is.na(d.all$r.a.nee.sim) 
    & !is.na(d.all$r.a.swc.5.cm.sim) 
    
    & d.all$r.a.lai.sim != 0
    & d.all$r.a.ter.sim != 0
    & d.all$r.a.gpp.sim != 0
    & d.all$r.a.nee.sim != 0
    & d.all$r.a.swc.5.cm.sim != 0
    
    & d.all$r.a.lai.osv != 0
    & d.all$r.a.ter.osv != 0
    & d.all$r.a.gpp.osv != 0
    & d.all$r.a.nee.osv != 0
    & d.all$r.a.swc.5.cm.osv != 0
    
    
    , ]
  #d.all <- d.all[   !is.na(d.all$three_dra.gpp.sim) & !is.na(d.all$three_dra.gpp.osv) , ]
 # nrow(d.all)
  
  
} # Rolling averages


d.all <<- d.all

