

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


R.dir <- ("./R_scripts/")


save.image('L.DNDC.Validate.RData')
load('L.DNDC.Validate.RData')

#save.image('calibr8.in.dat.RData')
#load('calibr8.in.dat.RData')




# Global parameters
{
  # Unit conversions
  cv.MJ.to.watts <<- 11.57
  
  
  e <<- 0.000000000001
  e.exp <<- 2.71828
  
  # Running averages --  switches
  r.avg <<- TRUE
  
  r.a.switch.swc.30.d.15.cm <<- TRUE
  r.a.switch.herb.agb <<- FALSE
  r.a.switch.lai <- FALSE
  r.a.switch.et <- FALSE
  r.a.switch.nee <- TRUE
  r.a.switch.swc.5.cm <<- FALSE
  r.a.switch.swc.15.cm <<- FALSE
  r.a.switch.swc.30.cm <<- FALSE
  r.a.switch.gpp <<- TRUE
  r.a.switch.ter <<- TRUE
  
  

  
  
  
  r.a.perd.herb.agb <- 3
  r.a.perd.lai <- 1
  r.a.perd.et <- 3
  r.a.perd.swc.5.cm <- 6
  r.a.perd.swc.15 <- 6
  r.a.perd.swc.30 <- 6
  r.a.perd.ter <- 7
  r.a.perd.gpp <- 7
  r.a.perd.nee <- 7
  
  # Drought related metrics
  r.a.perd.swc.30.d.15 <<- 120/2
  r.a.perd.et.30.d.15 <<- 120/2
  
  
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
  
  
  # ECT cages summation inclusion
  
  ect.biom.include.LM1 <<- FALSE
  ect.biom.include.LS1 <<-  FALSE
  
  ect.biom.include.LM2 <<-  TRUE
  ect.biom.include.LS2 <<-  FALSE
  
  ect.biom.include.LM3 <<- TRUE
  ect.biom.include.LS3 <<- TRUE
  
  ect.biom.include.LM4 <<-  FALSE
  ect.biom.include.LS4 <<- FALSE

  
  
  
  
  
  # Conversion factors
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
  
  drought.period.start <<- "2020-10-01"
  drought.period.mid <<- "2021-12-15"
  drought.period.end <<- "2022-12-30"
  post.drought.period.end <<- "2023-06-01"
  
  
  dipole.period.start <<- "2019-06-01"
  dipole.period.mid <<- "2019-09-15"
  dipole.period.end <<- "2019-12-30"
  dipole.period.valid.end <<- "2020-06-01"
  
  
  
  
}


library("languageR") ;  library("cowplot") ;  library('tidyr') ; library('readxl') ; library('ggplot2') ; library('stringr') ;library('stringi') ; library('chron') ; library('lubridate') ; library('ggpubr') ; library('tidyr')



# Initialize
source(   str_c(R.dir,'gg.params.R')   )
source( str_c(R.dir,'helpers.R'))


source('Eddy_transform.R')
source('biomass.osv.R')


# - - - - - - - - - Post LDNDC Run
valid8 <<- function(){
  
source(str_c(R.dir,'L.DNDC.in.new.R'))  ; print('Ran L.DNDC.in')
  


# source('LAI.in.R')


source( str_c(R.dir,'supp.data.in.R')) ; print('Ran supp.data.in')


d.eddy.real <- d.eddy.real[-c(2322), ]

#print(paste("nrow d.all:", nrow(d.all)))
#print(paste("nrow d.eddy.real:", nrow(d.eddy.real)))
#print(paste("gpp.osv in d.eddy.real:", "gpp.osv" %in% names(d.eddy.real)))


#head(d.sim$date.time)
#head(d.eddy.real$date)


#tail(d.sim$date.time)
#tail(d.eddy.real$date)

d.all <<- cbind(d.sim, d.eddy.real)



source(str_c(R.dir,'ECT.biomass.in.R'))  ; print('Ran ECT.biomass.in')


source(str_c(R.dir,'compute.R'))  ; print('Ran compute.R')

source(str_c(R.dir,'Smooth.R')) ; print('Ran Smooth.R')

source(str_c(R.dir,'periods.define.R'))  ; print('Ran periods.define.R')

source(str_c(R.dir,'error.decomp.R')) ; print('Ran error.decomp.R')


source(str_c(R.dir,'evaluation.R')) ; print('Ran evaluation.R')


# nRMSE -- normal method
print('All period nRMSE s method')
print(paste('RMSE for SWC 5  is' , metrics[metrics$osv.variable == 'r.a.swc.5.cm.osv' & metrics$period =="all", 'nrmse.sd'] ))
print(paste('RMSE for SWC 15  is' , metrics[metrics$osv.variable == 'r.a.swc.15.cm.osv' & metrics$period =="all", 'nrmse.sd'] ))
print(paste('RMSE for SWC 30  is' , metrics[metrics$osv.variable == 'r.a.swc.30.cm.osv' & metrics$period =="all", 'nrmse.sd'] ))
print(paste('RMSE for TER is' , metrics[metrics$osv.variable == "r.a.ter.osv" & metrics$period =="all" , 'nrmse.sd'] ))
print(paste('RMSE for GPP is' ,  metrics[metrics$osv.variable == "r.a.gpp.osv" & metrics$period =="all" , 'nrmse.sd'] ))
print(paste('RMSE for NEE is' ,  metrics[metrics$osv.variable == "r.a.nee.osv" & metrics$period =="all", 'nrmse.sd']  ))


# nRMSE -- normal method
print('All period nRMSE main method')
print(paste('nRMSE for Herb AGB is' , metrics[metrics$osv.variable == "r.a.herb.agb.osv"  & metrics$period =="all", 'nrmse'] ))

print(paste('nRMSE for SWC 5  is' , metrics[metrics$osv.variable == 'r.a.swc.5.cm.osv' & metrics$period =="all", 'nrmse'] ))
print(paste('nRMSE for SWC 15  is' , metrics[metrics$osv.variable == 'r.a.swc.15.cm.osv' & metrics$period =="all", 'nrmse'] ))
print(paste('nRMSE for SWC 30  is' , metrics[metrics$osv.variable == 'r.a.swc.30.cm.osv' & metrics$period =="all", 'nrmse'] ))
print(paste('nRMSE for TER is' , metrics[metrics$osv.variable == "r.a.ter.osv" & metrics$period =="all", 'nrmse'] ))
print(paste('nRMSE for GPP is' ,  metrics[metrics$osv.variable == "r.a.gpp.osv" & metrics$period =="all", 'nrmse'] ))
print(paste('nRMSE for NEE is' ,  metrics[metrics$osv.variable == "r.a.nee.osv" & metrics$period =="all", 'nrmse'] ))


# nRMSE -- range method
print(paste('nRMSEr for SWC 5  is' , metrics[metrics$osv.variable == 'r.a.swc.5.cm.osv' & metrics$period =="all", 'nrmse.r'] ))
print(paste('nRMSEr for SWC 15  is' , metrics[metrics$osv.variable == 'r.a.swc.15.cm.osv' & metrics$period =="all", 'nrmse.r'] ))
print(paste('nRMSEr for SWC 30  is' , metrics[metrics$osv.variable == 'r.a.swc.30.cm.osv' & metrics$period =="all", 'nrmse.r'] ))
print(paste('nRMSEr for TER is' , metrics[metrics$osv.variable == "r.a.ter.osv" & metrics$period =="all", 'nrmse.r'] ))
print(paste('nRMSEr for GPP is' ,  metrics[metrics$osv.variable == "r.a.gpp.osv" & metrics$period =="all", 'nrmse.r'] ))
print(paste('nRMSEr for NEE is' ,  metrics[metrics$osv.variable == "r.a.nee.osv" & metrics$period =="all" , 'nrmse.r'] ))


# nRMSE -- bias corrected
print('All period nRMSE bias corrected method')
print(paste('nRMSE for Herb AGB is' , metrics[metrics$osv.variable == "r.a.herb.agb.osv"  & metrics$period =="all", 'nrmse.bc'] ))
print(paste('nRMSEr for SWC 5  is' , metrics[metrics$osv.variable == 'r.a.swc.5.cm.osv' & metrics$period =="all", 'nrmse.bc'] ))
print(paste('nRMSEr for SWC 15  is' , metrics[metrics$osv.variable == 'r.a.swc.15.cm.osv' & metrics$period =="all", 'nrmse.bc'] ))
print(paste('nRMSEr for SWC 30  is' , metrics[metrics$osv.variable == 'r.a.swc.30.cm.osv' & metrics$period =="all", 'nrmse.bc'] ))
print(paste('nRMSEr for TER is' , metrics[metrics$osv.variable == "r.a.ter.osv" & metrics$period =="all", 'nrmse.bc'] ))
print(paste('nRMSEr for GPP is' ,  metrics[metrics$osv.variable == "r.a.gpp.osv" & metrics$period =="all", 'nrmse.bc'] ))
print(paste('nRMSEr for NEE is' ,  metrics[metrics$osv.variable == "r.a.nee.osv" & metrics$period =="all", 'nrmse.bc'] ))


d.all <- as.data.frame(d.all)
d.all <- d.all[,!duplicated(colnames(d.all))]


source(str_c(R.dir ,'gg.params.R'))
source(str_c(R.dir ,'gg.seasons.R'))

d.all.plot.conditions <- (!(d.all$omit.period.2)  & d.all$date >= start.date.cald  & d.all$date <= end.date.cald   )

}

valid8()






metrics[metrics$osv.variable ==  "r.a.ter.osv" & metrics$period == "all", "log.rmse.bc"]
metrics[metrics$osv.variable ==  "r.a.gpp.osv" & metrics$period == "all", "log.rmse.bc"]
metrics[metrics$osv.variable ==  "r.a.nee.osv" & metrics$period == "all", "log.rmse.bc"]
metrics[metrics$osv.variable ==  "r.a.herb.agb.osv" & metrics$period == "all", "log.rmse.bc"]

metrics[metrics$osv.variable ==  "r.a.ter.osv" & metrics$period == "all", "pe.bc"]
metrics[metrics$osv.variable ==  "r.a.gpp.osv" & metrics$period == "all", "pe.bc"]
metrics[metrics$osv.variable ==  "r.a.nee.osv" & metrics$period == "all", "pe.bc"]
metrics[metrics$osv.variable ==  "r.a.herb.agb.osv" & metrics$period == "all", "pe.bc"]



# PLOT series

{
  
  # biomass 
  gg.bio.decomp <-  gg.biom( 
    
    FALSE
    ,   FALSE
    
    ,TRUE 
    ,  FALSE
    
    , FALSE
    ,  FALSE
    
    , FALSE
    ,  FALSE
    
  )
  
  gg.bio.decomp 
  
  # TER

  
  gg.ter.no.labl <- gen.valid.plot( 'r.a.ter.osv'  , 'r.a.ter.sim'  , 'r.a.ter.sim.bc'   , gg.valid.ter.y.ax.lab , global.valid.ter.y.cord.high , global.valid.ter.y.cord.mid  , global.valid.ter.y.cord.bottm ,'no.label'  , FALSE ,TRUE)
  gg.ter.labl <- gen.valid.plot( 'r.a.ter.osv'  , 'r.a.ter.sim'  , 'r.a.ter.sim.bc'   , gg.valid.ter.y.ax.lab , global.valid.ter.y.cord.high , global.valid.ter.y.cord.mid  , global.valid.ter.y.cord.bottm ,'label' , FALSE , TRUE)
  
  
  gg.kosalam.ter <- gen.gg.kaba('ter')
  
  
  # GPP
  
  gg.gpp.no.labl <- gen.valid.plot( 'r.a.gpp.osv'  , 'r.a.gpp.sim'  , 'r.a.gpp.sim.bc'   , gg.valid.gpp.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' ,FALSE , TRUE)
  gg.gpp.labl <- gen.valid.plot( 'r.a.gpp.osv'  , 'r.a.gpp.sim'  , 'r.a.gpp.sim.bc'   , gg.valid.gpp.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label' , FALSE , TRUE)
  
  
  gg.kosalam.gpp <- gen.gg.kaba('gpp')
  
  
  # NEE
  gg.nee.no.labl <- gen.valid.plot( 'r.a.nee.osv'  , 'r.a.nee.sim'  , 'r.a.nee.sim.bc'   , gg.valid.nee.y.ax.lab , global.valid.nee.y.cord.high , global.valid.nee.y.cord.mid  , global.valid.nee.y.cord.bottm ,'no.label' , FALSE , TRUE)
  gg.nee.labl <- gen.valid.plot( 'r.a.nee.osv'  , 'r.a.nee.sim'  , 'r.a.nee.sim.bc'   , gg.valid.nee.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)
  
  
  gg.kosalam.nee <- gen.gg.kaba('nee')
  
  # SWC

  gg.swc.5.cm.no.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   ,  gg.valid.swc.5.cm.y.ax.lab, global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE , FALSE)
  gg.swc.5.cm.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   ,  gg.valid.swc.5.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', FALSE , TRUE)
  
  
  # 15 cm layer
  gg.swc.15.cm.no.labl <- gen.valid.plot( 'r.a.swc.15.cm.osv'  , 'r.a.swc.15.cm.sim'  , 'r.a.swc.15.cm.sim.bc'   , gg.valid.swc.15.cm.y.ax.lab, global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE , FALSE)
  gg.swc.15.cm.labl <- gen.valid.plot( 'r.a.swc.15.cm.osv'  , 'r.a.swc.15.cm.sim'  , 'r.a.swc.15.cm.sim.bc'   ,  gg.valid.swc.15.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', FALSE , TRUE)
  
  
  # Drought metric
  gg.swc.30d.r.a.15.cm.no.labl <- gen.valid.plot( 'r.a.swc.15.30d.cm.osv'  , 'r.a.swc.15.30d.cm.sim'  , 'r.a.swc.15.30d.cm.sim'   , gg.valid.swc.15.cm.y.ax.lab, global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE , FALSE)
  gg.swc.15.cm.labl <- gen.valid.plot( 'r.a.swc.15.cm.osv'  , 'r.a.swc.15.cm.sim'  , 'r.a.swc.15.cm.sim.bc'   ,  gg.valid.swc.15.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', FALSE , TRUE)
  
 

  # 30 cm layer
  gg.swc.30.cm.no.labl <- gen.valid.plot( 'r.a.swc.30.cm.osv'  , 'r.a.swc.30.cm.sim'  , 'r.a.swc.30.cm.sim.bc'   ,   gg.valid.swc.30.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE , FALSE)
  gg.swc.30.cm.labl <- gen.valid.plot( 'r.a.swc.30.cm.osv'  , 'r.a.swc.30.cm.sim'  , 'r.a.swc.30.cm.sim.bc'   ,  gg.valid.swc.30.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', FALSE , TRUE)
  
  
  gg.kosalam.swc <- gen.gg.kaba('swc')
  
  
  # ET
  gg.et.no.labl <- gen.valid.plot( 'r.a.et.osv'  , 'r.a.et.sim'  , 'r.a.et.sim.bc'   ,   gg.valid.et.y.ax.lab  , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE , FALSE)
  gg.et.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   ,   gg.valid.et.y.ax.lab  , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', FALSE , TRUE)
  
  

  
# Climate
{
    

  gg.rain.0 <- gg.theme  %>%   +  
      # ggplot() + 
      geom_bar(   data= d.all[ 
        d.all$covid %in% covid.status[c(1,2,3)] 
        #& ( d.all$date > covid.end.date | d.all$date < covid.start.date) 
        ,  ] ,
        aes( x = date.time 
             , y = precip.not.eddy.contns
             
        )  ,
        , stat = 'identity'  
        , width = p.br.wdth * 0.7
        , color = '#B6D0E2'
        , alpha = p.br.alpha ) +
    # theme(aspect.ratio = 1/3) +
    coord_cartesian()  +
    theme(
      
      plot.margin = margin(t = 0, r = 88, b = 0, l = 15.5, unit = "pt")
      
      
      , axis.text.y.right =element_text()
      , axis.title.y.right =element_text()
    ) 
    
  
gg.rain.temp <- gg.rain.0  %>%   +  geom_line( aes(x = date, y = (( temp.avg.osv *  gg.temp.scale)) ) 
               , linewidth = gg.temp.ln.width 
               
               , color=  '#FAA0A0' 
    ) + 
    # Add secondary axis that displays mpg * 1.2
    scale_y_continuous(
      # name = bquote(Daily rainfall~(mm~)),
      name = bquote("Rainfall (mm d"^-1*")")
      
      , sec.axis = sec_axis(transform = ~ (. /  gg.temp.scale ) , name =   gg.valid.daily.temp.lab)
    ) 
  
     
gg.rain.0
    
    
    
gg.rain.fin <-    gg.rain.temp  %>%   +  
      
      geom_segment(aes(x = as.Date(dipole.period.start)  , xend = as.Date(dipole.period.end) , y = lab.y.crd.dipole ) , linetype= 'dotted') +
      geom_segment(aes(x = as.Date(drought.period.start)  , xend = as.Date(drought.period.end) , y = lab.y.crd.drought ) , linetype= 'dotted' ) +
      geom_segment(aes(x = as.Date(el.nino.period.start )  , xend = as.Date(el.nino.period.end) , y = lab.y.crd.drought ) , linetype= 'dotted' ) +
      
      
      geom_segment(aes(x = as.Date(covid.start.date)  , xend = as.Date(covid.end.date) , y = lab.y.crd.dat.gaps ) , linetype= 'dotted') +
      geom_segment(aes(x = as.Date(  d.gap.2.period.start)  , xend = as.Date(  d.gap.2.period.end) , y = lab.y.crd.dat.gaps ) , linetype= 'dotted') +
      
      geom_segment(aes(x = as.Date(  boma.period.start )  , xend = as.Date(  boma.period.end) , y = lab.y.crd.boma ) , linetype= 'dotted') +
      # Grazing
      geom_label(
        mapping = aes(x =  as.Date(   boma.period.mid )   , y =lab.y.crd.boma , label = "Boma" ),
        fill = global.valid.text.background
        , color = global.valid.text.color
        , label.size = NA
        , size = gg.valid.label.fs 
        , hjust = .5
      ) +
      # DATA GAPS
      geom_label(
        mapping = aes(x =  as.Date( global.valid.covid.label.date )   , y = -7, label = gg.valid.label.covid.period  ),
        fill = global.valid.text.background
        , color = global.valid.text.color
        , label.size = NA
        , size = gg.valid.label.fs 
        , hjust = .5
      )   +
      geom_label(
        mapping = aes(x =  as.Date( global.valid.no.data.label.date )   , y = -7, label = gg.valid.label.no.data.period ),
        fill = global.valid.text.background
        , color = global.valid.text.color
        , label.size = NA
        , size = gg.valid.label.fs 
        , hjust = .5
      )   +
      
      # Water regimes
      geom_label(
        mapping = aes(x =  as.Date(   dipole.period.mid )   , y = lab.y.crd.dipole , label = "Dipole" ),
        fill = global.valid.text.background
        , color = global.valid.text.color
        , label.size = NA
        , size = gg.valid.label.fs 
        , hjust = .5
      ) +
      geom_label(
        mapping = aes(x =  as.Date(   drought.period.mid )   , y = lab.y.crd.drought , label = "Drought" ),
        fill = global.valid.text.background
        , color = global.valid.text.color
        , label.size = NA
        , size = gg.valid.label.fs 
        , hjust = .5
      ) +
      geom_label(
        mapping = aes(x =  as.Date(   el.nino.period.mid )   , y = lab.y.crd.drought , label = "El nino" ),
        fill = global.valid.text.background
        , color = global.valid.text.color
        , label.size = NA
        , size = gg.valid.label.fs 
        , hjust = .5
      ) +
      
      # Grazing
      geom_label(
        mapping = aes(x =  as.Date(   boma.period.mid )   , y =lab.y.crd.boma , label = "Boma" ),
        fill = global.valid.text.background
        , color = global.valid.text.color
        , label.size = NA
        , size = gg.valid.label.fs 
        , hjust = .5
      ) 
    
      gg.rain.0.p2 <- gg.rain.0 %>%   +  
        theme(
          
          plot.margin = margin(t = 1, r = 10, b = 1, l = 10, unit = "pt")
          
          
          , axis.text.y.right = element_blank()
          , axis.ticks.y.right  = element_blank()
          , axis.title.y.right = element_blank()
        )  +
        ylab(bquote("Rainfall (mm d"^-1*")"))
      
      
      gg.rain.fin.p2 <- gg.rain.fin %>%   +  
        theme(
          
          plot.margin = margin(t = 1, r = 10, b = 1, l = 10, unit = "pt")
          
          
          , axis.text.y.right = element_blank()
          , axis.ticks.y.right  = element_blank()
          , axis.title.y.right = element_blank()
        )  +
        ylab(bquote("Rainfall (mm d"^-1*")"))
      
      
    
}
  


{
  
gg.validate.1.labels <- c('a' ,'b' , 'c' , 'd' )

gg.ter.plot <-  gg.remv.x.lab( gg.ter.no.labl )
gg.gpp.plot <-  gg.remv.x.lab( gg.gpp.no.labl )
gg.nee.plot <-  gg.remv.x.lab( gg.nee.no.labl  )

gg.ter.plot.koba <- ggarrange(gg.ter.plot , gg.kosalam.ter , widths = c(2.4,0.35 ), nrow = 1) 
gg.gpp.plot.koba <- ggarrange(gg.gpp.plot , gg.kosalam.gpp , widths = c(2.4,0.35 ), nrow = 1) 
gg.nee.plot.koba <- ggarrange(gg.nee.plot , gg.kosalam.nee , widths = c(2.4,0.35 ), nrow = 1) 




gg.co2.flxs <- ggarrange(


gg.ter.plot.koba
, gg.gpp.plot.koba
,gg.nee.plot.koba

# ,     gg.rain.0
 ,    gg.rain.fin

, nrow = 4
, labels = gg.validate.1.labels 
, font.label = list(size = 12.5, color = "black", face = "bold")
, heights = c(1,1,1,.95)
, label.x = .008575
, label.y = 0.9775
)


gg.co2.flxs 


filename.gg.co2.flxs = 'Figures.out/gg.co2.flxs.jpg'

ggsave(filename = filename.gg.co2.flxs,  gg.co2.flxs , width = 10.8, height = 9 , dpi = 1000 )


}  # PLOT 1 - TER, GPP, NEE - out


{
  gg.hydro.labels <- c('a' ,'b' , 'c' , 'd' , 'e' )
    
    gg.swc.5.cm.plot.no.labl <-  gg.remv.x.lab( gg.swc.5.cm.no.labl )
    gg.swc.15.cm.plot.no.labl <-  gg.remv.x.lab( gg.swc.15.cm.no.labl )
    gg.swc.30.cm.plot.no.labl <- gg.remv.x.lab( gg.swc.30.cm.no.labl )
    gg.et.no.labl <- gg.remv.x.lab( gg.et.no.labl )
    

    gg.validate.hydro <- ggarrange(
      
      
      gg.swc.5.cm.plot.no.labl
      ,    gg.swc.15.cm.plot.no.labl 
      ,  gg.swc.30.cm.plot.no.labl
  #    , gg.et.no.labl
      
      ,   gg.rain.0.p2
      
    #  , nrow = 5
    , nrow = 4
      , labels = gg.hydro.labels 
      , heights = c( 
        1 
        , 1
        , 1 
       # , 1 
        , 1.3
        )
      
      , font.label = list(size = 11.5, color = "black", face = "bold")
     # , heights = c( 1 , 1 , 1 , 0.75)
      , label.x = .008575
      , label.y = 0.9775
      
      
    )
    
    gg.validate.hydro
    
  
    filename.gg.hydro = 'Figures.out/gg.hydro.jpg'
    
    ggsave(filename =  filename.gg.rain ,     gg.validate.hydro , width = 7.5 , height = 8 , dpi = 2500  )
 
    
     }  # PLOT 2 - SWC 5, 15, 30 - out
  
 
 
{ 
  
  
  d.all$r.a.ter.sim.bc
  
  gg.lnbf.def <<- ggplot()
  
gg.lnbf <- ggplot()    +  
        geom_point( data = d.all[ !is.na(d.all$r.a.ter.sim.bc) & d.all$r.a.ter.sim.bc != 0 & d.all$r.a.ter.osv != 0 & d.all$true.variables  & !d.all$omit.period.2 & d.all$period == periods[c(2)] & d.all$covid != "During covid"  , ] , aes( x = r.a.ter.osv , y = r.a.ter.sim.bc  ))+
geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")


tail(d.all$day.cnt)
9100-365

gg.lnbf <- ggplot()    +  
  geom_point( data = d.all[ !is.na(d.all$r.a.nee.sim.bc) & d.all$r.a.nee.sim.bc != 0 & d.all$r.a.nee.osv != 0 & d.all$true.variables & d.all$covid != "During covid" , ] , aes( x = r.a.nee.osv , y = r.a.nee.sim.bc  ))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+
coord_cartesian(xlim = c(-110, 100), ylim = c(-100, 100))



gg.lnbf <- ggplot()    +  
  geom_point( data = d.all[ !is.na(d.all$r.a.gpp.sim.bc) & d.all$r.a.gpp.osv != 0 & d.all$r.a.gpp.osv != 0 & d.all$period %in% periods[c(1,2,3)] & !d.all$omit.period.2 & d.all$true.variables & d.all$covid != "During covid" , ] , aes( x = r.a.gpp.osv  , y = r.a.gpp.sim.bc  ))+
geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))

  
  
  
  } # LOB plots
  
 
 # C regime plots
{

  
df.prep <- function(){
  
 n.clims <- 4
 n.metrics <- 2
 n.catgs <- 3
 p.metrics <- c('osv' ,'sim')
 
 d.all.C.flux <- data.frame(
   
   C.catg = c( rep( 'ter',  n.clims *   n.metrics )  , rep( 'gpp',  n.clims *  n.metrics ) , rep( 'nee',  n.clims *  n.metrics ))
   
   , C.metric = c( 
     rep( 'r.a.ter.osv',  n.clims ) 
     ,  rep( 'r.a.ter.sim.bc',  n.clims ) 
     
     ,  rep( 'r.a.gpp.osv',  n.clims ) 
     ,  rep( 'r.a.gpp.sim.bc',  n.clims ) 
     
     ,  rep( 'r.a.nee.osv',  n.clims ) 
     ,  rep( 'r.a.nee.sim.bc',  n.clims ) 
     
     )
   
   , dat.typ = c( 
     rep( 'Eddy tower',  n.clims ) 
     ,  rep( 'Landscape DNDC',  n.clims ) 
     
     ,rep( 'Eddy tower',  n.clims ) 
     ,  rep( 'Landscape DNDC',  n.clims ) 
     
     , rep( 'Eddy tower',  n.clims ) 
     ,  rep( 'Landscape DNDC',  n.clims ) 
     
   )
   
    , clim = rep(  seq(1,  n.clims ) ,   n.metrics * n.catgs )
   
  
   
   , value = NA
   , sd = NA
   
 )
 
 return( d.all.C.flux)
 
}

 

 for (r in 1:nrow( d.all.C.flux)){
   
   if (r == 1) { d.all.C.flux <- df.prep()}
   
   # test: c.metric <-  'r.a.ter.sim.bc' ; r <- 1 ; c.clim <- 1
   c.metric <- d.all.C.flux[r , 'C.metric']
   p.metric <-  d.all.C.flux[r , 'p.metric']
   c.clim <-  d.all.C.flux[r , 'clim']
   
   if (c.clim == 1){ c.period <- periods.ag.drought.norm  
   } else if (c.clim == 2){ c.period <- periods.ag.drought.high 
 } else if (c.clim == 3){ c.period <- periods.ag.drought.low  
} else if (c.clim == 4){ c.period <- "other"  }
   

   

  
   d.all.C.flux[r , 'value'] <-   mean(na.omit(d.all[d.all$period.ag.drt ==  c.period  ,c.metric]))
   d.all.C.flux[r , 'sd'] <-   sd(na.omit(d.all[d.all$period.ag.drt ==  c.period  ,c.metric]))
   
    # mean(na.omit(    d.all.C.flux.piv[d.all.C.flux.piv$C.metric == c.metric , 'value']   ))
 
 }
 
 
 
 d.all.C.flux[ d.all.C.flux$clim == 1 , 'clim.lab'] <- periods.ag.drought.labels[3]
 d.all.C.flux[ d.all.C.flux$clim == 2 , 'clim.lab'] <- periods.ag.drought.labels[2]
 d.all.C.flux[ d.all.C.flux$clim == 3 , 'clim.lab'] <- periods.ag.drought.labels[1]
 d.all.C.flux[ d.all.C.flux$clim == 4 , 'clim.lab'] <- periods.ag.drought.labels[4]
 
  d.all.C.flux[ d.all.C.flux$C.catg ==  "gpp" , 'value'] <-    d.all.C.flux[ d.all.C.flux$C.catg ==  "gpp" , 'value'] * (-1)
 
 
 d.all.C.flux$clim.lab<- factor( d.all.C.flux$clim.lab , levels = periods.ag.drought.labels.ordered)
 
 
 d.all.C.flux[ d.all.C.flux$C.catg == "ter" , 'C.catg.lab'] <-   gg.C.exchange.catg.lab.ter 
 d.all.C.flux[ d.all.C.flux$C.catg == "gpp" , 'C.catg.lab'] <-  gg.C.exchange.catg.lab.gpp
 d.all.C.flux[ d.all.C.flux$C.catg == "nee" , 'C.catg.lab'] <-  gg.C.exchange.catg.lab.nee
 
 C.catg.labs.ordered <-  c(
   gg.C.exchange.catg.lab.ter
   , gg.C.exchange.catg.lab.gpp
   ,gg.C.exchange.catg.lab.nee
 )
 
 
 
 d.all.C.flux$C.catg.lab  <- factor(d.all.C.flux$C.catg.lab , levels =  C.catg.labs)
 
 
 gg.C.flux.gen <- ggplot(   d.all.C.flux ) +
   theme(
     panel.grid.major = element_blank(),
     , panel.background = element_blank()
     , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
     , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
     ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
     
     
     , axis.title.x = element_blank()
     
     , legend.title = element_blank()
   ) +
   ylab( bquote("Flux (kg C ha"^-1*" d"^-1*")") ) +
   scale_fill_manual(
     values = c(
       
       "Eddy tower" = p.colors[1]
         ,  "Landscape DNDC" = p.colors[2]
        
         )
     
   )
   

 gg.C.flux.gen <-   gg.C.flux.gen %>%    +
   geom_bar(   aes(x = C.catg.lab , y = value , fill = dat.typ  ) , position = position_dodge()  , stat = 'identity') +
   geom_errorbar(aes(x = C.catg.lab, ymin = value - sd, ymax = value + sd )  , position = position_dodge2()) + 
 facet_grid( ~ clim.lab)
 
 gg.C.flux.gen
   
}
   

 # Rainfall regime plots
 {
   
   
   df.prep.rf <- function(){
     
     n.clims <- 4
     n.metrics <- 2
     n.catgs <- 1
     p.metrics <- c('osv' ,'sim')
     
     d.all.rain <- data.frame(
       
       catg = c( rep( 'precip',  n.clims *   n.metrics )  , rep( 'swc.15',  n.clims *  n.metrics ) )
       
       
       , metric = c( 
         rep( 'precip.osv',  n.clims ) 
         ,  rep( 'r.a.swc.15.cm.sim',  n.clims ) 
         
         
       )
       
       , dat.typ = c( 
         rep( 'Rainfall (mm/d)',  n.clims ) 
         ,  rep('Soil water content 15 cm (%)',  n.clims ) 
         
       )
       
       , clim = rep(  seq(1,  n.clims ) ,   n.metrics * n.catgs )
       
       
       
       , value = NA
       , sd = NA
       
     )
     
     return(  d.all.rain )
     
   }
   
   d.all.rain <- df.prep.rf() 
   
   for   (r in 1: nrow( d.all.rain)  ){
     
     if (  r == 1  ) {  d.all.rain <- df.prep.rf()  }
     
     # test: metric <-  'precip.osv' ; r <- 1 ; c.clim <- 1
     metric <-  d.all.rain[r , 'metric']
     # p.metric <-   d.all.rain [r , 'p.metric']
     c.clim <-   d.all.rain[r , 'clim']
     
     if (   c.clim == 1  ){ c.period <- periods.ag.drought.norm  
     } else if (c.clim == 2){ c.period <- periods.ag.drought.high 
     } else if (c.clim == 3){ c.period <- periods.ag.drought.low  
     } else if (c.clim == 4){ c.period <- "other"  }
     
     
     
     
     
     d.all.rain[r , 'value'] <-   mean(na.omit(d.all[d.all$period.ag.drt ==  c.period  ,metric]))
     d.all.rain[r , 'sd'] <-   sd(na.omit(d.all[d.all$period.ag.drt ==  c.period  ,c.metric]))
     
     # mean(na.omit(    d.all.C.flux.piv[d.all.C.flux.piv$C.metric == c.metric , 'value']   ))
     
   }
   
   
   
   d.all.rain[ d.all.rain$clim == 1 , 'clim.lab'] <- periods.ag.drought.labels[3]
   d.all.rain[ d.all.rain$clim == 2 , 'clim.lab'] <- periods.ag.drought.labels[2]
   d.all.rain[ d.all.rain$clim == 3 , 'clim.lab'] <- periods.ag.drought.labels[1]
   d.all.rain[ d.all.rain$clim == 4 , 'clim.lab'] <- periods.ag.drought.labels[4]
   

   
   d.all.rain$clim.lab  <- factor( d.all.rain$clim.lab , levels = periods.ag.drought.labels.ordered)
   
   
   d.all.rain[  d.all.rain$catg == "precip" , 'catg.lab'] <-   'Precipitation (mm/d)' 
   d.all.rain[  d.all.rain$catg == "swc.15", 'catg.lab'] <-  'SWC (%)' 
   
   catg.labs.ordered <-  c(
     'Precipitation (mm/d)' 
     , 'SWC (%)' 
   )
   
   
   
   d.all.rain$catg.lab  <- factor(d.all.rain$catg.lab , levels =    catg.labs.ordered)
   
   
   gg.water.gen <- ggplot(   d.all.rain ) +
     theme(
       panel.grid.major = element_blank(),
       , panel.background = element_blank()
       , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
       , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
       ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
       
       
       , axis.title.x = element_blank()
       
       , legend.title = element_blank()
     ) +
   #  ylab( bquote("Flux (kg C ha"^-1*" d"^-1*")")) +
     scale_fill_manual(
       values = c(
         
         "Rainfall (mm/d)" = 'lightblue'
         ,  "Soil water content 15 cm (%)" = 'grey'
         
       )
       
     )
   
   t.form.coef.water  <- 50
   
   
   gg.water  <- gg.water.gen %>%    +
     geom_bar(   aes(x = catg.lab , y = value *365 , fill = dat.typ  ) , position = position_dodge()  , stat = 'identity') +
   #  geom_errorbar(aes(x = catg.lab, ymin = value - sd, ymax = value + sd )  , position = position_dodge2()) + 
     facet_grid( ~ clim.lab) +
     scale_y_continuous(
       name = "Precipitation (mm/d)",
       sec.axis = sec_axis(    ~ . /365  , name = "SWC 15 cm (%)")
     ) 
   
   gg.water
   
 }
 
 
 
 
# Rainfall regime plots
 {
   
   t.form.coef <- 1
   
   d.all.hydro <- d.all %>%
     mutate(Metric_Right_Scaled = precip.osv * t.form.coef ) %>%
     # Pivot into a long format for easier ggplot mapping
     pivot_longer(
       cols = c( r.a.swc.15.30d.cm.osv, precip.osv), 
       names_to = "Metric", 
       values_to = "Value"
     )
   

   gg.rainf.gen <- ggplot(  d.all.hydro)+
     theme(
       panel.grid.major = element_blank(),
       , panel.background = element_blank()
       , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
       , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
       ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
       
       
       , axis.title.x = element_blank()
     )
   

  
gg.rain.reg <-   gg.rainf.gen %>%    +
     geom_col(   aes(x = period.ag.drt, y = Value, group = Metric , fill = Metric) , position = position_dodge() )+
 #geom_errorbar(   aes(x = period.ag.drt, y =  Value , group = Metric ) , position = position_dodge() , fun.data = mean_sdl,stat = 'summary')+
 # stat_summary(aes(x = period.ag.drt, y =  Value , ymin = Value +100 , ymax = Value - 100 ,group = Metric ) , fun.data = mean_sdl, geom = "errorbar", width = 0.2)
  #geom_bar(es(x = period.ag.drt, y = Value, group = Metric , fill = Metric)  , position = position_dodge2(preserve = "single"))
scale_y_continuous(
  name = "Precipitation (mm/d)",
  sec.axis = sec_axis(~ . /    t.form.coef, name = "Soil water content 15 cm (%)")
) 
gg.rain.reg 

}
 
 
 
 
}








mean.temp.all <- mean(d.all[  ,'temp.avg.osv' ])

mean(d.all[  str_detect(d.all$year.month, year.2018) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2019) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2020) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2021) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2022) ,'temp.avg.osv' ]) /mean.temp.all
mean(d.all[  str_detect(d.all$year.month, year.2023) ,'temp.avg.osv' ]) /mean.temp.all


mean.precip.all <- mean(d.all[  ,'precip.osv' ]) 

mean(d.all[  str_detect(d.all$year.month, year.2018) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2019) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2020) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2021) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2022) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2023) ,'precip.osv' ]) /mean.precip.all
mean(d.all[  str_detect(d.all$year.month, year.2024) ,'precip.osv' ]) /mean.precip.all




