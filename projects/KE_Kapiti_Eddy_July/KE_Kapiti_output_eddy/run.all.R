

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

R.dir <- ("./R_scripts/")


save.image('L.DNDC.Validate.RData')
load('L.DNDC.Validate.RData')

# Global parameters
{
  # Unit conversions
  cv.MJ.to.watts <<- 11.57
  
  # Running averages --  switches
  r.a.switch.lai <- FALSE
  r.a.switch.et <- FALSE
  r.a.switch.nee <- FALSE
  r.a.switch.swc.5.cm <<- TRUE
  r.a.switch.swc.15.cm <<- TRUE
  r.a.switch.swc.30.cm <<- TRUE
  r.a.switch.gpp <<- TRUE
  r.a.switch.ter <<- TRUE
  
  r.a.perd.lai <- 1
  r.a.perd.swc.5.cm <- 6
  r.a.perd.swc.15 <- 6
  r.a.perd.swc.30 <- 6
  r.a.perd.ter <- 6
  r.a.perd.gpp <- 6
  r.a.perd.nee <- 1
  
  
  
  # Global sets
  periods <<- c('dipole' , 'drought' , 'normal')
  
  var <- c(
    'swc.5' 
    ,'swc.15' 
    ,'swc.30' 
    , 'ter'
    , 'gpp'
    , 'nee'
    , 'lai'
  )
  
  osv.metric.vars <<- c(
    'r.a.swc.5.cm.osv' 
    , 'r.a.swc.15.cm.osv' 
    , 'r.a.swc.30.cm.osv' 
    , 'r.a.ter.osv'
    , 'r.a.gpp.osv'
    , 'r.a.nee.osv'
    , 'r.a.lai.osv'
  )
  
  
  sim.metric.vars <<-c(
    'r.a.swc.5.cm.sim' 
    ,'r.a.swc.15.cm.sim' 
    ,'r.a.swc.30.cm.sim' 
    , 'r.a.ter.sim'
    , 'r.a.gpp.sim'
    , 'r.a.nee.sim'
    , 'r.a.lai.sim'
  )
  
  sim.metric.vars.bc <<-c(
    'r.a.swc.5.cm.sim.bc' 
    ,  'r.a.swc.15.cm.sim.bc' 
    ,  'r.a.swc.30.cm.sim.bc' 
    , 'r.a.ter.sim.bc'
    , 'r.a.gpp.sim.bc'
    , 'r.a.nee.sim.bc'
    , 'r.a.lai.sim.bc'
  )
  
  period.label <<- c( 'Dipole' , "'20-22 drought" , "Normal")
  
  
  
  # Conversion factors
  cv.sq.m.2.ha <- 10000
  cv.microml.2.kg <- 0.000000001 
  cv.sec.2.yr <- 60*60*24*365
  cv.sec.2.d <- 60*60*24
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
  
  cor.type <<- 'spearman' 
  #cor.type <<- 'pearson'
  
  rd.decs.rmse  <<- 1
  
  drought.period.start <<- "2020-10-01"
  drought.period.mid <<- "2021-12-15"
  drought.period.end <<- "2022-12-30"
  
  dipole.period.start <<- "2019-06-01"
  dipole.period.mid <<- "2019-09-15"
  dipole.period.end <<- "2019-12-30"
  dipole.period.valid.end <<- "2020-06-01"
  
  
  
  
}


library("languageR") ;  library(cowplot) ;  library('tidyr') ; library('readxl') ; library('ggplot2') ; library('stringr') ;library('stringi') ; library('chron') ; library('lubridate') ; library('ggpubr') ; source('helpers.R') ; library('tidyr')


# Initialize
source('Eddy_transform.R')
source('biomass.osv.R')


# - - - - - - - - - Post LDNDC Run
{
source(str_c(R.dir,'L.DNDC.in.R'))

# source('LAI.in.R')


source( str_c(R.dir,'supp.data.in.R'))


d.eddy.real <- d.eddy.real[-c(2322), ]

d.all <- cbind(d.all, d.eddy.real)



source(str_c(R.dir,'ECT.biomass.in.R'))

source(str_c(R.dir,'compute.R'))


source(str_c(R.dir,'Smooth.R'))

source(str_c(R.dir,'periods.define.R'))


source(str_c(R.dir,'error.decomp.R'))


source(str_c(R.dir,'evaluation.R'))


print(paste('nRMSE for SWC 5  is' , metrics[metrics$osv.variable == 'r.a.swc.5.cm.osv' , 'nrmse'] ))
print(paste('nRMSE for SWC 15  is' , metrics[metrics$osv.variable == 'r.a.swc.15.cm.osv' , 'nrmse'] ))
print(paste('nRMSE for SWC 30  is' , metrics[metrics$osv.variable == 'r.a.swc.30.cm.osv' , 'nrmse'] ))
print(paste('nRMSE for TER is' , metrics[metrics$osv.variable == "r.a.ter.osv" , 'nrmse'] ))
print(paste('nRMSE for GPP is' ,  metrics[metrics$osv.variable == "r.a.gpp.osv" , 'nrmse'] ))
print(paste('nRMSE for NEE is' ,  metrics[metrics$osv.variable == "r.a.nee.osv" , 'nrmse'] ))



d.all <- as.data.frame(d.all)
d.all <- d.all[,!duplicated(colnames(d.all))]

source('gg.params.R')
source('gg.seasons.R')


d.all.plot.conditions <- (!(d.all$omit.period.2)  & d.all$date >= start.date.cald  & d.all$date <= end.date.cald   )

}


# PLOT series

{
  
  
  # biomass 
  gg.bio.decomp <-  gg.biom( 
    
    FALSE
    , FALSE
    
    , TRUE
    , FALSE
    
    , FALSE
    , FALSE
    
    , FALSE
    , FALSE
    
  )
  
  gg.bio.decomp 
  
  # TER
  gg.valid.ter.o <- gg.ter.labl
  
  
  gg.ter.no.labl <- gen.valid.plot( 'r.a.ter.osv'  , 'r.a.ter.sim'  , 'r.a.ter.sim.bc'   , gg.valid.ter.y.ax.lab , global.valid.ter.y.cord.high , global.valid.ter.y.cord.mid  , global.valid.ter.y.cord.bottm ,'no.label' , FALSE)
  gg.ter.labl <- gen.valid.plot( 'r.a.ter.osv'  , 'r.a.ter.sim'  , 'r.a.ter.sim.bc'   , gg.valid.ter.y.ax.lab , global.valid.ter.y.cord.high , global.valid.ter.y.cord.mid  , global.valid.ter.y.cord.bottm ,'label' , FALSE)
  
  
  gg.kosalam.ter <- gen.gg.kaba('ter')
  
  
  # GPP
  gg.valid.gpp.o <- gg.ter.labl
  
  
  gg.gpp.no.labl <- gen.valid.plot( 'r.a.gpp.osv'  , 'r.a.gpp.sim'  , 'r.a.gpp.sim.bc'   , gg.valid.gpp.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , FALSE)
  gg.gpp.labl <- gen.valid.plot( 'r.a.gpp.osv'  , 'r.a.gpp.sim'  , 'r.a.gpp.sim.bc'   , gg.valid.gpp.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label' , FALSE)
  
  
  gg.kosalam.gpp <- gen.gg.kaba('gpp')
  
  
  # NEE
  gg.valid.nee.o <- gg.nee.labl
  
  
  gg.nee.no.labl <- gen.valid.plot( 'r.a.nee.osv'  , 'r.a.nee.sim'  , 'r.a.nee.sim.bc'   , gg.valid.nee.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE)
  gg.nee.labl <- gen.valid.plot( 'r.a.nee.osv'  , 'r.a.nee.sim'  , 'r.a.nee.sim.bc'   , gg.valid.nee.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)
  
  
  gg.kosalam.nee <- gen.gg.kaba('nee')
  
  # SWC
  gg.valid.swc.o <- gg.swc.no.labl
  
  
  gg.swc.5.cm.no.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   ,  gg.valid.swc.5.cm.y.ax.lab, global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE)
  gg.swc.5.cm.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   ,  gg.valid.swc.5.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)
  
  
  # 15 cm layer
  gg.swc.15.cm.no.labl <- gen.valid.plot( 'r.a.swc.15.cm.osv'  , 'r.a.swc.15.cm.sim'  , 'r.a.swc.15.cm.sim.bc'   , gg.valid.swc.15.cm.y.ax.lab, global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE)
  gg.swc.15.cm.labl <- gen.valid.plot( 'r.a.swc.15.cm.osv'  , 'r.a.swc.15.cm.sim'  , 'r.a.swc.15.cm.sim.bc'   ,  gg.valid.swc.15.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)
  
  # 30 cm layer
  gg.swc.30.cm.no.labl <- gen.valid.plot( 'r.a.swc.30.cm.osv'  , 'r.a.swc.30.cm.sim'  , 'r.a.swc.30.cm.sim.bc'   ,   gg.valid.swc.30.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE)
  gg.swc.30.cm.labl <- gen.valid.plot( 'r.a.swc.30.cm.osv'  , 'r.a.swc.30.cm.sim'  , 'r.a.swc.30.cm.sim.bc'   ,  gg.valid.swc.30.cm.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label', TRUE)
  
  
  gg.kosalam.swc <- gen.gg.kaba('swc')
  
  
  # LAI
  gg.valid.lai.o <- gg.swc.no.labl
  
  
  gg.lai.no.labl <- gen.valid.plot( 'r.a.lai.osv'  , 'r.a.lai.sim'  , 'r.a.lai.sim.bc'   , gg.valid.swc.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'no.label' , TRUE)
  gg.lai.labl <- gen.valid.plot( 'r.a.swc.5.cm.osv'  , 'r.a.swc.5.cm.sim'  , 'r.a.swc.5.cm.sim.bc'   , gg.valid.swc.y.ax.lab , global.valid.gpp.y.cord.high , global.valid.gpp.y.cord.mid  , global.valid.gpp.y.cord.bottm ,'label')
  
  
  # Evapotranspiration
  {
    gg.valid.et <- gg.theme %>% + geom_line( aes(x = date.time, y = ET.osv , color = p.swc.osv.label) 
                                             , linewidth = p.ln.width * 0.6
                                             , color = p.nee.color.1
                                             
    ) + geom_line( aes(x = date.time, y = et.sim , color = p.swc.sim.label) 
                   , linewidth = p.ln.width * 0.6 
                   , color = p.nee.color.2
                   
    )   +
      
      
      ylab(gg.valid.et.y.lab) +
      facet_grid( ~ covid.et  , scales = 'free_x' , space = 'free') +
      theme(
        legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
        axis.title.x = element_blank() ,  
        legend.title = element_blank(),
        axis.title.y.right = element_blank() , 
        axis.text.y.right = element_blank() , 
        axis.text.x = element_text(angle = 270 ) ,
        #  legend.title = element_blank() ,
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
        , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
        ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
      )  
    
    # gg.valid.et
  }
  
  
  # Climate
  {
    
    cor(d.all$precip.osv , d.all$TAH.621.precip , use = "complete.obs")
    cor(d.all$precip.osv , d.all$TAH.677.precip , use = "complete.obs")
    cor(d.all$precip.osv , d.all$TAH.678.precip , use = "complete.obs")
    cor(d.all$precip.osv , d.all$TAH.814.precip , use = "complete.obs")
    
    cor(d.all$temp.avg.osv , d.all$TAH.621.temp.avg , use = "complete.obs")
    cor(d.all$temp.avg.osv , d.all$TAH.677.temp.avg , use = "complete.obs")
    cor(d.all$temp.avg.osv , d.all$TAH.678.temp.avg , use = "complete.obs")
    cor(d.all$temp.avg.osv , d.all$TAH.814.temp.avg , use = "complete.obs")
    
    cor(d.all$temp.max.osv , d.all$TAH.621.temp.max , use = "complete.obs")
    cor(d.all$temp.max.osv , d.all$TAH.677.temp.max , use = "complete.obs")
    cor(d.all$temp.max.osv , d.all$TAH.678.temp.max , use = "complete.obs")
    cor(d.all$temp.max.osv , d.all$TAH.814.temp.max , use = "complete.obs")
    
    cor(d.all$temp.min.osv , d.all$TAH.621.temp.min , use = "complete.obs")
    cor(d.all$temp.min.osv , d.all$TAH.677.temp.min , use = "complete.obs")
    cor(d.all$temp.min.osv , d.all$TAH.678.temp.min , use = "complete.obs")
    cor(d.all$temp.min.osv , d.all$TAH.814.temp.min , use = "complete.obs")
    
    
    
    
    
    
    
    gg.rain <-  gg.theme  %>%   +  
      geom_point(  data = d.all[,  ] ,
                   aes( x =date.time 
                        , y = TAH.621.precip)
                   , color = 'red' 
      ) +
      geom_point(  data = d.all[,  ] ,
                   aes( x =date.time 
                        , y = TAH.677.precip)
                   , color = 'green' 
      ) +
      geom_point(  data = d.all[,  ] ,
                   aes( x =date.time 
                        , y = TAH.678.precip)
                   , color = 'blue' 
      ) +
      geom_point(  data = d.all[,  ] ,
                   aes( x =date.time 
                        , y = TAH.814.precip)
                   , color = 'pink' 
      )
    
    
    sec <- ggh4x::help_secondary(
      name = "",
      primary = c(10, 35), secondary = c(0, 200),
    )
    
    scale <- 2.5
    shift <- 15
    
    gg.rain <- gg.theme  %>%   +  
      # ggplot() + 
      geom_bar(   data= d.all[ 
        d.all$covid %in% covid.status[c(1,2,3)] 
        #& ( d.all$date > covid.end.date | d.all$date < covid.start.date) 
        ,  ] ,
        aes( x = date.time 
             , y = precip.not.eddy.contns
             
        )  ,
        , stat = 'identity'  
        , width = p.br.wdth
        , color = 'blue'
        , alpha = p.br.alpha ) +
      geom_line( aes(x = date, y = (( temp.avg.osv *  scale)) ) 
                 , linewidth = gg.temp.ln.width 
                 
                 , color=  'red' 
      ) + 
      # Add secondary axis that displays mpg * 1.2
      scale_y_continuous(
        name = bquote(Precipiation~(mm~d^-1)),
        sec.axis = sec_axis(transform = ~ (. /  scale ) , name = "Daily mean temp ()")
      ) + 
      
      # theme(aspect.ratio = 1/3) +
      coord_cartesian()  +
      theme(
        
        plot.margin = margin(t = 0, r = 75, b = 0, l = 3.5, unit = "pt")
        
        
        , axis.text.y.right =element_text()
        , axis.title.y.right =element_text()
      )
    
    
    gg.rain
    
    
  }
  
  # Climate other
  {
    
    
    gg.climate.all <- ggplot( 
      d.eddy.oc
      ,   aes(x = date.time )  
    ) +  
      geom_line( aes(x = date, y = temp.avg.osv  ) 
                 , linewidth = gg.temp.ln.width 
                 
                 , color=  'red' 
      ) + 
      
      geom_line( aes(x = date, y = rh.osv   ) 
                 , linewidth = gg.temp.ln.width 
                 , color=  'darkorange'
      ) +
      
      geom_line( aes(x = date, y = ws.osv  ) 
                 , linewidth = gg.temp.ln.width 
                 , color=  'grey'
      ) +
      
      
      geom_bar(  #data = d.all[,  ] ,
        aes( x =date
             , y = precip.osv 
        )
        , stat = 'identity'  
        , width = p.br.wdth
        , color = p.br.clr 
        , alpha = p.br.alpha 
      )  
    
    gg.climate.all
  }
  
  
  

{
gg.validate.1.labels <- c('a' ,'b' , 'c' , 'd' )

gg.ter.plot.no.labl <-  gg.remv.x.lab( gg.ter.no.labl )
gg.gpp.plot.no.labl <-  gg.remv.x.lab( gg.gpp.no.labl )
gg.nee.plot.no.labl <-  gg.remv.x.lab( gg.nee.no.labl  )

gg.ter.plot.no.labl.koba <- ggarrange(gg.ter.plot.no.labl , gg.kosalam.ter , widths = c(2,0.5 ), nrow = 1) 
gg.gpp.plot.no.labl.koba <- ggarrange(gg.gpp.plot.no.labl , gg.kosalam.gpp , widths = c(2,0.5 ), nrow = 1) 
gg.nee.plot.no.labl.koba <- ggarrange(gg.nee.plot.no.labl , gg.kosalam.nee , widths = c(2,0.5 ), nrow = 1) 




gg.validate.1 <- ggarrange(


gg.ter.plot.no.labl.koba
, gg.gpp.plot.no.labl.koba
,gg.nee.plot.no.labl.koba
, gg.rain

, nrow = 4
, labels = gg.validate.1.labels 
, heights = c(1,1,1,.95)
, label.x = .008575
, label.y = 0.9775
)


gg.validate.1 



gg.valid.1.dpi  <-  1000

gg.valid.1.width <- 8.2
gg.valid.1.height  <- 10.0
filename.gg.validate.1 = 'Figures.out/gg.validate.1.jpg'

ggsave(filename = filename.gg.validate.1 ,  gg.validate.1 , width = gg.valid.1.width, height = gg.valid.1.height , dpi = gg.valid.1.dpi  )


# Plot 2
gg.validate.2.labels <- c('a' ,'b' ,'c' )

gg.valid.2.heights <- c(1,1,1.275)

gg.validate.2 <- ggarrange(

# gg.valid.lai
gg.valid.swc
,  gg.bio.decomp
, gg.climate

, labels = gg.validate.2.labels 

, heights = gg.valid.2.heights 

, nrow = 3
, label.x = .9575
, label.y = c(0.9175, 0.9175, 0.97)
)

gg.validate.2 


gg.valid.dpi  <-  2500

gg.valid.2.width <- 7.5
gg.valid.2.height  <- 8
filename.gg.validate.2 = 'Figures.out/gg.validate.2.jpg'

ggsave(filename =    filename.gg.validate.2 ,  gg.validate.2 , width = gg.valid.2.width, height = gg.valid.2.height , dpi = gg.valid.dpi  )
}  # PLOT 1 - TER, GPP, NEE - out


{
    gg.validate.1.labels <- c('a' ,'b' , 'c' , 'd' )
    
    gg.swc.5.cm.plot.no.labl <-  gg.remv.x.lab( gg.swc.5.cm.no.labl )
    gg.swc.15.cm.plot.no.labl <-  gg.remv.x.lab( gg.swc.15.cm.no.labl )
    gg.swc.30.cm.plot.no.labl <-  gg.remv.x.lab( gg.swc.30.cm.no.labl )
    
    gg.ter.plot.no.labl.koba <- ggarrange(gg.ter.plot.no.labl , gg.kosalam.ter , widths = c(2,0.5 ), nrow = 1) 
    gg.gpp.plot.no.labl.koba <- ggarrange(gg.gpp.plot.no.labl , gg.kosalam.gpp , widths = c(2,0.5 ), nrow = 1) 
    gg.nee.plot.no.labl.koba <- ggarrange(gg.nee.plot.no.labl , gg.kosalam.nee , widths = c(2,0.5 ), nrow = 1) 
    
    
    
    
    gg.validate.rain <- ggarrange(
      
      
      gg.swc.5.cm.plot.no.labl
      ,    gg.swc.15.cm.plot.no.labl 
      ,  gg.swc.30.cm.plot.no.labl
      
      , nrow = 3
      , labels = gg.validate.1.labels 
      , heights = c(1,1,1,.95)
      , label.x = .008575
      , label.y = 0.9775
    )
    
    
    gg.validate.rain
    
    

    filename.gg.rain = 'Figures.out/gg.rain.jpg'
    
    ggsave(filename =  filename.gg.rain ,    gg.validate.rain, width = 7.5 , height = 8 , dpi = 2500  )
 
    
     }  # PLOT 2 - SWC 5, 15, 30 - out
  
  
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


