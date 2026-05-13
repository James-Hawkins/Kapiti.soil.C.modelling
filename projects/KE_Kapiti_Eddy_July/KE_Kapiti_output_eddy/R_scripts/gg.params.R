

# source('gg.params.R')

p.precip.br.alpha  <<- 0.35
p.precip.br.wdth <<- .1
p.precip.bar.fill <<- 'grey'


gg.valid.label.fs <<- 2.7
gg.valid.sim.point.size <- .12



global.valid.ter.y.cord.high <<- 0.9 * max( c( na.omit(d.all$r.a.ter.osv ), na.omit(d.all$r.a.ter.sim)) )   
global.valid.ter.y.cord.mid <<- global.valid.ter.y.cord.high * 0.9
global.valid.ter.y.cord.bottm <<-  global.valid.ter.y.cord.high * 0.8


global.valid.gpp.y.cord.high <<- 0.9 * max( c( d.all$r.a.gpp.osv , d.all$r.a.gpp.sim) )   
global.valid.gpp.y.cord.mid <<- global.valid.gpp.y.cord.high * 0.9
global.valid.gpp.y.cord.bottm <<-  global.valid.gpp.y.cord.high * 0.8


global.valid.swc.y.cord.high <<- 62 # 0.9 * max( c( d.all$r.a.swc.osv , d.all$r.a.swc.sim) )   
global.valid.swc.y.cord.mid <<- global.valid.swc.y.cord.high * 0.9
global.valid.swc.y.cord.bottm <<-  global.valid.swc.y.cord.high * 0.8




global.valid.gpp.y.cord.covid <<- 25
global.valid.swc.y.cord.covid <- 25

gg.valid.label.covid.period <- ' - - - Covid - - -'

gg.valid.labels.h.just <<- 1

global.valid.covid.label.date <- as.Date("2021-04-01")

# Plot parameters
{
  
  gg.valid.labels <- c(
    'L-DNDC'
    ,     'Eddy flux tower'    
  )
  
  gg.valid.nee.y.ax.lab <<- bquote("NEE(kg C ha"^-1*" d"^-1*")")
  gg.valid.gpp.y.ax.lab <<- bquote("GPP (kg C ha"^-1*" d"^-1*")") 
  gg.valid.ter.y.ax.lab <<- bquote("R"[Eco]*" (kg C ha"^-1*" d"^-1*")")
  gg.valid.agb.grass.y.ax.lab  <<- 'Grass yield (kg/ha)'
  gg.valid.et.y.lab <<- 'Evapotranspiration (mm/d)'
  gg.valid.lai.y.lab <<- 'Leaf area index'
  gg.valid.agb.y.lab <<- 'Above-ground biomass (Mg DM/ha/yr)'
  
  gg.valid.leg.y.crd <- 0.78
  gg.valid.leg.x.crd <- 0.55
  
  gg.valid.y.ax.tit.fs <- 12
  
  
  p.x.ax.lab <<- 'Date (YY-MM-DD)'  
  
  gg.valid.swc.5.cm.y.ax.lab <<- 'SWC - 5 cm (%)'
  gg.valid.swc.15.cm.y.ax.lab <<- 'SWC - 15 cm (%)'
  gg.valid.swc.30.cm.y.ax.lab <<- 'SWC - 30 cm  (%)'
  p.et.y.ax.lab  <- 'Evapotranspiration (mm/d)'
  p.lai.y.ax.lab  <- 'Leaf area index'
  
  p.precip.sec.ax.tit <- 'Precipitation (mm/day)'
  
  
  global.valid.ter.y.cord <<- 42
  global.valid.gpp.y.cord <<- 90
  global.valid.swc.y.cord <<- 80
  global.valid.nee.y.cord <<- -125
  
  
  p.mrgn.main.top <- 0.2
  p.mrgn.main.right <-  1.12
  p.mrgn.main.bottom <- 0.05
  p.mrgn.main.left <- 0.2
  
  gg.climate.y.ax.lab.temp <<- 'Temperature (Degrees Celsius)'
  gg.temp.ln.width  <<- 0.55
  
  
  global.valid.sum.date <<- "2024-04-01"
  
  global.valid.text.color <- 'black'
  global.valid.text.background <- 'white'
  
  p.br.wdth <<- .15
  
  p.br.alpha <<- 0.6
  
  p.ln.width <<- 0.6
  
  p.date.interval.x.axis <- "3 month"
  
  gg.valid.date.r2.x.crd <<- 0.5
  gg.valid.date.r2.y.crd  <<- 75
  
  p.lab.nee.tx.fs <- 4.75
  
  gg.climate.x.txt.fs <- 13.25
  
  
  gg.valid.panel.border.line.thickness <- 1
  gg.valid.facet.text.size <- 11
  
  # NRMSE labels
  #gg.valid.lab.nee.rmse <-  paste0("NRMSE:~",NEE.NRMSE.actual )
  #gg.valid.lab.ter.rmse <-  paste0("NRMSE:~",TER.NRMSE.actual )
  #gg.valid.lab.gpp.rmse <-  paste0("NRMSE:~",GPP.NRMSE.actual )
  
  
  p.br.clr <<- '#87C0FF'
  p.ln.colr.mod.ub <<- '#E97451'
  p.ln.colr.mod.bc <<- '#9AE630'
  p.ln.colr.obsv  <<- '#585858'
  
  p.colors <- c(p.ln.colr.obsv , p.ln.colr.mod.ub  , p.ln.colr.mod.bc )
  
  p.nee.color.1 <- p.ln.colr.obsv
  p.nee.color.2 <- p.ln.colr.mod.ub
  p.nee.color.3 <- 'lightblue'
  p.nee.color.4 <- 'pink'
  
  p.lai.color.grass <<- '#FDC745'
  p.lai.color.trees <<- '#7BF1A8'
  p.lai.color.all <<- 'black'
  
  p.lai.color.obs <- 'black'
  
  gg.valid.date.x.ax.lab <- as.Date("2019-03-01")
  

  
  p.ssn.x.ranges.2019.rn.2.min <- start.date.cald
  p.ssn.x.ranges.2019.rn.2.max <- "2019-12-31"
  
  
  p.ssn.x.ranges.2020.dr.1.min <- "2020-01-01" 
  p.ssn.x.ranges.2020.dr.1.max <- "2020-02-29" 
  
  p.ssn.x.ranges.2020.rn.1.min <- "2020-03-01" 
  p.ssn.x.ranges.2020.rn.1.max <- "2020-05-31" 
  
  p.ssn.x.ranges.2020.dr.2.min <- "2020-06-01" 
  p.ssn.x.ranges.2020.dr.2.max <- final.date
  
  
  p.rn.ssn.clr <- '#eaffdf'
  p.dr.ssn.clr <- '#fef2c6'
  
  p.ssn.bg.alpha <- 0.1
  
  
}




gg.kosalam.gen <<- ggplot( biases.long ) +
  theme(
    plot.margin = margin( 
      
    0
      , 0
      , 0
      , 0.2
      
      , "cm"  ) , 
    
    axis.title.y = element_text(size = 10)
    , axis.title.x=  element_blank()
    , axis.ticks.x  = element_blank()
    , axis.text.x  = element_text( angle =90 )
    , panel.grid.major = element_blank()
    , panel.background = element_blank()
    , panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
  ) +
  ylab('Absolute error')




gg.theme <<-   ggplot( d.all[ d.all.plot.conditions ,  ] ,   aes(x = date.time)) +
  #  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d" , limits = c(start.date.cald , end.date.cald)) +
  scale_x_date(   limits = c(as.Date(start.date.cald) , as.Date(end.date.cald)),
                  #, date_labels = "%m %Y", # Format the labels as "Mon YYYY"
                  date_breaks = "3 months"
                  , expand=c(0.00025,0.00025)
                  
                  , date_labels = "%b - %Y"
                  #, date_breaks = "1 month"
                  
  ) +
  
  #geom_vline(xintercept =  as.numeric(as.Date(dipole.period.start)) , linetype = 'dotted' , linewidth =.1 ) +   
 # geom_vline(xintercept =  as.numeric(as.Date(dipole.period.end )) , linetype = 'dotted' , linewidth =.1 ) +  
  
 # geom_vline(xintercept =  as.numeric(as.Date(drought.period.start)) , linetype = 'dotted' , linewidth =.1 ) +   
 # geom_vline(xintercept =  as.numeric(as.Date(drought.period.end )) , linetype = 'dotted' , linewidth =.1) +   
   
  theme(
    legend.position = "none" , #c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
    axis.title.x = element_blank() ,  
    legend.title = element_blank(),
    axis.title.y.right = element_blank() , 
    axis.title.y.left = element_text(size = gg.valid.y.ax.tit.fs ) , 
    axis.text.y.right = element_blank() , 
    axis.text.x = element_text(angle = 270 , hjust = 0.5 , vjust = 0.5) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
    ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  )  +
  geom_rect(
    data = season.df[ !is.na(season.df$xmin) & !is.na(season.df$xmax) & !is.na(season.df$covid.climate) , ]  ,
    aes(xmin =  xmin
        , xmax = xmax, 
        ymin = ymin ,
        ymax = ymax
        , fill = fill
    )
    , inherit.aes = FALSE 
    , alpha = .2
  )  +
  scale_fill_manual(values = c(
    
    "#D6C1AB" =   "#D6C1AB"
    , "#98C3ED" = "#98C3ED"
    , '#CAD8ED' = '#CAD8ED' 
    , '#BBD6F2' = '#BBD6F2'
    , '#B89676' = '#B89676'
    , '#cc9966' = '#cc9966'
    
    
  ) ) +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "L-DNDC"  =  p.ln.colr.mod.ub
      , "Eddy flux tower" =  p.ln.colr.obsv  
      , 'bias.corrected' =  p.ln.colr.mod.bc
    ) 
    , breaks = c(
      gg.valid.labels[1]
      , gg.valid.labels[2]
      , 'bias.corrected' 
    ) 
  )  

