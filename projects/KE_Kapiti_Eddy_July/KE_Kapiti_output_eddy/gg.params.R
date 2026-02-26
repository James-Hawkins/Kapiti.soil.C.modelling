p.precip.br.alpha  <<- 0.35
p.precip.br.wdth <<- .1
p.precip.bar.fill <<- 'grey'


gg.valid.label.fs <<- 2.7


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
  
  gg.valid.nee.y.ax.lab <<- 'Net ecosystem exchange (kg C/ha/day)'  
  gg.valid.gpp.y.ax.lab <<- 'Gross primary productivity (kg C/ha/day)'  
  gg.valid.ter.y.ax.lab <<- 'Total ecosystem respiration (kg C/ha/day)'
  gg.valid.agb.grass.y.ax.lab  <<- 'Grass yield (kg/ha)'
  gg.valid.et.y.lab <<- 'Evapotranspiration (mm/d)'
  gg.valid.lai.y.lab <<- 'Leaf area index'
  gg.valid.agb.y.lab <- 'Dry matter yield (Mg/ha/yr)'
  
  gg.valid.leg.y.crd <- 0.78
  gg.valid.leg.x.crd <- 0.55
  
  gg.valid.y.ax.tit.fs <- 9
  
  
  p.x.ax.lab <<- 'Date (YY-MM-DD)'  
  
  gg.valid.swc.y.ax.lab <<- 'Soil water content (%)'
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
  gg.temp.ln.width  <<- 0.11
  
  
  global.valid.sum.date <<- "2024-04-01"
  
  global.valid.text.color <- 'black'
  global.valid.text.background <- 'white'
  
  p.br.wdth <<- .15
  
  p.br.alpha <<- 0.6
  
  p.ln.width <<- 0.4
  
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
  p.ln.colr.mod.ub <- '#EE4B2B'
  p.ln.colr.mod.bc <- '#2EF527'
  p.ln.colr.obsv  <- '#1B1212'
  
  p.colors <- c(p.ln.colr.obsv , p.ln.colr.mod.ub  , p.ln.colr.mod.bc )
  
  p.nee.color.1 <- p.ln.colr.obsv
  p.nee.color.2 <- p.ln.colr.mod
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