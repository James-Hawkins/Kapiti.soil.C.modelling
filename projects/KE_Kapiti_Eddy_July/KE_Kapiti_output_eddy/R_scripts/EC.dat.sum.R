



update.gen.fig <- function(){
  
  generic.theme <<- ggplot( d.eddy.real ) +
    theme(
      plot.margin = margin( 0.001, 0.001, 0.00000001, 0.001, "pt") ,
      
      
       legend.position = "none" ,
       axis.title.x = element_blank() , 
      
     axis.text.x = element_text(angle = 90 , vjust = 0.5 , size = 12) , 
     axis.text.y = element_text(vjust = 0.5 , size = 12) , 
     axis.ticks.x = element_line() , 
      
        legend.title = element_blank() ,
       panel.grid.major = element_blank(),
        panel.background = element_blank(),
       axis.title.y = element_text( size = 17.5),
       panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    ) + 
    xlab('') +
    geom_rect(
      data = season.df[  !is.na(season.df$xmin) & !is.na(season.df$xmax) & !is.na(season.df$covid.climate), ]  ,
      aes(xmin =  xmin
          , xmax = xmax, 
          ymin = ymin ,
          ymax = ymax
          , fill = fill
      )
      , inherit.aes = FALSE 
      , alpha = .2
    )  +
    scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
    scale_fill_manual(values = c(
      
      "#D6C1AB" =   "#D6C1AB"
      , "#98C3ED" = "#98C3ED"
      , '#CAD8ED' = '#CAD8ED' 
      , '#BBD6F2' = '#BBD6F2'
      , '#B89676' = '#B89676'
      , '#cc9966' = '#cc9966'
      
      
      ) ) 


  y.lab.rg <<- 'Radiation (W/m^2)   '
  y.lab.temp.avg <<-'Temperature (Degrees C)'
  y.lab.precip <<- 'Precipitation (mm/day)  '
  y.lab.rh <<- 'Relative humidity (%)  '
  y.lab.ws <<- 'Wind speed (m/s)   '
  
  color.mn.filled <<- 'orange'
  gg.clim.line.width <<- 0.75
  
  
}



# Temp
d.eddy.real$temp.avg.osv
d.eddy.real[ , 'temp.avg.subs.infd']  <- NA
d.eddy.real[ , 'temp.avg.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.temp.avg %in%  v.status.subs.filled.all & !is.na(d.eddy.real$variable.status.temp.avg ) , 'temp.avg.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.temp.avg  %in%  v.status.subs.filled.all  & !is.na(d.eddy.real$variable.status.temp.avg ), 'temp.avg.osv']
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.temp.avg ), 'temp.avg.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.temp.avg ), 'temp.avg.osv']

d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.temp.avg ), 'plot.temp.avg.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.temp.avg %in%  v.status.subs.filled.all  & !is.na(d.eddy.real$variable.status.temp.avg ), 'plot.temp.avg.osv'] <- NA

update.gen.fig()

gg.temp.avg <- generic.theme %>% +
  theme(
  #  axis.text.x = element_blank(),

    , axis.ticks.x =  element_blank(),
  ) +
  geom_line( aes(x = date, y = temp.avg.osv ) , color = 'grey' , linewidth = gg.clim.line.width ) +
 # geom_line( aes(x = date, y = temp.avg.mn.infd )  , color = 'pink') +
 geom_line( aes(x = date, y = temp.avg.subs.infd )  , color = color.mn.filled , linewidth =  gg.clim.line.width) +
  ylab(y.lab.temp.avg) 

gg.temp.avg



# Precip
d.eddy.real[ , 'precip.subs.infd']  <- NA
d.eddy.real[ , 'precip.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.precip %in%  v.status.subs.filled.all  & !is.na(d.eddy.real$variable.status.precip )  , 'precip.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.precip %in%  v.status.subs.filled.all & !is.na(d.eddy.real$variable.status.precip ) , 'precip.osv']
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled & !is.na(d.eddy.real$variable.status.precip ) , 'precip.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled & !is.na(d.eddy.real$variable.status.precip ) , 'precip.osv']

d.eddy.real[ d.eddy.real$variable.status.precip != v.status.mn.filled & d.eddy.real$variable.status.precip != v.status.subs.filled & !is.na(d.eddy.real$variable.status.precip ) , 'plot.precip.osv'] <- d.eddy.real[ d.eddy.real$variable.status.precip != v.status.mn.filled & d.eddy.real$variable.status.precip != v.status.subs.filled & !is.na(d.eddy.real$variable.status.precip ) , 'precip.osv']
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled & !is.na(d.eddy.real$variable.status.precip ) , 'plot.precip.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.precip %in%  v.status.subs.filled.all & !is.na(d.eddy.real$variable.status.precip ) , 'plot.precip.osv'] <- NA

update.gen.fig()

gg.precip <- generic.theme %>% +
 geom_line( aes(x = date, y = precip.osv ) , color = 'grey' , linewidth =  gg.clim.line.width) +
 # geom_line( aes(x = date, y = plot.precip.osv ) , color = 'grey' ) +
#  geom_line( aes(x = date, y = precip.mn.infd )  , color = 'pink') +
  geom_line( aes(x = date, y = precip.subs.infd )  , color = color.mn.filled , linewidth =  gg.clim.line.width) +
  ylab(y.lab.precip)
  

gg.precip

# Radiation
d.eddy.real[ , 'rg.subs.infd']  <- NA
d.eddy.real[ , 'rg.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.rg %in% v.status.subs.eo.filled.all & !is.na(d.eddy.real$variable.status.rg ) , 'rg.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rg %in% v.status.subs.eo.filled.all & !is.na(d.eddy.real$variable.status.rg ) , 'rg.osv']
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rg.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rg== v.status.mn.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rg.osv']

d.eddy.real[ d.eddy.real$variable.status.rg != v.status.mn.filled & d.eddy.real$variable.status.rg != v.status.subs.filled & !is.na(d.eddy.real$variable.status.rg ) , 'plot.rg.osv'] <- d.eddy.real[ d.eddy.real$variable.status.rg != v.status.mn.filled & d.eddy.real$variable.status.rg != v.status.subs.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rg.osv']
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.rg ) , 'plot.rg.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.rg %in% v.status.subs.eo.filled.all & !is.na(d.eddy.real$variable.status.rg ) , 'plot.rg.osv'] <- NA


update.gen.fig()

d.eddy.real[ !is.na(d.eddy.real$rg.subs.infd) , 'rg.subs.infd' ] 

gg.rg <- generic.theme %>% +
  geom_line( aes(x = date, y = rg.osv ) , color = 'grey'  , linewidth =  gg.clim.line.width) +
 # geom_line( aes(x = date, y = rg.mn.infd )  , color = 'pink') +
  geom_line(d.eddy.real[ !is.na(d.eddy.real$rg.subs.infd) , ] , mapping = aes(x = date, y = rg.subs.infd )  , color = color.mn.filled , linewidth =  gg.clim.line.width) +
  ylab(y.lab.rg)

gg.rg

# Relative humidity
d.eddy.real[ , 'rh.subs.infd']  <- NA
d.eddy.real[ , 'rh.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.rh %in% v.status.subs.eo.filled.all & !is.na(d.eddy.real$variable.status.rh ) , 'rh.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rh %in% v.status.subs.eo.filled.all & !is.na(d.eddy.real$variable.status.rh ), 'rh.osv']
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.mn.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rh.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rh== v.status.mn.filled & !is.na(d.eddy.real$variable.status.rh ), 'rh.osv']

d.eddy.real[ d.eddy.real$variable.status.rh != v.status.mn.filled & d.eddy.real$variable.status.rh != v.status.subs.filled, 'plot.rh.osv'] <- d.eddy.real[ d.eddy.real$variable.status.rh != v.status.mn.filled & d.eddy.real$variable.status.rh != v.status.subs.filled & !is.na(d.eddy.real$variable.status.rh ), 'rh.osv']
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.mn.filled & !is.na(d.eddy.real$variable.status.rh ), 'plot.rh.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.rh %in% v.status.subs.eo.filled.all & !is.na(d.eddy.real$variable.status.rh ), 'plot.rh.osv'] <- NA



update.gen.fig()

gg.rh <- generic.theme %>% +
  geom_line( aes(x = date, y = plot.rh.osv ) , color = 'grey' , linewidth =  gg.clim.line.width) +
 geom_line( aes(x = date, y = rh.subs.infd )  , color = color.mn.filled , linewidth =  gg.clim.line.width) +
  ylab(y.lab.rh) + theme(
  #  axis.text.x = element_text(angle = 290 , vjust = 0.5 ) 
  )


gg.rh

# Windspeed
d.eddy.real[ , 'ws.subs.infd']  <- NA
d.eddy.real[ , 'ws.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.ws %in% v.status.subs.eo.filled.all & !is.na(d.eddy.real$variable.status.ws ) , 'ws.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.ws %in% v.status.subs.eo.filled.all  & !is.na(d.eddy.real$variable.status.ws ), 'ws.osv']


d.eddy.real[ d.eddy.real$variable.status.ws != v.status.subs.filled & !is.na(d.eddy.real$variable.status.ws ), 'plot.ws.osv'] <- d.eddy.real[  d.eddy.real$variable.status.ws != v.status.subs.filled & !is.na(d.eddy.real$variable.status.ws ), 'ws.osv']
d.eddy.real[ d.eddy.real$variable.status.ws %in% v.status.subs.eo.filled.all & !is.na(d.eddy.real$variable.status.ws ), 'plot.ws.osv'] <- NA



update.gen.fig()

gg.ws <- generic.theme %>% +
  geom_line( aes(x = date, y = plot.ws.osv ) , color = 'grey', linewidth =  gg.clim.line.width ) +
  geom_line( aes(x = date, y = ws.subs.infd )  , color = color.mn.filled , linewidth =  gg.clim.line.width) +
  ylab(y.lab.ws) +
  theme(
    axis.text.x = element_text(angle = 290 , vjust = 0.5 ) 
  )

gg.ws


# Merge prep
#
gg.ws.merge <- gg.ws %>% + theme(axis.text.x = element_blank())
gg.rg.merge <- gg.rg %>% + theme(axis.text.x = element_blank())
gg.rh.merge <- gg.rh %>% + theme(axis.text.x = element_blank())
gg.temp.avg.merge <- gg.temp.avg %>% + theme(axis.text.x = element_blank())


{

gg.ec.sum.labels <- c(
    'a' 
  ,  'b'  
  , 'c'
  , 'd'
  , 'e'
  )




gg.ec.rel.clim.heights <- c(
1.0
, 1
,1
,1
, 1.45
)


} # Plot params

gg.ec.summary.clim <-  ggarrange(
  
  
    gg.ws.merge
  , gg.rh.merge
   , gg.rg.merge
   , gg.temp.avg.merge
  
  ,   gg.precip
  
  

  , ncol = 1
  , nrow = 5
  
 , labels =gg.ec.sum.labels 
  , heights = gg.ec.rel.clim.heights
)

# gg.ec.summary.clim


file.clim.fig <- str_c(  R.results.dir  , 'gg.ec.summary.clim.jpg' )



ggsave(
  filename = file.clim.fig,
  gg.ec.summary.clim ,
  width = 11,
  height = 14,
  dpi = 900
)




# SUMMARIZE which sources the final weather data comes from
table(d.eddy.real$variable.status.temp.avg)
table(d.eddy.real$variable.status.temp.min)
table(d.eddy.real$variable.status.temp.max)
table(d.eddy.real$variable.status.precip)


# Non TAHMO series
table(d.eddy.real$variable.status.rg)
table(d.eddy.real$variable.status.rh)
table(d.eddy.real$variable.status.ws)




# Identify how many dates/rows are missing observed data
nrow(  d.eddy.real[  is.na(d.eddy.real$temp.avg.osv) & d.eddy.real$date %in% sim.period , ] )
nrow(  d.eddy.real[  is.na(d.eddy.real$temp.min.osv) & d.eddy.real$date %in% sim.period , ] )
nrow(  d.eddy.real[  is.na(d.eddy.real$temp.max.osv) & d.eddy.real$date %in% sim.period , ] )
nrow(  d.eddy.real[  is.na(d.eddy.real$precip.osv) & d.eddy.real$date %in% sim.period , ] )

nrow(  d.eddy.real[  is.na(d.eddy.real$rg.osv) & d.eddy.real$date %in% sim.period , ] )
nrow(  d.eddy.real[  is.na(d.eddy.real$rh.osv) & d.eddy.real$date %in% sim.period , ] )
nrow(  d.eddy.real[  is.na(d.eddy.real$ws.osv) & d.eddy.real$date %in% sim.period , ] )

# Identify which dates there are still NA values
d.eddy.real[  is.na(d.eddy.real$temp.avg.osv) & d.eddy.real$date %in% sim.period , 'date'] 
d.eddy.real[  is.na(d.eddy.real$temp.min.osv) & d.eddy.real$date %in% sim.period , 'date'] 
d.eddy.real[  is.na(d.eddy.real$temp.max.osv) & d.eddy.real$date %in% sim.period , 'date'] 

d.eddy.real[  is.na(d.eddy.real$precip.osv) & d.eddy.real$date %in% sim.period , 'date'] 
d.eddy.real[  is.na(d.eddy.real$rg.osv) & d.eddy.real$date %in% sim.period , 'date'] 
d.eddy.real[  is.na(d.eddy.real$rh.osv) & d.eddy.real$date %in% sim.period , 'date'] 
d.eddy.real[  is.na(d.eddy.real$ws.osv) & d.eddy.real$date %in% sim.period , 'date'] 



# IDENTIFY CORRELATIONS WITH AUXILIARY DATASETS

# NASA POWER
precip.cmn.dates.POWER <- intersect( d.eddy.real[ d.eddy.real$variable.status.precip ==   "actual",   'date' ] , d.power$date )
temp.avg.cmn.dates.POWER <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.avg ==   "actual",   'date' ] , d.power$date )
temp.min.cmn.dates.POWER <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.min ==   "actual",   'date' ] , d.power$date )
temp.max.cmn.dates.POWER <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.max ==   "actual",   'date' ] , d.power$date )
rad.cmn.dates.POWER <- intersect( d.eddy.real[ d.eddy.real$variable.status.rg ==   "actual",   'date' ] , d.power$date )
ws.cmn.dates.POWER <- intersect( d.eddy.real[ d.eddy.real$variable.status.ws ==   "actual",   'date' ] , d.power$date )
rh.cmn.dates.POWER <- intersect( d.eddy.real[ d.eddy.real$variable.status.rh ==   "actual",   'date' ] , d.power$date )


# Pearson's correlations
cor(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER,   'precip.osv' ]  ,  d.power[d.power$date %in% precip.cmn.dates.POWER , 'precip' ]  )  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.POWER,   'temp.avg.osv' ]  ,  d.power[d.power$date %in% temp.avg.cmn.dates.POWER, 'temp.mn' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.POWER  ,   'temp.min.osv' ]  ,  d.power[d.power$date %in% temp.min.cmn.dates.POWER, 'temp.min' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.POWER ,   'temp.max.osv' ]  ,  d.power[d.power$date %in% temp.max.cmn.dates.POWER, 'temp.max' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% rad.cmn.dates.POWER ,   'rg.osv' ]  ,  d.power[d.power$date %in% rad.cmn.dates.POWER , 'rg' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% ws.cmn.dates.POWER ,   'ws.osv' ]  ,  d.power[d.power$date %in% ws.cmn.dates.POWER , 'ws' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% rh.cmn.dates.POWER,   'rh.osv' ]  ,  d.power[d.power$date %in% rh.cmn.dates.POWER , 'rh' ])  

# Mean comparisons
mn.bias.npower.precip <- mean( d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.POWER,   'precip.osv' ]  ) /   mean( d.power[d.power$date %in% precip.cmn.dates.POWER , 'precip' ]) 
mn.bias.npower.temp.mean <- mean(  d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.POWER,   'temp.avg.osv' ]   ) /   mean( d.power[d.power$date %in% temp.avg.cmn.dates.POWER, 'temp.mn' ])  
mn.bias.npower.temp.min <-  mean(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.POWER  ,   'temp.min.osv' ]   ) /   mean(  d.power[d.power$date %in% temp.min.cmn.dates.POWER, 'temp.min' ])  
mn.bias.npower.temp.max <-  mean(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.POWER ,   'temp.max.osv' ]  ) /   mean(  d.power[d.power$date %in% temp.max.cmn.dates.POWER, 'temp.max' ])  
mn.bias.npower.rg <-  mean(  d.eddy.real[ d.eddy.real$date %in% rad.cmn.dates.POWER ,   'rg.osv' ]  ) /   mean(  d.power[d.power$date %in% rad.cmn.dates.POWER , 'rg' ])  
mn.bias.npower.ws <- mean(   d.eddy.real[ d.eddy.real$date %in% ws.cmn.dates.POWER ,   'ws.osv' ]   ) /   mean(  d.power[d.power$date %in% ws.cmn.dates.POWER , 'ws' ])  
mn.bias.npower.rh  <-  mean(  d.eddy.real[ d.eddy.real$date %in% rh.cmn.dates.POWER,   'rh.osv' ] ) /   mean( d.power[d.power$date %in% rh.cmn.dates.POWER , 'rh' ])  




#  TAHMO 621
precip.cmn.dates.TAHMO.621 <- intersect( d.eddy.real[ d.eddy.real$variable.status.precip ==   "actual",   'date' ] , d.weather.subs.621[ !is.na(d.weather.subs.621$precip)    , 'date'] )
temp.avg.cmn.dates.TAHMO.621 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.avg ==   "actual",   'date' ] ,d.weather.subs.621[ !is.na(d.weather.subs.621$temp.mn)    , 'date'] )
temp.min.cmn.dates.TAHMO.621 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.min ==   "actual",   'date' ] , d.weather.subs.621[ !is.na(d.weather.subs.621$temp.min)    , 'date'] )
temp.max.cmn.dates.TAHMO.621 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.max ==   "actual",   'date' ] , d.weather.subs.621[ !is.na(d.weather.subs.621$temp.max)    , 'date'] )


# Pearson's correlations
cor(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.621 ,  'precip.osv' ]  ,  d.weather.subs.621[d.weather.subs.621$date %in% precip.cmn.dates.TAHMO.621 , 'precip' ]  ,  method = 'pearson')  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.621 ,  'temp.avg.osv' ]  ,  d.weather.subs.621[d.weather.subs.621$date %in% temp.avg.cmn.dates.TAHMO.621 , 'temp.mn' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.621,  'temp.min.osv' ]  ,  d.weather.subs.621[d.weather.subs.621$date %in% temp.min.cmn.dates.TAHMO.621, 'temp.min' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.621 ,  'temp.max.osv' ]  ,  d.weather.subs.621[d.weather.subs.621$date %in% temp.max.cmn.dates.TAHMO.621  , 'temp.max' ])  

# Mean comparisons
mn.bias.ta.621.precip <<-  mean(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.621 ,  'precip.osv' ] ) /   mean(  d.weather.subs.621[d.weather.subs.621$date %in% precip.cmn.dates.TAHMO.621 , 'precip' ]  ) 
mn.bias.ta.621.mean.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.621 ,  'temp.avg.osv' ] ) /  mean(  d.weather.subs.621[d.weather.subs.621$date %in% temp.avg.cmn.dates.TAHMO.621 , 'temp.mn' ])  
mn.bias.ta.621.min.temp <<- mean(    d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.621,  'temp.min.osv' ]  ) /  mean(  d.weather.subs.621[d.weather.subs.621$date %in% temp.min.cmn.dates.TAHMO.621, 'temp.min' ])  
mn.bias.ta.621.max.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.621 ,  'temp.max.osv' ] ) /  mean(  d.weather.subs.621[d.weather.subs.621$date %in% temp.max.cmn.dates.TAHMO.621  , 'temp.max' ])  



#  TAHMO 678  
precip.cmn.dates.TAHMO.678 <- intersect( d.eddy.real[ d.eddy.real$variable.status.precip ==   "actual",   'date' ] , d.weather.subs.678[ !is.na(d.weather.subs.678$precip)    , 'date'] )
temp.avg.cmn.dates.TAHMO.678 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.avg ==   "actual",   'date' ] ,d.weather.subs.678[ !is.na(d.weather.subs.678$temp.mn)    , 'date'] )
temp.min.cmn.dates.TAHMO.678 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.min ==   "actual",   'date' ] , d.weather.subs.678[ !is.na(d.weather.subs.678$temp.min)    , 'date'] )
temp.max.cmn.dates.TAHMO.678 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.max ==   "actual",   'date' ] , d.weather.subs.678[ !is.na(d.weather.subs.678$temp.max)    , 'date'] )


cor(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.678 ,  'precip.osv' ]  ,  d.weather.subs.678[d.weather.subs.678$date %in% precip.cmn.dates.TAHMO.678 , 'precip' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.678 ,  'temp.avg.osv' ]  ,  d.weather.subs.678[d.weather.subs.678$date %in% temp.avg.cmn.dates.TAHMO.678 , 'temp.mn' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.678,  'temp.min.osv' ]  ,  d.weather.subs.678[d.weather.subs.678$date %in% temp.min.cmn.dates.TAHMO.678, 'temp.min' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.678 ,  'temp.max.osv' ]  ,  d.weather.subs.678[d.weather.subs.678$date %in% temp.max.cmn.dates.TAHMO.678  , 'temp.max' ])  


# Mean comparisons
mn.bias.ta.678.precip <<- mean(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.678  ,  'precip.osv' ] ) /   mean(  d.weather.subs.678 [d.weather.subs.678$date %in% precip.cmn.dates.TAHMO.678  , 'precip' ]  ) 
mn.bias.ta.678.mean.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.678  ,  'temp.avg.osv' ] ) /  mean(  d.weather.subs.678 [d.weather.subs.678$date %in% temp.avg.cmn.dates.TAHMO.678  , 'temp.mn' ])  
mn.bias.ta.678.min.temp <<- mean(    d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.678 ,  'temp.min.osv' ]  ) /  mean(  d.weather.subs.678 [d.weather.subs.678$date %in% temp.min.cmn.dates.TAHMO.678 , 'temp.min' ])  
mn.bias.ta.678.max.temp <<-mean(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.678  ,  'temp.max.osv' ] ) /  mean(  d.weather.subs.678 [d.weather.subs.678$date %in% temp.max.cmn.dates.TAHMO.678   , 'temp.max' ])  


#  TAHMO 814  
precip.cmn.dates.TAHMO.814 <- intersect( d.eddy.real[ d.eddy.real$variable.status.precip ==   "actual",   'date' ] , d.weather.subs.814[ !is.na(d.weather.subs.814$precip)    , 'date'] )
temp.avg.cmn.dates.TAHMO.814 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.avg ==   "actual",   'date' ] ,d.weather.subs.814[ !is.na(d.weather.subs.814$temp.mn)    , 'date'] )
temp.min.cmn.dates.TAHMO.814 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.min ==   "actual",   'date' ] , d.weather.subs.814[ !is.na(d.weather.subs.814$temp.min)    , 'date'] )
temp.max.cmn.dates.TAHMO.814 <- intersect( d.eddy.real[ d.eddy.real$variable.status.temp.max ==   "actual",   'date' ] , d.weather.subs.814[ !is.na(d.weather.subs.814$temp.max)    , 'date'] )

cor(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.814 ,  'precip.osv' ]  ,  d.weather.subs.814[d.weather.subs.814$date %in% precip.cmn.dates.TAHMO.814 , 'precip' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.814 ,  'temp.avg.osv' ]  ,  d.weather.subs.814[d.weather.subs.814$date %in% temp.avg.cmn.dates.TAHMO.814 , 'temp.mn' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.814,  'temp.min.osv' ]  ,  d.weather.subs.814[d.weather.subs.814$date %in% temp.min.cmn.dates.TAHMO.814, 'temp.min' ])  
cor(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.814 ,  'temp.max.osv' ]  ,  d.weather.subs.814[d.weather.subs.814$date %in% temp.max.cmn.dates.TAHMO.814  , 'temp.max' ])  

# Mean comparisons
mn.bias.ta.814.precip <<- mean(   d.eddy.real[ d.eddy.real$date %in% precip.cmn.dates.TAHMO.814  ,  'precip.osv' ] ) /   mean(  d.weather.subs.814[d.weather.subs.814$date %in% precip.cmn.dates.TAHMO.814  , 'precip' ]  ) 
mn.bias.ta.814.mean.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.avg.cmn.dates.TAHMO.814  ,  'temp.avg.osv' ] ) /  mean(  d.weather.subs.814[d.weather.subs.814$date %in% temp.avg.cmn.dates.TAHMO.814  , 'temp.mn' ])  
mn.bias.ta.814.min.temp <<- mean(    d.eddy.real[ d.eddy.real$date %in% temp.min.cmn.dates.TAHMO.814 ,  'temp.min.osv' ]  ) /  mean(  d.weather.subs.814[d.weather.subs.814$date %in% temp.min.cmn.dates.TAHMO.814 , 'temp.min' ])  
mn.bias.ta.814.max.temp <<- mean(   d.eddy.real[ d.eddy.real$date %in% temp.max.cmn.dates.TAHMO.814  ,  'temp.max.osv' ] ) /  mean(  d.weather.subs.814[d.weather.subs.814$date %in% temp.max.cmn.dates.TAHMO.814   , 'temp.max' ])  


 
