



update.gen.fig <- function(){
  
  generic.theme <<- ggplot( d.eddy.real ) +
    theme(
      plot.margin = margin( 0.001, 0.001, 0.00000001, 0.001, "pt") ,
      
      
       legend.position = "none" ,
       axis.title.x = element_blank() , 
       axis.text.x = element_blank(),
        legend.title = element_blank() ,
       panel.grid.major = element_blank(),
        panel.background = element_blank(),
       axis.ticks.x = element_blank(), 
       axis.title.y = element_text( size = 11),
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
  gg.clim.line.width <<- 0.025
  
  
}



# Temp
d.eddy.real$temp.avg.osv
d.eddy.real[ , 'temp.avg.subs.infd']  <- NA
d.eddy.real[ , 'temp.avg.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.subs.filled & !is.na(d.eddy.real$variable.status.temp.avg ) , 'temp.avg.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.subs.filled & !is.na(d.eddy.real$variable.status.temp.avg ), 'temp.avg.osv']
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.temp.avg ), 'temp.avg.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.temp.avg ), 'temp.avg.osv']

d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.temp.avg ), 'plot.temp.avg.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.subs.filled & !is.na(d.eddy.real$variable.status.temp.avg ), 'plot.temp.avg.osv'] <- NA

update.gen.fig()

gg.temp.avg <- generic.theme %>% +
  theme(
  #  axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 290 , vjust = 0.5 ) 
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
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.subs.filled & !is.na(d.eddy.real$variable.status.precip )  , 'precip.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.precip == v.status.subs.filled & !is.na(d.eddy.real$variable.status.precip ) , 'precip.osv']
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled & !is.na(d.eddy.real$variable.status.precip ) , 'precip.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled & !is.na(d.eddy.real$variable.status.precip ) , 'precip.osv']

d.eddy.real[ d.eddy.real$variable.status.precip != v.status.mn.filled & d.eddy.real$variable.status.precip != v.status.subs.filled & !is.na(d.eddy.real$variable.status.precip ) , 'plot.precip.osv'] <- d.eddy.real[ d.eddy.real$variable.status.precip != v.status.mn.filled & d.eddy.real$variable.status.precip != v.status.subs.filled & !is.na(d.eddy.real$variable.status.precip ) , 'precip.osv']
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled & !is.na(d.eddy.real$variable.status.precip ) , 'plot.precip.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.subs.filled & !is.na(d.eddy.real$variable.status.precip ) , 'plot.precip.osv'] <- NA

update.gen.fig()

gg.precip <- generic.theme %>% +
  theme(
    axis.text.x = element_text(angle = 290 , vjust = 0.5 ) 
    , axis.ticks.x = element_line()
  ) +
 geom_line( aes(x = date, y = precip.osv ) , color = 'grey' , linewidth =  gg.clim.line.width) +
 # geom_line( aes(x = date, y = plot.precip.osv ) , color = 'grey' ) +
#  geom_line( aes(x = date, y = precip.mn.infd )  , color = 'pink') +
  geom_line( aes(x = date, y = precip.subs.infd )  , color = color.mn.filled , linewidth =  gg.clim.line.width) +
  ylab(y.lab.precip)
  

gg.precip

# Radiation
d.eddy.real[ , 'rg.subs.infd']  <- NA
d.eddy.real[ , 'rg.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.eo.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rg.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rg == v.status.eo.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rg.osv']
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rg.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rg== v.status.mn.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rg.osv']

d.eddy.real[ d.eddy.real$variable.status.rg != v.status.mn.filled & d.eddy.real$variable.status.rg != v.status.subs.filled & !is.na(d.eddy.real$variable.status.rg ) , 'plot.rg.osv'] <- d.eddy.real[ d.eddy.real$variable.status.rg != v.status.mn.filled & d.eddy.real$variable.status.rg != v.status.subs.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rg.osv']
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.mn.filled & !is.na(d.eddy.real$variable.status.rg ) , 'plot.rg.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.eo.filled & !is.na(d.eddy.real$variable.status.rg ) , 'plot.rg.osv'] <- NA


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
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.eo.filled & !is.na(d.eddy.real$variable.status.rh ) , 'rh.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rh == v.status.eo.filled & !is.na(d.eddy.real$variable.status.rh ), 'rh.osv']
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.mn.filled & !is.na(d.eddy.real$variable.status.rg ) , 'rh.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rh== v.status.mn.filled & !is.na(d.eddy.real$variable.status.rh ), 'rh.osv']

d.eddy.real[ d.eddy.real$variable.status.rh != v.status.mn.filled & d.eddy.real$variable.status.rh != v.status.subs.filled, 'plot.rh.osv'] <- d.eddy.real[ d.eddy.real$variable.status.rh != v.status.mn.filled & d.eddy.real$variable.status.rh != v.status.subs.filled & !is.na(d.eddy.real$variable.status.rh ), 'rh.osv']
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.mn.filled & !is.na(d.eddy.real$variable.status.rh ), 'plot.rh.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.eo.filled & !is.na(d.eddy.real$variable.status.rh ), 'plot.rh.osv'] <- NA



update.gen.fig()

gg.rh <- generic.theme %>% +
  geom_line( aes(x = date, y = plot.rh.osv ) , color = 'grey' , linewidth =  gg.clim.line.width) +
 # geom_line( aes(x = date, y = rh.mn.infd )  , color = 'pink') +
  geom_line( aes(x = date, y = rh.subs.infd )  , color = color.mn.filled , linewidth =  gg.clim.line.width) +
  ylab(y.lab.rh) + theme(
  #  axis.text.x = element_text(angle = 290 , vjust = 0.5 ) 
  )


gg.rh

# Windspeed
d.eddy.real[ , 'ws.subs.infd']  <- NA
d.eddy.real[ , 'ws.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.ws == v.status.eo.filled & !is.na(d.eddy.real$variable.status.ws ) , 'ws.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.ws == v.status.eo.filled & !is.na(d.eddy.real$variable.status.ws ), 'ws.osv']
d.eddy.real[ d.eddy.real$variable.status.ws == v.status.mn.filled & !is.na(d.eddy.real$variable.status.ws ), 'ws.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.ws == v.status.mn.filled & !is.na(d.eddy.real$variable.status.ws ), 'ws.osv']

d.eddy.real[ d.eddy.real$variable.status.ws != v.status.mn.filled & d.eddy.real$variable.status.ws != v.status.subs.filled & !is.na(d.eddy.real$variable.status.ws ), 'plot.ws.osv'] <- d.eddy.real[ d.eddy.real$variable.status.ws != v.status.mn.filled & d.eddy.real$variable.status.ws != v.status.subs.filled & !is.na(d.eddy.real$variable.status.ws ), 'ws.osv']
d.eddy.real[ d.eddy.real$variable.status.ws == v.status.mn.filled & !is.na(d.eddy.real$variable.status.ws ), 'plot.ws.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.ws == v.status.eo.filled & !is.na(d.eddy.real$variable.status.ws ), 'plot.ws.osv'] <- NA



update.gen.fig()

gg.ws <- generic.theme %>% +
  geom_line( aes(x = date, y = plot.ws.osv ) , color = 'grey', linewidth =  gg.clim.line.width ) +
#  geom_line( aes(x = date, y = ws.mn.infd )  , color = 'pink') +
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



gg.ec.sum.labels <- c(
    'a' 
  ,  'b'  
  , 'c'
  , 'd'
  , 'e'
  )

gg.ec.rel.heights <- c(
  1
  ,1
  ,1
  ,1
  ,1.3
  
)


gg.ec.rel.clim.heights <- c(
1.0
, 1
,1
,1
, 1.2
)

gg.ec.rel.non.clim.heights <- c(
  1
  , 1
  ,1.3
  
)

gg.ec.summary.clim <-  ggarrange(
  
  
    gg.ws.merge
  , gg.rh.merge
  , gg.rg.merge
  , gg.temp.avg.merge
  
  ,   gg.precip
  
  

  , ncol = 1
  , nrow = 5
  
 # , labels = gg.ec.sum.clim.labels 
  , heights = gg.ec.rel.clim.heights
)

gg.ec.summary.clim


ggsave( gg.ec.summary.clim , file())


ggsave(
  filename = "clim.sum.png",
  gg.ec.summary.clim,
  width = 8,
  height = 12,
  dpi = 300
)

gg.ec.summary.clim





# Climate out
gg.ec.in.dpi  <-  2500

gg.ec.clim.in.width <- 5.5
gg.ec.clim.in.height  <- 7
filename.gg.ec.clim = 'Figures.out/gg.ec.summary.clim.jpg'

ggsave(filename = filename.gg.ec.clim,  gg.ec.summary.clim  , height = gg.ec.clim.in.height , width = gg.ec.clim.in.width , dpi = gg.valid.dpi  )

# Non - climate out
gg.ec.in.dpi  <-  2500

gg.ec.non.clim.in.width <- 5.5
gg.ec.non.clim.in.height  <- 7
filename.gg.ec.non.clim = 'Figures.out/gg.ec.summary.non.clim.jpg'

ggsave(filename = filename.gg.ec.non.clim,  gg.ec.summary.non.clim  , height = gg.ec.non.clim.in.height , width = gg.ec.non.clim.in.width , dpi = gg.valid.dpi  )



gg.ec.in.dpi  <-  2500

gg.ec.in.width <- 5.5
gg.ec.in.height  <- 11
filename.gg.ec.in = 'Figures.out/ec.in.jpg'

ggsave(filename = filename.gg.ec.in ,  gg.ec.summary , height = gg.ec.in.height , width = gg.ec.in.width , dpi = gg.valid.dpi  )




# 
