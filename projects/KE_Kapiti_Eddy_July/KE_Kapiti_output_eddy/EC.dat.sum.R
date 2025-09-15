



update.gen.fig <- function(){
  
  generic.theme <<- ggplot(d.eddy.real )+
    theme(
      legend.position = "bottom" ,
      # axis.title.x = element_blank() , 
      axis.text.x = element_blank(),
      #  legend.title = element_blank() ,
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.ticks.x = element_blank(), 
      panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    ) + xlab('') +
    scale_x_date(date_breaks = "3 month", date_labels =  "%y-%m-%d") 
  
  
  
}


y.lab.rg <- 'Radiation (W/m^2)   '
y.lab.temp.avg <- 'Temperature (Degrees C)'
y.lab.precip <- 'Precipitation (mm/day)  '
y.lab.rh <- 'Relative humidity (%)  '
y.lab.ws <- 'Wind speed (m/s)   '


color.mn.filled <- 'orange'

# Temp
d.eddy.real$temp.avg.osv
d.eddy.real[ , 'temp.avg.subs.infd']  <- NA
d.eddy.real[ , 'temp.avg.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.subs.filled , 'temp.avg.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.subs.filled , 'temp.avg.osv']
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled , 'temp.avg.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled , 'temp.avg.osv']

d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.mn.filled , 'plot.temp.avg.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.subs.filled , 'plot.temp.avg.osv'] <- NA

update.gen.fig()

gg.temp.avg <- generic.theme %>% +
  geom_line( aes(x = date, y = temp.avg.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = temp.avg.mn.infd )  , color = 'pink') +
  geom_line( aes(x = date, y = temp.avg.subs.infd )  , color = color.mn.filled) +
  ylab(y.lab.temp.avg)

gg.temp.avg


# Precip
d.eddy.real[ , 'precip.subs.infd']  <- NA
d.eddy.real[ , 'precip.mn.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.subs.filled , 'precip.subs.infd'] <- d.eddy.real[ d.eddy.real$variable.status.precip == v.status.subs.filled , 'precip.osv']
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled , 'precip.mn.infd'] <- d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled , 'precip.osv']

d.eddy.real[ d.eddy.real$variable.status.precip == v.status.mn.filled , 'plot.precip.osv'] <- NA
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.subs.filled , 'plot.precip.osv'] <- NA

update.gen.fig()

gg.precip <- generic.theme %>% +
  geom_line( aes(x = date, y = precip.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = precip.mn.infd )  , color = 'pink') +
  geom_line( aes(x = date, y = precip.subs.infd )  , color = color.mn.filled) +
  ylab(y.lab.precip)+
  theme(
    axis.text.x = element_text(angle = 290 , vjust = 0.5 ) 
  )

gg.precip

# Radiation
d.eddy.real[ , 'rg.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.mn.filled , 'rg.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rg == v.status.mn.filled , 'rg.osv']
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.mn.filled , 'plot.rg.osv'] <- NA

update.gen.fig()

gg.rg <- generic.theme %>% +
  geom_line( aes(x = date, y = rg.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = rg.infd )  , color = 'pink') +
  ylab(y.lab.rg)

gg.rg

# Relative humidity
d.eddy.real[ , 'rh.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.mn.filled , 'rh.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rh == v.status.mn.filled , 'rh.osv']
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.mn.filled , 'plot.rh.osv'] <- NA

update.gen.fig()

gg.rh <- generic.theme %>% +
  geom_line( aes(x = date, y = rh.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = rh.infd )  , color = 'pink') +
  ylab(y.lab.rh)


gg.rh

# Windspeed
d.eddy.real[ , 'ws.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.ws == v.status.mn.filled , 'ws.infd'] <- d.eddy.real[ d.eddy.real$variable.status.ws == v.status.mn.filled , 'ws.osv']
d.eddy.real[ d.eddy.real$variable.status.ws == v.status.mn.filled , 'plot.ws.osv'] <- NA

update.gen.fig()

gg.ws <- generic.theme %>% +
  geom_line( aes(x = date, y = ws.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = ws.infd )  , color = 'pink') +
  ylab(   y.lab.ws  ) +
  theme(
    axis.text.x = element_text(angle = 290 , vjust = 0.5 ) 
  )

gg.ws


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
1
,1.3
)

gg.ec.rel.non.clim.heights <- c(
  1
  , 1
  ,1.3
  
)

gg.ec.summary.clim <-  ggarrange(
  
  gg.temp.avg
  ,   gg.precip

  , ncol = 1
  , nrow = 2
  
 # , labels = gg.ec.sum.clim.labels 
  , heights = gg.ec.rel.clim.heights
)

gg.ec.summary.clim


gg.ec.summary.non.clim <-  ggarrange(
gg.rg 
, gg.rh
, gg.ws

, ncol = 1
, nrow = 3

  # , labels = gg.ec.sum.clim.labels 
  , heights = gg.ec.rel.non.clim.heights
)

gg.ec.summary.non.clim 


gg.ec.summary.clim
gg.ec.summary <-  ggarrange(
  
  gg.temp.avg
  ,   gg.precip
  ,   gg.rg 
  , gg.rh
  , gg.ws
  
  , ncol = 1
  , nrow = 5
  
  , labels = gg.ec.sum.labels 
  , heights = gg.ec.rel.heights
)

gg.ec.summary



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
