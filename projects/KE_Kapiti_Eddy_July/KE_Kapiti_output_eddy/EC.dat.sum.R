


generic.theme <- ggplot(d.eddy.real )+
  theme(
    legend.position = "bottom" ,
    # axis.title.x = element_blank() , 
   axis.text.x = element_blank(),
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + xlab('') +
  scale_x_date(date_breaks = "3 month", date_labels =  "%y-%m-%d") 

  
y.lab.rg <- 'Radiation (W/m^2)   '
y.lab.temp.avg <- 'Temperature (Degree C)'
y.lab.precip <- 'Precipitation (mm/day)  '
y.lab.rh <- 'Relative humidity (%)  '
y.lab.ws <- 'Wind speed (m/s)   '


# Temp
d.eddy.real$temp.avg.osv
d.eddy.real[ , 'temp.avg.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.filled , 'temp.avg.infd'] <- d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.filled , 'temp.avg.osv']
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.filled , 'plot.temp.avg.osv'] <- NA

gg.temp.avg <- generic.theme %>% +
  geom_line( aes(x = date, y = temp.avg.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = temp.avg.infd )  , color = 'pink') +
  ylab(y.lab.temp.avg)

gg.temp.avg


# Precip
d.eddy.real[ , 'precip.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.filled , 'precip.infd'] <- d.eddy.real[ d.eddy.real$variable.status.precip == v.status.filled , 'precip.osv']
d.eddy.real[ d.eddy.real$variable.status.precip== v.status.filled , 'plot.precip.osv'] <- NA

gg.precip <- generic.theme %>% +
  geom_line( aes(x = date, y = precip.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = precip.infd )  , color = 'pink') +
  ylab(y.lab.precip)


gg.precip

# Radiation
d.eddy.real[ , 'rg.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.filled , 'rg.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rg == v.status.filled , 'rg.osv']
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.filled , 'plot.rg.osv'] <- NA

gg.rg <- generic.theme %>% +
  geom_line( aes(x = date, y = rg.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = rg.infd )  , color = 'pink') +
  ylab(y.lab.rg)


gg.rg

# Relative humidity
d.eddy.real[ , 'rh.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.filled , 'rh.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rh == v.status.filled , 'rh.osv']
d.eddy.real[ d.eddy.real$variable.status.rh == v.status.filled , 'plot.rh.osv'] <- NA

gg.rh <- generic.theme %>% +
  geom_line( aes(x = date, y = rh.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = rh.infd )  , color = 'pink') +
  ylab(y.lab.rh)


gg.rh

# Windspeed
d.eddy.real[ , 'ws.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.ws == v.status.filled , 'ws.infd'] <- d.eddy.real[ d.eddy.real$variable.status.ws == v.status.filled , 'ws.osv']
d.eddy.real[ d.eddy.real$variable.status.ws == v.status.filled , 'plot.ws.osv'] <- NA

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


gg.ec.in.dpi  <-  2500

gg.ec.in.width <- 5.5
gg.ec.in.height  <- 11
filename.gg.ec.in = 'Figures.out/ec.in.jpg'

ggsave(filename = filename.gg.ec.in ,  gg.ec.summary , height = gg.ec.in.height , width = gg.ec.in.width , dpi = gg.valid.dpi  )




# 
