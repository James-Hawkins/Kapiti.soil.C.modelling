


generic.theme <- ggplot(d.eddy.real )+
  theme(
    legend.position = "bottom" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 290 , vjust = 0.5 ) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + xlab('') +
  scale_x_date(date_breaks = "3 month", date_labels =  "%y-%m-%d") 

  
y.lab.rg <- 'Radiation ()'
y.lab.temp.avg <- 'Temperature'

gg.rainf <- generic.theme %>% +
  geom_line( aes(x = date, y = precip.osv , color = variable.status.precip) ) 


gg.rg <- generic.theme %>% +
  geom_line( aes(x = date, y = rg.osv , color = variable.status.rg) ) 



# Temp
d.eddy.real$temp.avg.osv
d.eddy.real[ , 'temp.avg.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.filled , 'temp.avg.infd'] <- d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.filled , 'temp.avg.osv']
d.eddy.real[ d.eddy.real$variable.status.temp.avg == v.status.filled , 'plot.temp.avg.osv'] <- NA

gg.temp.avg <- generic.theme %>% +
  geom_line( aes(x = date, y = temp.avg.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = temp.avg.infd )  , color = 'pink') +
  ylab(y.lab.temp.avg)


# Precip
d.eddy.real[ , 'precip.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.precip == v.status.filled , 'precip.infd'] <- d.eddy.real[ d.eddy.real$variable.status.precip == v.status.filled , 'precip.osv']
d.eddy.real[ d.eddy.real$variable.status.precip== v.status.filled , 'plot.precip.osv'] <- NA

gg.precip <- generic.theme %>% +
  geom_line( aes(x = date, y = precip.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = precip.infd )  , color = 'pink') +
  ylab(y.lab.precip)


gg.rg

# Radiation
d.eddy.real[ , 'rg.infd']  <- NA
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.filled , 'rg.infd'] <- d.eddy.real[ d.eddy.real$variable.status.rg == v.status.filled , 'rg.osv']
d.eddy.real[ d.eddy.real$variable.status.rg == v.status.filled , 'plot.rg.osv'] <- NA

gg.rg <- generic.theme %>% +
  geom_line( aes(x = date, y = rg.osv ) , color = 'grey' ) +
  geom_line( aes(x = date, y = rg.infd )  , color = 'pink') +
  ylab(y.lab.rg)


gg.rg
