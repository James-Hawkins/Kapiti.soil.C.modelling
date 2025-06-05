setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


cv.sq.m.2.ha <- 10000


require(ggplot2)


kapiti.dry.months <<- c('01','02','06','07','08','09')
kapiti.wet.months <<- c('03','04','05', '10','11','12')


{

  d.b.sl.chem <<- read.csv('base_soilchemistry-daily.csv'
                             #, sheet = 'base_soilchemistry-daily' 
                            # , col_types = "text"
                             )
  
  d.gz.sl.chem <<- read.csv('grazed_soilchemistry-daily.csv' 
                          #    , col_types = "text"
                              )
  
  d.b.phys <<- read.csv('base_physiology-daily.csv'
                           #, sheet = 'base_soilchemistry-daily' 
                           # , col_types = "text"
  )
  
  d.gz.phys <<- read.csv('grazed_physiology-daily.csv' 
                            #    , col_types = "text"
  )
  
  

  
  names(d.gz.sl.chem)[3] <- 'date.time'
  names(  d.gz.sl.chem )[6] <- 'emis.hetero'
  
  names(d.b.sl.chem )[3] <- 'date.time'
  names(    d.b.sl.chem  )[6] <- 'emis.hetero'
  
  
  
  
  names(  d.gz.phys )[3] <- 'date.time'
  names(  d.gz.phys )[27] <- 'co2.upt'
  names(  d.gz.phys )[24] <- 'maint.resp'
  names(  d.gz.phys )[25] <- 'transp.resp'
  names(  d.gz.phys )[26] <- 'growth.resp'
  
  names(   d.b.phys )[3] <- 'date.time'
  names(   d.b.phys )[27] <- 'co2.upt'
  names(   d.b.phys )[24] <- 'maint.resp'
  names(   d.b.phys )[25] <- 'transp.resp'
  names(   d.b.phys )[26] <- 'growth.resp'
  
  
  d.b.sl.chem$date.time <-  as.POSIXct(     d.b.sl.chem$date.time , format="%Y-%m-%d %H:%M:%S")
  d.gz.sl.chem$date.time <-  as.POSIXct(    d.gz.sl.chem$date.time , format="%Y-%m-%d %H:%M:%S")
  
  
  
  d.b.sl.chem$sim <- 'base'
  d.gz.sl.chem$sim <- 'grazed'
  
  
  d.b.all <- as.data.frame( (cbind( d.b.sl.chem ,   d.b.phys ))     )
  d.gz.all <- as.data.frame( (cbind( d.gz.sl.chem ,   d.gz.phys ))     )
  

  d.all <-   as.data.frame( rbind(   d.b.all , d.gz.all))
    
View(d.all)

d.all$growth.resp

d.all$date.time <- as.Date(d.all$date.time)



d.all$GPP <- cv.sq.m.2.ha * (-1) * d.all$co2.upt
d.all$TER <- cv.sq.m.2.ha *  (d.all$maint.resp + d.all$transp.resp + d.all$growth.resp) + d.all$emis.hetero


unique(d.all$GPP)
summary(d.all$GPP)
mean(d.all$GPP)
mean(d.all$TER)

d.all$NEE <- d.all$GPP + d.all$TER

d.all$NEE.sum <- sum(d.all$NEE)



# plot params
{

p.y.ax.lab <- 'Net ecosystem exchange (kg C/ha/yr)'  
p.x.ax.lab <- 'Time period (year)' 

p.nee.compare.tot.y.ax.lab <- 'Mean daily net ecosystem exchange (kg C/ha/d)'  
p.nee.compare.tot.x.ax.lab <- 'Time period (year)' 

}





d.all <- d.all[!duplicated(as.list(d.all))]

d.all$month.only <- format(d.all$date.time , "%m")

unique(d.all$month.only)

rows <- rep(  c(unique(d.all$sim )), 3 ) 
cols <- c( 'sim' , 'mean' , 'sd' , 'period')



d.sum <- data.frame(matrix( 
  ncol = length( cols )
  , nrow = 6
  , dimnames = list( rows , cols))
)

d.sum[1:3, 'sim'] <- 'base'
d.sum[4:6, 'sim'] <- 'grazed'

d.sum[ c(1,4), 'period'] <- 'Annual'
d.sum[ c(2,5), 'period'] <- 'Dry'
d.sum[ c(3,6), 'period'] <- 'Rainy'


for ( s in rows){
  
d.sum[ d.sum$sim == s & d.sum$period == 'Annual' , 'mean'] <- mean(d.all[d.all$sim == s  , 'NEE'])
d.sum[ d.sum$sim == s & d.sum$period == 'Dry' , 'mean'] <- mean(d.all[d.all$sim == s & d.all$month.only %in% kapiti.dry.months , 'NEE'])
d.sum[ d.sum$sim == s & d.sum$period == 'Rainy' , 'mean'] <- mean(d.all[d.all$sim == s & d.all$month.only %in% kapiti.wet.months , 'NEE'])

d.sum[ d.sum$sim == s & d.sum$period == 'Annual' , 'sd'] <- sd(d.all[d.all$sim == s  , 'NEE'])
d.sum[ d.sum$sim == s & d.sum$period == 'Dry' , 'sd'] <- sd(   d.all[d.all$sim == s & d.all$month.only %in% kapiti.dry.months , 'NEE'])
d.sum[ d.sum$sim == s & d.sum$period == 'Rainy' , 'sd'] <- sd(d.all[d.all$sim == s & d.all$month.only %in% kapiti.wet.months , 'NEE'])


}




#View(d.sum)


p.dodge.wdth <- 0.6

p.nee.compare.tot <- ggplot( d.sum ,   aes(   y = mean , x = sim , group = period , fill = period )   ) +
geom_bar(   stat = "identity"  
                           , width = p.dodge.wdth
                           , position = position_dodge()
  ) + 
  geom_errorbar( aes( ymin = mean - sd
                      , ymax = mean  + sd )
                 , width = p.dodge.wdth
                 ,position=position_dodge()
  ) + 
  theme(
    legend.position = "right" , 
    axis.text.x = element_text(angle = 270) ,
    legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.nee.compare.tot.x.ax.lab) +
  ylab(p.nee.compare.tot.y.ax.lab)

  
  

p.nee.compare.dl <- ggplot( d.all,   aes(  x = date.time , y = NEE , group = sim , color = sim )   ) +
geom_line() +
scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
scale_color_manual( name = 'NEE' ,values = c("base" = "darkblue", "grazed" = "red")) +
theme(
legend.position = "right" ,
# axis.title.x = element_blank() , 
axis.text.x = element_text(angle = 270) ,
legend.title = element_blank() ,
panel.grid.major = element_blank(),
panel.background = element_blank(),
panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
) + 
xlab(p.x.ax.lab) +
ylab(p.y.ax.lab)

}



p.nee.compare.dl



c.plot <- ggarrange(
  p.nee.compare.dl
  , p.nee.compare.tot 
  , ncol = 2
  , nrow = 1
  #,  common.legend = c(2)
 # , legend.grob = legend.bv.bst
  , legend = "bottom"
 # , widths =  widths.bv.best
  , labels = c("a", "b")
  , hjust = c(0,0.25)
)

c.plot


