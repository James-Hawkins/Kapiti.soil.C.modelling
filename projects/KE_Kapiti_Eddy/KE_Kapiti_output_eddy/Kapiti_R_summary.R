

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

# Conversion factors
cv.sq.m.2.ha <- 10000
cv.microml.2.kg <- 0.000000001 
cv.sec.2.yr <- 60*60*24*365
cv.sec.2.d <- 60*60*24
cv.mml.c.2.co2 <<- 12


library.in <- function(){
  
  
  
  library("languageR")
  library('readxl')
  library('ggplot2')
  library(stringr)
  library(stringi)
  
  library('chron')
  library('lubridate')
  library('ggpubr')
  
  
  
}
library.in()

"""
Main issues needing worked out

1. How to handle missing raw data
  average over day
  focus only on continuous segments
  

"""

# Global model settings
{
}

source('Eddy_transform.R')

{

# L-DNDC modelled outputs
{

d.sl.chem <<- read.csv('KE_Kapiti_soilchemistry-daily.csv')
  
d.physio  <<- read.csv('KE_Kapiti_physiology-daily.csv')
  
d.watr  <<- read.csv('KE_Kapiti_watercycle-daily.csv')

# Rename columns
names(d.sl.chem )[6] <- 'emis.hetero'

names(d.physio)[3] <- 'date.time'
names(d.physio)[25] <- 'maint.resp'
names(d.physio)[26] <- 'transp.resp'
names(d.physio)[27] <- 'growth.resp'
names(d.physio)[28] <- 'co2.upt'
names(d.physio)[39] <- 'lai.sim'

names(d.watr)[3] <- 'date.time'
names(d.watr)[5] <- 'precip.sim'
names(d.watr)[7] <- 'et.sim.mm'
names(d.watr)[26] <- 'sw.5'
names(d.watr)[27] <- 'sw.10'
names(d.watr)[28] <- 'sw.15'
names(d.watr)[29] <- 'sw.20'
names(d.watr)[30] <- 'sw.30'
names(d.watr)[31] <- 'sw.40'
names(d.watr)[32] <- 'sw.50'
names(d.watr)[33] <- 'sw.60'



# merged model data
{
  
  d.all <- cbind( d.sl.chem$emis.hetero , d.physio)
  
  d.all <- cbind( d.all , d.watr)
  
  names(d.all)[1] <- 'emis.hetero'
  d.all$date.time <- as.Date(d.all$date.time ,  format="%Y-%m-%d")
  #d.all$date.time <- as.Date(d.all$date.time ,  format="%m/%d/%Y")
  
  d.all$day.cnt <- NA
  
  
  for (r in 1:nrow(d.all)  ){
    
    d.all[ r , 'day.cnt'] <- r 
    
  }
  
  tail(d.all$date.time )
  
  frst.date <- which( d.all$date.time  == first.date.cald )
  end.date <- which( d.all$date.time == secd.date.cald  )
  
  d.all <- d.all[d.all$day.cnt >= frst.date 
                 & d.all$day.cnt <= end.date
                 ,  ]
  
  nrow(d.all)
  
  

  
}


}


# L-DNDC raw data
d.eddy.clim  <<- read.csv('KE_Kapiti_climate_eddy.csv')

names(d.eddy.clim)[1] <- 'yr'
names(d.eddy.clim)[2] <- 'day.cnt'
names(d.eddy.clim)[7] <- 'precip'
d.eddy.clim <- d.eddy.clim[ 23:nrow(d.eddy.clim) ,  ]



# Climate data
{
  # Insert calendar date into climate data
  for (r in 1:nrow(d.eddy.clim)){
    
    day.cnt <- d.eddy.clim[ r , 'day.cnt']
    year <- d.eddy.clim[ r , 'yr']
    origin <- str_c(d.eddy.clim[d.eddy.clim$day.cnt ==  day.cnt & d.eddy.clim$yr == year, 'yr'],'-01-01')
    
    day.cnt <- as.numeric(day.cnt)
    
    d.eddy.clim[r,'date'] <-  as.Date( day.cnt ,  origin = origin)
    
    
  }
  
  
  
  d.eddy.clim <- d.eddy.clim[
    d.eddy.clim$date >= first.date.cald
    & d.eddy.clim$date <= secd.date.cald
    ,  ]
  
  nrow(d.eddy.clim)
  
  d.eddy.clim$precip <- as.numeric(d.eddy.clim$precip)
  
}



d.eddy.real <- d.eddy.real[
  d.eddy.real$date >= first.date.cald
  & d.eddy.real$date <= secd.date.cald
  ,  ]




nrow(d.eddy.clim)
nrow(d.watr)
nrow(d.physio)
nrow(d.sl.chem)
nrow(d.all)

# View(d.eddy.clim)
#  View(d.all)





# Convert main variables to numeric
convert.numeric.list <- c(
  'transp.resp'
  , 'growth.resp'
  , 'maint.resp'
  , 'emis.hetero'
  ,'co2.upt'
  )

for (l in convert.numeric.list){
  print(paste(l))
  
  d.all[,l] <- as.numeric( d.all[,l])
  
}







nrow(d.eddy.clim)

nrow(d.all)

d.all$date.time
d.eddy.real$date
d.eddy.clim$date



d.all <- cbind(d.all, d.eddy.real)

#d.all <- cbind(d.all, d.eddy.clim)

# Variable transformations
{
  
# Observed

d.all$gpp.osv.kg.ha <- (-1) * d.all$gpp.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
d.all$reco.osv.kg.ha <-  d.all$reco.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 
  
  
# convert observed eddy in mm per sq m per s to kg per ha
d.all$NEE.obs.kg.ha <- d.all$nee.osv * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 

d.all[   is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA
d.all[d.all$NEE.obs.kg.ha < -90 & !is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA



summary(d.all$co2.upt)
summary(d.all$maint.resp)
summary(d.all$transp.resp)
summary(d.all$growth.resp)
summary(d.all$emis.hetero)

hist(d.all$co2.upt)
hist(d.all$maint.resp)
hist(d.all$transp.resp)
hist(d.all$growth.resp)
hist(d.all$emis.hetero)

# MODELLED
d.all$GPP.sim <- cv.sq.m.2.ha * (-1) * d.all$co2.upt
d.all$TER.sim <- cv.sq.m.2.ha *  (d.all$maint.resp + d.all$transp.resp + d.all$growth.resp) + d.all$emis.hetero

d.all$NEE.mod <-   d.all$TER + d.all$GPP 

hist(d.all$GPP.sim)
hist(d.all$TER.sim)
hist(d.all$NEE.mod)

}


# -- Model validation
{
  
# NEE
#R2
sim.var <- 'NEE.mod'
observed.var <- 'NEE.obs.kg.ha'
mean <- mean(na.omit(d.all[d.all$variable.status == v.status.actual , observed.var] ))
NEE.tss <- sum( ( d.all[d.all$variable.status == v.status.actual , observed.var ]  -   mean )^2)
NEE.rss <-  sum( ( d.all[d.all$variable.status == v.status.actual , sim.var ]  -  d.all[d.all$variable.status == v.status.actual , observed.var ]  )^2)
NEE.R2 <- 1 - NEE.rss / NEE.tss

  
# Actual  data
sim.var <- 'NEE.mod'
observed.var <- 'NEE.obs.kg.ha'
NEE.RMSE.actual <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual , observed.var ] - d.all[d.all$variable.status == v.status.actual , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual , observed.var]))
NEE.NRMSE.actual <- 100 * NEE.RMSE.actual/ sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual , observed.var ]^2))))

NEE.NRMSE.actual <- round( NEE.NRMSE.actual , 1)

print(paste('NRMSE for NEE ' , NEE.NRMSE.actual))


#  TER
# R2

sim.var <- 'TER.sim'
observed.var <- 'reco.osv.kg.ha' 
mean <- mean(na.omit(d.all[d.all$variable.status == v.status.actual , observed.var] ))
TER.tss <- sum( ( d.all[d.all$variable.status == v.status.actual , observed.var ]  -   mean )^2)
TER.rss <-  sum( ( d.all[d.all$variable.status == v.status.actual , sim.var ]  -  d.all[d.all$variable.status == v.status.actual , observed.var ]  )^2)
TER.R2 <- 1 - (NEE.rss / NEE.tss)


TER.RMSE.actual <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual , observed.var ] - d.all[d.all$variable.status == v.status.actual , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual , observed.var]))
TER.NRMSE.actual <- 100 * TER.RMSE.actual / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual , observed.var ]^2))))

TER.NRMSE.actual <- round( TER.NRMSE.actual , 1)

print(paste('NRMSE for TER ' , TER.NRMSE.actual))

#  GPP
sim.var <- 'GPP.sim'
observed.var <- 'gpp.osv.kg.ha'

GPP.RMSE.actual <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual , observed.var ] - d.all[d.all$variable.status == v.status.actual , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual , observed.var]))
GPP.NRMSE.actual <- 100 * GPP.RMSE.actual / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual , observed.var ]^2))))

GPP.NRMSE.actual <- round( GPP.NRMSE.actual , 1)

print(paste('NRMSE for TER ' , GPP.NRMSE.actual))




# SWC - 5 cm layer
sim.var <- 'sw.5'
observed.var <- 'swc.3.pc.osv'

SWC.RMSE.actual <-  sqrt(   sum( na.omit(d.all[d.all$variable.status == v.status.actual , observed.var ] - d.all[d.all$variable.status == v.status.actual , sim.var ])^2  ) )  / length(na.omit(d.all[d.all$variable.status == v.status.actual , observed.var]))
SWC.NRMSE.actual <- 100 * SWC.RMSE.actual / sqrt(mean(na.omit(sqrt(d.all[d.all$variable.status == v.status.actual , observed.var ]^2))))

SWC.NRMSE.actual <- round( SWC.NRMSE.actual , 1)

print(paste('NRMSE for SWC ' , SWC.NRMSE.actual))



precip.compare <- data.frame(d.all$precip.sim , d.all$precip.osv)
precip.compare <- na.omit(precip.compare)
cor(precip.compare)

}

d.all$swc.3.pc.osv


# plot params
{
  
gg.valid.labels <- c(
    'Simulated'
    ,     'Observed'    
  )
  
gg.valid.nee.y.ax.lab <<- 'Net ecosystem exchange (kg C/ha/day)'  
gg.valid.gpp.y.ax.lab <<- 'Gross primary productivity (kg C/ha/day)'  
gg.valid.ter.y.ax.lab <<- 'Total ecosystem respiration (kg C/ha/day)'
  
gg.valid.leg.y.crd <- 0.78
gg.valid.leg.x.crd <- 0.15
gg.valid.scale.precip.axis <- 1
  
p.x.ax.lab <<- 'Date (YY-MM-DD)'  

p.swc.y.ax.lab <- 'Soil water content (%)'
p.et.y.ax.lab  <- 'Evapotranspiration (mm/d)'
p.lai.y.ax.lab  <- 'Leaf area index'

p.precip.sec.ax.tit <- 'Precipitation (mm/day)'
  
p.br.wdth <<- .15

p.br.alpha <<- 0.6

p.ln.width <- 0.8

p.date.interval.x.axis <- "3 month"

gg.valid.date.r2.x.crd <<- 0.5
gg.valid.date.r2.y.crd  <<- 82

p.lab.nee.tx.fs <- 3.75

# NRMSE labels
gg.valid.lab.swc.rmse <-  paste0("NRMSE:~",SWC.NRMSE.actual )
gg.valid.lab.nee.rmse <-  paste0("NRMSE:~",NEE.NRMSE.actual )
gg.valid.lab.ter.rmse <-  paste0("NRMSE:~",TER.NRMSE.actual )
gg.valid.lab.gpp.rmse <-  paste0("NRMSE:~",GPP.NRMSE.actual )



p.br.clr <<- 'lightblue'
p.ln.colr.mod <- 'lightgreen'
p.ln.clr.obsv  <- 'darkgrey'

p.colors <- c(p.ln.clr.obsv , p.ln.colr.mod  , p.br.clr)


p.nee.label.1 <- "NEE, obsd"
p.nee.label.2 <- "NEE, simd"
p.nee.label.3 <- "GPP"
p.nee.label.4 <- "TER"

p.nee.color.1 <- p.ln.clr.obsv
p.nee.color.2 <- p.ln.colr.mod
p.nee.color.3 <- 'lightblue'
p.nee.color.4 <- 'pink'

gg.valid.date.x.ax.lab <- as.Date("2019-03-01")


p.swc.osv.label  <- 'Observed'
p.swc.sim.label <- 'Simulated'


p.ssn.x.ranges.2019.rn.2.min <- first.date.cald
p.ssn.x.ranges.2019.rn.2.max <- "2019-12-31"


p.ssn.x.ranges.2020.dr.1.min <- "2020-01-01" 
p.ssn.x.ranges.2020.dr.1.max <- "2020-02-29" 

p.ssn.x.ranges.2020.rn.1.min <- "2020-03-01" 
p.ssn.x.ranges.2020.rn.1.max <- "2020-05-31" 

p.ssn.x.ranges.2020.dr.2.min <- "2020-06-01" 
p.ssn.x.ranges.2020.dr.2.max <- secd.date.cald 


gg.valid.scale.precip.axis.swc <- 1
gg.valid.scale.precip.axis.ter <- 1
gg.valid.scale.precip.axis.gpp <- 1
gg.valid.scale.precip.axis.nee <- 1 


p.rn.ssn.clr <- '#eaffdf'
p.dr.ssn.clr <- '#fef2c6'

p.ssn.bg.alpha <- 0.1

}

d.all <- as.data.frame(d.all)
d.all <- d.all[,!duplicated(colnames(d.all))]


# All plots

{
  

gg.valid.swc <- ggplot( d.all[ d.all$sw.5 > 0 ,  ] ,   aes(x = date.time)  
) + 
  geom_line( aes(x = date.time, y = swc.3.pc.osv  , color= p.swc.osv.label 
                                                                 ) 
             , linewidth = p.ln.width 
             
  ) +  
  geom_line( aes(x = date.time, y = sw.5  , color= p.swc.sim.label ) 
             , linewidth = p.ln.width 
             
  ) +  
  geom_bar(  data = d.all,
             aes( x = date.time
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
            , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  scale_y_continuous(
    p.swc.y.ax.lab, 
    sec.axis = sec_axis(~ . * 1 / gg.valid.scale.precip.axis.swc , name = p.precip.sec.ax.tit )
  ) +
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Observed" = p.nee.color.1
      , "Simulated"  = p.nee.color.2
    ) 
    , breaks = c(
      p.swc.osv.label
      ,  p.swc.sim.label
    )) +
  theme(
    legend.position = c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ),
    axis.title.x = element_blank() ,  
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() , 
    axis.text.x = element_blank() , 
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.swc.y.ax.lab)+
    annotate("text"
             , x =   gg.valid.date.x.ax.lab   , 
             , y =  gg.valid.date.r2.y.crd
             , parse = TRUE 
             , label = gg.valid.lab.swc.rmse
             , size = p.lab.nee.tx.fs
             , hjust = 0
    )

gg.valid.swc



gg.valid.ter <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
) +  
  geom_line( aes(x = date, y = reco.osv.kg.ha , color= gg.valid.labels[2]   ) 
             , linewidth = p.ln.width 
             
  ) +   
  geom_line( aes(x = date, y = TER.sim  , color= gg.valid.labels[1]  ) 
             , linewidth = p.ln.width 
             
  ) +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Simulated" = p.nee.color.2
      ,"Observed"  = p.nee.color.1
    ) 
    , breaks = c(
      gg.valid.labels[1]
      , gg.valid.labels[2]
    ) 
  )  + 
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  scale_y_continuous(
    gg.valid.ter.y.ax.lab , 
    sec.axis = sec_axis(~   . / gg.valid.scale.precip.axis.ter , name = p.precip.sec.ax.tit )
  ) +
  theme(
    legend.position = "none" ,
   # legend.title = element_blank(),
  #  axis.title.x = element_blank() , 
  axis.text.x = element_blank() , 
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(gg.valid.ter.y.ax.lab) + 
  geom_bar(  data = d.all[,  ] ,
             aes( x =date.time 
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
 annotate("text"
  , x =   gg.valid.date.x.ax.lab   , 
 , y =  gg.valid.date.r2.y.crd
 , parse = TRUE 
, label = gg.valid.lab.ter.rmse
  , size =p.lab.nee.tx.fs
  , hjust = 0
 )

gg.valid.ter



gg.valid.gpp <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
) +  
  geom_line( aes(x = date, y = GPP.sim  , color= gg.valid.labels[1]  ) 
             , linewidth = p.ln.width 
             
  ) +   
  geom_line( aes(x = date, y = gpp.osv.kg.ha , color=  gg.valid.labels[2]   ) 
             , linewidth = p.ln.width 
             
  ) +   
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Simulated" = p.nee.color.2
      ,"Observed"  = p.nee.color.1
    ) 
    , breaks = c(
      gg.valid.labels[1]
      , gg.valid.labels[2]
    ) 
  )  + 
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  scale_y_continuous(
    gg.valid.gpp.y.ax.lab , 
    sec.axis = sec_axis(~   . / gg.valid.scale.precip.axis.gpp  , name = p.precip.sec.ax.tit )
  ) +
  theme(
    legend.position = "none" ,
    legend.title = element_blank(),   
    axis.title.x = element_blank() , 
    axis.title.y.right = element_blank() , 
    axis.text.y.right = element_blank() ,
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  ylab(gg.valid.gpp.y.ax.lab) + 
  geom_bar(  data = d.all[,  ] ,
             aes( x =date.time 
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  annotate("text"
           , x =   gg.valid.date.x.ax.lab   , 
           , y =  gg.valid.date.r2.y.crd
           , parse = TRUE 
           , label = gg.valid.lab.gpp.rmse
           , size = p.lab.nee.tx.fs
           , hjust = 0
  )

gg.valid.gpp

gg.valid.nee <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
) +  
  # - Observed
  geom_line(  aes(x = date.time 
                  , y = NEE.obs.kg.ha 
                  , colour= gg.valid.labels[2]
  )  
  ,linewidth = p.ln.width
  ) +  
  # - Modelled
  geom_line( aes(x = date.time
                 , y = NEE.mod 
                 , colour=  gg.valid.labels[1]
  ) 
  , linewidth = p.ln.width 
  
  ) + 
  geom_bar(  data = d.all,
             aes( x = date.time
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Simulated" = p.nee.color.2  
      ,"Observed"  = p.nee.color.1
    ) 
    , breaks = c(
      gg.valid.labels[1]
      , gg.valid.labels[2]
    ) 
  )  + 
  scale_y_continuous(
    gg.valid.nee.y.ax.lab, 
    sec.axis = sec_axis(~   . / gg.valid.scale.precip.axis.nee , name = p.precip.sec.ax.tit )
  ) +
  theme(
    legend.position = "none" ,
    axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  ylab(gg.valid.nee.y.ax.lab) + 
  geom_bar(  data = d.all[,  ] ,
             aes( x =date.time 
                  , y = precip.osv 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  annotate("text"
           , x =   gg.valid.date.x.ax.lab   
           , y =  gg.valid.date.r2.y.crd
           , parse = TRUE 
           , label = gg.valid.lab.nee.rmse 
           , size =p.lab.nee.tx.fs
           , hjust = 0
  )

gg.valid.nee

gg.validate.labels <- c('a' ,'b' ,'c' , 'd')

gg.validate <- ggarrange(
  
  gg.valid.swc 
  ,   gg.valid.ter 
  ,   gg.valid.gpp 
  ,  gg.valid.nee 
  
  , labels = gg.validate.labels 
)

gg.validate





gg.valid.dpi  <-  2500

gg.valid.width <- 10.5
gg.valid.height  <- 8
filename.gg.validate = 'Figures.out/gg.validate.jpg'

ggsave(filename =    filename.gg.validate ,  gg.validate , width = gg.valid.width, height = gg.valid.height , dpi = gg.valid.dpi  )

}

p.et <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
) + 
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2019.rn.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2019.rn.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill =  p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.rn.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.rn.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_bar(  data = d.all[,  ] ,
             aes( x = date.time
                  , y = precip 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  # - Observed
  geom_line(  aes(x = date.time
                  , y = et.real.mm
                  , colour= 'Observed'
  )  
  ,linewidth = p.ln.width
  ) +  
  # - Modelled
  geom_line( aes(x = date.time
                 , y = et.sim.mm
                 , colour= 'Modelled'
  ) 
  , linewidth = p.ln.width 
  ) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  scale_y_continuous(
    p.et.y.ax.lab , 
    sec.axis = sec_axis(~   . , name = p.precip.sec.ax.tit )
  ) +
  scale_colour_manual(
    name = ''
    , values =   c( 
      'Observed' = p.ln.clr.obsv
      ,'Modelled'= p.ln.colr.mod
    ) 
    , breaks = c(
      'Observed'
      ,  'Modelled'
    )) +
  theme(
    legend.position = "bottom" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.y.ax.lab) #+ 
# annotate("text"
#  , x =   nee.date.x.ax.lab   , 
# , y =  p.lab.nee.r2.y.crd
# , parse = TRUE 
#, label = p.lab.nee.r2 
#  , size =p.lab.nee.tx.fs
#  , hjust = 0
# )

func <- function(){

p.swc <- ggplot( d.all[ d.all$swc.1 > 0 ,  ] ,   aes(x = date.time)  
) +  geom_rect(
  aes(xmin = as.Date( p.ssn.x.ranges.2019.rn.2.min , format = '%Y-%m-%d'),
      xmax = as.Date(p.ssn.x.ranges.2019.rn.2.max , format = '%Y-%m-%d'),
      ymin = -Inf,
      ymax = Inf), alpha = p.ssn.bg.alpha , fill =  p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.rn.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.rn.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_line( aes(x = date.time, y = swc.1   , color= p.swc.osv.label ) 
             , linewidth = p.ln.width 
             
  ) +
  geom_line( aes(x = date.time, y = sw.10  , color= p.swc.sim.label ) 
             ,
             , linewidth = p.ln.width 
             
  ) +  
  geom_bar(  data = d.all[,  ] ,
             aes( x = date.time
                  , y = precip
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  scale_y_continuous(
    p.swc.y.ax.lab, 
    sec.axis = sec_axis(~ . * 1, name = p.precip.sec.ax.tit )
  ) +
  #geom_line(    aes(x = date.time, y = nee)       ) +
  #scale_x_discrete(aes('day.cnt') , day.cnt , labels = d.all$date.time  ) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  scale_colour_manual(
    name = ''
    , values =   c( 
      "Observed" = p.nee.color.1
      , "Simulated"  = p.nee.color.2
    ) 
    , breaks = c(
      p.swc.osv.label
      ,  p.swc.sim.label
    )) +
  theme(
    legend.position = "bottom" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.swc.y.ax.lab)



p.lai <- ggplot( d.all[ !is.na(d.all$lai.real != -99.99 ),  ] ,   aes(x = date.time )  
) + 
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2019.rn.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2019.rn.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill =  p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.rn.1.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.rn.1.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.rn.ssn.clr ) +
  geom_rect(
    aes(xmin = as.Date( p.ssn.x.ranges.2020.dr.2.min , format = '%Y-%m-%d'),
        xmax = as.Date(p.ssn.x.ranges.2020.dr.2.max , format = '%Y-%m-%d'),
        ymin = -Inf,
        ymax = Inf), alpha = p.ssn.bg.alpha , fill = p.dr.ssn.clr) +
  geom_bar(  data = d.all[,  ] ,
             aes( x = date.time
                  , y = precip 
             )
             , stat = 'identity'  
             , width = p.br.wdth
             , color = p.br.clr 
             , alpha = p.br.alpha 
  ) +
  # - Observed
  geom_line( data = d.all[ !is.na(d.all$lai.real != -99.99 ),  ] , 
             aes(x = date.time
                  , y = lai.real
                  , colour= 'Observed'
  )  
  ,linewidth = p.ln.width
  ) +  
  # - Modelled
  geom_line( aes(x = date.time
                 , y =   lai.sim
                 , colour= 'Modelled'
  ) 
  , linewidth = p.ln.width 
  ) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  scale_y_continuous(
    p.et.y.ax.lab , 
    sec.axis = sec_axis(~   . , name = p.precip.sec.ax.tit )
  ) +
  scale_colour_manual(
    name = ''
    , values =   c( 
      'Observed' = p.ln.clr.obsv
      ,'Modelled'= p.ln.colr.mod
    ) 
    , breaks = c(
      'Observed'
      ,  'Modelled'
    )) +
  theme(
    legend.position = "bottom" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.y.ax.lab) #+ 
# annotate("text"
#  , x =   nee.date.x.ax.lab   , 
# , y =  p.lab.nee.r2.y.crd
# , parse = TRUE 
#, label = p.lab.nee.r2 
#  , size =p.lab.nee.tx.fs
#  , hjust = 0
# )


p.lai
p.et
p.swc
p.nee 
}


filename <- 'kapiti.validate.nee.png'

p.width <- 600
p.height  <- 300
plot.dpi <- 1000

ggsave(filename =    filename ,  p.nee, width = 5 , height =4  , dpi = plot.dpi )


filename.swc <- 'kapiti.validate.swc.png'

p.width <- 600
p.height <- 300
plot.dpi <- 1000

ggsave(filename =    filename.swc ,  p.swc , width = 5 , height =4  , dpi = plot.dpi )


}


p.nee
p.swc
