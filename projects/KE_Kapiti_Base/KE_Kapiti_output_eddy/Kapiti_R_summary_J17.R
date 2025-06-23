

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



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
  
  
}
library.in()


{


# L-DNDC modelled outputs
d.sl.chem <<- read.csv('KE_Kapiti_soilchemistry-daily.csv')
  
d.physio  <<- read.csv('KE_Kapiti_physiology-daily.csv')
  
d.watr  <<- read.csv('KE_Kapiti_watercycle-daily.csv')


# L-DNDC raw data
d.eddy.clim  <<- read.csv('KE_Kapiti_climate_eddy_2019_336_onwards.csv')


# Observed (measured) data from EC tower

d.eddy.real  <<- read.csv('20210513_1119_KE_kapiti_overview_plots(Sheet1).csv')


colnames(d.eddy.real)

# Rename columns
names(d.sl.chem )[6] <- 'emis.hetero'

names(d.physio)[3] <- 'date.time'
names(d.physio)[25] <- 'maint.resp'
names(d.physio)[26] <- 'transp.resp'
names(d.physio)[27] <- 'growth.resp'
names(d.physio)[28] <- 'co2.upt'
names(d.physio)[39] <- 'lai.sim'


names(d.eddy.real)[2] <- 'nee'
names(d.eddy.real)[6] <- 'et.real.mm'
names(d.eddy.real)[7] <- 'lai.real'
names(d.eddy.real)[8] <- 'sw.osv.65.cm'
names(d.eddy.real)[9] <- 'sw.osv.25.cm'
names(d.eddy.real)[10] <- 'sw.osv.5.cm'




names(d.eddy.clim)[1] <- 'yr'
names(d.eddy.clim)[2] <- 'day.cnt'
names(d.eddy.clim)[7] <- 'precip'
d.eddy.clim <- d.eddy.clim[ 23:nrow(d.eddy.clim) ,  ]


names(d.watr)[3] <- 'date.time'
names(d.watr)[7] <- 'et.sim.mm'
names(d.watr)[27] <- 'sw.10'
names(d.watr)[28] <- 'sw.15'
names(d.watr)[29] <- 'sw.20'
names(d.watr)[30] <- 'sw.30'
names(d.watr)[31] <- 'sw.40'
names(d.watr)[32] <- 'sw.50'
names(d.watr)[33] <- 'sw.60'


# RE-ASSIGN OUTLIERS (in observed data) to NA
d.eddy.real[d.eddy.real$sw.osv.5.cm  < -50 , 'sw.osv.5.cm'] <- NA
d.eddy.real[d.eddy.real$sw.osv.25.cm  < -50 , 'sw.osv.25.cm'] <- NA
d.eddy.real[d.eddy.real$sw.osv.65.cm  < -50 , 'sw.osv.65.cm'] <- NA


d.eddy.real[d.eddy.real$nee  < -99 , 'nee'] <- NA

nrow(d.watr)
nrow(d.physio)
nrow(d.sl.chem)



d.all <- cbind( d.sl.chem$emis.hetero , d.physio)

d.all <- cbind( d.all , d.watr)

names(d.all)[1] <- 'emis.hetero'

nrow(d.all)


#  View(d.watr)
#  View(d.all)
#  View(d.eddy.clim)



d.all$date.time <- as.Date(d.all$date.time ,  format="%Y-%m-%d")



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


d.all$day.cnt <- NA


for (r in 1:nrow(d.all)  ){
  
  d.all[ r , 'day.cnt'] <- r 
  
}




for (r in 1:nrow(d.eddy.clim)){
  
day.cnt <- d.eddy.clim[ r , 'day.cnt']
year <- d.eddy.clim[ r , 'yr']
origin <- str_c(d.eddy.clim[d.eddy.clim$day.cnt ==  day.cnt & d.eddy.clim$yr == year, 'yr'],'-01-01')

day.cnt <- as.numeric(day.cnt)

d.eddy.clim[r,'date'] <-  as.Date( day.cnt ,  origin = origin)

   
}


first.date.cald <- "2019-12-02" 
secd.date.cald <- "2020-07-02"  

frst.date <- which( d.all$date.time == first.date.cald )
end.date <- which( d.all$date.time == secd.date.cald  )

d.all <- d.all[d.all$day.cnt >= frst.date & d.all$day.cnt <= end.date ,  ]



d.eddy.clim <- d.eddy.clim[d.eddy.clim$date >= first.date.cald & d.eddy.clim$date <=secd.date.cald ,  ]

d.eddy.clim$precip <- as.numeric(d.eddy.clim$precip)


d.eddy.real$datetime <- as.Date(d.eddy.real$datetime , "%m/%d/%Y")


d.eddy.real <- d.eddy.real[d.eddy.real$datetime >= first.date.cald & d.eddy.real$date <= secd.date.cald ,  ]


d.all <- cbind(d.all, d.eddy.real)

d.all <- cbind(d.all, d.eddy.clim)



# -- Model validation

# Observed
# convert observed eddy in mm per sq m per s to kg per ha
d.all$NEE.obs.kg.ha <- d.all$nee * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 



d.all[d.all$NEE.obs.kg.ha < -90 & !is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'] <- NA


# MODELLED
d.all$GPP <- cv.sq.m.2.ha * (-1) * d.all$co2.upt
d.all$TER <- cv.sq.m.2.ha *  (d.all$maint.resp + d.all$transp.resp + d.all$growth.resp) + d.all$emis.hetero

d.all$NEE.mod <-   d.all$TER + d.all$GPP 

d.all$NEE.mod.sum <- sum(d.all$NEE.mod)




# Calcuate R2 valueS

# NEE
NEE.tss <-  sum (sqrt ( ( d.all$NEE.obs.kg.ha)^2) )
NEE.rss <- sum(sqrt ( (  d.all$NEE.obs.kg.ha - d.all$NEE.mod )^2) )
NEE.R2 <- 1 - NEE.rss /NEE.tss 


NEE.NRMSE <- 100* mean(sqrt ( (  (d.all$NEE.obs.kg.ha - d.all$NEE.mod ) )^2) ) / mean(  d.all$NEE.obs.kg.ha )

print(paste('R2 for NEE is ' , NEE.R2))

# SWC - 5 cm layer
  


swc.5.tss <-      sum (sqrt ( ( d.all[ !is.na(d.all$sw.osv.5.cm) ,'sw.osv.5.cm' ])^2) )
swc.5.rss <-      sum(sqrt ( ( d.all[ !is.na(d.all$sw.osv.5.cm),'sw.osv.5.cm' ] - d.all[!is.na(d.all$sw.osv.5.cm),'sw.10' ])^2) )

swc.5.R2 <- 1 - swc.5.rss / swc.5.tss



swc.5.nrmse <-  mean(sqrt ( ( d.all[ !is.na(d.all$sw.osv.5.cm),'sw.osv.5.cm' ] - d.all[!is.na(d.all$sw.osv.5.cm),'sw.10' ])^2) ) / mean(d.all[ ,'sw.osv.5.cm' ])

print(paste('R2 for SWC is ' , swc.5.R2))



# plot params
{
  
p.y.ax.lab <<- 'Net ecosystem exchange (kg C/ha/day)'  
p.x.ax.lab <<- 'Date (YY-MM-DD)'  

p.swc.y.ax.lab <- 'Soil water content (%)'
p.et.y.ax.lab  <- 'Evapotranspiration (mm/d)'
p.lai.y.ax.lab  <- 'Leaf area index'

p.precip.sec.ax.tit <- 'Precipitation (mm/day)'
  
p.br.wdth <<- .15

p.br.alpha <<- 0.6

p.ln.width <- 0.8



p.lab.nee.r2.x.crd <<- 0.5
p.lab.nee.r2.y.crd  <<- 23

p.lab.nee.tx.fs <- 4.75
p.lab.nee.r2 <-  paste0("R^2~",NEE.rss)



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

nee.date.x.ax.lab <- as.Date("2020-05-01")


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


p.rn.ssn.clr <- '#eaffdf'
p.dr.ssn.clr <- '#fef2c6'

p.ssn.bg.alpha <- 0.1

}


d.all <- d.all[!duplicated(as.list(d.all))]



# ggplot with legend for different line aesthetics
# https://stackoverflow.com/questions/65929800/ggplot2-separate-legend-for-multiple-geom-lines

p.nee <- ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha ),  ] ,   aes(x = date.time )  
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
  , y = NEE.obs.kg.ha 
, colour= p.nee.label.1
)  
              ,linewidth = p.ln.width
) +  
  # - Modelled
  geom_line( aes(x = date.time
                 , y = NEE.mod 
                 , colour=  p.nee.label.2
                 ) 
             , linewidth = p.ln.width 
             
  ) +    
  geom_line( aes(x = date.time, y = TER  , color= p.nee.label.3) 
            , linewidth = p.ln.width 
             
  ) +   
  geom_line( aes(x = date.time, y = GPP   , color= p.nee.label.4) 
             , linewidth = p.ln.width 
             
  ) +   

  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  scale_colour_manual(
    name = ''
    , values =   c( 
        "NEE, obsd" = p.nee.color.1 #"NEE, obsd"
      ,"NEE, simd"  = p.nee.color.2
      ,"TER" = p.nee.color.3
      ,"GPP" = p.nee.color.4
    ) 
    , breaks = c(
      p.nee.label.1
      , p.nee.label.2 
      , p.nee.label.3 
      , p.nee.label.4 
    )
#, guide = guide_legend(override.aes = list(linetype = c( 1 , 2 )))
  )  + 
  scale_y_continuous(
    p.swc.y.ax.lab, 
    sec.axis = sec_axis(~   . , name = p.precip.sec.ax.tit )
  ) +
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

p.nee




p.swc <- ggplot( d.all[ d.all$sw.osv.5.cm > 0 ,  ] ,   aes(x = date.time)  
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
     geom_line( aes(x = date.time, y = sw.osv.5.cm   , color= p.swc.osv.label ) 
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


p.swc <- ggplot( d.all[ d.all$sw.osv.5.cm > 0 ,  ] ,   aes(x = date.time)  
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
  geom_line( aes(x = date.time, y = sw.osv.5.cm   , color= p.swc.osv.label ) 
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


filename <- 'kapiti_validate.png'

p.width <- 600
p.height  <- 300
plot.dpi <- 1000

ggsave(filename =    filename ,  p.nee, width = 5 , height =4  , dpi = plot.dpi )


}


p.nee
p.swc
