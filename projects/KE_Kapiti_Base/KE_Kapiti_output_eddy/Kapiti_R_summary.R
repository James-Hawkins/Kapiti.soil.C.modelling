

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
  
  
  library('chron')
  library('lubridate')
  
  
}
library.in()




{

  
  
d.sl.chem <<- read.csv('KE_Kapiti_soilchemistry-daily.csv')
  
d.physio  <<- read.csv('KE_Kapiti_physiology-daily.csv')
  
d.eddy.real  <<- read.csv('20210513_1119_KE_kapiti_overview_plots(Sheet1).csv')


d.eddy.clim  <<- read.csv('kapiti_climate_eddy.csv')

d.watr  <<- read.csv('KE_Kapiti_watercycle-daily.csv')




names(d.sl.chem )[6] <- 'emis.hetero'

#names(d.physio)[3] <- 'date.time'
names(d.physio)[25] <- 'maint.resp'
names(d.physio)[26] <- 'transp.resp'
names(d.physio)[27] <- 'growth.resp'
names(d.physio)[28] <- 'co2.upt'


names(d.eddy.real)[2] <- 'NEE.obs'

names(d.eddy.clim)[1] <- 'date'
#names(d.eddy.clim)[2] <- 'day'
names(d.eddy.clim)[7] <- 'precip'


names(d.watr)[3] <- 'date.time'
names(d.watr)[27] <- 'sw.10'
names(d.watr)[28] <- 'sw.15'
names(d.watr)[29] <- 'sw.20'
names(d.watr)[30] <- 'sw.30'
names(d.watr)[31] <- 'sw.40'
names(d.watr)[32] <- 'sw.50'
names(d.watr)[33] <- 'sw.60'




colnames(d.sl.chem)
colnames(d.physio)

d.all <- cbind( d.sl.chem$emis.hetero , d.physio)

d.all <- cbind( d.all , d.watr)

names(d.all)[1] <- 'emis.hetero'

nrow(d.all)
View(d.watr)


#d.all$date.time <-  as.POSIXct(d.all$date.time, format="%d-%m-%Y %H:%M")

d.all$date.time <- as.Date(d.all$date.time)

#d.all$date.time <- as.Date(d.all$date.time, "%m/%d/%Y")

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


nrow(d.all)

first.date.cald <- "2018-08-01" 
secd.date.cald <- "2020-07-02"  

frst.date <- which( d.all$date.time == "2018-08-01" )
end.date <- which( d.all$date.time == "2020-07-02" )

d.all <- d.all[d.all$day.cnt >= frst.date & d.all$day.cnt <= end.date ,  ]

# days for climate data
d.eddy.clim$date <- as.Date(d.eddy.clim$date , "%m/%d/%Y")
  
  
d.eddy.clim <- d.eddy.clim[d.eddy.clim$date >= first.date.cald& d.eddy.clim$date <=secd.date.cald ,  ]



nrow(d.eddy.clim)

# Merge eddy real with simulated data
 
d.all <- cbind(d.all, d.eddy.real)

d.all <- cbind(d.all, d.eddy.clim)

nrow(d.eddy.clim)
nrow(d.all)



# OBSERVED
# convert observed eddy in mm per sq m per s to kg per ha
d.all$NEE.obs.kg.ha <- d.all$NEE.obs * cv.sq.m.2.ha * cv.microml.2.kg * cv.mml.c.2.co2  * cv.sec.2.d 


summary( d.all$NEE.obs.kg.ha  )

summary(d.all[d.all$NEE.obs.kg.ha > -30 & !is.na(d.all$NEE.obs.kg.ha) , 'NEE.obs.kg.ha'])


# MODELLED
d.all$GPP <- cv.sq.m.2.ha * (-1) * d.all$co2.upt
d.all$TER <- cv.sq.m.2.ha *  (d.all$maint.resp + d.all$transp.resp + d.all$growth.resp) + d.all$emis.hetero

d.all$NEE.mod <- d.all$GPP + d.all$TER 

d.all$NEE.mod.sum <- sum(d.all$NEE.mod)




d.all$NEE.nrmse <-  100 *(    sqrt ( (d.all$NEE.mod - d.all$NEE.obs.kg.ha)^2) /  d.all$NEE.obs.kg.ha)

print(paste('Mean NRMSE is ' , mean(d.all$NEE.nrmse)))



# plot params
{
  
p.y.ax.lab <<- 'Net ecosystem exchange (kg C/ha/day)'  
p.x.ax.lab <<- 'Date (YY-MM-DD)'  

p.swc.y.ax.lab <- 'Soil water content (top layer) (%)'
  
p.br.wdth <<- .15

p.br.alpha <<- 0.6

p.ln.width <- 0.8

p.br.clr <<- 'darkgrey'
p.ln.colr.mod <- 'lightgreen'

}


d.all <- d.all[!duplicated(as.list(d.all))]

d.all$sw.

colnames(d.all)

d.all$sw.


p.swc <- ggplot( d.all[d.all$SWC_1 > 0 ,  ] ,   aes(x = date.time)  
) +
# geom_bar(  data = d.all[d.all$NEE.obs.kg.ha > -100 ,  ] ,
          #  aes( x = date.time , y = precip/10 )
           #  , stat = 'identity'  
            #, width = p.br.wdth
            # , color = p.br.clr 
            # , alpha = p.br.alpha 
            # ) +
     geom_line( aes(x = date.time, y = SWC_3) 
                 , color= p.br.clr 
                 , linewidth = p.ln.width 
                 
   ) +
   geom_line( aes(x = date.time, y = sw.40) 
              , color= p.ln.colr.mod
              , linewidth = p.ln.width 
              
   ) +  
 # scale_y_continuous(
   # p.swc.y.ax.lab, 
    # sec.axis = sec_axis(~ . * 10, name = "mpg (UK)")
  # ) +
  #geom_line(    aes(x = date.time, y = nee)       ) +
   #scale_x_discrete(aes('day.cnt') , day.cnt , labels = d.all$date.time  ) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  theme(
    legend.position = "right" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
  #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.swc.y.ax.lab)

p.swc


p.nee <- ggplot( d.all[d.all$NEE.obs.kg.ha > -100 ,  ] ,   aes(x = date.time, y = NEE.mod )  
) + 
  #geom_bar(  data = d.all[d.all$NEE.obs.kg.ha > -100 ,  ] ,
            #    aes( x = date.time , y = precip/10 )
              ##  , stat = 'identity'  
              #  , width = p.br.wdth
              #  , color = p.br.clr 
               # , alpha = p.br.alpha 
                # , sec.axis = sec_axis(~ . * 10
                # , name = "mpg (UK)"
#) +
  geom_line(  linewidth = p.ln.width
              , color = p.ln.colr.mod) +
  geom_line( aes(x = date.time, y = NEE.obs.kg.ha) 
             , color= 'grey'
             , linewidth = p.ln.width 
             
  ) +    
 # scale_y_continuous(
  #  p.y.ax.lab, 
   # sec.axis = sec_axis(~ . * 10, name = "mpg (UK)")
 # ) +
  #geom_line(    aes(x = date.time, y = nee)       ) +
  #scale_x_discrete(aes('day.cnt') , day.cnt , labels = d.all$date.time  ) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%y-%m-%d") +
  theme(
    legend.position = "right" ,
    # axis.title.x = element_blank() , 
    axis.text.x = element_text(angle = 270) ,
    #  legend.title = element_blank() ,
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  ) + 
  xlab(p.x.ax.lab) +
  ylab(p.y.ax.lab)




filename <- 'kapiti_validate.png'


p.width <- 600
p.height  <- 300
plot.dpi <- 1000

ggsave(filename =    filename ,  p.nee, width = 5 , height =4  , dpi = plot.dpi )


}


p.nee
p.swc
