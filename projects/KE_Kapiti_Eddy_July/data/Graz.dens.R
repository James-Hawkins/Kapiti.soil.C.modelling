
library('ggplot2') ; library(ggh4x) ; library('readxl')

# Parameters
e <- 2.71828 
perim.frac.pios.1 <- 0.04043563 
perim.frac.pios.2 <- 0.02265 
frac.water.water.pan <- 0.92


# smaller --> larger GD

# Parameters for exponential decay function

k.boma <- 0.004
k.waterp <- 0.004 #0.003  0.1/1000   .


# Kapiti wide populations
popn.bov <- 2550 ; popn.sp <- 1200 ; popn.gt <- 250



#' Main issues to discuss with S/J/K
#' what values of k to use
#' 
#' 
#' predicted water area of Kapiti using QGIS statistics method is 442715 sq m
#' with total Kapiti land area of 1.35344e+08 - this is 442715 /  135344000 = 0.033% water body to total land area -- seems legit
#' Piosphere 1 is 14345.5 sqm or 14345.5 / 442715 = 3.2% of all water surface area in Kapiti
#' 
#' Perimeter method
#' All water bodies: 14714.5
#' Piosphere 1: 595 / 14714.5 = 0.04043563
#' Piosphere 2: 333.304 / 14714.5 = 0.0226514
#' 
#' questions for Nelson/Sospeter:
#' what is the average/maximum distance herders typically bring cattle when 
#' herding them during the day? 
#' what fraction of drinking water comes from water pans vs. dedicated piped water troughs 
#' (near Bomas)
#' 
#' 


{


# ~ ~ ~ ~ - - - - - - - - - - - - - - - Grazing density computation 
boma.dat <- as.data.frame( read_excel( 'Boma.sites.xlsx') ) 



boma.dat$start   <- as.Date( boma.dat$start , format = "%m/%d/%Y")  
boma.dat$end   <- as.Date( boma.dat$end , format = "%m/%d/%Y") 



date.start <- "2018-07-28" ; date.end <- "2024-12-03"

dates <-  seq(from = as.Date( date.start  ), 
              to   = as.Date( date.end  ), 
              by   = "day")

time.period <- length( dates )

g.dat.0 <- data.frame( matrix( NA, nrow = time.period  , ncol = 1 )   )

g.dat.0$day <- dates



#' Column notation
#' grz.dns <- grazing density ; grz.hpd <- hours per day
col.grz.dns.TLU <- 'grz.dns.TLU' ;  col.grz.dns.TLU.d <- 'grz.dns.TLU.d' ; col.grz.hpd <- 'hpd'  ; col.src <- 'source' ; col.type <- 'type'
col.source.label <- 'source.label' ; col.type <- col.type.label <- 'type.label'

# Initialize as NA
g.dat.0[ , col.grz.dns.TLU ] <- NA
g.dat.0[ , col.grz.dns.TLU.d ] <- NA
#g.dat.0[ , col.grz.hpd  ] <- NA
g.dat.0[ , col.src ] <- NA
g.dat.0[ , col.type ] <- NA
g.dat.0[ , col.type.label  ] <- NA
g.dat.0[ , col.source.label  ] <- NA

#  Source characteristics
{
  

  # Distances
  distance.boma.1 <- boma.dat[boma.dat$boma.no == 1 , 'distance.ect.footprint.meters']  
  distance.boma.2 <- boma.dat[boma.dat$boma.no == 2 , 'distance.ect.footprint.meters'] 
  distance.boma.3 <- boma.dat[boma.dat$boma.no == 3 , 'distance.ect.footprint.meters'] 
  distance.boma.4 <- boma.dat[boma.dat$boma.no == 4 , 'distance.ect.footprint.meters'] 
  distance.boma.5 <- boma.dat[boma.dat$boma.no == 5 , 'distance.ect.footprint.meters'] 
  distance.boma.6 <- boma.dat[boma.dat$boma.no == 6 , 'distance.ect.footprint.meters']  
  distance.boma.7 <- boma.dat[boma.dat$boma.no == 7 , 'distance.ect.footprint.meters'] 
  
  distance.waterp.1 <- 570 
  distance.waterp.2 <- 1652 
  
  


  
  # TLU convs 
  TLU.mat.bov <- 350 / 250 
  TLU.juv.bov <- 260 / 250  
  TLU.all.bov <- 270 / 250  
  
  TLU.mat.sp <- 60 / 250 
  TLU.juv.sp <- 30 / 250
  
  TLU.mat.gt <- 45 / 250 
  TLU.juv.gt <- 25 / 250
  
  TLU.mat.cam <- 400 / 250 
  TLU.juv.cam <- 320 / 250
  
  TLU.all.bv <- 320/ 250 
  TLU.all.sp <- 65/ 250 
  TLU.all.gt <- 40 / 250 
  
  tlu.eqvs.boma.1 <- 1.2 ; tlu.eqvs.boma.2 <- 1.2 ; tlu.eqvs.boma.3 <- 1.2 ; tlu.eqvs.piosph.bov <- 1.2 ;  tlu.eqvs.piosph.sr <- 1.2 ;  tlu.eqvs.wild <- 0.7
  
  
  
  # Boma or  piosphere specific populations
  popn.TLU.boma.1 <-  ( 140 * 3 *  TLU.mat.bov   ) 
  
  
  # Still need to count/tally these two (2,3)
  popn.TLU.boma.2 <-  ( 
    boma.dat[boma.dat$boma.no == 2 , 'quant.juv.bov']  *  TLU.juv.bov 
                        + boma.dat[boma.dat$boma.no == 2 , 'quant.mat.bov']  * TLU.mat.bov 
                        + boma.dat[boma.dat$boma.no == 2 , 'quant.juv.sp'] * TLU.juv.sp
                        + boma.dat[boma.dat$boma.no == 2 , 'quant.mat.sp'] * TLU.mat.sp 
                        + boma.dat[boma.dat$boma.no == 2 , 'quant.juv.gt'] * TLU.juv.gt
                        + boma.dat[boma.dat$boma.no == 2 , 'quant.mat.gt'] * TLU.mat.gt 
                        + boma.dat[boma.dat$boma.no == 2 , 'quant.juv.cam'] * TLU.juv.cam
                        + boma.dat[boma.dat$boma.no == 2 , 'quant.mat.cam'] * TLU.mat.cam
                      ) 
  
  
  
  popn.TLU.boma.3 <- ( 
    boma.dat[boma.dat$boma.no == 3 , 'quant.juv.bov']  *  TLU.juv.bov 
    + boma.dat[boma.dat$boma.no == 3 , 'quant.mat.bov']  * TLU.mat.bov 
    + boma.dat[boma.dat$boma.no == 3 , 'quant.juv.sp'] * TLU.juv.sp
    + boma.dat[boma.dat$boma.no == 3 , 'quant.mat.sp'] * TLU.mat.sp 
    + boma.dat[boma.dat$boma.no == 3 , 'quant.juv.gt'] * TLU.juv.gt
    + boma.dat[boma.dat$boma.no == 3 , 'quant.mat.gt'] * TLU.mat.gt 
    + boma.dat[boma.dat$boma.no == 3 , 'quant.juv.cam'] * TLU.juv.cam
    + boma.dat[boma.dat$boma.no == 3 , 'quant.mat.cam'] * TLU.mat.cam
  ) 
  
  # 5 - North
  popn.TLU.boma.5 <-  ( 
    boma.dat[boma.dat$boma.no == 5 , 'quant.juv.bov']  *  TLU.juv.bov 
    + boma.dat[boma.dat$boma.no == 5 , 'quant.mat.bov']  * TLU.mat.bov 
    + boma.dat[boma.dat$boma.no == 5 , 'quant.juv.sp'] * TLU.juv.sp
    + boma.dat[boma.dat$boma.no == 5 , 'quant.mat.sp'] * TLU.mat.sp 
    + boma.dat[boma.dat$boma.no == 5 , 'quant.juv.gt'] * TLU.juv.gt
    + boma.dat[boma.dat$boma.no == 5 , 'quant.mat.gt'] * TLU.mat.gt 
    + boma.dat[boma.dat$boma.no == 5 , 'quant.juv.cam'] * TLU.juv.cam
    + boma.dat[boma.dat$boma.no == 5 , 'quant.mat.cam'] * TLU.mat.cam
  ) 
  # 6 - Northwest 2 
  popn.TLU.boma.6 <-  ( 
    boma.dat[boma.dat$boma.no == 6 , 'quant.juv.bov']  *  TLU.juv.bov 
    + boma.dat[boma.dat$boma.no == 6 , 'quant.mat.bov']  * TLU.mat.bov 
    + boma.dat[boma.dat$boma.no == 6 , 'quant.juv.sp'] * TLU.juv.sp
    + boma.dat[boma.dat$boma.no == 6 , 'quant.mat.sp'] * TLU.mat.sp 
    + boma.dat[boma.dat$boma.no == 6 , 'quant.juv.gt'] * TLU.juv.gt
    + boma.dat[boma.dat$boma.no == 6 , 'quant.mat.gt'] * TLU.mat.gt 
    + boma.dat[boma.dat$boma.no == 6 , 'quant.juv.cam'] * TLU.juv.cam
    + boma.dat[boma.dat$boma.no == 6 , 'quant.mat.cam'] * TLU.mat.cam
  ) 
  # 7 - Southeast
  popn.TLU.boma.7 <-  ( 
    boma.dat[boma.dat$boma.no == 7 , 'quant.juv.bov']  *  TLU.juv.bov 
    + boma.dat[boma.dat$boma.no == 7 , 'quant.mat.bov']  * TLU.mat.bov 
    + boma.dat[boma.dat$boma.no == 7 , 'quant.juv.sp'] * TLU.juv.sp
    + boma.dat[boma.dat$boma.no == 7 , 'quant.mat.sp'] * TLU.mat.sp 
    + boma.dat[boma.dat$boma.no == 7 , 'quant.juv.gt'] * TLU.juv.gt
    + boma.dat[boma.dat$boma.no == 7 , 'quant.mat.gt'] * TLU.mat.gt 
    + boma.dat[boma.dat$boma.no == 7 , 'quant.juv.cam'] * TLU.juv.cam
    + boma.dat[boma.dat$boma.no == 7 , 'quant.mat.cam'] * TLU.mat.cam
  ) 
  
  
  # 4 Northwest - note: data on species and numbers not reported. So take average of all other bomas
  popn.TLU.boma.4 <-  ( popn.TLU.boma.1 
                        + popn.TLU.boma.2
                        + popn.TLU.boma.3
                        + popn.TLU.boma.5
                        + popn.TLU.boma.6
                        + popn.TLU.boma.7
  ) / 7
  
  
  popn.TLU.waterp.1.bov <- popn.bov * perim.frac.pios.1 * frac.water.water.pan 
  popn.TLU.waterp.2.bov <- popn.bov * perim.frac.pios.2 * frac.water.water.pan 
  popn.TLU.waterp.1.cap <- ( popn.sp *  TLU.all.sp +  popn.gt   * TLU.all.gt) * perim.frac.pios.1 * frac.water.water.pan 
  popn.TLU.waterp.2.cap <- ( popn.sp *  TLU.all.sp +  popn.gt   * TLU.all.gt) * perim.frac.pios.2 * frac.water.water.pan 
  
  popn.TLU.waterp.1.all <- popn.TLU.waterp.1.bov +   popn.TLU.waterp.1.cap
  popn.TLU.waterp.2.all <- popn.TLU.waterp.2.bov +   popn.TLU.waterp.2.cap
  
  #
  SD.TLU.d.wherb <- 0.35
  
  ks <- c(
    
      k.boma      # Boma 1   # not consistent with literature .. 
    , k.boma     # Boma 2
    , k.boma     # Boma 3
    , k.boma     # Boma 4
    , k.boma     # Boma 5
    , k.boma     # Boma 6
    , k.boma     # Boma 7
    , k.waterp      # Waterpoint 1 - LR
    , k.waterp      # Waterpoint 1 - SR
    
    ,0.05         # Wildlife grazing 
    
  )
  
  
  popns.TLU <- c(
    
    popn.TLU.boma.1
    , popn.TLU.boma.2
    , popn.TLU.boma.3
    , popn.TLU.boma.4
    , popn.TLU.boma.5
    , popn.TLU.boma.6
    , popn.TLU.boma.7
    ,   popn.TLU.waterp.1.all 
    ,   popn.TLU.waterp.2.all
    , 0
    
  )
  
  distances <- c(
    
      distance.boma.1
    , distance.boma.2
    , distance.boma.3
    , distance.boma.4
    , distance.boma.5
    , distance.boma.6
    , distance.boma.7
    ,   distance.waterp.1
    

    ,   distance.waterp.2
    
    , 500
    
  )
  
# Descriptive parameters  
  
sources <- c( 
  
    'boma.1' 
  , 'boma.2'
  , 'boma.3'
  , 'boma.4'
  , 'boma.5'
  , 'boma.6'
  , 'boma.7'
  , 'waterp.1.all'
  , 'waterp.2.all'
  , 'wild.herb'
  ,'all'
  
  
)
  
source.all <- 'all'

color.boma.1 <- 'red'  
color.boma.2 <- '#880808' 
color.boma.3 <- '#FAA0A0' 
color.boma.4 <- 'purple' 
color.boma.5 <- '#5D3FD3'
color.boma.6 <- '#CF9FFF' 
color.boma.7 <- '#D70040'
color.waterp.1 <- '#81b4e7' 
color.waterp.2 <- '#add8e6' 
color.wildlife <- '#A8A8A8'  
color.cumulative <- 'black'

colors <- c( 
  
     color.boma.1
  ,  color.boma.2 
  ,  color.boma.3
  ,  color.boma.4
  ,  color.boma.5
  ,  color.boma.6
  ,  color.boma.7
  ,  color.waterp.1 
  ,  color.waterp.2
  ,  color.wildlife
  , color.cumulative
)
  

start.periods <- c(
  

  boma.dat[boma.dat$boma.no == 1 , 'start']          # Boma 1
  ,  boma.dat[boma.dat$boma.no == 2 , 'start']     # Boma 2
  , boma.dat[boma.dat$boma.no == 3 , 'start']      # Boma 3
  , boma.dat[boma.dat$boma.no == 4 , 'start']     # Boma 4
  , boma.dat[boma.dat$boma.no == 5 , 'start']    # Boma 5
  , boma.dat[boma.dat$boma.no == 6 , 'start']    # Boma 6
  , boma.dat[boma.dat$boma.no == 7 , 'start']   # Boma 7
  
    , date.start
  , date.start
  , date.start
)

end.periods <- c(
  
  
  boma.dat[boma.dat$boma.no == 1 , 'end']          # Boma 1
  ,  boma.dat[boma.dat$boma.no == 2 , 'end']     # Boma 2
  , boma.dat[boma.dat$boma.no == 3 , 'end']      # Boma 3
  , boma.dat[boma.dat$boma.no == 4 , 'end']     # Boma 4
  , boma.dat[boma.dat$boma.no == 5 , 'end']    # Boma 5
  , boma.dat[boma.dat$boma.no == 6 , 'end']    # Boma 6
  , boma.dat[boma.dat$boma.no == 7 , 'end']   # Boma 7
     , date.end      # Waterpan 1
  , date.end  # Waterpan 2
  , date.end  # Wildlife
)

hrs.p.days <- c(
  
    10
  , 10
  , 10
  , 10
  , 10
  , 10
  , 10
  , 10
  , 10
  , 24
)


source.labels <- c(
  'Boma 1'
  ,'Boma 2'
  ,'Boma 3'
  ,'Boma 4'
  ,'Boma 5'
  ,'Boma 6'
  ,'Boma 7'
  , 'Waterpan 1'
  , 'Waterpan 2'
  , 'Wild herbivores'
  ,'All'
)

type.lab.source <- 'By species/source' ;  type.lab.all <- 'Cumulative'

type.labels <- c(
  
    type.lab.source
    , type.lab.source
  , type.lab.source
    , type.lab.source
  , type.lab.source
  , type.lab.source
   , type.lab.source
  , type.lab.source
  , type.lab.source
  , type.lab.source
  , type.lab.all
  
)


type.ind <- 'ind' ; type.all <- 'all'

types <- c(
  
    type.ind 
  , type.ind 
  , type.ind 
  , type.ind 
  , type.ind 
  , type.ind 
  , type.ind 
  , type.ind 
  
  , type.ind 
  , type.ind 
  
  , type.all 
  
)



}

# ~ ~ ~ Define grazing regimes

for (i in 1 : (length( sources)-1) ){
  
  # test: i <- 8

  g.dat <- g.dat.0
  
  popn.TLU <- popns.TLU[i]  ; distance.m <- distances[i]
  
  SD.TLU <- popn.TLU  * e^( -(1) * ks[i] * distance.m)
  SD.TLU.d <-  SD.TLU * hrs.p.days[i] / 24
  
  period <- seq( from = as.Date(start.periods[i] ) , to = as.Date(  end.periods[i] )  , by = "days")
  
  in.period <- ( g.dat$day %in% period  )
  out.period <- !( g.dat$day %in% period )
  
  
  g.dat[  ,  col.src ] <- sources[i]
  g.dat[  ,  col.type ] <-  types[i]
  
  if (  sources[i] == "wild.herb" ) { SD.TLU <- SD.TLU.d <- SD.TLU.d.wherb  }
  
  
  g.dat[  in.period, col.grz.dns.TLU] <- SD.TLU
  g.dat[  in.period, col.grz.dns.TLU.d] <- SD.TLU.d
  
  g.dat[  out.period  , col.grz.dns.TLU] <- 0
  g.dat[  out.period  , col.grz.dns.TLU.d] <- 0
  
  
  # Assign the global dataframe with the local one (first iter)
  if (i == 1){ g.dat.all <-  g.dat }
  else { g.dat.all <- rbind( g.dat.all , g.dat ) }

  
}


if (FALSE){
  
unique(g.dat.all$source)
unique(g.dat.all$type)
summary(g.dat.all$grz.dns.TLU.d)
unique(g.dat.all[  is.na(g.dat.all$grz.dns.TLU.d) , 'source'])

}

# ~ ~ - - - - - - Summation
{
  
g.dat.sum <- g.dat.0

g.dat.sum[  ,  col.src ] <- source.all
g.dat.sum[  ,  col.type ] <- type.all
g.dat.sum[  ,  col.grz.dns.TLU ] <- 0
g.dat.sum[  ,  col.grz.dns.TLU.d ] <- 0

for (  s in sources[  c(1:(length(sources)-1))  ]  ){
  for (  d in g.dat.sum$day   ){
    
    # test: d <- g.dat.sum$day[200] ;  s <- sources[1]
    
    g.dat.sum[ g.dat.sum$day == d & g.dat.sum$source == 'all'  ,  col.grz.dns.TLU ] <- (
      g.dat.sum[ g.dat.sum$day == d & g.dat.sum$source == 'all'   ,  col.grz.dns.TLU ] + 
        g.dat.all[  g.dat.all$day == d & g.dat.all$source == s , col.grz.dns.TLU ] 
      )
    
    
    g.dat.sum[ g.dat.sum$day == d & g.dat.sum$source == 'all'  ,  col.grz.dns.TLU.d ] <- ( 
      g.dat.sum[ g.dat.sum$day == d & g.dat.sum$source == 'all'   ,  col.grz.dns.TLU.d ] + 
        g.dat.all[  g.dat.all$day == d & g.dat.all$source == s , col.grz.dns.TLU.d ] )
    
    
    
 }}


View(g.dat.all)

g.dat.all[  is.na(g.dat.all$grz.dns.TLU.d) ,'type']


colnames(   g.dat.all  )
colnames(   g.dat.sum  )

g.dat.all <- rbind( g.dat.all , g.dat.sum )

}

# ~ ~ - - - - - -  PLOT  - - - - ~ ~
{
  


for (  s in sources )  {
  
  # s <- sources[6]
  
  i <- which(  s == sources  )
  
  
g.dat.all[ g.dat.all$source == sources[i] , 'source.label'] <- source.labels[i]
g.dat.all[ g.dat.all$type == types[i] , 'type.label'] <- type.labels[i] 


}

# Parmaters
max.y.cumulative <-1.2 * max(  g.dat.all[   g.dat.all$type.label == type.lab.all , 'grz.dns.TLU.d'  ])     
max.y.source <-1.2 * max(  g.dat.all[   g.dat.all$type.label == type.lab.source , 'grz.dns.TLU.d'  ])     



g.dat.all$type.label <- factor( g.dat.all$type.label , levels = unique(type.labels)  )
g.dat.all$source.label <- factor( g.dat.all$source.label  , levels = unique(source.labels)  )




} # - Plot pre - process

# unique(g.dat.all$day)
# unique(g.dat.all$grz.dns.TLU.d)
# unique(g.dat.all$source)
# unique(g.dat.all$type)

# unique(g.dat.all$source.label)
# unique(g.dat.all$type.label)



} # Run all


gg.grz.dns <- ggplot(   g.dat.all   ) +
  geom_line(   aes(  x =  day , y =  grz.dns.TLU.d , linetype = source.label , color = source.label)  , linewidth = .8 ) + 
  facet_wrap(  
    .   ~ type.label 
    ,  ncol = 1
     ,  scales = "free_y"
    #  ,  axes = "b" 
    ) + 
  facetted_pos_scales(
    y = list(
      type.label  == "Source specific" ~ scale_y_continuous(limits = c(0, max.y.source)),  # Scale for Facet A
      type.label == "Cumulative" ~ scale_y_continuous(limits = c(0, max.y.cumulative ))   # Scale for Facet B
    )
  ) +
  # scale_y_continuous( limits = c(0, max.y) ,  breaks = seq(  0, max.y, 2) ) +
  ylab( bquote(Cumulative~stocking~rate~(TLU~days~ha^-1))) +
  
    scale_x_date(
      date_breaks = "3 months"
      , date_labels = "%Y-%m" 
      
      ) +  
  
  theme(
    
    
    , panel.grid.major = element_blank(),
    , panel.background = element_blank()
    , strip.background = element_rect(color='black', fill='white',linewidth = 1, linetype="solid")
    , strip.text.x = element_text(size =  11 , color = 'black' )
    ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    
    
    # Axes
    , axis.text.x = element_text( size = 8.5 , angle = 90  )
    
    , axis.title.x = element_blank()
    
    # Legend 
    , legend.title = element_blank()
    
    # , legend.position = "inside"
    , legend.position = c( 1.12 , 0.5 )
    , legend.text = element_text( size = 8.5 )
    
    # Margins
    , plot.margin = margin( t = 0.11 , b = .1, r = 3.1 , l = 0.1 , unit = 'cm')
    
  ) +
  scale_linetype_manual(
    values = 
      c( 
        "Boma 1" = 3
        , "Boma 2" = 3
        , "Boma 3" = 3
        , "Boma 4" = 3
        , "Boma 5" = 3
        , "Boma 6" = 3
        , "Boma 7" = 3
        , "Waterpan 1" = 4
        , "Waterpan 2" = 4
        
      
        , "Wild herbivores"= 6
        
        , "All" = 1
      
      )
  ) +
  scale_color_manual(
    values = 
      c(
        "Boma 1" = colors[1]
        , "Boma 2" = 'purple'
        , "Boma 3" = colors[3]
        , "Boma 4" = colors[4]
        , "Boma 5" = colors[5]
        , "Boma 6" = colors[6]
        , "Boma 7" = colors[7]
        
        , "Waterpan 1" = colors[8]
        , "Waterpan 2"  = colors[9]
        
        
        , "Wild herbivores"= colors[10]
        
        , "All" = colors[11]
        
        
      )
  ) +
  guides(color = guide_legend(nrow = 6  )  )  #length( source.labels ) 


gg.grz.dns



grz.dens.width <- 7.2
grz.dens.ht <- 5.4


ggsave(  filename = 'graz.dens.jpg' ,     gg.grz.dns  , width = grz.dens.width , height = grz.dens.ht )



unique( g.dat.all[g.dat.all$grz.dns.TLU.d > 30  ,   'source' ]  )

summary(  g.dat.all[ g.dat.all$source ==  "boma.1"   , 'grz.dns.TLU.d' ] )
summary(  g.dat.all[ g.dat.all$source ==  "boma.5"   , 'grz.dns.TLU.d' ] )


