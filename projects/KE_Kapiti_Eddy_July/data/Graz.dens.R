
library('ggplot2') ; library(ggh4x)

# Parameters
e <- 2.71828 


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
#' 
perim.frac.pios.1 <- 0.04043563   ; perim.frac.pios.2 <- 0.02265



# ~ ~ ~ ~ - - - - - - - - - - - - - - - Grazing density computation 

date.start <- "2018-07-25" ; date.end <- "2024-12-30"

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
g.dat.0[ , col.grz.hpd  ] <- NA
g.dat.0[ , col.src ] <- NA
g.dat.0[ , col.type ] <- NA
g.dat.0[ , col.type.label  ] <- NA
g.dat.0[ , col.source.label  ] <- NA

#  Source characteristics
{
  
  # smaller --> larger GD
  
  # Parameters for exponential decay function
  k.boma <- 0.003
  k.pios <- 0.0045
  
  k.boma <- 0.0038
  k.pios <- 0.005
  
  # Distances
  distance.boma.1 <- 1350 ;  distance.boma.2 <- 2550 ; distance.pios.1 <- 570 ;  distance.pios.2 <- 1652 ; 
  
  
  # Kapiti wide populations
  popn.bov <- 2400 ; popn.cap <- 1000
  
  
  # Boma or  piosphere specific populations
  popn.boma.1 <-  200 ;   popn.boma.2 <-  195;   popn.pios.1.bv <-  popn.bov * perim.frac.pios.1  ;  popn.pios.1.cap <-  popn.cap * perim.frac.pios.1 ; popn.pios.2.bv <-  popn.bov * perim.frac.pios.2 ;  popn.pios.2.cap <-  popn.cap * perim.frac.pios.2
  
  
  ks <- c(
    
      k.boma      # Boma 1   # not consistent with literature .. 
    , k.boma     # Boma 2
    , k.pios      # Piosphere 1 - LR
    , k.pios      # Piosphere 1 - SR
    , k.pios      # Piosphere 2 - LR
    , k.pios     # Piosphere 2 - SR
    
    ,0.05         # Wildlife grazing 
    
  )
  
  
  popns <- c(
    
      200
    , 195
    , 2500 * 0.04
    , 1450 * 0.032
    , 2500 * 0.04
    , 1450 * 0.032
    , 0
    
  )
  
  distances <- c(
    
      distance.boma.1
    , distance.boma.2
    
     # Piosphere 1
    ,   distance.pios.1
    ,   distance.pios.1
    
     # Piosphere 2
    ,   distance.pios.2
    ,   distance.pios.2
    
    , 500
    
  )
  
# Descriptive parameters  
  
sources <- c( 
  
    'boma.1' 
  , 'boma.2'
  , 'pios.1.bovines'
  , 'pios.1.caprinae'
  , 'pios.2.bovines'
  , 'pios.2.caprinae'
  , 'wild.herb'
  ,'all'
  
  
)
  
source.all <- 'all'

color.boma.1 <- 'red' ; color.boma.2 <- '#FAA0A0' ; color.piosphere.1 <- '#81b4e7' ; color.piosphere.2 <- '#add8e6' ; color.wildlife <- '#A8A8A8'

colors <- c( 
  
     color.boma.1
  ,  color.boma.2 
  ,  color.piosphere.1 
  ,  color.piosphere.1 
  ,  color.piosphere.2
  ,  color.piosphere.2
  ,  color.wildlife
  
)
  

start.periods <- c(
  
    "2018-07-25"
  , "2022-01-01"
  , date.start
  , date.start
  , date.start
  , date.start
  , date.start
)

end.periods <- c(
  "2018-12-30"
  , "2022-06-30"
  , date.end
  , date.end
  , date.end
  , date.end
  , date.end
)

hrs.p.days <- c(
  
    10
  , 10
  , 10
  , 10
  , 10
  , 10
  , 24
)



tlu.eqvs.boma.1 <- 1.2 ; tlu.eqvs.boma.2 <- 1.2 ; tlu.eqvs.piosph.bov <- 1.2 ;  tlu.eqvs.piosph.sr <- 1.2 ;  tlu.eqvs.wild <- 0.7

TLU.convs <- c(
  
  tlu.eqvs.boma.1 
    , tlu.eqvs.boma.2
    
    ,  tlu.eqvs.piosph.bov
    , tlu.eqvs.piosph.sr
    ,  tlu.eqvs.piosph.bov
    ,  tlu.eqvs.piosph.sr
    ,   tlu.eqvs.wild
    
    
  )


source.labels <- c(
  'Boma 1'
  ,'Boma 2'
  , 'Piosphere 1 - Bovines'
  , 'Piosphere 1 - Caprinae'
  , 'Piosphere 2 - Bovines'
  , 'Piosphere 2 - Caprinae'
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
  , type.all 
  
)



}

# ~ ~ ~ Define grazing regimes

for (i in 1 : (length( sources)-1) ){

  g.dat <- g.dat.0
  
  popn.TLU <- popns[i] * TLU.convs[i] ; distance.m <- distances[i]
  
  SD.TLU <- popn.TLU  * e^( -(1) * ks[i] * distance.m)
  SD.TLU.d <-  SD.TLU * hrs.p.days[i] / 24
  
  period <- seq( from = as.Date(start.periods[i] ) , to = as.Date(  end.periods[i] )  , by = "days")
  
  in.period <- ( g.dat$day %in% period  )
  out.period <- !( g.dat$day %in% period )
  
  
  g.dat[  ,  col.src ] <- sources[i]
  g.dat[  ,  col.type ] <-  type.ind
  
  if (s == "wild.herb" ) { SD.TLU <-  SD.TLU.d <- SD.TLU.d  }
  
  
  g.dat[  in.period, col.grz.dns.TLU] <- SD.TLU
  g.dat[  in.period, col.grz.dns.TLU.d] <- SD.TLU.d
  g.dat[  in.period, col.grz.hpd] <- hrs.p.days[i]
  
  g.dat[  out.period  , col.grz.dns.TLU] <- 0
  g.dat[  out.period  , col.grz.dns.TLU.d] <- 0
  g.dat[ out.period , col.grz.hpd] <- 0
  
  
  # Assign the global dataframe with the local one (first iter)
  if (i == 1){ g.dat.all <-  g.dat }
  else { g.dat.all <- rbind( g.dat.all , g.dat ) }

  
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




gg.grz.dns <- ggplot(   g.dat.all   ) +
  geom_line(   aes(  x =  day , y =  grz.dns.TLU.d , linetype = source.label , color = source.label)  ) + 
  facet_wrap (  
    .   ~ type.label 
    ,  ncol = 1
    ,  scales = "free"
    ) + 
  facetted_pos_scales(
    y = list(
      type.label  == "Source specific" ~ scale_y_continuous(limits = c(0, max.y.source)),  # Scale for Facet A
      type.label == "Cumulative" ~ scale_y_continuous(limits = c(0, max.y.cumulative ))   # Scale for Facet B
    )
  ) +
  # scale_y_continuous( limits = c(0, max.y) ,  breaks = seq(  0, max.y, 2) ) +
  ylab( bquote(Grazing~density~(TLU~days~ha^-1))) +
  
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
        "Boma 1" = 2
        , "Boma 2" = 2
        , "Piosphere 1 - Bovines" = 4
        , "Piosphere 1 - Caprinae" = 4
        , "Piosphere 2 - Bovines" = 5
        , "Piosphere 2 - Caprinae" = 5
        
        
        , "Wild herbivores"= 6
        
        , "All" = 1
      
      )
  ) +
  scale_color_manual(
    values = 
      c(
        "Boma 1" = colors[1]
        , "Boma 2" = colors[2]
        
        , "Piosphere 1 - Bovines" = colors[3]
        , "Piosphere 1 - Caprinae" = colors[4]
        , "Piosphere 2 - Bovines" = colors[5]
        , "Piosphere 2 - Caprinae" = colors[6]
        
        
        , "Wild herbivores"= colors[7]
        
        , "All" = colors[8]
        
        
      )
  ) +
  guides(color = guide_legend(nrow = 6  )  )  #length( source.labels ) 


gg.grz.dns



grz.dens.width <- 7.2
grz.dens.ht <- 5.4


ggsave(  filename = 'graz.dens.jpg' ,     gg.grz.dns  , width = grz.dens.width , height = grz.dens.ht )



summary(  g.dat.all[ g.dat.all$source ==  "boma.1"  & g.dat.all$day == g.dat.all$day[30] , 'grz.dns.TLU.d' ] )
