
library(ggrepel)  ; library(forcats) ; library('ggplot2')


{
d.s.comp.q1 <<- read_xlsx(
  '../data/Kapiti Biodiversity and Plants Cover_EC Tower_April 2026+JH.xlsx'
 ,  sheet = 'T2_Q1'
  )

d.s.comp.q2 <<- read_xlsx(
  '../data/Kapiti Biodiversity and Plants Cover_EC Tower_April 2026+JH.xlsx'
  ,  sheet = 'T2_Q2'
)


d.s.comp.q3 <<- read_xlsx(
  '../data/Kapiti Biodiversity and Plants Cover_EC Tower_April 2026+JH.xlsx'
  ,  sheet = 'T2_Q3'
)


d.s.comp.q4 <<- read_xlsx(
  '../data/Kapiti Biodiversity and Plants Cover_EC Tower_April 2026+JH.xlsx'
  ,  sheet = 'T2_Q4'
)


d.s.comp.q5 <<- read_xlsx(
  '../data/Kapiti Biodiversity and Plants Cover_EC Tower_April 2026+JH.xlsx'
  ,  sheet = 'T2_Q5'
)

d.s.comp.q1 <<- as.data.frame(d.s.comp.q1)
d.s.comp.q2 <<- as.data.frame(d.s.comp.q2)
d.s.comp.q3 <<- as.data.frame(d.s.comp.q3)
d.s.comp.q4 <<- as.data.frame(d.s.comp.q4)
d.s.comp.q5 <<- as.data.frame(d.s.comp.q5)






}


d.s.comp.all <- rbind(
  
  d.s.comp.q1
  #, d.s.comp.q2
  , d.s.comp.q3
  #, d.s.comp.q4
  , d.s.comp.q5
  
)

d.s.comp.all[,'gen_species'] <- str_c( d.s.comp.all[,'Genus'] ,'\n' , d.s.comp.all[,'Species'])





d.s.comp.all <-  d.s.comp.all[ !is.na(d.s.comp.all$ldndc.type) , ]

species.tall.pern <<- (unique(d.s.comp.all[d.s.comp.all$ldndc.type == 'tall.perennial.grass' , 'gen_species']))
species.med.pern <<- (unique(d.s.comp.all[d.s.comp.all$ldndc.type == 'medium.perennial.grass' , 'gen_species']))
species.forb <<- (unique(d.s.comp.all[d.s.comp.all$ldndc.type == 'forb' , 'gen_species']))
species.turf <<- (unique(d.s.comp.all[d.s.comp.all$ldndc.type == 'turf.grass' , 'gen_species']))


quadr.w.1 <- 0.7 ; quadr.w.3 <- 0.2 ; quadr.w.5 <- 0.1


#  colnames(d.s.comp.all)
d.s.comp.all[  d.s.comp.all$quadrat == 1   ,"cover.pct.wgtd"  ] <- d.s.comp.all[  d.s.comp.all$quadrat == 1    ,"cover.pct"  ] * quadr.w.1 
d.s.comp.all[  d.s.comp.all$quadrat == 3   ,"cover.pct.wgtd"  ] <- d.s.comp.all[  d.s.comp.all$quadrat == 3    ,"cover.pct"  ] * quadr.w.3 
d.s.comp.all[  d.s.comp.all$quadrat == 5   ,"cover.pct.wgtd"  ] <- d.s.comp.all[  d.s.comp.all$quadrat == 5     ,"cover.pct"  ] * quadr.w.5 


row.count <- 1 ; new.row <- nrow(d.s.comp.all) + 1

for (gs in unique(d.s.comp.all$gen_species) ){
  
  if (is.na(gs)){ next}
  
  # gs <- unique(d.s.comp.all$gen_species)[1]
  
  d.s.comp.all[new.row  , 'quadrat'] <- 'weighted'
  d.s.comp.all[new.row  , 'gen_species'] <- gs
  
  if (  gs %in% species.tall.pern  ){   d.s.comp.all[new.row  , "ldndc.type"] <- "tall.perennial.grass"  }
  if (  gs %in% species.med.pern  ){   d.s.comp.all[new.row  , "ldndc.type"] <- "medium.perennial.grass"  }
  if (  gs %in% species.forb  ){   d.s.comp.all[new.row  , "ldndc.type"] <- "forb"  }
  if (  gs %in% species.turf  ){   d.s.comp.all[new.row  , "ldndc.type"] <- "turf"  }
  
  
quadr.1.val <-  d.s.comp.all[d.s.comp.all$quadrat == '1' & d.s.comp.all$gen_species ==  gs & !is.na(d.s.comp.all$gen_species)  , 'cover.pct'] 
quadr.3.val <-  d.s.comp.all[d.s.comp.all$quadrat == '3' & d.s.comp.all$gen_species ==  gs & !is.na(d.s.comp.all$gen_species)  , 'cover.pct'] 
quadr.5.val <-  d.s.comp.all[d.s.comp.all$quadrat == '5' & d.s.comp.all$gen_species ==  gs & !is.na(d.s.comp.all$gen_species)  , 'cover.pct'] 

sum <- 0 ; count  <- 0

if (  length(quadr.1.val)  > 0 ){ sum <- sum + quadr.1.val * quadr.w.1 ; count <- count + 1}
if (  length(quadr.3.val)  > 0 ){ sum <- sum + quadr.3.val * quadr.w.3 ; count <- count + 1}
if (  length(quadr.5.val)  > 0 ){ sum <- sum + quadr.5.val * quadr.w.5 ; count <- count + 1}

    
    
  d.s.comp.all[new.row  , 'cover.pct.wgtd'] <- sum / 3
  
  
  new.row <- new.row + 1 ; row.count <- row.count  + 1
}

#  View(d.s.comp.all[d.s.comp.all$quadrat == 'weighted' & d.s.comp.all$ldndc.type == "forb",])
#  View(d.s.comp.all[d.s.comp.all$quadrat == 'weighted' & d.s.comp.all$ldndc.type == "tall.perennial.grass" ,])
#  View(d.s.comp.all[d.s.comp.all$quadrat == 'weighted' & d.s.comp.all$ldndc.type == "medium.perennial.grass",])

sum(d.s.comp.all[d.s.comp.all$quadrat == 'weighted' & d.s.comp.all$ldndc.type == 'forb' & !is.na(d.s.comp.all$ldndc.type) ,"cover.pct.wgtd"])
sum(d.s.comp.all[d.s.comp.all$quadrat == 'weighted' & d.s.comp.all$ldndc.type == "tall.perennial.grass" & !is.na(d.s.comp.all$ldndc.type) ,"cover.pct.wgtd"])
sum(d.s.comp.all[d.s.comp.all$quadrat == 'weighted' & d.s.comp.all$ldndc.type == "medium.perennial.grass" & !is.na(d.s.comp.all$ldndc.type) ,"cover.pct.wgtd"])



d.s.comp.all$ldndc.type.num  <- as.numeric(d.s.comp.all$ldndc.type)


ldndc.types <- c("turf" ,  "medium.perennial.grass" , "tall.perennial.grass" , "forb"  )

ldndc.type.labels <- c( 'Turf grass' , 'Medium\ngrass' , 'Tall\ngrass' , 'Forb' )
d.s.comp.all$ldndc.type.label <- NA
d.s.comp.all[ !is.na(d.s.comp.all$ldndc.type) & d.s.comp.all$ldndc.type == "turf" , 'ldndc.type.label'] <- ldndc.type.labels[1]
d.s.comp.all[ !is.na(d.s.comp.all$ldndc.type) & d.s.comp.all$ldndc.type == "medium.perennial.grass"  , 'ldndc.type.label'] <- ldndc.type.labels[2]
d.s.comp.all[ !is.na(d.s.comp.all$ldndc.type) & d.s.comp.all$ldndc.type == "tall.perennial.grass"  , 'ldndc.type.label'] <- ldndc.type.labels[3]
d.s.comp.all[ !is.na(d.s.comp.all$ldndc.type) & d.s.comp.all$ldndc.type ==  "forb" , 'ldndc.type.label'] <-  ldndc.type.labels[4]
d.s.comp.all$ldndc.type.label <- factor(d.s.comp.all$ldndc.type.label , levels  =ldndc.type.labels )


d.s.comp.all$panel.label <- 'Species per Plant\nfuntional type (PFT)'

#colnames(d.s.comp.all)

d.s.comp.all$quadrat.all <- 'all'
d.s.comp.all <- d.s.comp.all[  !is.na(d.s.comp.all$quadrat.all) , ]


d.func.gps <- data.frame(
  
  func.group = ldndc.types
 # ,  func.group.label = ldndc.type.labels 
  , cover = NA
  , x.catg = 'Quadrats - weighted mean'
  , panel.label.area = 'PFT-mean\nland cover'
 , panel.label.biom = 'PFT-mean\nbiomass'
)


d.func.gps[d.func.gps$func.group == ldndc.types[1] , 'cover' ] <- sum(  na.omit(d.s.comp.all[d.s.comp.all$ldndc.type == ldndc.types[1] , 'cover.pct.wgtd' ]  ))
d.func.gps[d.func.gps$func.group == ldndc.types[2] , 'cover' ] <- sum(  na.omit(d.s.comp.all[d.s.comp.all$ldndc.type == ldndc.types[2] , 'cover.pct.wgtd' ]  ))
d.func.gps[d.func.gps$func.group == ldndc.types[3] , 'cover' ] <- sum(  na.omit(d.s.comp.all[d.s.comp.all$ldndc.type == ldndc.types[3] , 'cover.pct.wgtd' ]  ))
d.func.gps[d.func.gps$func.group == ldndc.types[4] , 'cover' ] <- sum(  na.omit(d.s.comp.all[d.s.comp.all$ldndc.type == ldndc.types[4] , 'cover.pct.wgtd' ]  ))

sum(
  d.func.gps[d.func.gps$func.group == ldndc.types[1] , 'cover' ]
  , d.func.gps[d.func.gps$func.group == ldndc.types[2] , 'cover' ]
  , d.func.gps[d.func.gps$func.group == ldndc.types[3] , 'cover' ]
  , d.func.gps[d.func.gps$func.group == ldndc.types[4] , 'cover' ]
)


reference.biomass.kg.yr <- c(1200,1000,1300,550)


d.func.gps$biom.frac <- NA
d.func.gps[d.func.gps$func.group == ldndc.types[1] , 'biom.frac' ] <- reference.biomass.kg.yr[1] * d.func.gps[d.func.gps$func.group == ldndc.types[1] , 'cover' ] / sum(  reference.biomass.kg.yr[c(1:4)] * d.func.gps[d.func.gps$func.group %in% ldndc.types , 'cover' ]  )
d.func.gps[d.func.gps$func.group == ldndc.types[2] , 'biom.frac' ] <- reference.biomass.kg.yr[2] * d.func.gps[d.func.gps$func.group == ldndc.types[2] , 'cover' ] / sum(  reference.biomass.kg.yr[c(1:4)] * d.func.gps[d.func.gps$func.group %in% ldndc.types , 'cover' ]  )
d.func.gps[d.func.gps$func.group == ldndc.types[3] , 'biom.frac' ] <- reference.biomass.kg.yr[3] * d.func.gps[d.func.gps$func.group == ldndc.types[3] , 'cover' ] / sum(  reference.biomass.kg.yr[c(1:4)] * d.func.gps[d.func.gps$func.group %in% ldndc.types , 'cover' ]  )
d.func.gps[d.func.gps$func.group == ldndc.types[4] , 'biom.frac' ] <- reference.biomass.kg.yr[4] * d.func.gps[d.func.gps$func.group == ldndc.types[4] , 'cover' ] / sum(  reference.biomass.kg.yr[c(1:4)] * d.func.gps[d.func.gps$func.group %in% ldndc.types , 'cover' ]  )

sum(d.func.gps[d.func.gps$func.group %in% ldndc.types , 'biom.frac' ] )


if (FALSE) {

  # Mean biomass fractions
  d.func.gps[d.func.gps$func.group == ldndc.types[1] , 'biom.frac' ] * 100
  d.func.gps[d.func.gps$func.group == ldndc.types[2] , 'biom.frac' ] * 100
  d.func.gps[d.func.gps$func.group == ldndc.types[3] , 'biom.frac' ] * 100
  d.func.gps[d.func.gps$func.group == ldndc.types[4] , 'biom.frac' ] * 100
  
  
}

d.func.gps$func.group.label <- NA
d.func.gps[ !is.na(d.func.gps$func.group) & d.func.gps$func.group  == "turf" , 'func.group.label'] <- ldndc.type.labels[1]
d.func.gps[ !is.na(d.func.gps$func.group) & d.func.gps$func.group  == "medium.perennial.grass"  , 'func.group.label'] <- ldndc.type.labels[2]
d.func.gps[ !is.na(d.func.gps$func.group) & d.func.gps$func.group  == "tall.perennial.grass"  , 'func.group.label'] <- ldndc.type.labels[3]
d.func.gps[ !is.na(d.func.gps$func.group) & d.func.gps$func.group  ==  "forb" , 'func.group.label'] <-  ldndc.type.labels[4]
d.func.gps$func.group.label <- factor(d.func.gps$func.group.label , levels  =ldndc.type.labels )

# View(d.func.gps)


# Plot parameters
{
  species.text.fs <- 3.25 
  strip.text.fs <- 9.75
  
  
}
    
  

# Mean across first 5 quadrats
gg.func.groups.area <-  ggplot(
  d.func.gps
  
  ) + 
  geom_bar( 
    mapping = aes( x =   x.catg  , y = cover , fill = func.group.label )
           ,      position = position_fill( reverse = TRUE)
    , stat="identity"
  , width =   0.6
    
    ) +
  geom_text(  
    mapping =   aes(  
      label = ifelse(cover > .3 , as.character(func.group.label), ifelse(func.group.label == 'Turf grass'  , as.character(func.group.label), "")) 
      #label = func.group.label
      , x =  x.catg 
      , y = cover  
      , group = func.group.label 
      ), 
   # position = position_fill(vjust = .5), # Centers text in each stack
         position = position_fill(vjust = 0.5, reverse = TRUE) , 
    color = "blue",                        # Makes text readable on dark fills
     fill = NA
    ,size = species.text.fs
    , show.legend = FALSE
    , fontface = "italic"
    , stat = "unique"
    
 ) +
  facet_grid( ~ panel.label.area) +
  theme(
        legend.position  = 'none'
        
        
        , panel.grid.major = element_blank(),
        , panel.background = element_blank()
        , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
        , strip.text.x = element_text(size =   strip.text.fs , color = 'black' )
        ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
        
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank()
        , axis.title.x = element_blank()
      
        
        , plot.margin = margin(t = 5, r = 5, b = 25, l = 5, unit = "pt")
  ) + 
  ylab(bquote("Land cover (m"^2*" m"^-2*")")) +
  scale_fill_manual(values = c(  
    'Turf grass' = '#B8FFB8' 
    ,  "Medium\ngrass" = '#5CFF5C'
    ,  "Tall\ngrass" = '#00D100'
    ,  "Forb" = 'purple'
    )   )

gg.func.groups.area



gg.func.groups.biom <-  ggplot(
  d.func.gps
  
) + 
  geom_bar( 
    mapping = aes( x =   x.catg  , y = biom.frac, fill = func.group.label )
    ,      position = position_fill( reverse = TRUE)
    , stat = "identity"
    , width =   0.6
    
  ) +
  geom_text(  
    mapping =   aes(  
      label = ifelse(cover > .3 , as.character(func.group.label), ifelse(func.group.label == 'Turf grass'  , as.character(func.group.label), "")) 
      #label = func.group.label
      , x =  x.catg 
      , y = biom.frac  
      , group = func.group.label 
    ), 
    # position = position_fill(vjust = .5), # Centers text in each stack
    position = position_fill(vjust = 0.5, reverse = TRUE) , 
    color = "blue",                        # Makes text readable on dark fills
    fill = NA
    ,size = species.text.fs
    , show.legend = FALSE
    , fontface = "italic"
    , stat = "unique"
    
  ) +
  facet_grid( ~ panel.label.biom ) +
  theme(
    legend.position  = 'none'
    
    
    , panel.grid.major = element_blank(),
    , panel.background = element_blank()
    , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
    , strip.text.x = element_text(size =  9.75 , color = 'black' )
    ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
    
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
    , axis.title.x = element_blank()
    
    
    , plot.margin = margin(t = 5, r = 5, b = 25, l = 5, unit = "pt")
  ) + 
  ylab(bquote("Relative biomass (kg kg"^-1*")")) +
  scale_fill_manual(values = c(  
    'Turf grass' = '#B8FFB8' 
    ,  "Medium\ngrass" = '#5CFF5C'
    ,  "Tall\ngrass" = '#00D100'
    ,  "Forb" = 'purple'
  )   )

gg.func.groups.biom



# All quadrat average -- by LDNDC type
  gg.species.all <- ggplot(d.s.comp.all[!is.na(d.s.comp.all$gen_species) &  !is.na(d.s.comp.all$cover.pct.wgtd) & d.s.comp.all$quadrat != 'NA'   & d.s.comp.all$quadrat =="weighted" & !is.na(d.s.comp.all$ldndc.type), ]
                             ,  mapping = aes( x =  ldndc.type.label , y = cover.pct.wgtd , fill = gen_species) ) +
  geom_bar( 
    #,  mapping = aes( x =  quadrat.all , y = cover.pct.wgtd , fill = gen_species)
           , position="fill"
           , stat="identity"
           , width = 0.6
           
           ) +
  geom_text(  
  mapping =   aes(   label = ifelse(cover.pct.wgtd > .3 , as.character(gen_species), ifelse(ldndc.type.label == 'Turf grass'  , as.character(gen_species), "")) , x =  ldndc.type.label , y = cover.pct.wgtd), 
    position = position_fill(vjust = 0.5), # Centers text in each stack
    color = "blue",                        # Makes text readable on dark fills
   # fill = NA
  ,size =   species.text.fs
  , show.legend = FALSE
  , fontface = "italic"

  ) +
    facet_grid( ~ panel.label ) +
    theme(legend.position = 'none'
          
          
         , panel.grid.major = element_blank(),
          , panel.background = element_blank()
          , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
          , strip.text.x = element_text(size =  9.75 , color = 'black' )
          ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
          
         , axis.ticks.x = element_blank()
         
         , axis.title.x = element_blank()
         
         , axis.title.y = element_blank()
         , axis.ticks.y = element_blank()
         , axis.text.y = element_blank()
         
         , plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
          ) + 
    ylab('Land cover (m/m)')
  

gg.species.all






gg.figure.species <- ggarrange(
  
  
  gg.func.groups.area
  ,gg.species.all
  ,  gg.func.groups.biom
  
  , ncol= 3
  , labels = c('a' , 'b' , 'c')
  , font.label = list(size = 10.25, color = "black", face = "bold")
  , widths = c( 2.3 , 5  , 2.3)
  , label.x = c( 0.025 , -0.025 , 0.025)
  , label.y = 0.9775
)

gg.figure.species




ggsave(filename = 'Figures.out/gg.species.jpg' ,  gg.figure.species , width = 5.85, height = 2.85 , dpi = 1000  )





# ~ ~ ~ ~ - - - - - - - - - - - - - - - Grazing density computation 
e <- 2.71828 ; k.boma <- 0.05 ; k.borehole <- 0.325



dates <-  seq(from = as.Date("2018-07-25"), 
                to   = as.Date("2024-12-30"), 
                by   = "day")

time.period <- length( dates )

g.dat.0 <- data.frame( matrix( NA, nrow = time.period  , ncol = 1 )   )

g.dat.0$day <- dates



#' Column notation
#' grz.dns <- grazing density ; grz.hpd <- hours per day
col.grz.dns <- 'grz.dns' ;  col.grz.hpd <- 'hpd'  ; col.src <- 'source' ; col.type <- 'type'



g.dat.0[ , col.grz.dns ] <- NA
g.dat.0[ , col.grz.hpd  ] <- NA
g.dat.0[ , col.src ] <- NA
g.dat.0[ , col.type ] <- NA


# ~ ~ ~ Define grazing epochs

sources <- c('boma.1')


# ~ ~ - - BOMA 1
g.dat.B1 <- g.dat.0
g.pd.Boma.1.start <- as.Date("2018-07-25") ; g.pd.Boma.1.end <- as.Date("2018-12-30")

g.Boma.1.popn <- 90 ; b.Boma.1.distance.m <- 150



g.Boma.1.Dens <- g.Boma.1.popn * e^( -(1) * k * b.Boma.1.distance.m)



in.period <- ( g.dat.B1$day %in% seq(g.pd.Boma.1.start ,  g.pd.Boma.1.end  ) )
out.period <- !( g.dat.B1$day %in% seq(g.pd.Boma.1.start ,  g.pd.Boma.1.end  ) )


g.dat.B1[  ,  col.src ] <- sources[1]
g.dat.B1[  ,  col.type ] <- 'ind'

g.dat.B1[  in.period, col.grz.dns] <- 1.5
g.dat.B1[  in.period, col.grz.hpd] <- 8

g.dat.B1[  out.period  , col.grz.dns] <- 0
g.dat.B1[ out.period , col.grz.hpd] <- 0








g.dat.all <- rbind( g.dat.B1 )

# ~ ~ - - - - - - Summation
g.dat.sum <- g.dat.0

g.dat.sum[  ,  col.src ] <- 'all'
g.dat.sum[  ,  col.type ] <- 'sum'
g.dat.sum[  ,  col.grz.dns ] <- 0

for (  s in sources   ){
for (  d in g.dat.sum$day   ){
  
  # test: d <- g.dat.sum$day[200] ;  s <- sources[1]

g.dat.sum[ g.dat.sum$day == d & g.dat.sum$source == 'all'  ,  col.grz.dns ] <- ( g.dat.sum[ g.dat.sum$day == d & g.dat.sum$source == 'all'   ,  col.grz.dns ] + g.dat.all[  g.dat.all$day == d & g.dat.all$source == s , col.grz.dns  ] )


}}


g.dat.all <- rbind( g.dat.all , g.dat.sum )


# ~ ~ - - - - - -  PLOT  - - - - ~ ~

unique(g.dat.all$day)
unique(g.dat.all$grz.dns)
unique(g.dat.all$source)
unique(g.dat.all$type)



g.dat.all[g.dat.all$type == "ind" , 'type.label'] <- 'By source'
g.dat.all[g.dat.all$type == "sum" , 'type.label'] <- 'Total' 
  
  

g.dat.all[ g.dat.all$type == 'sum' & g.dat.all$day ==  "2018-07-27"       , 'grz.dns']


gg.grz.dns <- ggplot(   g.dat.all   ) +
             geom_line(   aes(  x =  day , y =  grz.dns , group = source)  ) + 
            facet_grid ( . ~ type.label) + 
 ylim(  limits = c(0, 10)  ) +
  ylab( bquote(Grazing~density~(TLU~~hd^-1~d^-1))) +
  xlab('Date') +  
  theme(
  
  
  , panel.grid.major = element_blank(),
  , panel.background = element_blank()
  , strip.background = element_rect(color='black', fill='white', size= 1, linetype="solid")
  , strip.text.x = element_text(size =  11 , color = 'black' )
  ,  panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
  
)
