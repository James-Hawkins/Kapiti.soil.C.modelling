





{
#  - - RCP 4.5 
# GCM 1
mod.out.r4.gcm.1.soil <- get.LDNDC.out.dat(  r.4, 1 , 'soil')
mod.out.r4.gcm.1.water <- get.LDNDC.out.dat(  r.4, 1 , 'water')
mod.out.r4.gcm.1.physio <- get.LDNDC.out.dat(  r.4, 1 , 'physio')
mod.out.r4.gcm.1.all <- cbind( mod.out.r4.gcm.1.soil , mod.out.r4.gcm.1.water , mod.out.r4.gcm.1.physio )



# GCM 2
mod.out.r4.gcm.2.soil <- get.LDNDC.out.dat(  r.4, 2 , 'soil')
mod.out.r4.gcm.2.water <- get.LDNDC.out.dat(  r.4, 2 , 'water')
mod.out.r4.gcm.2.physio <- get.LDNDC.out.dat(  r.4, 2 , 'physio')
mod.out.r4.gcm.2.all <- cbind( mod.out.r4.gcm.2.soil , mod.out.r4.gcm.2.water , mod.out.r4.gcm.2.physio)


# GCM 3
mod.out.r4.gcm.3.soil <- get.LDNDC.out.dat(  r.4, 3 , 'soil')
mod.out.r4.gcm.3.water <- get.LDNDC.out.dat(  r.4, 3 , 'water')
mod.out.r4.gcm.3.physio <- get.LDNDC.out.dat(  r.4, 3 , 'physio')
mod.out.r4.gcm.3.all <- cbind( mod.out.r4.gcm.3.soil , mod.out.r4.gcm.3.water , mod.out.r4.gcm.3.physio)


# GCM 4
mod.out.r4.gcm.4.soil <- get.LDNDC.out.dat(  r.4, 4 , 'soil')
mod.out.r4.gcm.4.water <- get.LDNDC.out.dat(  r.4, 4 , 'water')
mod.out.r4.gcm.4.physio <- get.LDNDC.out.dat(  r.4, 4 , 'physio')
mod.out.r4.gcm.4.all <- cbind( mod.out.r4.gcm.4.soil , mod.out.r4.gcm.4.water , mod.out.r4.gcm.4.physio)


# GCM 5
mod.out.r4.gcm.5.soil <- get.LDNDC.out.dat(  r.4, 5 , 'soil')
mod.out.r4.gcm.5.water <- get.LDNDC.out.dat(  r.4, 5 , 'water')
mod.out.r4.gcm.5.physio <- get.LDNDC.out.dat(  r.4, 5 , 'physio')
mod.out.r4.gcm.5.all <- cbind( mod.out.r4.gcm.5.soil , mod.out.r4.gcm.5.water , mod.out.r4.gcm.5.physio)


# GCM 6
mod.out.r4.gcm.6.soil <- get.LDNDC.out.dat(  r.4, 6 , 'soil')
mod.out.r4.gcm.6.water <- get.LDNDC.out.dat(  r.4, 6 , 'water')
mod.out.r4.gcm.6.physio <- get.LDNDC.out.dat(  r.4, 6 , 'physio')
mod.out.r4.gcm.6.all <- cbind( mod.out.r4.gcm.6.soil , mod.out.r4.gcm.6.water , mod.out.r4.gcm.6.physio)

# GCM 7
mod.out.r4.gcm.7.soil <- get.LDNDC.out.dat(  r.4, 7 , 'soil')
mod.out.r4.gcm.7.water <- get.LDNDC.out.dat(  r.4, 7 , 'water')
mod.out.r4.gcm.7.physio <- get.LDNDC.out.dat(  r.4, 7 , 'physio')
mod.out.r4.gcm.7.all <- cbind( mod.out.r4.gcm.7.soil , mod.out.r4.gcm.7.water , mod.out.r4.gcm.7.physio)


# - - RCP 8.5 
# GCM 1
mod.out.r8.gcm.1.soil <- get.LDNDC.out.dat(  r.8, 1 , 'soil')
mod.out.r8.gcm.1.water <- get.LDNDC.out.dat(  r.8, 1 , 'water')
mod.out.r8.gcm.1.physio <- get.LDNDC.out.dat(  r.8, 1 , 'physio')
mod.out.r8.gcm.1.all <- cbind( mod.out.r8.gcm.1.soil , mod.out.r8.gcm.1.water , mod.out.r8.gcm.1.physio)



#mod.out.r8.gcm.1.physio$co2.upt / mod.out.r4.gcm.1.physio$co2.upt 
#mod.out.r8.gcm.2.physio$co2.upt / mod.out.r4.gcm.2.physio$co2.upt 

  
# GCM 2
mod.out.r8.gcm.2.soil <- get.LDNDC.out.dat(  r.8, 2 , 'soil')
mod.out.r8.gcm.2.water <- get.LDNDC.out.dat(  r.8, 2 , 'water')
mod.out.r8.gcm.2.physio <- get.LDNDC.out.dat(  r.8, 2 , 'physio')
mod.out.r8.gcm.2.all <- cbind( mod.out.r8.gcm.2.soil , mod.out.r8.gcm.2.water , mod.out.r8.gcm.2.physio)


# GCM 3
mod.out.r8.gcm.3.soil <- get.LDNDC.out.dat(  r.8, 3 , 'soil')
mod.out.r8.gcm.3.water <- get.LDNDC.out.dat(  r.8, 3 , 'water')
mod.out.r8.gcm.3.physio <- get.LDNDC.out.dat(  r.8, 3 , 'physio')
mod.out.r8.gcm.3.all <- cbind( mod.out.r8.gcm.3.soil , mod.out.r8.gcm.3.water , mod.out.r8.gcm.3.physio)


# GCM 4
mod.out.r8.gcm.4.soil <- get.LDNDC.out.dat(  r.8, 4 , 'soil')
mod.out.r8.gcm.4.water <- get.LDNDC.out.dat(  r.8, 4 , 'water')
mod.out.r8.gcm.4.physio <- get.LDNDC.out.dat(  r.8, 4 , 'physio')
mod.out.r8.gcm.4.all <- cbind( mod.out.r8.gcm.4.soil , mod.out.r8.gcm.4.water , mod.out.r8.gcm.4.physio)


# GCM 5
mod.out.r8.gcm.5.soil <- get.LDNDC.out.dat(  r.8, 5 , 'soil')
mod.out.r8.gcm.5.water <- get.LDNDC.out.dat(  r.8, 5 , 'water')
mod.out.r8.gcm.5.physio <- get.LDNDC.out.dat(  r.8, 5 , 'physio')
mod.out.r8.gcm.5.all <- cbind( mod.out.r8.gcm.5.soil , mod.out.r8.gcm.5.water , mod.out.r8.gcm.5.physio)


# GCM 6
mod.out.r8.gcm.6.soil <- get.LDNDC.out.dat(  r.8, 6 , 'soil')
mod.out.r8.gcm.6.water <- get.LDNDC.out.dat(  r.8, 6 , 'water')
mod.out.r8.gcm.6.physio <- get.LDNDC.out.dat(  r.8, 6 , 'physio')
mod.out.r8.gcm.6.all <- cbind( mod.out.r8.gcm.6.soil , mod.out.r8.gcm.6.water , mod.out.r8.gcm.6.physio)


# GCM 7
mod.out.r8.gcm.7.soil <- get.LDNDC.out.dat(  r.8, 7 , 'soil')
mod.out.r8.gcm.7.water <- get.LDNDC.out.dat(  r.8, 7 , 'water')
mod.out.r8.gcm.7.physio <- get.LDNDC.out.dat(  r.8, 7 , 'physio')
mod.out.r8.gcm.7.all <- cbind( mod.out.r8.gcm.7.soil , mod.out.r8.gcm.7.water , mod.out.r8.gcm.7.physio)


mod.out.r4.gcm.1.all$gcm <- 1
mod.out.r4.gcm.2.all$gcm <- 2
mod.out.r4.gcm.3.all$gcm <- 3
mod.out.r4.gcm.4.all$gcm <- 4
mod.out.r4.gcm.5.all$gcm <- 5
mod.out.r4.gcm.6.all$gcm <- 6
mod.out.r4.gcm.7.all$gcm <- 7

mod.out.r8.gcm.1.all$gcm <- 1
mod.out.r8.gcm.2.all$gcm <- 2
mod.out.r8.gcm.3.all$gcm <- 3
mod.out.r8.gcm.4.all$gcm <- 4
mod.out.r8.gcm.5.all$gcm <- 5
mod.out.r8.gcm.6.all$gcm <- 6
mod.out.r8.gcm.7.all$gcm <- 7

# RCP
mod.out.r4.gcm.1.all$rcp <- r.4
mod.out.r4.gcm.2.all$rcp <- r.4
mod.out.r4.gcm.3.all$rcp <- r.4
mod.out.r4.gcm.4.all$rcp <- r.4
mod.out.r4.gcm.5.all$rcp <- r.4
mod.out.r4.gcm.6.all$rcp <- r.4
mod.out.r4.gcm.7.all$rcp <- r.4

mod.out.r8.gcm.1.all$rcp <- r.8
mod.out.r8.gcm.2.all$rcp <- r.8
mod.out.r8.gcm.3.all$rcp <- r.8
mod.out.r8.gcm.4.all$rcp <- r.8
mod.out.r8.gcm.5.all$rcp <- r.8
mod.out.r8.gcm.6.all$rcp <- r.8
mod.out.r8.gcm.7.all$rcp <- r.8


mod.out.fc <- rbind(
  
  mod.out.r4.gcm.1.all
  , mod.out.r4.gcm.2.all
  , mod.out.r4.gcm.3.all
  , mod.out.r4.gcm.4.all
  , mod.out.r4.gcm.5.all
  , mod.out.r4.gcm.6.all
  , mod.out.r4.gcm.7.all
  
  , mod.out.r8.gcm.1.all
  , mod.out.r8.gcm.2.all
  , mod.out.r8.gcm.3.all
  , mod.out.r8.gcm.4.all
  , mod.out.r8.gcm.5.all
  , mod.out.r8.gcm.6.all
  , mod.out.r8.gcm.7.all
)


} # RETRIEVE/STORE LDNDC OUT DATA, by RCP and GCM

# REPORTING METRICS
View(  mod.out.fc  )


{
  
cols.2.average <- c( 'GPP.sim' , 'TER.sim' , 'NEE.mod' , 'ag.biom.kg.m2')
  


mod.out.fc$date <- as.Date( mod.out.fc$datetime ,  format="%Y-%m-%d")

nr.fut.sum.start <- as.Date("2030-01-01")
nr.fut.sum.end <- as.Date("2030-12-31")

period.2.average <- seq( from = nr.fut.sum.start , to = nr.fut.sum.end, by = 'day' )


num.simulation.months <<- interval(  nr.fut.sum.start,  nr.fut.sum.end  ) %/% days(1)
num.osv.months <<- interval(  min(d.all$date) ,   max(d.all$date)  ) %/% days(1)

# GPP
mod.out.fc$GPP.sim <- cv.sq.m.2.ha * ( mod.out.fc$co2.upt )


# Reco
mod.out.fc$TER.sim <- cv.sq.m.2.ha *  ( mod.out.fc$maint.resp +  mod.out.fc$transp.resp +  mod.out.fc$growth.resp) +  mod.out.fc$emis.hetero
  
  



# NET ECOSYSTEM EXCHANGE
mod.out.fc$NEE.mod <- (1) * mod.out.fc$TER.sim - mod.out.fc$GPP.sim






# HERBACEOUS BIOMASS
mod.out.fc$ag.biom.grass.Mg.ha <- mod.out.fc$ag.biom.kg.m2  * cv.sq.m.2.ha / 1000
  



mod.out.fc <- define.seasons(mod.out.fc , 'date')


names(mod.out.fc) <- make.unique(  names(  mod.out.fc  ) )

} # INDICATOR COMPUTATION


{

# ASSIGN AVERAGE VALUES UNDER 'GCM 0' FOR MAIN REPORTING VARIABLES
mod.out.fc <- gen.mean.gcm.dat( mod.out.fc , r.4 , cols.2.average , period.2.average)
mod.out.fc <- gen.mean.gcm.dat( mod.out.fc , r.8 , cols.2.average , period.2.average)



# REDEFINE COLUMN NAMES TO MATCH HARMOMIZED FORMAT
mod.out.fc[mod.out.fc$rcp == r.4 , 'catg'] <- r.4
mod.out.fc[mod.out.fc$rcp == r.8 , 'catg'] <- r.8

mod.out.fc$TER.uni.kg.ha.d <- mod.out.fc$TER.sim
mod.out.fc$GPP.uni.kg.ha.d <- mod.out.fc$GPP.sim
mod.out.fc$NEE.uni.kg.ha.d <- mod.out.fc$NEE.mod 
mod.out.fc$BIOM.uni.kg.ha.d <- mod.out.fc$ag.biom.grass.Mg.ha 


mod.out.fc$cald.mth




# MERGE WITH OBSERVED DATA
d.all$catg <- 'osv'

d.all$TER.uni.kg.ha.d <- d.all$reco.osv.kg.ha
d.all$GPP.uni.kg.ha.d <- d.all$gpp.osv.kg.ha
d.all$NEE.uni.kg.ha.d <- d.all$NEE.obs.kg.ha

d.all$BIOM.uni.kg.ha.d <- d.all$osv.biom.Mg.ha.ALL  

series <<- c(   'TER.uni.kg.ha.d'
              , 'GPP.uni.kg.ha.d'
              , 'NEE.uni.kg.ha.d'
              , 'BIOM.uni.kg.ha.d'
              )

series.labels <- c(  'Reco'
                     , 'Gross primary productivity'
                     ,'Net ecosystem exchange'
                     , 'Biomass'
)
              

catgs <<- c( 'osv' , r.4 , r.8 )

catg.labels <<- c( '2018-2024'
                  , 'Near future R-4.5'
                  , 'Near future R-8.5'
                  )    
                     
                     

sum.dat.cols <- c(
   'catg'
  , 'season'
  , 'date'
  , 'TER.uni.kg.ha.d'
  , 'GPP.uni.kg.ha.d'
  , 'NEE.uni.kg.ha.d'
  ,  'BIOM.uni.kg.ha.d'
  
)


unique(mod.out.fc$season)

d.all <- define.seasons( d.all  , 'date' ) 
mod.out.fc <- define.seasons( mod.out.fc  , 'date' ) 


mod.out.fc.2 <- mod.out.fc[ mod.out.fc$gcm == 0 ,sum.dat.cols]


mod.out.fc[ mod.out.fc$gcm == 0, sum.dat.cols[7] ]

View(mod.out.fc.2 )

d.sum.all <- rbind(
  
  mod.out.fc[ mod.out.fc$gcm == 0, sum.dat.cols ]
  
  , d.all[, sum.dat.cols ]
  
)



# mod.out.fc.2 <- mod.out.fc[ !is.na(mod.out.fc$datetime)  , ]

#View(d.sum.all)



d.sum.all$facet_label.TER <- 'Reco'
d.sum.all$facet_label.GPP <- 'Gross primary productivity'
d.sum.all$facet_label.NEE <- 'Net ecosystem exchange'



d.sum.all$season.label <- NA
d.sum.all[d.sum.all$season == ssn.SD & !is.na( d.sum.all$season) , 'season.label'] <- ssn.label.SD
d.sum.all[d.sum.all$season == ssn.LR  & !is.na( d.sum.all$season), 'season.label'] <- ssn.label.LR
d.sum.all[d.sum.all$season == ssn.LD & !is.na( d.sum.all$season), 'season.label'] <- ssn.label.LD
d.sum.all[d.sum.all$season == ssn.SR & !is.na( d.sum.all$season), 'season.label'] <- ssn.label.SR

d.sum.all$season.label <- factor( d.sum.all$season.label , c( ssn.labels.ordered)    )

d.sum.all <- d.sum.all[  !is.na(d.sum.all$season), ]

d.sum.all$year.count <- NA

d.sum.all[ d.sum.all$catg == r.4 | d.sum.all$catg == r.8 ,  'year.count'] <-   num.simulation.months /365
d.sum.all[ d.sum.all$catg == "osv" ,  'year.count'] <-  num.osv.months/365


ann.dat <- get.annual.mean.sd(  series  , series.labels )


d.sum.all[  d.sum.all$catg == catgs[2]   , 'BIOM.uni.kg.ha.d' ]

} # DATA FORMAT HARMONIZATION



# Plot out


# TER
sum.ter.plot.ssn <- gen.sum.gg.theme  %>% +
  stat_summary( d.sum.all[  !is.na(d.sum.all$TER.uni.kg.ha.d)  , ], 
                mapping = aes(x = catg , y = TER.uni.kg.ha.d  )
                , 
                fun = "mean", geom = "bar", fill = "skyblue") +
  stat_summary(
    d.sum.all , 
    mapping = aes(x = catg , y = TER.uni.kg.ha.d ) ,
    fun.data = "mean_sdl", 
    fun.args = list(mult = 1), 
    geom = "errorbar", 
    width = 0.2
  ) + 
ggh4x::facet_nested( 
.  ~  facet_label.TER + season.label 
  , strip = strip_nested(size = "variable")
  , render_empty = FALSE
)    

# sum.ter.plot.ssn




# GPP
sum.gpp.plot <- gen.sum.gg.theme  %>% +
  stat_summary( d.sum.all, 
                mapping = aes(x = catg , y = GPP.uni.kg.ha.d  )
                , 
                fun = "mean", geom = "bar", fill = "skyblue") +
  stat_summary(
    d.sum.all , 
    mapping = aes(x = catg , y = GPP.uni.kg.ha.d ) ,
    fun.data = "mean_sdl", 
    fun.args = list(mult = 1), 
    geom = "errorbar", 
    width = 0.2
  ) + 
  ggh4x::facet_nested( 
    .  ~  facet_label.GPP + season.label 
    , strip = strip_nested(size = "variable")
    , render_empty = FALSE
  )    


# NEE
sum.nee.plot <- gen.sum.gg.theme  %>% +
  stat_summary( d.sum.all, 
                mapping = aes(x = catg , y = NEE.uni.kg.ha.d  )
                , 
                fun = "mean", geom = "bar", fill = "skyblue") +
  stat_summary(
    d.sum.all , 
    mapping = aes(x = catg , y = NEE.uni.kg.ha.d ) ,
    fun.data = "mean_sdl", 
    fun.args = list(mult = 1), 
    geom = "errorbar", 
    width = 0.2
  ) + 
  ggh4x::facet_nested( 
    .  ~  facet_label.NEE + season.label 
    , strip = strip_nested(size = "variable")
    , render_empty = FALSE
  )    


# BIOMASS
sum.ter.plot.ssn <- gen.sum.gg.theme  %>% +
  stat_summary( d.sum.all[  !is.na(d.sum.all$BIOM.uni.kg.ha.d)  , ], 
                mapping = aes(x = catg , y = BIOM.uni.kg.ha.d  )
                , 
                fun = "mean", geom = "bar", fill = "skyblue") +
  stat_summary(
    d.sum.all[  !is.na(d.sum.all$BIOM.uni.kg.ha.d)  , ],  
    mapping = aes(x = catg , y = BIOM.uni.kg.ha.d) ,
    fun.data = "mean_sdl", 
    fun.args = list(mult = 1), 
    geom = "errorbar", 
    width = 0.2
  ) + 
  ggh4x::facet_nested( 
    .  ~  facet_label.TER + season.label 
    , strip = strip_nested(size = "variable")
    , render_empty = FALSE
  )    


# ANNUALIZED


sum.ter.plot.annl <- gen.annual.sum.plot(  series[1] )

sum.gpp.plot.annl <- gen.annual.sum.plot(  series[2] )

sum.nee.plot.annl <- gen.annual.sum.plot(  series[3] )





mod.out.fc$season


# Summary statistics





summary(  mod.out.fc[ mod.out.fc$rcp == r.4 & mod.out.fc$gcm == 1,  'GPP.sim'  ] )


sum( d.sum.all[ d.sum.all$rcp == r.4 & d.sum.all$gcm == 0,  'GPP.sim'  ]  / sum( d.sum.all[ d.sum.all$rcp == r.4 ,  'year.count'  ][1] ) )

d.sum.all$year.count
