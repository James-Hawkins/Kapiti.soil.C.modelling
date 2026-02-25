

# source('gg.seasons.R')

rn.ssn.clr.dipole <- 'turquoise'
rn.ssn.clr <- 'cyan'
rn.ssn.clr.drought <- 'lightblue'

dr.ssn.clr.dipole <-  'tan'
dr.ssn.clr <- 'brown'
dr.ssn.clr.drought <- 'red'


season.df <- data.frame(
  xmin = rep(NA,2) ,
  xmax = rep(NA,2),
  ymin = -Inf,
  ymax = Inf,
  fill = c(rn.ssn.clr  ,  dr.ssn.clr)
, covid.climate = rep(NA,2)
)


season.df[1,'xmin'] <-   start.date.cald
season.df[1,'xmax'] <-  "2018-10-01"

season.df[1,'covid.climate'] <-  "Pre-covid"


season.df[1,'covid.gpp'] <-  unq.covid.gpp[1]
season.df[1,'covid.ter'] <-  unq.covid.ter[1]

# July - October ; cool, dry (Long dry)
# Oct - Nov ; short rains
# Dec - March ; warm, dry (Short dry)
# April - June ; long rains


# drought : october 2020 - early 2023

season.cutoffs <<- c(
  
  season.df[1,'xmin'] 
  
  # Long dry
  
  , season.df[1,'xmax']
  
  # Short rains
  
  , "2018-11-30" 
  
  # Short dry
  
  , "2019-03-30" 
  
  # Long rains
  
  , "2019-06-30" # End long rainy
  

  , "2019-10-01" # End long rainy
  
  , "2019-11-30" # End long rainy
  
  # Dipole ends
  
  
  , covid.start.date
  
  , covid.end.date
  
  ,  "2022-03-30"
  
  # Short dry
  
  ,  "2022-06-30"   
  
  # Long rains
  
  , "2022-09-30" 
  
  # Long dry
  
  , "2022-11-30" 
  
  # Short dry

  , "2023-03-30"
  
  # Long rains

  , "2023-06-30"
  
  
  , "2023-09-30"
  
  
  
  , "2023-11-30"
  
  
  , "2024-03-30"
  
  , "2024-07-01"
  
  , "2024-10-01"
  
 , end.date.cald
 
  )



ssn.fills <- c(
  
  dr.ssn.clr.dipole
  , rn.ssn.clr.dipole
  ,dr.ssn.clr.dipole
  , rn.ssn.clr.dipole 
  ,dr.ssn.clr.dipole
  , rn.ssn.clr.dipole
  , dr.ssn.clr.dipole
  
  # Dipole ends
  
  , dr.ssn.clr.drought
  , rn.ssn.clr.drought
  , dr.ssn.clr.drought
  , rn.ssn.clr.drought
  , dr.ssn.clr.drought
  , rn.ssn.clr.drought
  , dr.ssn.clr.drought
  
  , rn.ssn.clr.drought
  , dr.ssn.clr.drought
  , rn.ssn.clr.drought
  , dr.ssn.clr.drought
  , rn.ssn.clr
  , dr.ssn.clr
  , rn.ssn.clr
  , dr.ssn.clr
  , rn.ssn.clr
)

season.df[1,'fill'] <- ssn.fills[1]

for (p in 2 : length(season.cutoffs)){
  
  current.cut.off <- season.cutoffs[p]
  next.cut.off <- season.cutoffs[p+1]
  
  season.df[p,'xmin'] <- current.cut.off 
  season.df[p,'xmax'] <-  next.cut.off
  
  season.df[p,'ymax'] <- Inf
  season.df[p,'ymin'] <- -Inf
  
  season.df[p,'fill'] <- ssn.fills[p]
  

  if (current.cut.off < covid.start.date){ 
    
   
     season.df[p,'covid.climate'] <- "Pre-covid"
     
     season.df[p,'covid.ter'] <- unq.covid.ter[1]
     season.df[p,'covid.gpp'] <- unq.covid.gpp[1]
   
    
     } else if (current.cut.off == covid.start.date & current.cut.off < covid.end.date ){ 
     
        season.df[p,'covid.climate'] <- NA
        
        season.df[p,'covid.ter'] <- unq.covid.ter[2]
        season.df[p,'covid.gpp'] <- unq.covid.gpp[2]
        
        
   
       } else if (current.cut.off >= covid.end.date){ 
      
         
         
         season.df[p,'covid.climate'] <- "Post-covid"
         
         season.df[p,'covid.ter'] <- unq.covid.ter[3]
         season.df[p,'covid.gpp'] <- unq.covid.gpp[3]
    
      }
  
  
  
}  

season.df[length(season.cutoffs),'xmax'] <-  season.cutoffs[length(season.cutoffs)]


season.df$xmin <- as.Date(season.df$xmin, format = "%Y-%m-%d")
season.df$xmax <- as.Date(season.df$xmax, format = "%Y-%m-%d")


unq.covid.climate <- unique(d.all$covid.climate )


season.df$covid.climate  <- factor( season.df$covid.climate , levels = unq.covid.climate )


season.df <<- season.df


