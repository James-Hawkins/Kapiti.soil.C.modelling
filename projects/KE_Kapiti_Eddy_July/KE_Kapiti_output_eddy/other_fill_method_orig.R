



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


season.df[1,'xmin'] <- "2018-06-01"
season.df[1,'xmax'] <-  "2018-06-30"

season.df[1,'covid.climate'] <-  "Pre-covid"
season.df[1,'covid.gpp'] <-  unq.covid.gpp[1]

# July - October ; cool, dry
# Oct - Nov ; short rains
# Dec - March ; warm, dry
# April - June ; long rainy


# Drought: 

season.cutoffs <- c(
  
  season.df[1,'xmin'] 
  , season.df[1,'xmax']
  
  , "2018-10-01" # End long dry - start short rains
  #, "2018-11-30" # End short rains 
  
  , "2018-12-01" # End short rains - start long dry
  , "2019-03-30" # End rainy
  
 # , "2019-03-30" # End dry
  
  , "2019-04-01" # End dry
  , "2019-06-30" # End long rainy
  
  # Dipole ends
  
  , "2019-07-01" # End long rainy
  , "2019-10-30" # End long rainy
  
  , "2019-11-30" # End long rainy
  , "2019-12-01" # End long rainy
  
  , covid.start.date
  
  , covid.end.date
  
  ,  "2022-03-30"
  ,  "2022-04-01"
  ,  "2022-06-30"   # long rainy
  
  , "2022-07-01" # long dry
  , "2022-10-30" # long dry
  , "2022-11-01" # long dry
  

  , "2022-12-03"
  )

# drought : october 2020 - early 2023

ssn.fills <- c(
  
  rn.ssn.clr.dipole 
  ,dr.ssn.clr.dipole
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
  
)



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
     season.df[p,'covid.gpp'] <- unq.covid.gpp[1]
   
    
     } else if (current.cut.off == covid.start.date & current.cut.off < covid.end.date ){ 
     
        season.df[p,'covid.climate'] <- NA
        season.df[p,'covid.gpp'] <- unq.covid.gpp[2]
        
        
   
       } else if (current.cut.off >= covid.end.date){ 
      
         
         season.df[p,'covid.climate'] <- "Post-covid"
         season.df[p,'covid.gpp'] <- unq.covid.gpp[3]
    
      }
  
  
  
}  

season.df[length(season.cutoffs),'xmax'] <-  season.cutoffs[length(season.cutoffs)]


season.df$xmin <- as.Date(season.df$xmin, format = "%Y-%m-%d")
season.df$xmax <- as.Date(season.df$xmax, format = "%Y-%m-%d")


unq.covid.climate <- unique(d.all$covid.climate )


season.df$covid.climate  <- factor( season.df$covid.climate , levels = unq.covid.climate )



