

# PARAMETERS RELATING TO FUTURE SCENARIO ANALYSIS AND REPORTING AGAINST HISTORICAL



nr.fut.sum.start <- as.Date("2030-01-01")
nr.fut.sum.end <- as.Date("2035-01-01")
period.2.average <<- seq( from = nr.fut.sum.start , to = nr.fut.sum.end, by = 'day' )

hist.sim.period.start <- as.Date("2010-01-01")
hist.sim.period.end <- as.Date("2024-12-02")
hist.period <<- seq( from = hist.sim.period.start , to = hist.sim.period.end, by = 'day' )

num.simulation.months <<- interval(  nr.fut.sum.start,  nr.fut.sum.end  ) %/% days(1)
num.osv.months <<- interval(  min(d.all$date) ,   max(d.all$date)  ) %/% days(1)



symb.mn.sd <- '\u00b1'  ;  
gcm.shrt.rns.days <- 90


r.4 <<- 'rcp4' ;  r.8 <<- 'rcp8' 



series.all <<- c(   'TER.uni.kg.ha.d'
                    , 'GPP.uni.kg.ha.d'
                    , 'NEE.uni.kg.ha.d'
                    , 'BIOM.uni.kg.ha.d'
)

series.labels <<- c(  'Total ecosystem respiration (Reco)'
                      , 'Gross primary productivity (GPP)'
                      ,'Net ecosystem exchange (NEE)'
                      , 'Herbaceous above ground net primary productivity (HABNPP)'
)

series.labels.annual <<- c(  'Reco,\n annual total'
                             , 'GPP,\n annual total'
                             ,'NEE,\n annual total'
                             , 'HABNPP,\nannual total'
)



# Scenario categories
catgs <<- c( 'osv' , r.4 , r.8 )

catg.labels <<- c( '2018-2024'
                   , '2030-2040 R-4'
                   , '2030-2040 R-8'
)    

catg.near.fut <<- c(r.4 , r.8)


# Columns in output dataframe
sum.dat.cols <<- c(
  'catg'
  , 'season'
  , 'date'
  , 'TER.uni.kg.ha.d'
  , 'GPP.uni.kg.ha.d'
  , 'NEE.uni.kg.ha.d'
  ,  'BIOM.uni.kg.ha.d'
  
)




