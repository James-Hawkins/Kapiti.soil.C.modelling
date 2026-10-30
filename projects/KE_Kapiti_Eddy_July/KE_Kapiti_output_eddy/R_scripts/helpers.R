

# source( str_c(R.dir,'helpers.R'))

`%+=%` <<- function(e1,e2){ eval.parent(substitute(e1 <- e1 + e2))}


try.rnorm.sample <<- function( draws , mean, sd ){
  
  # test: draws <- 4 ; mean <- 10 ; sd <- 4
  
  res <-  attempt( lkddkjdl , silent = TRUE )
  
  while (  is_try_error(res)      ){
    
    res <- attempt ( res.list <- abs( rnorm(n = draws, mean =    mean, sd = sd ) ) )
    
  } 
  
  return (   res.list   )
}



try.runif.sample <<- function( draws , min, max ){
  
  # test: draws <- 4 ; min <- 1 ; max <- 4
  
  res <-  attempt( lkddkjdl , silent = TRUE )
  
  while (  is_try_error(res)      ){
    
    res <- attempt ( res.list <- abs( runif( draws, min =    min, max = max ) ) )
    
  } 
  
  return (   res.list   )
}



try.range.sample <<- function( seq , quant, replace ){
  
  # test: seq <- seq(1:10) ; quant <- 3 ; replace <- FALSE
  
  res <-  attempt( lkddkjdl , silent = TRUE )
  
  while (  is_try_error(res)      ){
    
    res <- attempt ( res.list <- sample( seq, quant , replace = replace) )
                
  } 
  
  return (   res.list   )
}


gen.valid.plot <<- function( 
    y.var.osv 
    , y.var.sim 
    , y.var.sim.bc 
    , y.lab 
    , global.valid.y.cord.high 
    , global.valid.y.cord.mid 
    , global.valid.y.cord.bottm 
    , type
    , include.raw.sim
    , include.BCd.sim
    ){
  
  test <- function(){
    
    y.var.osv <- 'r.a.et.osv'
    y.var.sim <- 'r.a.et.sim'
    y.var.sim.bc <- 'r.a.et.sim.bc'  
    y.lab <- gg.valid.ter.y.ax.lab 
    global.valid.y.cord.high <- global.valid.ter.y.cord.high
    global.valid.y.cord.mid <- global.valid.ter.y.cord.mid
    global.valid.y.cord.bottm <- global.valid.ter.y.cord.bottm
    
    
    include.BCd.sim <<- TRUE
    include.raw.sim <<- TRUE
    
    type <- 'no.label' 
    
  }
  

  
  d.loc <- d.all
  d.loc[,'plot.y.var.osv'] <- d.loc[,y.var.osv]  
  d.loc[,'plot.y.var.sim'] <- d.loc[,y.var.sim] 
  d.loc[,'plot.y.var.sim.bc'] <- d.loc[,y.var.sim.bc] 
  
  d.obs.pre.covid <- d.loc[ d.all.plot.conditions & d.loc$covid %in% covid.status[c(1)] , ]
  d.obs.post.covid <- d.loc[ d.all.plot.conditions & d.loc$covid %in% covid.status[c(2)] , ]
  
  if ( y.var.osv == 'r.a.ter.osv' | y.var.osv == 'r.a.gpp.osv'  | y.var.osv == 'r.a.nee.osv'){   lab.y.crd.dipole <- -2.5 ; lab.y.crd.drought <- -2.5}
 # if ( y.var.osv == 'r.a.gpp.osv'){   lab.y.crd.dipole <- 2.5 ; lab.y.crd.drought <- 2.5}
  if ( y.var.osv == 'r.a.swc.5.cm.osv'){   lab.y.crd.dipole <- -2.5 ; lab.y.crd.drought <- -2.5}
  if ( y.var.osv == 'r.a.swc.15.cm.osv'){   lab.y.crd.dipole <- -2.5 ; lab.y.crd.drought <- -2.5}
  if ( y.var.osv == 'r.a.swc.30.cm.osv'){   lab.y.crd.dipole <- -2.5 ; lab.y.crd.drought <- -2.5}
  if ( y.var.osv == 'r.a.et.osv'){   lab.y.crd.dipole <- -2.5 ; lab.y.crd.drought <- -2.5}
  

  
  min.y <- min(  na.omit( d.loc[,  y.var.osv] )  , na.omit( d.loc[,y.var.sim ]) )
  max.y <-  max( na.omit(d.loc[,  y.var.osv])  ,  na.omit( d.loc[,y.var.sim ]) ) * 1.1
  
  gg.valid.no.labl <- gg.theme  %>%   +   #ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha )  & d.all$covid %in% covid.status[c(1,2)] ,  ] ,   aes(x = date.time ) ) +  
    
    
    geom_line( 
      data = d.obs.pre.covid
      , aes(x = date, y =   plot.y.var.osv , color=   gg.valid.labels[2] ) 
      , linewidth = p.ln.width  
    ) +
    
    geom_line( 
      data = d.obs.post.covid 
      , aes(x = date, y =  plot.y.var.osv , color= gg.valid.labels[2]  ) 
      , linewidth = p.ln.width  
    ) +
    

    
   # geom_line(  data = d.loc[ d.all.plot.conditions & d.loc$covid %in% covid.status[c(1,2,3)] , ]
              #  ,aes(x = date, y = plot.y.var.sim.bc , color = 'bias.corrected'  ) 
                # , linewidth = p.ln.width 
                #, size = gg.valid.sim.point.size
 #   ) +   
    
   # geom_segment(aes(x = as.Date(dipole.period.start)  , xend = as.Date(dipole.period.end) , y = lab.y.crd.dipole ) , linetype= 'dashed') +
  #  geom_segment(aes(x = as.Date(drought.period.start)  , xend = as.Date(drought.period.end) , y = lab.y.crd.drought ) , linetype= 'dashed' ) +
    
    
    # scale_x_date(limits = c(as.Date(start.date.cald) , as.Date(end.date.cald)),
    # date_labels = "%m %Y", # Format the labels as "Mon YYYY"
    # date_breaks = "3 months"
    #, expand=c(0.00025,0.00025)
    #) +
    #scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d" ) +
   # facet_wrap(~ covid.ter ,  scales = 'free_x' , as.table = FALSE) +
     #facet_grid( ~ covid.ter  , scales = 'free_x' , space = 'free_x') +
    theme(
      plot.margin = margin( 
        
        p.mrgn.main.top
        , p.mrgn.main.right
        ,  p.mrgn.main.bottom 
        , p.mrgn.main.left
        
        , "cm"  ) , 
      legend.position = "none" , #c(gg.valid.leg.x.crd , gg.valid.leg.y.crd ) ,
      legend.title = element_blank(),
      axis.title.x = element_blank() , 
         axis.ticks.x = element_blank() , 
      #  legend.title = element_blank() ,
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
      , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
      , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
    ) + 
    ylab( y.lab ) +
    theme(
    plot.margin = margin(
      t = 0
      , r = 0* 28
      , b = 0
      , l = 3.5
      , unit = "pt")
    ) +
    coord_cartesian(ylim = c( min.y , max.y )) 
  
  
  # theme(aspect.ratio = 1/3)

  
  if ( include.BCd.sim ){
    
    gg.valid.no.labl <-  gg.valid.no.labl  %>%   +  
    geom_line(  data = d.loc[ d.all.plot.conditions & d.loc$covid %in% covid.status[c(1,2,3)] , ]
                 ,aes(x = date, y = plot.y.var.sim.bc , color= 'bias.corrected' ) 
                 
                , linewidth = p.ln.width 
                , linetype = 'solid'
    ) 
    
  }
  
  if ( include.raw.sim ){ 
    
    gg.valid.no.labl <-  gg.valid.no.labl  %>%   +  
      geom_line(  data = d.loc[ d.all.plot.conditions & d.loc$covid %in% covid.status[c(1,2,3)] , ]
                  ,aes(x = date, y = plot.y.var.sim , color =      "L-DNDC") 
                  
                  , linewidth = p.ln.width 
                  , linetype = 'dotted'
      ) 
    
  }
  

  
  gg.valid.labl <- gg.valid.no.labl %>%  + geom_label(
    #data = d.all[ d.all$covid == "Post-covid"  , ],
    mapping = aes(x =  as.Date( global.valid.sum.date )   , y = global.valid.y.cord.high , label = metrics[metrics$osv.variable == y.var.osv & metrics$period == period.dipole , 'valid.text']  ),
    fill = global.valid.text.background
    , color = global.valid.text.color
    , label.size = NA
    , size = gg.valid.label.fs 
    , hjust = gg.valid.labels.h.just
  ) +
    geom_label(
      data = d.loc[1  , ],
      mapping = aes(x =  as.Date( global.valid.sum.date )   , y = global.valid.y.cord.mid , label = metrics[metrics$osv.variable == y.var.osv & metrics$period == period.drought , 'valid.text']  ),
      fill = global.valid.text.background
      , color = global.valid.text.color
      , label.size = NA
      , size = gg.valid.label.fs 
      , hjust = gg.valid.labels.h.just
    ) +
    geom_label(
      data = d.loc[1  , ],
      mapping = aes(x =  as.Date( global.valid.sum.date )   , y = global.valid.y.cord.bottm, label = metrics[metrics$osv.variable == y.var.osv  & metrics$period == period.normal , 'valid.text']  ),
      fill = global.valid.text.background
      , color = global.valid.text.color
      , label.size = NA
      , size = gg.valid.label.fs 
      , hjust = gg.valid.labels.h.just
    ) +
   geom_label(
  mapping = aes(x =  as.Date( global.valid.covid.label.date )   , y = 10 , label = gg.valid.label.covid.period  ),
   fill = global.valid.text.background
  , color = global.valid.text.color
   , label.size = NA
   , size = gg.valid.label.fs 
  , hjust = .5
    )   +
    geom_label(
      mapping = aes(x =  as.Date(   dipole.period.mid )   , y = lab.y.crd.dipole , label = "Pluvial" ),
      fill = global.valid.text.background
      , color = global.valid.text.color
      , label.size = NA
      , size = gg.valid.label.fs 
      , hjust = .5
    ) +
    geom_label(
      mapping = aes(x =  as.Date(   drought.period.mid )   , y = lab.y.crd.drought , label = "Drought" ),
      fill = global.valid.text.background
      , color = global.valid.text.color
      , label.size = NA
      , size = gg.valid.label.fs 
      , hjust = .5
    ) 

  
  if (type == 'label') { return (gg.valid.labl )} else 
  if (type == 'no.label') { return (gg.valid.no.labl)}
}  




gen.gg.kaba <<- function(series){
  
  # test: series <- 'ter'
  
  if (series == 'ter') { bias.cond <- bias.cond.ter ; y.lab <- gg.kobasal.ter.y.ax.lab}
  if (series == 'gpp') { bias.cond <- bias.cond.gpp ; y.lab <- gg.kobasal.gpp.y.ax.lab}
  if (series == 'nee') { bias.cond <- bias.cond.swc ; y.lab <- gg.kobasal.nee.y.ax.lab}
  if (series == 'swc') { bias.cond <- bias.cond.swc ; y.lab <- 'Absolute error'}
  
  biases.long$error.type.label<- factor( biases.long$error.type.label , levels = c('MB' , 'SDSD' , 'LCS'))
  
  
 plot <- gg.kosalam.gen %>%   + 
    geom_bar( 
      biases.long[  bias.cond , ]
      , mapping = aes( x = error.type.label, y = error^0.5)
      , position="dodge"
      , stat="identity"
      , fill = '#A9A9A9'
      , width = 0.47475
    ) +
    facet_grid( period.label ~ . , switch = "y") +
    scale_y_continuous(position = "right") +
   ylab(y.lab)
  
  
  return (   plot  )
  
}


gg.remv.x.lab <<- function( plot ){
  
  
  plot <-  plot %>%  +   
    theme(
      axis.text.x = element_blank()
      ,      axis.ticks.x = element_blank()
    ) 
  
  return(plot)
  
}

gg.remv.dims <<- function( plot ){
  
  
  plot <-  plot %>%  +   
    coord_cartesian()  
  
  return(plot)
  
}


gg.biom <<- function(  LM.1 ,  LS.1,  LM.2 ,  LS.2 ,  LM.3,  LS.3 ,  LM.4 ,  LS.4   ){
  
  test <- function(){
    
    
    LM.1 <- TRUE
    LS.1 <- TRUE
  
    LM.2 <- FALSE
    LS.2 <- FALSE
    
    LM.3 <- FALSE
    LS.3 <- FALSE
    
    LM.4 <- FALSE
    LS.4 <- FALSE
    
  }
  
  

  plot  <-   gg.theme  %>%   +  
    geom_line( d.all , mapping = aes(x = date, y =  ag.biom.grass.kg.ha/1000 , color=    p.ln.colr.obsv  ) 
               , linewidth = p.ln.width 
    ) +
    
    scale_colour_manual(
      name = ''
      , values =   c( 
        
        "L-DNDC" =  p.ln.colr.mod.ub
        
        ,"LM1"  =  "pink" 
        ,"LS1"  =  "yellow"
        
        ,"LM2"  =  "purple" 
        ,"LS2"  =  "orange"
        
        ,"LM3"  =  "blue" 
        ,"LS3"  =  "red"
        
        ,"LM4"  =  "grey" 
        ,"LS4"  =  "brown"
        
      ) 
      , breaks = c(
        "L-DNDC"
        
        ,"LM1" 
        ,"LS1" 
        
        ,"LM2" 
        ,"LS2"  
        
        ,"LM3" 
        ,"LS3"  
        
        ,"LM4" 
        ,"LS4" 
        
      ) 
    )  + 
    scale_x_date(date_breaks = p.date.interval.x.axis, date_labels =  "%y-%m-%d") +
    theme(
      plot.margin = margin( 
        
        p.mrgn.main.top
        , p.mrgn.main.right
        ,  p.mrgn.main.bottom 
        , p.mrgn.main.left
        
        , "cm"  ) , 
      
      
      #   legend.position = "none" ,
      legend.title = element_blank(),   
      axis.title.x = element_blank() , 
      axis.title.y.right = element_blank() , 
      axis.text.y.right = element_blank() ,
      axis.ticks.y.right = element_blank() ,
      axis.text.x = element_text(angle = 90 ,  ) , 
      #  legend.title = element_blank() ,
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, linewidth =1)
      , strip.background = element_rect(color='black', fill='white', size= gg.valid.panel.border.line.thickness, linetype="solid")
      , strip.text.x = element_text(size =  gg.valid.facet.text.size , color = 'black' )
    ) +
    ylab(gg.valid.agb.y.lab ) 
  
  
plot  <-  plot  %>% +  
    geom_line(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.ALL  ) , color = p.ln.colr.mod.ub
                 , linewidth = p.ln.width 
    ) 
 # geom_point(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.LM1  , color = 'LM1') 
             # , linewidth = p.ln.width 
#  ) +
  #  geom_point(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.LM2  , color = 'LM2') 
  #        , linewidth = p.ln.width 
#) +
 # geom_point(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.LM3  , color = 'LM3') 
             # , linewidth = p.ln.width 
#  ) +
#  geom_point(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.LM4  , color = 'LM4') 
             # , linewidth = p.ln.width 
 # ) +
 # geom_point(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.LS1  , color = 'LM1') 
            # , linewidth = p.ln.width 
 # ) +
 # geom_point(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.LS2  , color = 'LM2') 
             # , linewidth = p.ln.width 
 # ) +
 # geom_point(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.LS3  , color = 'LM3') 
             # , linewidth = p.ln.width 
 # ) +
 # geom_point(d.all , mapping = aes(x = date, y = osv.biom.Mg.ha.LS4  , color = 'LM4') 
             # , linewidth = p.ln.width 
 # ) 


if ( LM.1 == TRUE ){
plot  <-       plot   %>%   +  
geom_point( aes(x = date, y =  osv.biom.Mg.ha.LM1 , color =  "LM1" ) 
         , size = gg.biom.ln.width
) 

#plot 

}
  
if ( LS.1 == TRUE ){
plot  <-       plot   %>%   +  
geom_point( aes(x = date, y = osv.biom.Mg.ha.LS1 , color =  "LS1" ) 
           , linewidth = gg.biom.ln.width
) 
}
  
  
if ( LM.2 == TRUE ){
  
plot  <-  plot   %>%   + 
  geom_point( aes( x = date, y = osv.biom.Mg.ha.LM2 , color =  'LM2') 
   , linewidth = gg.biom.ln.width
  ) 


}
  
if ( LS.2 == TRUE ){


  plot  <-  plot   %>%   + 
    geom_point( aes( x = date, y = osv.biom.Mg.ha.LS2 , color =  'LS2') 
                , linewidth = gg.biom.ln.width
    ) 

}


  
if ( LM.3 == TRUE ){


plot  <-  plot   %>%   + 
  geom_point( aes( x = date, y = osv.biom.Mg.ha.LM3 , color =  'LM3') 
              , linewidth = gg.biom.ln.width 
  ) 

}
  

if ( LS.3 == TRUE ){


  plot  <-  plot   %>%   + 
    geom_point( aes( x = date, y = osv.biom.Mg.ha.LS3 , color =  'LS3') 
                , linewidth = gg.biom.ln.width
    ) 

}
  
  if ( LM.4 == TRUE ){
    

    plot  <-  plot   %>%   +  
     
      geom_point( aes( x = date, y = osv.biom.Mg.ha.LM4 , color =   "LM4") 
                  , linewidth = gg.biom.ln.width
      ) 
    
  }
  
if ( LS.4 == TRUE ){


plot  <-  plot      %>%   +  
  
  geom_point( aes( x = date, y = osv.biom.Mg.ha.LS4 , color =   "LS4") 
              , linewidth = gg.biom.ln.width
  ) 

}

return (  plot )
  
  
}


gg.swc.all.depths <- gg.theme  %>%   +
  geom_line( 
    data = d.all
    , aes(x = date, y =  r.a.swc.5.cm.osv   ) , color= 'blue'
    , linewidth = p.ln.width  
  ) +
  geom_line( 
    data = d.all
    , aes(x = date, y =  r.a.swc.15.cm.osv  ) , color= 'red' 
    , linewidth = p.ln.width  
  ) +
  geom_line( 
    data = d.all
    , aes(x = date, y =  r.a.swc.30.cm.osv  ) , color= 'black' 
    , linewidth = p.ln.width  
  ) 




gen.mean.gcm.dat <<- function( d.frame , rcp, columns , period ){
  
  # test: rcp <- r.8
  # test: d.frame <- mod.out.fc ;  rcp <- r.4 ; columns <- cols.2.average  ; period <- period.2.average
  
  nrow.start <- nrow( d.frame )  
  
  for ( d in period  ) {
    
    # d <- period[2]
    
    date <- as.Date(d)
    
    if (  d == period[1] ) { new.row <- nrow.start + 1  
    
    } else { new.row <- new.row + 1  }
    
    print(paste('Running date ' , date ,'for rcp ' ,  rcp) )
    
    d.frame[  new.row  , 'gcm'] <- 0
    d.frame[  new.row  , 'rcp'] <-  rcp
    d.frame[  new.row  , 'date'] <-  date 
    
    for ( c in columns  ) {
      
      if ( c == "ag.biom.delta.kg.m2" & d == period[1] ) { d.frame[  new.row , c] <-0 ; next }
      
    # c <- columns[4]
      
      d.frame[  new.row , c] <- mean(
        
     
          d.frame[ d.frame$rcp == rcp &  d.frame$gcm == 1 & d.frame$date == date & !is.na(d.frame$gcm) & !is.na(d.frame$rcp) , c] 
        ,  d.frame[  d.frame$rcp == rcp & d.frame$gcm == 2 & d.frame$date == date & !is.na(d.frame$gcm) & !is.na(d.frame$rcp) , c] 
         ,  d.frame[  d.frame$rcp == rcp & d.frame$gcm == 3 & d.frame$date == date & !is.na(d.frame$gcm) & !is.na(d.frame$rcp) , c] 
         , d.frame[  d.frame$rcp == rcp & d.frame$gcm == 4 & d.frame$date == date & !is.na(d.frame$gcm) & !is.na(d.frame$rcp) , c] 
         , d.frame[  d.frame$rcp == rcp & d.frame$gcm == 5 & d.frame$date == date & !is.na(d.frame$gcm) & !is.na(d.frame$rcp) , c] 
         , d.frame[  d.frame$rcp == rcp & d.frame$gcm == 6 & d.frame$date == date & !is.na(d.frame$gcm) & !is.na(d.frame$rcp) , c] 
        , d.frame[  d.frame$rcp == rcp & d.frame$gcm == 7 & d.frame$date == date & !is.na(d.frame$gcm) & !is.na(d.frame$rcp) , c] 
      )
      
    }
  }
  
  
  d.frame <- d.frame[   !is.na(   d.frame$date  )  ,  ]
  
  return (  d.frame  )  
  
  
}


get.annual.mean.sd <- function(  series.all  , series.labels ){
  
  # test: series.all <- series.all 
  
  all.cols <- c(  'catg' , 'mean' , 'sd'  , 'series'  )
  
  data <- data.frame(  matrix( 
    ncol = length(     all.cols ) 
    , nrow = 1
   , dimnames = list( 1 ,   all.cols ))
  )
  
  r.cnt <- 1
  
  for ( c in catgs) {
  for ( s in series.all ){
    
    # test:  c <- r.4   ; s <- series.all[1]
    
    data[r.cnt , 'catg'] <- c
    data[r.cnt , 'series'] <- s
    
    data[r.cnt , 'series.label'] <- series.labels.annual[ which(series.all == s ) ]
    data[r.cnt , 'catg.label'] <- catg.labels[ which(c == catgs ) ]
    
    data[r.cnt , 'mean.Mg.ha.yr'] <- mean( na.omit( d.sum.all[d.sum.all$catg == c , s] ) ) * 365  / 1000
    data[r.cnt , 'sd.Mg.ha.yr'] <-  sd( na.omit(d.sum.all[d.sum.all$catg == c , s]  )) * 365 / 1000
    
    
    r.cnt <- r.cnt + 1
    
    
  } } 
    
    return (data)
    
}
  
  
  


get.mn.sd.string <<- function( series , round ){
  
  
  # series <- c(1,2,3,4,5)
  
  mean <-  round( mean( na.omit(  series ) ) ,  round )
  sd <- round( sd( na.omit(  series ) ) ,  round)
  
  
  return (  str_c( mean   , symb.mn.sd  ,   sd  )          )
  
  
  
}

gen.mn.sd.string.from.values <<- function( mean , sd){
  
  
  if ( is.na(sd)  ) { sd <- 0 }
  
  string <- str_c( mean ,  symb.mn.sd , sd )
  
  return (string)
  
}


get.simple.average.numeric <<- function( numeric.series ){
  
  # series <- c(1,2,3,4,5)
  
  return ( mean( na.omit( series  ))         )
  
  
  
}



gen.clim.series <<- function(   dat.in  ){
  
  # dat.in <- gcm.all[ gcm.r.cnd.near.fut.r4.gcm.1 , ] 
  
  #View(  dat.in  )
  
  
  
  dat.out <- data.frame(
    
    format(  as.Date(   dat.in[  ,'date' ]    ), "%Y")
    , as.character(dat.in[  ,'day.cnt' ] ) 
    
    
    
    ,  round ( dat.in[ , 'mean.t.dC' ]   , decimal.round ) 
    ,  round ( dat.in[ , 'min.t.dC' ]   , decimal.round ) 
    ,  round ( dat.in[ , 'max.t.dC' ]   , decimal.round ) 
    
    
    ,  round ( dat.in[ , 'rag.MJ.m2' ] * cv.mj.2.watts , decimal.round ) 
    
    ,  round ( dat.in[ , 'rain.mm' ]  , decimal.round ) 
    ,  round ( dat.in[ , 'rel.h' ]  , decimal.round ) 
    ,  round ( dat.in[ , 'wind.m.s' ]  , decimal.round ) 
    
  )
  
  colnames(  dat.out ) <- c(
    '*'
    ,'*'
    , 'tavg'
    , 'tmin'
    , 'tmax'
    , 'grad'
    , 'prec'
    , 'rh'
    , 'wind'
    
  )
  
  dat.out <- dat.out[ !is.na( dat.out$tavg ) ,]
  
  
  
  
  return (   dat.out   )
 
  
  
  
}


gen.achem.series <<- function(  dat.in  ){
  

  dat.out <- data.frame(
    
    format(  as.Date(   dat.in[  ,'date' ]    ), "%Y")
    , as.character(dat.in[  ,'day.cnt' ] ) 
    
    
    ,  0.000133852
    ,  0.00029193
    ,  0.000681279
    ,  0.00068127
    ,  0.000681279

  )
      
  
  
  colnames(  dat.out ) <- c(
    '*'
    ,'*'
    , 'no3'
    , 'nh4'
    , 'no2dry'
    , 'nh4dry'
    , 'no3dry'
    
  )
  
  return (   dat.out   )
  
}



get.LDNDC.out.dat <<- function( rcp , gcm ,type ){
  
  # test: rcp <- r.4 ; gcm <- 1
  
  if (  rcp == r.4  ) { rcp <- 'r4' }
  if (  rcp == r.8  ) { rcp <- 'r8' }
  
  abbv.watr <- str_c( 'Near_future/KE_Kapiti_near_fut_', rcp ,'_gcm_' ,gcm, '_watercycle-daily.csv' )
  
  abbv.physio <- str_c( 'Near_future/KE_Kapiti_near_fut_', rcp ,'_gcm_' ,gcm, '_physiology-daily.csv' )
  
  abbv.soil <- str_c( 'Near_future/KE_Kapiti_near_fut_', rcp ,'_gcm_' ,gcm, '_soilchemistry-daily.csv' )
  
  
  
  d.watr  <<- read.csv(  abbv.watr )
  
  d.physio <<- read.csv(  abbv.physio)
  
  d.soil <<- read.csv( abbv.soil )
  
  
  
  # Specify column names
  names(d.physio)[25] <- 'maint.resp'
  names(d.physio)[26] <- 'transp.resp'
  names(d.physio)[27] <- 'growth.resp'
  names(d.physio)[28] <- 'co2.upt'
  
  names(d.physio)[38] <- 'ag.biom.kg.m2'
  
  names(  d.soil)[6] <- 'emis.hetero'
  
  
  

  d.physio <- d.physio[ d.physio$species == species.str.id.all ,]
  
  
  d.physio$ag.biom.delta.kg.m2 <- NA
  d.physio[ 1 , 'ag.biom.delta.kg.m2'] <- 0
  
  for ( r in 2 : nrow(  d.physio  ) ) {
    
    d.physio[ r  , 'ag.biom.delta.kg.m2'] <-  d.physio[ r  , 'ag.biom.kg.m2'] - d.physio[ r - 1 , 'ag.biom.kg.m2']
    
  }
  
  d.physio[ !is.na( d.physio$ag.biom.delta.kg.m2 ) & d.physio$ag.biom.delta.kg.m2 < 0 , 'ag.biom.delta.kg.m2'] <- 0
  
  
  if (type == 'soil') { return(d.soil) }
  if (type == 'water') { return(d.watr) }
  if (type == 'physio') { return(d.physio) }
  else { (print('Error, wrong type'))}
  
  
}


define.seasons <<- function(  d.frame  , date.var ){
  
  
  # test: d.frame <- mod.out.fc ; date.var <- 'date'
  
  d.frame$cald.mth <- as.numeric(format( d.frame[, date.var], "%m"))
  
  d.frame[, 'is.sht.rn'] <- FALSE
  d.frame[, 'is.lng.rn'] <- FALSE
  d.frame[, 'is.sht.dr'] <- FALSE
  d.frame[, 'is.lng.dr'] <- FALSE
  
  
  d.frame[  d.frame$cald.mth %in% cald.mos.sht.rn , 'is.sht.rn'] <- TRUE
  d.frame[  d.frame$cald.mth %in% cald.mos.lng.rn , 'is.lng.rn'] <- TRUE
  d.frame[  d.frame$cald.mth %in% cald.mos.sht.dr , 'is.sht.dr'] <- TRUE
  d.frame[  d.frame$cald.mth %in% cald.mos.lng.dr , 'is.lng.dr'] <- TRUE
  
  d.frame$season <- NA
  
  d.frame[  d.frame$cald.mth %in% cald.mos.sht.rn , 'season'] <- ssn.SR
  d.frame[  d.frame$cald.mth %in% cald.mos.lng.rn , 'season'] <- ssn.LR
  d.frame[  d.frame$cald.mth %in% cald.mos.sht.dr , 'season'] <- ssn.SD
  d.frame[  d.frame$cald.mth %in% cald.mos.lng.dr , 'season'] <- ssn.LD
  
  
  return (  d.frame  )
 
}



gen.sum.gg.theme <<-   ggplot(  ) +
  theme(
    panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.65)
    
    ,strip.background = element_rect(color='black', fill='white', linewidth= 0.65, linetype="solid")
    ,strip.text.x = element_text(size =  7.0 , color = 'black' , face = "bold" 
                                 
                                 ,  margin = margin(t = 2.6, r = 2.6, b = 2.6, l = 2.6, unit = "pt"))
    
    , strip.text.y = element_text(size =  9.75 , color = 'black' , face = "bold" 
                                  
                                  ,  margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))  
    
    
    , axis.text.x = element_text( angle = 90 , hjust = 1 , vjust = 0.5 , size = 8.5)
    , axis.text.y = element_text( hjust = 1 , vjust = 0.5 , size = 8.5)
    
    , legend.background = element_rect(fill = "transparent", color = NA)
    
    , axis.title.x = element_blank()
    , axis.ticks.x = element_blank()
  )  
  


gen.annual.sum.plot <<- function( series ){
  
  # series <- series.all[4]
  
  if ( series == series.all[1] ){  y.tit <-  gg.valid.ter.y.ax.lab.annual  } 
  if ( series == series.all[2] ){  y.tit <-  gg.valid.gpp.y.ax.lab.annual } 
  if ( series == series.all[3] ){  y.tit <-  gg.valid.nee.y.ax.lab.annual } 
  if ( series == series.all[4] ){  y.tit <-  gg.valid.biom.y.ax.lab.annual  } 
  
  
  
  plot <- gen.sum.gg.theme  %>% +
    
    geom_bar( ann.dat[ann.dat$series == series , ]  , mapping = aes( x = catg.label , y = mean.Mg.ha.yr  )  , width = 0.5   , stat = "identity" , fill = gg.nf.bar.color) +
    geom_errorbar( ann.dat[ann.dat$series == series , ]  , mapping = aes( x = catg.label , ymin = mean.Mg.ha.yr - sd.Mg.ha.yr , ymax = mean.Mg.ha.yr + sd.Mg.ha.yr)  , width = 0.2 , linewidth = 0.3 ) +
    
    
    ggh4x::facet_nested( 
      .  ~  series.label 
      , strip = strip_nested(size = "variable")
      , render_empty = FALSE
    )    +
     theme( 
      
       axis.title.y = element_text( size = 8.15 )
      
      ) +
    ylab(  y.tit   )
  
  
  
   if ( series == series.all[1] ){  plot <- plot %>% + theme(axis.text.x = element_blank())  
   } else if ( series == series.all[2] ){  plot <- plot %>% + theme(axis.text.x = element_blank())
   } else if ( series == series.all[3] ){  plot <- plot %>% + theme(axis.text.x = element_blank()) 
   } else if ( series == series.all[4] ){ plot <- plot %>% + theme(axis.text.x = element_text( size = 7 ))   } 
  
  
  
  return (  plot)
  
}

gen.seasonal.sum.plot <<- function( series ){
  
  
  # series <- series.all[4]
  
  data <- d.sum.all
  

    if ( series == series.all[1] ){ data$facet.label <- data$facet_label.TER   ; data$y.value <- d.sum.all[ , 'TER.uni.kg.ha.d']  ; y.tit <-  gg.valid.ter.y.ax.lab ; data <- data[   !is.na( data$TER.uni.kg.ha.d) , ]  } 
    if ( series == series.all[2] ){ data$facet.label <- data$facet_label.GPP   ; data$y.value <- d.sum.all[ , 'GPP.uni.kg.ha.d']  ;y.tit <-  gg.valid.gpp.y.ax.lab  ; data <- data[   !is.na( data$GPP.uni.kg.ha.d) , ] } 
    if ( series == series.all[3] ){ data$facet.label <- data$facet_label.NEE   ; data$y.value <- d.sum.all[ , 'NEE.uni.kg.ha.d'] ; y.tit <-  gg.valid.nee.y.ax.lab ; data <- data[   !is.na( data$NEE.uni.kg.ha.d) , ] } 
    if ( series == series.all[4] ){ data$facet.label <- data$facet_label.BIOM  ; data$y.value <- d.sum.all[ , 'BIOM.uni.kg.ha.d'] ; y.tit <-  gg.valid.biom.y.ax.lab  ; data <- data[   !is.na( data$BIOM.uni.kg.ha.d), ] } 
  
  
 plot <- gen.sum.gg.theme  %>% +
    stat_summary(  data, 
                  mapping = aes(x = catg.label, y = y.value )
                  , na.rm = TRUE
                  , fun = "mean"
                  , geom = "bar"
                  , fill = gg.nf.bar.color
                  , width = 0.65
                  , linewidth = 0.3
                  ) +
    stat_summary(
      data  ,  
      mapping = aes(x = catg.label, y = y.value   ) ,
      fun.data = "mean_sdl", 
      fun.args = list(mult = 1), 
      geom = "errorbar", 
      width = 0.2
      , linewidth = 0.3
    ) + 
    ggh4x::facet_nested( 
      .  ~  facet.label + season.label 
      , strip = strip_nested(size = "variable")
      , render_empty = FALSE
    )    +
   ylab( y.tit ) +
   theme( 
     axis.title.y = element_text(size = 7.8)
     )
  
 if ( series == series.all[1] ){  plot <- plot %>% + theme(axis.text.x = element_blank())  
 }else if ( series == series.all[2] ){  plot <- plot %>% + theme(axis.text.x = element_blank())
 }else if ( series == series.all[3] ){  plot <- plot %>% + theme(axis.text.x = element_blank()) 
 } else if ( series == series.all[4] ){ plot <- plot %>% + theme(axis.text.x = element_text( size = 7  ))   } 
 
 
 return (   plot   )
 
}
  



