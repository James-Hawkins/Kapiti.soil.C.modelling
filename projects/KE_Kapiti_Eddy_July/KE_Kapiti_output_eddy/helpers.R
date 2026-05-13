

# source('helpers.R')

gen.valid.plot <<- function( 
    y.var.osv 
    , y.var.sim 
    , y.var.sim.bc 
    , y.lab 
    , global.valid.y.cord.high 
    , global.valid.y.cord.mid 
    , global.valid.y.cord.bottm 
    , type
    , omit.BCd
    ){
  
  test <- function(){
    
    y.var.osv <- 'r.a.nee.osv'
    y.var.sim <- 'r.a.nee.sim'
    y.var.sim.bc <- 'r.a.nee.sim.bc'  
    y.lab <- gg.valid.ter.y.ax.lab 
    global.valid.y.cord.high <- global.valid.ter.y.cord.high
    global.valid.y.cord.mid <- global.valid.ter.y.cord.mid
    global.valid.y.cord.bottm <- global.valid.ter.y.cord.bottm
    
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
  
  
  
  gg.valid.no.labl <- gg.theme  %>%   +   #ggplot( d.all[ !is.na(d.all$NEE.obs.kg.ha )  & d.all$covid %in% covid.status[c(1,2)] ,  ] ,   aes(x = date.time ) ) +  
    
    
    geom_line( 
      data = d.obs.pre.covid
      , aes(x = date, y =  plot.y.var.osv , color= gg.valid.labels[2]  ) 
      , linewidth = p.ln.width  
    ) +
    
    geom_line( 
      data = d.obs.post.covid 
      , aes(x = date, y =  plot.y.var.osv , color= gg.valid.labels[2]  ) 
      , linewidth = p.ln.width  
    ) +
    
    geom_line(  data = d.loc[ d.all.plot.conditions & d.loc$covid %in% covid.status[c(1,2,3)] , ]
                 ,aes(x = date, y = plot.y.var.sim , color = gg.valid.labels[1]   ) 
                  , linewidth = p.ln.width 
    ) +   
    
   # geom_line(  data = d.loc[ d.all.plot.conditions & d.loc$covid %in% covid.status[c(1,2,3)] , ]
              #  ,aes(x = date, y = plot.y.var.sim.bc , color = 'bias.corrected'  ) 
                # , linewidth = p.ln.width 
                #, size = gg.valid.sim.point.size
 #   ) +   
    
    geom_segment(aes(x = as.Date(dipole.period.start)  , xend = as.Date(dipole.period.end) , y = lab.y.crd.dipole ) , linetype= 'dashed') +
    geom_segment(aes(x = as.Date(drought.period.start)  , xend = as.Date(drought.period.end) , y = lab.y.crd.drought ) , linetype= 'dashed' ) +
    
    
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
      #   axis.text.x = element_blank() , 
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
    )
  
  
  # theme(aspect.ratio = 1/3)

  
  
  if ( !omit.BCd){
    
    gg.valid.no.labl <-  gg.valid.no.labl  %>%   +  
    geom_line(  data = d.loc[ d.all.plot.conditions & d.loc$covid %in% covid.status[c(1,2,3)] , ]
                 ,aes(x = date, y = r.a.ter.sim.bc , color= 'bias.corrected' ) 
                 
                , linewidth = p.ln.width 
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
  
  if (series == 'ter') { bias.cond <- bias.cond.ter ; y.lab <- 'Absolute error'}
  if (series == 'gpp') { bias.cond <- bias.cond.gpp ; y.lab <- 'Absolute error'}
  if (series == 'swc') { bias.cond <- bias.cond.swc ; y.lab <- 'Absolute error'}
  if (series == 'nee') { bias.cond <- bias.cond.swc ; y.lab <- 'Absolute error'}
  
  biases.long$error.type.label<- factor( biases.long$error.type.label , levels = c('MB' , 'SDSD' , 'LCS'))
  
  
 plot <- gg.kosalam.gen %>%   + 
    geom_bar( 
      biases.long[  bias.cond , ]
      , mapping = aes( x = error.type.label, y = error^0.5)
      , position="dodge"
      , stat="identity"
      , fill = '#A9A9A9'
    ) +
    facet_grid( period.label ~ . , switch = "y") +
    scale_y_continuous(position = "right") +
   ylab(y.lab)
  
  
  return (plot)
  
}


gg.remv.x.lab <<- function( plot ){
  
  
  plot <-  plot %>%  +   
    theme(
      axis.text.x = element_blank()
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
    geom_line( d.all , mapping = aes(x = date, y = ag.biom.grass.kg.ha /1000 , color=  "L-DNDC" ) 
               , linewidth = p.ln.width 
    ) +
    
    scale_colour_manual(
      name = ''
      , values =   c( 
        
        "L-DNDC" = 'green'
        
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
  
  
if ( LM.1 == TRUE ){
plot  <-       plot   %>%   +  
geom_point( aes(x = date, y = osv.biom.Mg.ha.LM1 , color =  "LM1" ) 
        # , linewidth = p.ln.width 
) 
}
  
if ( LS.1 == TRUE ){
plot  <-       plot   %>%   +  
geom_point( aes(x = date, y = osv.biom.Mg.ha.LS1 , color =  "LS1" ) 
          # , linewidth = p.ln.width 
) 
}
  
if ( LM.2 == TRUE ){
plot  <-       plot   %>%   +  
geom_point( aes(x = date, y = osv.biom.Mg.ha.LM2 , color =  "LM2" ) 
          # , linewidth = p.ln.width 
) 
}
  
if ( LS.2 == TRUE ){
plot  <-       plot   %>%   +  
geom_point( aes(x = date, y = osv.biom.Mg.ha.LS2 , color =  "LS2" ) 
            # , linewidth = p.ln.width 
) 
}

  
if ( LM.3 == TRUE ){
    plot  <-       plot   %>%   +  
      geom_point( aes(x = date, y = osv.biom.Mg.ha.LM3 , color =  "LM3" ) 
                  # , linewidth = p.ln.width 
      ) 
  }
  
  

if ( LS.3 == TRUE ){
plot  <-       plot   %>%   +  
geom_point( aes(x = date, y = osv.biom.Mg.Ma.LS3 , color =  "LS3" ) 
          # , linewidth = p.ln.width 
) 
}
  
if ( LM.4 == TRUE ){
plot  <-  plot   %>%   +  
geom_point( aes(x = date, y = osv.biomMg.ha.LM4 , color =  "LM4" ) 
            # , linewidth = p.ln.width 
) 
}
  
if ( LS.4 == TRUE ){
plot  <-       plot   %>%   +  
geom_point( aes(x = date, y = osv.biom.Mg.ha.LS4 , color =  "LS4" ) 
            # , linewidth = p.ln.width 
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



