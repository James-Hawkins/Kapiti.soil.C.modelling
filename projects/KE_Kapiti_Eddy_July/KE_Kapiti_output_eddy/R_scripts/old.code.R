# DATA GAPS
geom_label(
  mapping = aes(x =  as.Date( global.valid.covid.label.date )   , y = -7, label = gg.valid.label.covid.period  ),
  fill = global.valid.text.background
  , color = global.valid.text.color
  , label.size = NA
  , size = gg.valid.label.fs 
  , hjust = .5
)   +
  geom_label(
    mapping = aes(x =  as.Date( global.valid.no.data.label.date )   , y = -7, label = gg.valid.label.no.data.period ),
    fill = global.valid.text.background
    , color = global.valid.text.color
    , label.size = NA
    , size = gg.valid.label.fs 
    , hjust = .5
  )   +
  
  # Water regimes
  geom_label(
    mapping = aes(x =  as.Date(   dipole.period.mid )   , y = lab.y.crd.dipole , label = "Dipole" ),
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
  ) +
  geom_label(
    mapping = aes(x =  as.Date(   el.nino.period.mid )   , y = lab.y.crd.drought , label = "El nino" ),
    fill = global.valid.text.background
    , color = global.valid.text.color
    , label.size = NA
    , size = gg.valid.label.fs 
    , hjust = .5
  ) 