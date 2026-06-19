#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

if (file.exists('calibr8.in.dat.RData')) {
  load('calibr8.in.dat.RData')
}


library("languageR") ;  library('tidyr') ; library('readxl') ;  library('stringr') ;library('stringi') ; library('chron') ; library('lubridate') ; library('ggplot2')

if (!exists("valid8")) {
  source('run.all.R')
}

try(valid8())

if (exists("metrics")) {
  write.csv(metrics, str_c(R.dir,'metrics.csv'))
  # Also write to parent dir for convenience
  write.csv(metrics, 'metrics.csv')
}
