
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

load('calibr8.in.dat.RData')

 save.image('calibr8.in.dat.RData')

#rm(list = ls()) 

library("languageR") ;  library('tidyr') ; library('readxl') ;  library('stringr') ;library('stringi') ; library('chron') ; library('lubridate') 



valid8()

write.csv(metrics, 'metrics.csv')
