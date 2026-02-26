
# Wind direction

summary(d.eddy.raw[(365*48*(7/12)):(365*48*6.8) , 'wind_dir'])   # Period average (entire)




summary(d.eddy.raw[(1*365*48*(7/12)):365*48*1.5  , 'wind_dir'])
summary(d.eddy.raw[(1*365*48*1.5):365*48*2.5  , 'wind_dir'])
summary(d.eddy.raw[(1*365*48*2.5):365*48*3.5  , 'wind_dir'])
summary(d.eddy.raw[(1*365*48*4.5):365*48*5.5  , 'wind_dir'])




mn.wnd.dir.2018 <- mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2018")  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.dir.2019 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2019")  , 'wind_dir'])) #mean(na.omit(d.eddy.raw[(1*365*48*1.5):365*48*2.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.dir.2020 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2020")  , 'wind_dir'])) #mean(na.omit(d.eddy.raw[(1*365*48*2.5):365*48*3.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.dir.2021 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2021")  , 'wind_dir'])) #mean(na.omit(d.eddy.raw[(1*365*48*3.5):365*48*4.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.dir.2022 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2022")  , 'wind_dir'])) #mean(na.omit(d.eddy.raw[(1*365*48*2.5):365*48*3.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.dir.2023 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2023")  , 'wind_dir'])) #mean(na.omit(d.eddy.raw[(1*365*48*3.5):365*48*4.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.dir.2024 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2024")  , 'wind_dir'])) #mean(na.omit(d.eddy.raw[(1*365*48*3.5):365*48*4.5  , 'wind_dir'])) #+ 360 * 0.5


mn.wnd.dir.2018 
mn.wnd.dir.2019 
mn.wnd.dir.2020 
mn.wnd.dir.2021 
mn.wnd.dir.2022 
mn.wnd.dir.2023 
mn.wnd.dir.2024 



mn.wnd.dir.2023.june <- mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2023-01")  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.spd.2023.june <- mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2023-01")  , 'wind_speed'])) #+ 360 * 0.5 



# Wind speed
mn.wnd.spd.2018 <- mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2018")  , 'wind_speed'])) #+ 360 * 0.5
mn.wnd.spd.2019 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2019")  , 'wind_speed'])) #mean(na.omit(d.eddy.raw[(1*365*48*1.5):365*48*2.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.spd.2020 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2020")  , 'wind_speed'])) #mean(na.omit(d.eddy.raw[(1*365*48*2.5):365*48*3.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.spd.2021 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2021")  , 'wind_speed'])) #mean(na.omit(d.eddy.raw[(1*365*48*3.5):365*48*4.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.spd.2022 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2022")  , 'wind_speed'])) #mean(na.omit(d.eddy.raw[(1*365*48*2.5):365*48*3.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.spd.2023 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2023")  , 'wind_speed'])) #mean(na.omit(d.eddy.raw[(1*365*48*3.5):365*48*4.5  , 'wind_dir'])) #+ 360 * 0.5
mn.wnd.spd.2024 <-  mean(na.omit(d.eddy.raw[ str_detect( d.eddy.raw$date , "2024")  , 'wind_speed'])) #mean(na.omit(d.eddy.raw[(1*365*48*3.5):365*48*4.5  , 'wind_dir'])) #+ 360 * 0.5

mn.wnd.spd.2018 
mn.wnd.spd.2019 
mn.wnd.spd.2020 
mn.wnd.spd.2021 
mn.wnd.spd.2022 
mn.wnd.spd.2023 
mn.wnd.spd.2024 

# Hist
hist(d.eddy.raw[(1*365*48*.5):365*48*1.5  , 'wind_dir'])
hist(d.eddy.raw[(1*365*48*1.5):365*48*2.5  , 'wind_dir'])
hist(d.eddy.raw[(1*365*48*2.5):365*48*3.5  , 'wind_dir'])
hist(d.eddy.raw[(1*365*48*4.5):365*48*5.5  , 'wind_dir'])



summary(d.eddy.raw[(1*365*48*.5):365*48*1.5  , 'wind_speed'])


# Comparison of weather observations across sources to detect biases
# Eddy
ect.mn.precip.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'precip.osv'])) 
ect.mn.temp.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'temp.avg.osv'])) 

ect.mn.precip.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'precip.osv'])) 
ect.mn.temp.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'temp.avg.osv']))

ect.mn.precip.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'precip.osv'])) 
ect.mn.temp.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'temp.avg.osv']))

ect.mn.precip.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'precip.osv']))
ect.mn.temp.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'temp.avg.osv']))

ect.mn.precip.2022 <-mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'precip.osv'])) 
ect.mn.temp.2022 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'temp.avg.osv']))

ect.mn.precip.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'precip.osv'])) 
ect.mn.temp.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'temp.avg.osv']))

ect.mn.precip.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'precip.osv'])) 
ect.mn.temp.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'temp.avg.osv']))

# NASA POWER
sum(d.power[d.power$year == "2018" , 'Precip'])
mean(d.power[d.power$year == "2018" , 'Temp.avg'])

sum(d.power[d.power$year == "2019" , 'Precip'])
mean(d.power[d.power$year == "2019" , 'Temp.avg'])

sum(d.power[d.power$year == "2020" , 'Precip'])
mean(d.power[d.power$year == "2020" , 'Temp.avg'])


# TA 677
ta.677.mn.precip.2019 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2019") , 'precip']))
ta.677.mn.temp.2019 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2019") , 'temp.mn']))

ta.677.mn.precip.2020 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2020") , 'precip']))
ta.677.mn.temp.2020 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2020") , 'temp.mn']))

ta.677.mn.precip.2021 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2021") , 'precip']))
ta.677.mn.temp.2021 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2021") , 'temp.mn']))

ta.677.mn.precip.2022 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2022") , 'precip']))
ta.677.mn.temp.2022 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2022") , 'temp.mn']))

ta.677.mn.precip.2023 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2023") , 'precip']))
ta.677.mn.temp.2023 <- mean(na.omit(d.weather.subs[  str_detect( as.Date(d.weather.subs$date) ,"2023") , 'temp.mn']))

# TA 621
ta.621.mn.precip.2019 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2019") , 'precip']))
ta.621.mn.temp.2019 <-mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2019") , 'temp.mn']))

ta.621.mn.precip.2020 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2020") , 'precip']))
ta.621.mn.temp.2020 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2020") , 'temp.mn']))

ta.621.mn.precip.2021 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2021") , 'precip']))
ta.621.mn.temp.2021 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2021") , 'temp.mn']))

ta.621.mn.precip.2022 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2022") , 'precip']))
ta.621.mn.temp.2022 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2022") , 'temp.mn']))

ta.621.mn.precip.2023 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2023") , 'precip']))
ta.621.mn.temp.2023 <- mean(na.omit(d.weather.subs.2[  str_detect( as.Date(d.weather.subs.2$date) ,"2023") , 'temp.mn']))


# TA 678
ta.678.mn.precip.2019 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2019") , 'precip']))
ta.678.mn.temp.2019 <-mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2019") , 'temp.mn']))

ta.678.mn.precip.2020 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2020") , 'precip']))
ta.678.mn.temp.2020 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2020") , 'temp.mn']))

ta.678.mn.precip.2021 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2021") , 'precip']))
ta.678.mn.temp.2021 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2021") , 'temp.mn']))

ta.678.mn.precip.2022 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2022") , 'precip']))
ta.678.mn.temp.2022 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2022") , 'temp.mn']))

ta.678.mn.precip.2023 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2023") , 'precip']))
ta.678.mn.temp.2023 <- mean(na.omit(d.weather.subs.3[  str_detect( as.Date(d.weather.subs.3$date) ,"2023") , 'temp.mn']))


# Comparisons
# vs. POWER
sum(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2018")  , 'Precip'])) /sum(d.power[d.power$year == "2018" , 'Precip'])
mean(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2018")  , 'Temp'])) /mean(d.power[d.power$year == "2018" , 'Temp.avg'])

sum(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2019")  , 'Precip'])) /sum(d.power[d.power$year == "2019" , 'Precip'])
mean(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2019")  , 'Temp']))/ mean(d.power[d.power$year == "2019" , 'Temp.avg'])

sum(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2020")  , 'Precip'])) / sum(d.power[d.power$year == "2020" , 'Precip'])
mean(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2020")  , 'Temp']))/ mean(d.power[d.power$year == "2019" , 'Temp.avg'])


# vs. TA 677
mn.bias.ta.677.precip.2019 <- ect.mn.precip.2019  / ta.677.mn.precip.2019 
mn.bias.ta.677.precip.2020 <- ect.mn.precip.2020  /ta.677.mn.precip.2020 
mn.bias.ta.677.precip.2021 <- ect.mn.precip.2021  /ta.677.mn.precip.2021 
mn.bias.ta.677.precip.2022 <- ect.mn.precip.2022  /ta.677.mn.precip.2022
mn.bias.ta.677.precip.2023 <- ect.mn.precip.2023  / ta.677.mn.precip.2023 

mn.bias.ta.677.precip <<- mean(mn.bias.ta.677.precip.2019 ,mn.bias.ta.677.precip.2020 , mn.bias.ta.677.precip.2022  )


mn.bias.ta.677.temp.2019 <- ta.677.mn.temp.2019 /  ect.mn.temp.2019 
mn.bias.ta.677.temp.2020 <- ta.677.mn.temp.2020 /  ect.mn.temp.2020
mn.bias.ta.677.temp.2021 <- ta.677.mn.temp.2021 /  ect.mn.temp.2021 
mn.bias.ta.677.temp.2022 <- ta.677.mn.temp.2022 /  ect.mn.temp.2022 
mn.bias.ta.677.temp.2023 <- ta.677.mn.temp.2023 /  ect.mn.temp.2023 

mn.bias.ta.677.temp <<- mean(mn.bias.ta.677.temp.2019 ,mn.bias.ta.677.temp.2020 , mn.bias.ta.677.temp.2022  )


# vs. TA 621
mn.bias.ta.621.precip.2019 <- ect.mn.precip.2019  / ta.621.mn.precip.2019 
mn.bias.ta.621.precip.2020 <- ect.mn.precip.2020  /ta.621.mn.precip.2020 
mn.bias.ta.621.precip.2021 <- ect.mn.precip.2021  /ta.621.mn.precip.2021 
mn.bias.ta.621.precip.2022 <- ect.mn.precip.2022  /ta.621.mn.precip.2022
mn.bias.ta.621.precip.2023 <- ect.mn.precip.2023  / ta.621.mn.precip.2023 

mn.bias.ta.621.precip <<-  mean(mn.bias.ta.621.precip.2019 ,mn.bias.ta.621.precip.2020 , mn.bias.ta.621.precip.2022  )


mn.bias.ta.621.temp.2019 <- ta.621.mn.temp.2019 /  ect.mn.temp.2019 
mn.bias.ta.621.temp.2020 <- ta.621.mn.temp.2020 /  ect.mn.temp.2020
mn.bias.ta.621.temp.2021 <- ta.621.mn.temp.2021 /  ect.mn.temp.2021 
mn.bias.ta.621.temp.2022 <- ta.621.mn.temp.2022 /  ect.mn.temp.2022 
mn.bias.ta.621.temp.2023 <- ta.621.mn.temp.2023 /  ect.mn.temp.2023 

mn.bias.ta.621.temp <<-  mean(mn.bias.ta.621.temp.2019 ,mn.bias.ta.621.temp.2020 , mn.bias.ta.621.temp.2022  )


# vs. TA 678
mn.bias.ta.678.precip.2019 <- ect.mn.precip.2019  / ta.678.mn.precip.2019 
mn.bias.ta.678.precip.2020 <- ect.mn.precip.2020  /ta.678.mn.precip.2020 
mn.bias.ta.678.precip.2021 <- ect.mn.precip.2021  /ta.678.mn.precip.2021 
mn.bias.ta.678.precip.2022 <- ect.mn.precip.2022  /ta.678.mn.precip.2022
mn.bias.ta.678.precip.2023 <- ect.mn.precip.2023  / ta.678.mn.precip.2023 

mn.bias.ta.678.precip <<-  mean(mn.bias.ta.678.precip.2019 , mn.bias.ta.678.precip.2020 , mn.bias.ta.678.precip.2022 , mn.bias.ta.678.precip.2023 )


mn.bias.ta.678.temp.2019 <- ta.678.mn.temp.2019 /  ect.mn.temp.2019 
mn.bias.ta.678.temp.2020 <- ta.678.mn.temp.2020 /  ect.mn.temp.2020
mn.bias.ta.678.temp.2021 <- ta.678.mn.temp.2021 /  ect.mn.temp.2021 
mn.bias.ta.678.temp.2022 <- ta.678.mn.temp.2022 /  ect.mn.temp.2022 
mn.bias.ta.678.temp.2023 <- ta.678.mn.temp.2023 /  ect.mn.temp.2023 

mn.bias.ta.678.temp <<- mean(mn.bias.ta.678.temp.2019 ,mn.bias.ta.678.temp.2020 , mn.bias.ta.678.temp.2022 , mn.bias.ta.678.temp.2023 )






sum(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2019")  , 'Precip'])) /sum(d.power[d.power$year == "2019" , 'Precip']) / ta.677.mn.precip.2019
mean(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2019")  , 'Temp']))/ mean(d.power[d.power$year == "2019" , 'Temp.avg'])

sum(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2020")  , 'Precip'])) / sum(d.power[d.power$year == "2020" , 'Precip'])
mean(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2020")  , 'Temp']))/ mean(d.power[d.power$year == "2019" , 'Temp.avg'])

(1.04+1.8+1.3)/3