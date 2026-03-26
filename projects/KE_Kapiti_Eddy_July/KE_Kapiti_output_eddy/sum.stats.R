
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
{
# Eddy
ect.mn.precip.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'precip.osv'])) 
ect.mn.temp.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'temp.avg.osv'])) 
ect.mx.temp.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'temp.max.osv']))  
ect.min.temp.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'temp.min.osv'])) 
ect.mn.rad.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'rg.osv'])) 
ect.mn.rh.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'rh.osv'])) 
ect.mn.ws.2018 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2018")  , 'ws.osv'])) 


ect.mn.precip.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'precip.osv'])) 
ect.mn.temp.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'temp.avg.osv']))
ect.mx.temp.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'temp.max.osv']))  
ect.min.temp.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'temp.min.osv'])) 
ect.mn.rad.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'rg.osv'])) 
ect.mn.rh.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'rh.osv'])) 
ect.mn.ws.2019 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2019")  , 'ws.osv'])) 


ect.mn.precip.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'precip.osv'])) 
ect.mn.temp.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'temp.avg.osv']))
ect.mx.temp.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'temp.max.osv']))  
ect.min.temp.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'temp.min.osv'])) 
ect.mn.rad.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'rg.osv'])) 
ect.mn.rh.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'rh.osv'])) 
ect.mn.ws.2020 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2020")  , 'ws.osv'])) 

ect.mn.precip.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'precip.osv']))
ect.mn.temp.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'temp.avg.osv']))
ect.mx.temp.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'temp.max.osv']))  
ect.min.temp.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'temp.min.osv'])) 
ect.mn.rad.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'rg.osv'])) 
ect.mn.rh.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'rh.osv'])) 
ect.mn.ws.2021 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2021")  , 'ws.osv'])) 


ect.mn.precip.2022 <-mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'precip.osv'])) 
ect.mn.temp.2022 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'temp.avg.osv']))
ect.mx.temp.2022 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'temp.max.osv']))  
ect.min.temp.2022 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'temp.min.osv'])) 
ect.mn.rad.2022 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'rg.osv'])) 
ect.mn.rh.2022 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'rh.osv'])) 
ect.mn.ws.2022 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2022")  , 'ws.osv'])) 


ect.mn.precip.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'precip.osv'])) 
ect.mn.temp.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'temp.avg.osv']))
ect.mx.temp.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'temp.max.osv']))  
ect.min.temp.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'temp.min.osv'])) 
ect.mn.rad.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'rg.osv'])) 
ect.mn.rh.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'rh.osv'])) 
ect.mn.ws.2023 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2023")  , 'ws.osv'])) 


ect.mn.precip.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'precip.osv'])) 
ect.mn.temp.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'temp.avg.osv']))
ect.mx.temp.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'temp.max.osv']))  
ect.min.temp.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'temp.min.osv'])) 
ect.mn.rad.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'rg.osv'])) 
ect.mn.rh.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'rh.osv'])) 
ect.mn.ws.2024 <- mean(na.omit(d.eddy.daily[   str_detect( as.Date(d.eddy.daily$date) ,"2024")  , 'ws.osv'])) 




# NASA POWER
npower.mn.precip.2018 <- mean(d.power[d.power$year == "2018" , 'Precip'])
npower.mn.temp.2018<- mean(d.power[d.power$year == "2018" , 'Temp.avg'])
npower.mx.temp.2018<- mean(d.power[d.power$year == "2018" , 'Temp.max'])
npower.min.temp.2018<- mean(d.power[d.power$year == "2018" , 'Temp.min'])
npower.mn.rad.2018 <- mean(d.power[d.power$year == "2018" , 'Radiation']) * cv.MJ.to.watts
npower.mn.rh.2018 <- mean(d.power[d.power$year == "2018" , 'Relative.humid']) 
npower.mn.ws.2018 <- mean(d.power[d.power$year == "2018" , 'Wind.speed']) 

npower.mn.precip.2019 <- mean(d.power[d.power$year == "2019" , 'Precip'])
npower.mn.temp.2019 <- mean(d.power[d.power$year == "2019" , 'Temp.avg'])
npower.mx.temp.2019<- mean(d.power[d.power$year == "2019" , 'Temp.max'])
npower.min.temp.2019<- mean(d.power[d.power$year == "2019" , 'Temp.min'])
npower.mn.rad.2019 <- mean(d.power[d.power$year == "2019" , 'Radiation']) * cv.MJ.to.watts
npower.mn.rh.2019 <- mean(d.power[d.power$year == "2019" , 'Relative.humid']) 
npower.mn.ws.2019 <- mean(d.power[d.power$year == "2019" , 'Wind.speed']) 

npower.mn.precip.2020 <- mean(d.power[d.power$year == "2020" , 'Precip'])
npower.mn.temp.2020 <- mean(d.power[d.power$year == "2020" , 'Temp.avg'])
npower.mx.temp.2020 <- mean(d.power[d.power$year == "2020" , 'Temp.max'])
npower.min.temp.2020 <- mean(d.power[d.power$year == "2020" , 'Temp.min'])
npower.mn.rad.2020 <- mean(d.power[d.power$year == "2020" , 'Radiation']) * cv.MJ.to.watts
npower.mn.rh.2020 <- mean(d.power[d.power$year == "2020" , 'Relative.humid']) 
npower.mn.ws.2020 <- mean(d.power[d.power$year == "2020" , 'Wind.speed']) 

npower.mn.precip.2021 <- mean(d.power[d.power$year == "2021" , 'Precip'])
npower.mn.temp.2021 <- mean(d.power[d.power$year == "2021" , 'Temp.avg'])
npower.mx.temp.2021 <- mean(d.power[d.power$year == "2021" , 'Temp.max'])
npower.min.temp.2021 <- mean(d.power[d.power$year == "2021" , 'Temp.min'])
npower.mn.rad.2021 <- mean(d.power[d.power$year == "2021" , 'Radiation']) * cv.MJ.to.watts
npower.mn.rh.2021 <- mean(d.power[d.power$year == "2021" , 'Relative.humid']) 
npower.mn.ws.2021 <- mean(d.power[d.power$year == "2021" , 'Wind.speed']) 

npower.mn.precip.2022 <- mean(d.power[d.power$year == "2022" , 'Precip'])
npower.mn.temp.2022 <- mean(d.power[d.power$year == "2022" , 'Temp.avg'])
npower.mx.temp.2022 <- mean(d.power[d.power$year == "2022" , 'Temp.max'])
npower.min.temp.2022 <- mean(d.power[d.power$year == "2022" , 'Temp.min'])
npower.mn.rad.2022 <- mean(d.power[d.power$year == "2022" , 'Radiation']) * cv.MJ.to.watts
npower.mn.rh.2022 <- mean(d.power[d.power$year == "2022" , 'Relative.humid']) 
npower.mn.ws.2022 <- mean(d.power[d.power$year == "2022" , 'Wind.speed']) 



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
ta.621.mn.precip.2019 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2019") , 'precip']))
ta.621.mn.temp.2019 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2019") , 'temp.mn']))
ta.621.max.temp.2019 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2019") , 'temp.max']))
ta.621.min.temp.2019 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2019") , 'temp.min']))

ta.621.mn.precip.2020 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2020") , 'precip']))
ta.621.mn.temp.2020 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2020") , 'temp.mn']))
ta.621.max.temp.2020 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2020") , 'temp.max']))
ta.621.min.temp.2020 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2020") , 'temp.min']))

ta.621.mn.precip.2021 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2021") , 'precip']))
ta.621.mn.temp.2021 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2021") , 'temp.mn']))
ta.621.max.temp.2021 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2021") , 'temp.max']))
ta.621.min.temp.2021 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2021") , 'temp.min']))

ta.621.mn.precip.2022 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2022") , 'precip']))
ta.621.mn.temp.2022 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2022") , 'temp.mn']))
ta.621.max.temp.2022 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2022") , 'temp.max']))
ta.621.min.temp.2022 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2022") , 'temp.min']))


ta.621.mn.precip.2023 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2023") , 'precip']))
ta.621.mn.temp.2023 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2023") , 'temp.mn']))
ta.621.max.temp.2023 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2023") , 'temp.max']))
ta.621.min.temp.2023 <- mean(na.omit(d.weather.subs.621[  str_detect( as.Date(d.weather.subs.621$date) ,"2023") , 'temp.min']))


# TA 678
ta.678.mn.precip.2019 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2019") , 'precip']))
ta.678.mn.temp.2019 <-mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2019") , 'temp.mn']))

ta.678.mn.precip.2020 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2020") , 'precip']))
ta.678.mn.temp.2020 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2020") , 'temp.mn']))

ta.678.mn.precip.2021 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2021") , 'precip']))
ta.678.mn.temp.2021 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2021") , 'temp.mn']))

ta.678.mn.precip.2022 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2022") , 'precip']))
ta.678.mn.temp.2022 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2022") , 'temp.mn']))

ta.678.mn.precip.2023 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2023") , 'precip']))
ta.678.mn.temp.2023 <- mean(na.omit(d.weather.subs.678[  str_detect( as.Date(d.weather.subs.678$date) ,"2023") , 'temp.mn']))

# TA 814
ta.814.mn.precip.2023 <- mean(na.omit(d.weather.subs.814[  str_detect( as.Date(d.weather.subs.814$date) ,"2023") , 'precip']))
ta.814.mn.temp.2023 <-mean(na.omit(d.weather.subs.814[  str_detect( as.Date(d.weather.subs.814$date) ,"2023") , 'temp.mn']))
ta.814.max.temp.2023 <-mean(na.omit(d.weather.subs.814[  str_detect( as.Date(d.weather.subs.814$date) ,"2023") , 'temp.max']))
ta.814.min.temp.2023 <-mean(na.omit(d.weather.subs.814[  str_detect( as.Date(d.weather.subs.814$date) ,"2023") , 'temp.min']))

ta.814.mn.precip.2024 <- mean(na.omit(d.weather.subs.814[  str_detect( as.Date(d.weather.subs.814$date) ,"2024") , 'precip']))
ta.814.mn.temp.2024 <-mean(na.omit(d.weather.subs.814[  str_detect( as.Date(d.weather.subs.814$date) ,"2024") , 'temp.mn']))
ta.814.max.temp.2024 <-mean(na.omit(d.weather.subs.814[  str_detect( as.Date(d.weather.subs.814$date) ,"2024") , 'temp.max']))
ta.814.min.temp.2024 <-mean(na.omit(d.weather.subs.814[  str_detect( as.Date(d.weather.subs.814$date) ,"2024") , 'temp.min']))



# Comparisons
# vs. POWER
mn.bias.npower.precip.2018 <- ect.mn.precip.2018  / npower.mn.precip.2018
mn.bias.npower.temp.mean.2018 <- ect.mn.temp.2018  / npower.mn.temp.2018
mn.bias.npower.temp.max.2018 <- ect.mx.temp.2018  / npower.mx.temp.2018
mn.bias.npower.temp.min.2018 <- ect.mn.temp.2018  / npower.min.temp.2018
mn.bias.npower.rad.2018 <- ect.mn.rad.2018  / npower.mn.rad.2018
mn.bias.npower.rh.2018 <- ect.mn.rh.2018  / npower.mn.rh.2018
mn.bias.npower.ws.2018 <- ect.mn.ws.2018  / npower.mn.ws.2018



mn.bias.npower.precip.2019 <- ect.mn.precip.2019  / npower.mn.precip.2019
mn.bias.npower.temp.mean.2019 <- ect.mn.temp.2019  / npower.mn.temp.2019
mn.bias.npower.temp.max.2019 <- ect.mx.temp.2019  / npower.mx.temp.2019
mn.bias.npower.temp.min.2019 <- ect.mn.temp.2019  / npower.min.temp.2019
mn.bias.npower.rad.2019 <- ect.mn.rad.2019  / npower.mn.rad.2019
mn.bias.npower.rh.2019 <- ect.mn.rh.2019  / npower.mn.rh.2019
mn.bias.npower.ws.2019 <- ect.mn.ws.2019  / npower.mn.ws.2019



mn.bias.npower.precip.2020 <- ect.mn.precip.2020  / npower.mn.precip.2020
mn.bias.npower.temp.mean.2020 <- ect.mn.temp.2020  / npower.mn.temp.2020
mn.bias.npower.temp.max.2020 <- ect.mx.temp.2020  / npower.mx.temp.2020
mn.bias.npower.temp.min.2020 <- ect.mn.temp.2020  / npower.min.temp.2020
mn.bias.npower.rad.2020 <- ect.mn.rad.2020  / npower.mn.rad.2020
mn.bias.npower.rh.2020 <- ect.mn.rh.2020  / npower.mn.rh.2020
mn.bias.npower.ws.2020 <- ect.mn.ws.2020  / npower.mn.ws.2020


mn.bias.npower.precip.2022 <- ect.mn.precip.2022  / npower.mn.precip.2022
mn.bias.npower.temp.mean.2022 <- ect.mn.temp.2022  / npower.mn.temp.2022
mn.bias.npower.temp.max.2022 <- ect.mx.temp.2022  / npower.mx.temp.2022
mn.bias.npower.temp.min.2022 <- ect.mn.temp.2022  / npower.min.temp.2022
mn.bias.npower.rad.2022 <- ect.mn.rad.2022  / npower.mn.rad.2022
mn.bias.npower.rh.2022 <- ect.mn.rh.2022  / npower.mn.rh.2022
mn.bias.npower.ws.2022 <- ect.mn.ws.2022  / npower.mn.ws.2022



# Period averages
mn.bias.npower.precip <<- mean(mn.bias.npower.precip.2018 , mn.bias.npower.precip.2019 ,mn.bias.npower.precip.2020 , mn.bias.npower.precip.2022 )

mn.bias.npower.temp.mean <<- mean(mn.bias.npower.temp.mean.2018 , mn.bias.npower.temp.mean.2019 ,mn.bias.npower.temp.mean.2020 ,mn.bias.npower.temp.mean.2022)
mn.bias.npower.temp.max <<- mean(mn.bias.npower.temp.max.2018 , mn.bias.npower.temp.max.2019 ,mn.bias.npower.temp.max.2020,mn.bias.npower.temp.max.2022 )
mn.bias.npower.temp.min <<- mean(mn.bias.npower.temp.min.2018 , mn.bias.npower.temp.min.2019 ,mn.bias.npower.temp.min.2020 ,mn.bias.npower.temp.min.2022)

mn.bias.npower.rad <<- mean(mn.bias.npower.rad.2018 , mn.bias.npower.rad.2019 ,mn.bias.npower.rad.2020 ,mn.bias.npower.rad.2022)
mn.bias.npower.rh <<- mean(mn.bias.npower.rh.2018 , mn.bias.npower.rad.2019 ,mn.bias.npower.rad.2020 ,mn.bias.npower.rad.2022 )
mn.bias.npower.ws <<- mean(mn.bias.npower.ws.2018 , mn.bias.npower.ws.2019 ,mn.bias.npower.ws.2020,mn.bias.npower.ws.2022  )



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

# Max temp
mn.bias.ta.621.max.temp.2019 <- ta.621.max.temp.2019 /  ect.mx.temp.2019
mn.bias.ta.621.max.temp.2020 <- ta.621.max.temp.2020 /  ect.mx.temp.2020
mn.bias.ta.621.max.temp.2021 <- ta.621.max.temp.2021 /  ect.mx.temp.2021
mn.bias.ta.621.max.temp.2022 <- ta.621.max.temp.2022 /  ect.mx.temp.2022
mn.bias.ta.621.max.temp.2023 <- ta.621.max.temp.2023 /  ect.mx.temp.2023

mn.bias.ta.621.max.temp <<-  mean(
  mn.bias.ta.621.max.temp.2019 
  ,mn.bias.ta.621.max.temp.2020
  , mn.bias.ta.621.max.temp.2021
  , mn.bias.ta.621.max.temp.2022
  ,mn.bias.ta.621.max.temp.2023
  
  )

# Min temp
mn.bias.ta.621.min.temp.2019 <- ta.621.min.temp.2019 /  ect.min.temp.2019
mn.bias.ta.621.min.temp.2020 <- ta.621.min.temp.2020 /  ect.min.temp.2020
mn.bias.ta.621.min.temp.2021 <- ta.621.min.temp.2021 /  ect.min.temp.2021
mn.bias.ta.621.min.temp.2022 <- ta.621.min.temp.2022 /  ect.min.temp.2022
mn.bias.ta.621.min.temp.2023 <- ta.621.min.temp.2023 /  ect.min.temp.2023

mn.bias.ta.621.min.temp <<-  mean(
  mn.bias.ta.621.min.temp.2019 
  ,mn.bias.ta.621.min.temp.2020
  , mn.bias.ta.621.min.temp.2021
  , mn.bias.ta.621.min.temp.2022
  ,mn.bias.ta.621.min.temp.2023
  
)


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

# VS TA 814
mn.bias.ta.814.temp.2023 <- ta.814.mn.temp.2023 /  ect.mn.temp.2023 
mn.bias.ta.814.temp.2024 <- ta.814.mn.temp.2024/  ect.mn.temp.2024 

mn.bias.ta.814.max.temp.2023 <- ta.814.max.temp.2023 /  ect.mx.temp.2023 
mn.bias.ta.814.max.temp.2024 <- ta.814.max.temp.2024/  ect.mx.temp.2024 

mn.bias.ta.814.min.temp.2023 <- ta.814.min.temp.2023 /  ect.min.temp.2023 
mn.bias.ta.814.min.temp.2024 <- ta.814.min.temp.2024/  ect.min.temp.2024 

mn.bias.ta.814.precip.2023 <- ta.814.mn.precip.2023 /  ect.mn.precip.2023
mn.bias.ta.814.precip.2024 <- ta.814.mn.precip.2024/  ect.mn.precip.2024 

mn.bias.ta.814.temp <<- mean( mn.bias.ta.814.temp.2023 , mn.bias.ta.814.temp.2024 )
mn.bias.ta.814.precip <<- mean( mn.bias.ta.814.precip.2023 , mn.bias.ta.814.precip.2024 )
mn.bias.ta.814.max.temp <<- mean( mn.bias.ta.814.max.temp.2023 , mn.bias.ta.814.max.temp.2024 )
mn.bias.ta.814.min.temp <<- mean( mn.bias.ta.814.min.temp.2023 , mn.bias.ta.814.min.temp.2024 )
}



sum(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2019")  , 'Precip'])) /sum(d.power[d.power$year == "2019" , 'Precip']) / ta.677.mn.precip.2019
mean(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2019")  , 'Temp']))/ mean(d.power[d.power$year == "2019" , 'Temp.avg'])

sum(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2020")  , 'Precip'])) / sum(d.power[d.power$year == "2020" , 'Precip'])
mean(na.omit(d.eddy.raw[   str_detect( as.Date(d.eddy.raw$date) ,"2020")  , 'Temp']))/ mean(d.power[d.power$year == "2019" , 'Temp.avg'])

(1.04+1.8+1.3)/3






summary(  d.all[d.all$period == period.dipole , '']   )



ggplot( data = d.all , aes( x = date , y = precip.osv)) +
  geom_line()+
 #geom_line( 
   # data = d.weather.subs[  !is.na( d.weather.subs$precip ),  ]
   # , aes(x = date , y = precip) 
   # , color = 'red'
    #, alpha = 0
   # ) + 
  geom_line( 
    data = d.weather.subs.621[  !is.na( d.weather.subs.621$precip ),  ]
    , aes(x = date , y = precip) 
    , color = 'yellow'
    
  ) +
geom_line( 
  data = d.weather.subs.678[  !is.na( d.weather.subs.678$precip ),  ]
  , aes(x = date , y = precip) 
  , color = 'orange'
  
) 


cor ( na.omit(d.all[d.all$date %in% d.weather.subs.621$date , 'precip.osv']) , na.omit(d.weather.subs.621[d.weather.subs.621$date %in% d.all$date,'precip']))
