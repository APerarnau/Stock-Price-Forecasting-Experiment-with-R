#Stock analysis long y short

library('ggplot2')
library('forecast')
library('tseries')
library('quantmod')
#library('Hmisc')
#library()

f <- file.choose()
f

stock <- read.csv(f, header = T, sep = ',')
head(stock)
tail(stock, 20)

#check tail and remove lines as needed
#better to start on Monday
stock = stock[3:1258,]
View(stock)

#stock 2yr of data
#check to begin on Monday
stock6m = stock[746:1258,]
rownames(stock6m) <- 1:nrow(stock6m)
View(stock6m)

# Conversion to time series Year, day, month
#check that the final date is as close as possible to the real one

#start     = year and fraction of year in decimal, 
#            if the year is 2016, month 11 and day 17    =    2016 + 10/12  + 17/365
#frequency = 252, aproximately, number of trading days for long

first(stock)
last(stock)
stockOpen   = ts(stock['Open'],   start = c(2014.016, 1), frequency = 252)
tail(stockOpen)
first(stock6m)
last(stock6m)
stockOpen6m = ts(stock6m['Open'], start = c(2016.974, 1), frequency = 253)
tail(stockOpen6m)

# plot, just to look at and check the correct data is loaded in
par(mfrow=c(2,1))
par(mar=c(1,4,4,4))
plot(stockOpen,   ylab ='Open Price 5yr', col = 'blue')
par(mar=c(4,4,1,4))
plot(stockOpen6m, ylab ='Open Price 2yr', col = 'red')

#-------------------------  Converting Data into Regularly Spaced Time series -------

stockDf = as.data.frame(stock)
stockDf6m = as.data.frame(stock6m)

stockDf['Date']   = as.Date(stockDf[['Date']], '%Y-%m-%d')
stockDf6m['Date'] = as.Date(stockDf6m[['Date']], '%Y-%m-%d')

head(stockDf)
tail(stockDf)

#make a sequence of date values Year Month Day
full_dates = seq.Date(from = as.Date('2014-01-06'), to = as.Date('2018-12-31'), by = 'day')
#head(full_dates)
#tail(full_dates)

# idem 6m
head(stockDf6m)
tail(stockDf6m)
full_dates6m = seq.Date(from = as.Date('2016-12-19'), to = as.Date('2018-12-31'), by = 'day')
#head(full_dates6m)
#tail(full_dates6m)

#pack it into dataframes
full_dates   = data.frame(Date = full_dates)
full_dates6m = data.frame(Date = full_dates6m)

# Padding with full_dates
full_Stock   = merge(stockDf,   full_dates,   by ='Date', all.y = T)
full_Stock6m = merge(stockDf6m, full_dates6m, by ='Date', all.y = T)

# remove initial days to start on Monday if necesary
View(full_Stock)
full_Stock   = full_Stock[1:1821,]

View(full_Stock6m)
full_Stock6m = full_Stock6m[1:743,]

#removing sundays and saturdays, 
full_Stock = full_Stock[-(seq(from = 7, to = nrow(full_Stock), by =7)),]
full_Stock = full_Stock[-(seq(from = 6, to = nrow(full_Stock), by =6)),]
full_Stock6m = full_Stock6m[-(seq(from = 7, to = nrow(full_Stock6m), by =7)),]
full_Stock6m = full_Stock6m[-(seq(from = 6, to = nrow(full_Stock6m), by =6)),]

#use last observation carried forward to fill absent data 
full_Stock   = na.locf(full_Stock)
full_Stock6m = na.locf(full_Stock6m) 
#full_Stock6m = na.locf(full_Stock6m, option = "nocb") # next option carried backward if needed  
#full_Stock6m = na.locf(full_Stock6m, option = "nocb", na.remaining = "rm") # next option carried backward if needed  
#manually replace one value below
#full_Stock6m['4', 'Close'] <- full_Stock6m['5', 'Close']
#-------------------------- Forecasts --------------------------------------------------

#convert close price data into a numeric format

first(full_Stock)

arranco      = 2014.016
frecuencia   = 261
close_price = unlist(full_Stock['Close'])
close_price = ts(as.numeric(close_price),     frequency = c(frecuencia), start = c(arranco))
last(full_Stock)
tail(close_price)

first(full_Stock6m)

arranco6m      = 2016.974
frecuencia6m   = 263
close_price6m = unlist(full_Stock6m['Close'])
close_price6m = ts(as.numeric(close_price6m), frequency = c(frecuencia6m), start = c(arranco6m))
last(full_Stock6m)
tail(close_price6m)

#------------------------------------ FANS -------------------------------
#random walk with drift
md =   rwf(close_price,   h = 90, drift = T, level = c(68,95), fan = FALSE, lambda = NULL)
md6m = rwf(close_price6m, h = 90, drift = T, level = c(68,95), fan = FALSE, lambda = NULL)
#plot bonito, no tan practico, incluir fan = TRUE arriba

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
plot(md, ylab = "Forecast rwd", col = 'blue', include = 290)
grid(NULL, NULL, col = 'black')
par(mar=c(4,4,2,4))
plot(md6m, ylab = "Forecast rwd", col = 'red', include = 290)
grid(NULL, NULL, col = 'black')


#Holt's method damped
#fitHoltD   = holt(close_price,   gamma = F, level = c(68,95), h = 90, damped = T)
#fitHoltD6m = holt(close_price6m, gamma = F, level = c(68,95), h = 90, damped = T)

#par(mfrow=c(2,1))
#par(mar=c(2,4,4,4))
#plot(forecast(fitHoltD),   ylab = "Price", col = 'blue', include = 290)
#grid(NULL, NULL, col = 'black')
#par(mar=c(4,4,2,4))
#plot(forecast(fitHoltD6m), ylab = "Price", col = 'red', include = 290)
#grid(NULL, NULL, col = 'black')

#Holt's method Not damped
fitHoltnD =   holt(close_price,   gamma = F, level = c(68,95), h = 90, damped = F)
fitHoltnD6m = holt(close_price6m, gamma = F, level = c(68,95), h = 90, damped = F)

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
plot(forecast(fitHoltnD),  ylab = "Price", col = 'blue', include = 290)
#ylim = (c(as.integer(min(close_price)), as.integer(max(close_price))
grid(NULL, NULL, col = 'black')


par(mar=c(4,4,2,4))
plot(forecast(fitHoltnD6m), ylab = "Price", col = 'red', include = 290)
grid(NULL, NULL, col = 'black')

# Arima model, be careful, it can take a lot of time
#stockArima =   auto.arima(close_price,   stepwise = T, approximation = T, trace = T)
#stockArima6m = auto.arima(close_price6m, stepwise = T, approximation = T, trace = T)

#arima(0,1,0) with drift, coefficients depend on the run above, choose the best with drift
fitArimaDrift   = Arima(close_price,   order=c(0,1,0), include.drift = T)
fitArimaDrift6m = Arima(close_price6m, order=c(0,1,0), include.drift = T)

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
plot(forecast(fitArimaDrift,   h = 90), ylab = "Price", col = 'blue', include = 290)
grid(NULL, NULL, col = 'black')
par(mar=c(4,4,2,4))
plot(forecast(fitArimaDrift6m, h = 90), ylab = "Price", col = 'red',  include = 290)
grid(NULL, NULL, col = 'black')

#----------------------------------------- Decomposition -----------------------
close_price4stl   = unlist(full_Stock['Close'])
close_price4stl6m = unlist(full_Stock6m['Close'])

# the larger the frequency term the smoother the trend

close_price4stl   = ts(as.numeric(close_price4stl),   frequency = c(frecuencia),  start = c(arranco))
tail(close_price4stl)

close_price4stl6m = ts(as.numeric(close_price4stl6m), frequency = c(frecuencia6m),  start = c(arranco6m))
tail(close_price4stl6m)
#check on the prints that final dates are as close as possible to the data

close_price4stl   = stl(close_price4stl,   s.window = 16, t.window = 36, robust = TRUE)
close_price4stl6m = stl(close_price4stl6m, s.window = 16, t.window = 36, robust = TRUE)
#
#t.window controls wiggliness of trend    component, 
#s.window controls wiggliness of seasonal component
#the larger the number the less wiggliness

plot(close_price4stl)

plot(close_price4stl6m)

#------------------------------------------Predictions-------------------------
# to change predictions play with      s.window  y t.window

# adjust these variables to fit the plot
ydwn = 200
yup  = 300

# stl+random walk prediction 
prediccionStl   = forecast(close_price4stl,   method = 'naive', level = c(68,95), h = 90)
prediccionStl6m = forecast(close_price4stl6m, method = 'naive', level = c(68,95), h = 90)

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
plot(forecast(prediccionStl,   h = 90), ylab = "Price", col = 'blue', 
     include = 90, xlim =c(2018.65, 2019.35), ylim = c(ydwn,yup))
grid(NULL, NULL, col = 'black')
par(mar=c(4,4,2,4))
plot(forecast(prediccionStl6m, h = 90), ylab = "Price", col = 'red', 
     include = 90, xlim =c(2018.65, 2019.35), ylim = c(ydwn,yup))
grid(NULL, NULL, col = 'black')

# rwdrfit prediction
prediccionRwd =   forecast(close_price4stl,   method = 'rwd', level = c(68,95), h = 90)
prediccionRwd6m = forecast(close_price4stl6m, method = 'rwd', level = c(68,95), h = 90)

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
plot(forecast(prediccionRwd,   h = 90), ylab = "Price", col = 'blue', 
     include = 90, xlim =c(2018.65, 2019.35), ylim = c(ydwn,yup))
grid(NULL, NULL, col = 'black')
par(mar=c(4,4,2,4))
plot(forecast(prediccionRwd6m, h = 90), ylab = "Price", col = 'red',  
     include = 90, xlim =c(2018.65, 2019.35), ylim = c(ydwn,yup))
grid(NULL, NULL, col = 'black')

# ets prediction
prediccionEts   = forecast(close_price4stl,   method = 'ets', level = c(68,95), h = 90)
prediccionEts6m = forecast(close_price4stl6m, method = 'ets', level = c(68,95), h = 90)

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
plot(prediccionEts, ylab = "Price", col = 'blue', 
     include = 90, xlim =c(2018.65, 2019.35), ylim = c(ydwn,yup))
grid(NULL, NULL, col = 'black')
par(mar=c(4,4,2,4))
plot(prediccionEts6m, ylab = "Price", col = 'red',  
     include = 90, xlim =c(2018.65, 2019.35), ylim = c(ydwn,yup))
grid(NULL, NULL, col = 'black')

# Arima prediction
prediccionArima   = forecast(close_price4stl,   method = 'arima', h = 90, level = c(68,95) )
prediccionArima6m = forecast(close_price4stl6m, method = 'arima', h = 90, level = c(68,95) )

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
plot(prediccionArima, ylab = "Price", col = 'blue', include = 90,
      xlim =c(2018.65, 2019.35), ylim = c(ydwn,yup))
grid(NULL, NULL, col = 'black')
par(mar=c(4,4,2,4))
plot(prediccionArima6m, ylab = "Price", col = 'red', include = 90,
      xlim =c(2018.65, 2019.35), ylim = c(ydwn,yup))
grid(NULL, NULL, col = 'black')

# Arima Long prediction
prediccionArima   = forecast(close_price4stl,   method = 'arima', h = 180, level = c(68,95))
prediccionArima6m = forecast(close_price4stl6m, method = 'arima', h = 180, level = c(68,95))

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
marco = 0.1
plot(prediccionArima, ylab = "Price", col = 'blue', xlim =c(2017.7, 2019.7), ylim = c(ydwn*(1 - marco),  yup*(1 + marco)))
grid(NULL, NULL, col = 'black')
par(mar=c(4,4,2,4))
plot(prediccionArima6m, ylab = "Price", col = 'red', xlim =c(2017.7, 2019.7), ylim = c(ydwn*(1 - marco), yup*(1 + marco)))
grid(NULL, NULL, col = 'black')

# Ets Long prediction
prediccionEts   = forecast(close_price4stl,   method = 'ets', h = 180, level = c(68,95))
prediccionEts6m = forecast(close_price4stl6m, method = 'ets', h = 180, level = c(68,95))

par(mfrow=c(2,1))
par(mar=c(2,4,4,4))
marco = 0.1
plot(prediccionEts, ylab = "Price", col = 'blue', xlim =c(2017.7, 2019.7), ylim = c(ydwn*(1 - marco),  yup*(1 + marco)))
grid(NULL, NULL, col = 'black')
par(mar=c(4,4,2,4))
plot(prediccionEts6m, ylab = "Price", col = 'red', xlim =c(2017.7, 2019.7), ylim = c(ydwn*(1 - marco), yup*(1 + marco)))
grid(NULL, NULL, col = 'black')

#                                 Fourier predictions --- still working on them!!!
#plot 
close_price = unlist(full_Stock['Close'])

par(mfrow=c(3,1))
plot(stockOpen,   ylab ='Open Price', col = 'blue')

#plot with index before Fourier
plot(index(full_Stock), as.numeric(unlist(full_Stock['Open'])), type = 'l', col = 'blue')

#linear trend
tendencia = lm(as.numeric(unlist(full_Stock['Open'])) ~ index(full_Stock), data = stock)
abline(tendencia, col = 'red')

#remove linear trend
noTendencia = tendencia$residuals
plot(index(full_Stock), noTendencia, type = 'l', col = 'blue')

#Fourier

# repeat plots
par(mfrow=c(3,1))
plot(index(full_Stock), noTendencia, type = 'l', col = 'blue', main = 'Original Stock')

#ahora Fourier
fourierStock = fft(noTendencia)

#usually 64 harmonics are enough
armonicos = 64
ene = length(fourierStock)

plot(Mod(fourierStock[1:armonicos]), type = 'h', main = 'Stock Spectrum')

filtro = rep(0, ene)
filtro[0:armonicos] = 1

#plot filtered stock
stockFiltered = filtro * fourierStock
plot(index(stockFiltered), y=Re(fft(stockFiltered, inverse=TRUE) / ene ) , type='l', 
     main = 'Filtered Stock')

#go forward 200 index units
forward = 200
forwardIndex = (tail(index(stockFiltered), n = 1) + 1):(tail(index(stockFiltered), n = 1) + forward)

summary(tendencia)
be = coef(summary(tendencia))[1, 1]
eme =  coef(summary(tendencia))[2, 1]

tendenciaFutura = eme*forwardIndex + be

stockPredicto = Re(fft(stockFiltered, inverse=TRUE) / ene )[1:forward] + tendenciaFutura

# see results
par(mfrow=c(3,1))
plot(index(stock), as.numeric(unlist(stock['Open'])), type = 'l', col = 'blue')

#the following plot is in index units
plot(stockPredicto, type ='l', col = 'darkblue')

#snapping prediction to the end of data
#results tend to be very smoothed and influenced by the linear trend. I have to improve that...
stockYpredicto = c( as.numeric(unlist(stock['Open'])), stockPredicto)
longo = length(stockYpredicto)
plot(stockYpredicto[(longo - 1.8*forward):longo], type ='l', col = 'darkblue', ylim = c(ydwn*(1 - marco), yup*(1 + marco)))
grid(NULL, NULL, col = 'black')

#                                          Fourier 6m predictions
#same procedures as before
close_price6m = unlist(full_Stock6m['Close'])

par(mfrow=c(3,1))
plot(stockOpen6m,   ylab ='Open Price', col = 'blue')

plot(index(full_Stock6m), as.numeric(unlist(full_Stock6m['Open'])), type = 'l', col = 'blue')

tendencia6m = lm(as.numeric(unlist(full_Stock6m['Open'])) ~ index(full_Stock6m), data = stock)
abline(tendencia6m, col = 'red')

noTendencia6m = tendencia6m$residuals
plot(index(full_Stock6m), noTendencia6m, type = 'l', col = 'blue')

#Fourier

par(mfrow=c(3,1))
plot(index(full_Stock6m), noTendencia6m, type = 'l', col = 'blue', main = 'Original Stock')

fourierStock6m = fft(noTendencia6m)

armonicos6m = 64
ene6m = length(fourierStock6m)

plot(Mod(fourierStock6m[1:armonicos6m]), type = 'h', main = 'Stock Spectrum')

filtro6m = rep(0, ene6m)
filtro6m[0:armonicos6m] = 1

stockFiltered6m = filtro6m * fourierStock6m
plot(index(stockFiltered6m), y=Re(fft(stockFiltered6m, inverse=TRUE) / ene6m ) , type='l', 
     main = 'Filtered Stock')

forward6m = 200
forwardIndex6m = (tail(index(stockFiltered6m), n = 1) + 1):(tail(index(stockFiltered6m), n = 1) + forward6m)

summary(tendencia6m)
be6m = coef(summary(tendencia6m))[1, 1]
eme6m =  coef(summary(tendencia6m))[2, 1]

tendenciaFutura6m = eme6m*forwardIndex6m + be6m
stockPredicto6m = Re(fft(stockFiltered6m, inverse=TRUE) / ene6m )[1:forward6m] + tendenciaFutura6m

#check results
par(mfrow=c(3,1))
plot(index(stock6m), as.numeric(unlist(stock6m['Open'])), type = 'l', col = 'blue')

plot(stockPredicto6m, type ='l', col = 'darkblue')

stockYpredicto6m = c( as.numeric(unlist(stock['Open'])), stockPredicto6m)
longo6m = length(stockYpredicto6m)
plot(stockYpredicto6m[(longo6m - 1.8*forward6m):longo6m], type ='l', col = 'darkblue', ylim = c(ydwn*(1 - marco), yup*(1 + marco)))
grid(NULL, NULL, col = 'black')

# 2 Fourier Long and 6m plot
par(mfrow=c(2,1))
plot(stockYpredicto[(longo - 3*forward):longo], type ='l', col = 'darkblue', ylim = c(ydwn*(1 - marco), yup*(1 + marco)))
grid(NULL, NULL, col = 'black')
plot(stockYpredicto6m[(longo6m - 3*forward6m):longo6m], type ='l', col = 'red', ylim = c(ydwn*(1 - marco), yup*(1 + marco)))
grid(NULL, NULL, col = 'black')


