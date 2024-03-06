#+ Relazione di serie storiche
#+ Jacopo D'alto
#+ Anna Chiara Mattazzi
#+ Luca Quercini
#+ Sonia Spinelli


##########################################################################
##                        OPERAZIONI PRELIMINARI                        ##
##########################################################################

library(forecast)
library(ggplot2)
library(dplyr)
library(xts)

# https://www.kaggle.com/datasets/sumanthvrao/daily-climate-time-series-data
data<-read.table("C:/Users/sonia/OneDrive/Desktop/UNI/2' ANNO/2 SEM/STAT APPL/SERIE STORICHE/labo/DailyDelhiClimateTrain.csv", dec=".", sep=",",  header=T)
attach(data)

# controllo dei valori anomali: 
summary(meantemp)
summary(meanpressure) #pressione negativa e pressione massima: impossibili
summary(humidity)
summary(wind_speed)

# sostituzione dei dati anomali
par(mfrow=c(1,2))
boxplot(meanpressure, main="Boxplot meanpressure con valori anomali")
detach(data)
data$meanpressure[data$meanpressure >1083 | data$meanpressure <948]<- mean(data$meanpressure)
attach(data)
summary(meanpressure)
boxplot(meanpressure, main="Boxplot meanpressure ripulito")
par(mfrow=c(1,1))

# correlazioni
data2=cbind(meantemp, meanpressure, wind_speed, humidity)
matrice_correlazione=cor(data2)
print(round(matrice_correlazione, 3))


#regressione di meantemp a partire da meanpreassure
# sono le due variabili piu correlate con correlazione =
cor(meantemp, meanpressure)
lm(meantemp ~ meanpressure)
reg<-lm(meantemp ~ meanpressure)
plot(meanpressure, meantemp, cex=1, col="red", pch=20, main="Regressione lineare" )
abline(reg, col="blue", lwd=2)
summary(reg)

#R quadro di 0.77

###############################################################
##                 plot delle serie storiche                 ##
###############################################################

data$date <- as.Date(date)

#serie storica della temperatura
temp_ts<- ts(meantemp,frequency=365, start=c(2013,1)) 
is.ts(temp_ts)
start(temp_ts)
end(temp_ts)
plot.ts(temp_ts, main="Temperatura")

#pressione
pr_ts<- ts(data$meanpressure,frequency=365, start=c(2013,1)) 
plot.ts(pr_ts, main="Pressione")

#humidity
hum_ts<- ts(humidity,frequency=365, start=c(2013,1)) 
plot.ts(hum_ts, main="Umidità")

#vento
win_ts<- ts(wind_speed,frequency=365, start=c(2013,1)) 
plot.ts(win_ts, main="Velocità del vento")


data_ts=ts(data[c("meantemp", "meanpressure")],frequency=365, start=c(2013,1))
plot.ts(data_ts, main="Serie storiche della Temperatura e Pressione")

################# ANALISI DI SERIE STORICHE ####################

#dati settimanali
weeklymean = apply.weekly(data, mean)
weeklymean_ts = ts(weeklymean, start = c(2013,01,06), deltat=1/52)
plot.ts(weeklymean_ts)

###############################################################
##                        TEMPERATURA                        ##
###############################################################

plot(decompose(weeklymean_ts[,1])) 
ggseasonplot(weeklymean_ts[,1],col=c("red", "orange", "green", "blue", "violet"), main="Plot stagionale temperatura")
lag.plot(weeklymean_ts[,1],16, set.lags =seq(1,16*4 , b =4))

# c'è un trendo positivo ma molto piccolo : range di valori (24.5 - 27)
# c'è stagionalità annuale
# la varianza si direbbe abbastanza stazionaria
# l'errore è piuttosto aleatorio: è centrato nello zero e oscilla tra -6 e 6, valori piuttosto bassi

trasf_temp<-weeklymean_ts[,1]  %>% 
  diff(lag=52) %>% 
  diff()
ggtsdisplay(trasf_temp, main="Temperatura") 


#################### diagnostica dei resudui ####################
#standardizzo i residui
std = (trasf_temp- mean(trasf_temp))/sd(trasf_temp)

#qqplot
par(mfrow=c(1,3)) 
qqnorm(meantemp, xlim=c(-3,3), main="Q-Q Plot valore originale") 
qqline(meantemp) 
qqnorm(trasf_temp, ylim=c(-6,6), xlim=c(-3,3), main="Q-Q Plot residui") 
qqline(trasf_temp)
qqnorm(std, ylim=c(-6,6), xlim=c(-3,3), main="Q-Q Plot residui standardizzati") 
qqline(std)
par(mfrow=c(1,1)) 



#################### Formulazione del modello ####################
#+ p=4
#+ d=1
#+ q=1
#+ P=0
#+ D=1
#+ Q=1
#+ s=52
#+ SARIMA(p,d,q)(P,D,Q)s

weeklymean_ts[,1] %>%
  Arima(order=c(4,1,1), seasonal=list(order = c(0,1,1), period = 52)) %>%
  residuals() %>% ggtsdisplay(main="Modello SARIMA(4,1,1)(0,1,1)[52]")


fit<-arima(weeklymean_ts[,1],order = c(4, 1, 1), seasonal = list(order = c(0, 1, 1), period=52)) 
fit
lag.plot(fit$residuals,do.lines=FALSE) 

# i residui hanno ACF e PACF sempre dentro le bande
# il plot presenta residui abbastanza dispersi


############ confronto con il modello automatico #####################
modello<- auto.arima(weeklymean_ts[,1])
modello
modello %>% residuals() %>% ggtsdisplay(main="SARIMA(3,1,1)(1,1,0)[52]")
lag.plot(modello$residuals,do.lines=FALSE) 


########################### predizione ##############################

fit %>% 
  forecast(h = 53) %>%
  autoplot(main="Forecasts from SARIMA(4,1,1)(0,1,1)",ylab="Temperatura", fcol="red")

#95 e 80 % confidenza

#######################################################################
##                             PRESSIONE                             ##
#######################################################################

plot(decompose(weeklymean_ts[,4])) 
ggseasonplot(weeklymean_ts[,4],col=c("red", "orange", "green", "blue", "violet"), main="Plot stagionale temperatura")
lag.plot(weeklymean_ts[,4],16, set.lags =seq(1,16*4 , b =4))

#+ trend poco significatico 
#+ stagionalità annuale
#+ la varianza si direbbe abbastanza stazionaria
#+ l'errore è piuttosto aleatorio


trasf_pr<- weeklymean_ts[,4]  %>% 
  diff(lag=52) %>% 
  diff() 
  
ggtsdisplay(trasf_pr, main="Pressione")

######################## diagnostica dei resudui ##########################
#standardizzo i residui
std1 = (trasf_pr- mean(trasf_pr))/sd(trasf_pr)

#qqplot
par(mfrow=c(1,3)) 
qqnorm(meanpressure, xlim=c(-3,3), main="Q-Q Plot valore originale") 
qqline(meanpressure) 
qqnorm(trasf_pr, ylim=c(-6,6), xlim=c(-3,3), main="Q-Q Plot residui") 
qqline(trasf_pr)
qqnorm(std1, ylim=c(-6,6), xlim=c(-3,3), main="Q-Q Plot residui standardizzati") 
qqline(std1)
par(mfrow=c(1,1)) 


################### Formulazione del modello ###########################
#+ q=2
#+ p=5
#+ d=1
#+ Q=0
#+ P=0
#+ D=1
#+ s=52
#+ sARIMA(p,d,q)(P,D,Q)s

weeklymean_ts[,4] %>%
  Arima(order=c(5,1,2), seasonal=list(order = c(1,1,0), period = 52)) %>%
  residuals() %>% ggtsdisplay(main="Modello SARIMA(5,1,2)(1,1,0)[52]")


fit1<-arima(weeklymean_ts[,4],order = c(5, 1, 2), seasonal = list(order = c(1, 1, 0), period=52)) 
fit1
lag.plot(fit1$residuals,do.lines=FALSE) 

# i residui hanno ACF e PACF sempre dentro le bande, sembrano rumore bianco
# il plot presenta residui abbastanza dispersi

############### confronto con il modello automatico #####################
modello1<- auto.arima(weeklymean_ts[,4])
modello1
modello1 %>% residuals() %>% ggtsdisplay(main="Modello SARIMA(4,1,1)(1,1,0)[52]")
lag.plot(modello1$residuals,do.lines=FALSE) 


############################ predizione ###############################

modello1 %>% 
  forecast(h = 53) %>%
  autoplot(main="Forecasts from SARIMA(4,1,1)(1,1,0)[52]", ylab="Pressione", fcol="cornflowerblue")

#95 e 80 % confidenza

#######################################################################
####                 autocorrelazione incrociata                   ####
#######################################################################

ggCcf(weeklymean_ts[,1], weeklymean_ts[,4]) + ggtitle( "Autocorrelazione incrociata tra Temperatura e Pressione")


detach(data)