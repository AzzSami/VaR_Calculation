rm(list=ls(all=TRUE))
setwd("C:/Users/Utilisateur/OneDrive/Iref M2/Semestre 1/Value at risk/Projet")
library(QuantTools) 
library(xts)
library(TSA)
library(tseries)
library(FinTS)
library(lmtest)
library(forecast) 
library(CADFtest)
library(moments)
library(urca)
library(BatchGetSymbols)
library(forecast)
library(lmtest)
library(TSA)
library(tseries)
library(FinTS)




# set dates
first.date <- "2010-01-04"
last.date <- "2021-10-31"
freq.data <- 'daily'
type.return<-'log'
# set tickers
tickers <- '4911.T'
tab <- BatchGetSymbols(tickers = tickers, 
                       first.date = first.date,
                       last.date = last.date, 
                       freq.data = freq.data,
                       type.return=type.return,
                       cache.folder = file.path(tempdir(), 
                                                'BGS_Cache') )
head(tab$df.tickers)
pt<-tab$df.tickers$price.adjusted
dates<-tab$df.tickers$ref.date

dpt=diff(pt)
rt=tab$df.tickers$ret.adjusted.prices[-1]
N<-length(rt)
rte=rt[1:1966]
rtt=rt[1967:N]

length(rte)
length(rt)
###################################################
### Préparation des dates
###################################################

dates_rt = dates[-1]
dates_rte = dates_rt[1:1966]
dates_rte[1966]

###################################################
### Analyse du chronogramme
###################################################


op<-par(mfrow=c(3,1))
plot(dates,pt,type='l',ylab="indice Shiseido",col=3)
plot(dates[2:length(dates)],dpt,type='l',col=2,ylab="variations de Shiseido")
plot(dates[2:length(dates)],rt,type='l',col=1,ylab="rendement de Shiseido")
par(op)

###################################################
### Tests de Racine Unitaire
###################################################


#Dickey-Fuller

summary(ur.df(rte,type="trend",lag=0))

summary(ur.df(rte,type="drift",lag=0))

summary(ur.df(rte,type="none",lag=0))


#Autocorrelation des résidus 

plot(ur.df(rte,type="none",lag=0))

#Critère de Schwert

length(rte)
Schwert=as.integer(12*(length(rte)/100)^(0.25))
print(Schwert)

pmax=Schwert

summary(CADFtest(rte,criterion="MAIC",type="none",max.lag.y=Schwert))

#Test de Dickey-Fuller augmenté

summary(ur.df(rte,type="none",lag=8))


summary(ur.df(rte,type="none",lag=7))
summary(ur.df(rte,type="none",lag=6))
summary(ur.df(rte,type="none",lag=5))
summary(ur.df(rte,type="none",lag=4))
summary(ur.df(rte,type="none",lag=3))
summary(ur.df(rte,type="none",lag=2))
summary(ur.df(rte,type="none",lag=1))
summary(ur.df(rte,type="none",lag=0))


#Deuxième analyse top-down

summary(ur.df(rte,type="none",lag=pmax))
summary(ur.df(rte,type="none",lag=pmax-1))
summary(ur.df(rte,type="none",lag=pmax-2))
summary(ur.df(rte,type="none",lag=pmax-3))
summary(ur.df(rte,type="none",lag=pmax-4))
summary(ur.df(rte,type="none",lag=pmax-5))


#Tests de Z.A

#Selon le MAIC
summary(ur.za(rte,model = "both",lag= 8))
summary(ur.za(rte,model = "intercept",lag= 8))
summary(ur.za(rte,model = "intercept",lag= 7))
summary(ur.za(rte,model = "intercept",lag= 6))
summary(ur.za(rte,model = "intercept",lag= 5))
summary(ur.za(rte,model = "intercept",lag= 4))
summary(ur.za(rte,model = "intercept",lag= 3))
summary(ur.za(rte,model = "intercept",lag= 2))
summary(ur.za(rte,model = "intercept",lag= 1))
summary(ur.za(rte,model = "intercept",lag= 0))


#Selon Top-Down

summary(ur.za(rte,model = "both",lag= 20))

#Date de rupture (TB) est à la 1373-ième observation : "2015-08-10"
dates_rte = dates_rt[1:1966]
Rupture = dates_rte[1373]
Rupture

#Graphique 
plot(ur.za(rte, model="both",lag=20))

#Test de LS

#Commande pour importer le code ne foncitonne pas, passer par l'onglet Code
source("C:\\Users\\Utilisateur\\OneDrive\\Iref M2\\Semestre 1\\Value at risk\\Projet\\LeeStrazicichUnitRootTest.R")

#1 date de rupture
myBreaks <- 1

#Procedure Top-Down
myModel <- "break"
myLags <- 20

myLS_test <- ur.ls(y=rte, model = myModel, breaks = myBreaks, lags = myLags, method = "GTOS",pn = 0.1, print.results = "print" )

#Procedure MAIC
myModel <- "break"
myLags <- 8

myLS_test <- ur.ls(y=rte, model = myModel, breaks = myBreaks, lags = myLags, method = "GTOS",pn = 0.1, print.results = "print" )


#2 dates de rupture
myBreaks <- 2

#Procedure Top-Down
myModel <- "break"
myLags <- 20

myLS_test <- ur.ls(y=rte, model = myModel, breaks = myBreaks, lags = myLags, method = "GTOS",pn = 0.1, print.results = "print" )

#Date du break avec 1 date de rupture

dates_rte[1744]

###################################################
### Skewness
###################################################

agostino.test(rte)

###################################################
### Kurtosis
###################################################


anscombe.test(rte)

###################################################
### Autocorrélation (des carrés) des rendements
###################################################


#ACF

op<-par(mfrow=c(2,1))
Acf(rte,main= 'ACF du rendement logarithmique' )
Acf(rte^2,main= 'ACF du rendement logarithmique au carré' )
par(op)

#PACF

op<-par(mfrow=c(2,1))
Pacf(rte,main= 'ACF du rendement logarithmique' )
Pacf(rte^2,main= 'ACF du rendement logarithmique au carré' )
par(op)


#Test de Ljung-box


pvaluesrte =rep(0,40)
pvaluesrte2 =rep(0,40)
for (i in 1:40 ) {
  pvaluesrte[i] = Box.test(rte,lag=i,type="Ljung-Box")$p.value
  pvaluesrte2[i] = Box.test(rte^2,lag=i,type="Ljung-Box")$p.value
}
#Rendements normaux
pvaluesrte

#Rendements au carré 
pvaluesrte2


###################################################
### TEST ARMA 
###################################################

eacf(rte)

reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,NA,NA,NA,NA,NA,0,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,NA,NA,NA,NA,0,0,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,NA,NA,NA,0,0,0,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,NA,NA,0,0,0,0,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,NA,0,0,0,0,0,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,NA,0,0,0,0,0,0,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(NA,0,0,0,0,0,0,0,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(0,0,0,0,0,0,0,0,0,0,NA,NA))
coeftest(reg1)
reg1 <-Arima(rte, order=c(0,0,11),fixed=c(0,0,0,0,0,0,0,0,0,0,NA,0))
coeftest(reg1)
BIC(reg1)
#-10245.17, meilleure BIC actuellement

#MODELE ANNEXES 
#Deuxieme meilleure modele 
reg2 <-Arima(rte, order=c(1,0,12),fixed=c(0,0,0,0,0,0,0,0,0,0,0,NA,0,0))
coeftest(reg2)
BIC(reg2)
#-10245.17, moins bon car ARMA(0,11) est probablement meilleure que ARMA(1,12), on minimise plus les p et q du modèle.

#1/ Modèle marche pas car NaN 
reg3 <-Arima(rte, order=c(2,0,2),fixed=c(NA,NA,NA,NA,NA))
coeftest(reg3)
BIC(reg3)
#-10210.56, moins bon 

reg3 <-Arima(rte, order=c(2,0,3),fixed=c(NA,NA,NA,NA,NA))
coeftest(reg3)
BIC(reg3)
#-10210.56, moins bon 

reg3 <-Arima(rte, order=c(2,0,4),fixed=c(NA,NA,NA,NA,NA))
coeftest(reg3)
BIC(reg3)
#-10210.56, moins bon 

reg3 <-Arima(rte, order=c(2,0,5),fixed=c(NA,NA,NA,NA,NA))
coeftest(reg3)
BIC(reg3)
#-10210.56, moins bon 

reg3 <-Arima(rte, order=c(2,0,6),fixed=c(NA,NA,NA,NA,NA,NA))
coeftest(reg3)
BIC(reg3)
#-10210.56, moins bon 

reg3 <-Arima(rte, order=c(2,0,7),fixed=c(NA,NA,NA,NA,NA,NA,NA))
coeftest(reg3)
BIC(reg3)
#-10210.56, moins bon 

#2/ Modèle marche pas car NaN 
reg4 <-Arima(rte, order=c(3,0,3),fixed=c(NA,NA,NA,NA,NA,NA,NA))
coeftest(reg4)

reg4 <-Arima(rte, order=c(3,0,3),fixed=c(NA,NA,NA,NA,NA,0,NA))
coeftest(reg4)

reg4 <-Arima(rte, order=c(3,0,3),fixed=c(NA,NA,0,NA,NA,NA,NA))
coeftest(reg4)
BIC(reg4)
#-10203.3, moins bon.

reg4bis <-Arima(rte, order=c(4,0,4),fixed=c(NA,NA,NA,NA,0,NA,NA,NA,NA))
coeftest(reg4bis)
BIC(reg4bis)

#3/ Modèle marche pas car NaN 
reg5 = Arima(rte, order = c(6,0,7))
reg5 = Arima(rte, order = c(6,0,7),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,NA))
coeftest(reg5)

reg5 = Arima(rte, order = c(6,0,7),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,0))
coeftest(reg5)

reg5 = Arima(rte, order = c(6,0,7),fixed=c(NA,NA,NA,NA,0,NA,NA,NA,NA,NA,0,NA,0,NA))
coeftest(reg5)

reg5 = Arima(rte, order = c(6,0,7),fixed=c(NA,NA,NA,NA,0,NA,NA,0,NA,NA,0,NA,0,NA))
coeftest(reg5)

reg5 = Arima(rte, order = c(6,0,7),fixed=c(NA,0,NA,NA,0,NA,NA,0,NA,NA,0,NA,0,NA))
coeftest(reg5)
#On aboutit à des valeurs manquantes
BIC(reg5)
#-10187.51, moins bon 

#4/ Modèle marche pas car NaN 

reg6 <-Arima(rte, order=c(5,0,5),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
coeftest(reg6)

reg6 <-Arima(rte, order=c(5,0,5),fixed=c(NA,NA,NA,NA,0,NA,NA,NA,NA,0,NA))
coeftest(reg6)

reg6 <-Arima(rte, order=c(5,0,5),fixed=c(NA,NA,NA,NA,0,NA,NA,NA,NA,0,0))
coeftest(reg6)
#Valeurs manquantes
BIC(reg6)
#-10192.49, moins bon 


###################################################
### RÉSIDUS 
###################################################

residu<-reg1$res
t.test(residu)

###################################################
### STANDARDISATION DES RÉSIDUS 
###################################################
residuv=(residu-mean(residu))/sd(residu)
K<-40
tmp<-rep(0,K)
for(i in 1:K){
  tmp[i]<-Box.test(residuv,lag=i,type="Ljung-Box")$p.value
}
tmp

###################################################
### BIC
###################################################
BIC(reg1)


###################################################
### CLUSTER DE VOLATILITÉ
###################################################

LM1<-ArchTest(as.numeric(rte),lag=1)
LM1


LM2<-ArchTest(as.numeric(rte),lag=2)
LM2

LM10<-ArchTest(as.numeric(rte),lag=10)
LM10

LM20<-ArchTest(as.numeric(rte),lag=20)
LM20


LM30<-ArchTest(as.numeric(rte),lag=30)
LM30


LM40<-ArchTest(as.numeric(rte),lag=40)
LM40


###################################################
### GARCH
###################################################

volat<-garch(residuv,order=c(1,1))
summary(volat)

ArchTest(volat$res,lag=1)
ArchTest(volat$res,lag=2)
ArchTest(volat$res,lag=3)
ArchTest(volat$res,lag=4)
ArchTest(volat$res,lag=5)

ArchTest(volat$res,lag=6)

ArchTest(volat$res,lag=10)
ArchTest(volat$res,lag=20)
ArchTest(volat$res,lag=30)
ArchTest(volat$res,lag=40)

###################################################
### QUEUE DE DISTRIBUTION DES RÉSIDUS
###################################################

anscombe.test(volat$res)

N_rte=length(rte)

###################################################
### EFFET DE LEVIER
###################################################

sig<-rep(0,N_rte)
for(t in 1:N_rte)
{
  sig[t]<-sqrt(sum(rte[t-22]-(sum(rte[t-22]/22)))^2/22)
}
sigma=sig[24:N_rte]*100
plot(log(pt[24:N_rte]),type='l',col=2,axes=F,xlab="", ylab="")
axis(2,at=seq(0.5,5,by=0.25))
par(new=T)
plot(sigma,col=3,type='l',axes = F,xlab="",ylab="",sub = "Logarithme de Shiseido journalier et écart-type récursif journalier des rendements")
axis(4,at=seq(0,3,by=0.25))
legend("topleft", c("log(pt)","sigma"),col = c(2,3),lty=c(1,1))


###################################################
### SAISONNALITÉ
###################################################


#EFFET WEEK-END

jour=format(dates_rte, format = "%A")
tableaures <- data.frame(matrix(NA,ncol=5,nrow=4))
colnames(tableaures) <- c("lundi","mardi","mercredi","jeudi","vendredi")
rownames(tableaures) <- c("moyenne en %","écart-type annuel en %","skewness","kurtosis")

rtemar<-as.numeric(rte[jour=="mardi"])
mardi<-mean(rtemar) #moyenne journaliere
tableaures[1,2] <- mardi*100 #moyenne journaliere en %
tableaures[2,2] <- sd(rtemar)*100*sqrt(252) #ecart-type annualise en %
tableaures[3,2] <- skewness(rtemar)
tableaures[4,2] <- kurtosis(rtemar)

rtemer<-as.numeric(rte[jour=="mercredi"])
mer<-mean(rtemer)
tableaures[1,3] <- mer*100
tableaures[2,3] <- sd(rtemer)*100*sqrt(252)
tableaures[3,3] <- skewness(rtemer)
tableaures[4,3] <- kurtosis(rtemer)

rtejeu<-as.numeric(rte[jour=="jeudi"])
jeudi<-mean(rtejeu)
tableaures[1,4] <- jeudi*100
tableaures[2,4] <- sd(rtejeu)*100*sqrt(252)
tableaures[3,4] <- skewness(rtejeu)
tableaures[4,4] <- kurtosis(rtejeu)

rteven<-as.numeric(rte[jour=="vendredi"])
ven<-mean(rteven)
tableaures[1,5] <- ven*100
tableaures[2,5] <- sd(rteven)*100*sqrt(252)
tableaures[3,5] <- skewness(rteven)
tableaures[4,5] <- kurtosis(rteven)

rtelun<-as.numeric(rte[jour=="lundi"])
lundi<-mean(rtelun)
tableaures[1,1] <- lundi*100
tableaures[2,1] <- sd(rtelun)*100*sqrt(252)
tableaures[3,1] <- skewness(rtelun)
50
tableaures[4,1] <- kurtosis(rtelun)
tableaures

#EFFET JANVIER 


monthplot(rte, ylab="Rendement",main="Rendement logarithmique de l'action Shiseido par mois", cex.main=1,col.base=2,lwd.base=3,col = 8)

