rm(list=ls(all=TRUE))
setwd("C:/Users/Utilisateur/OneDrive/Iref M2 IPad/Value At Risk/Projet")

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
library(ghyp)
library(rugarch)
library(zoo)
library(mnormt)
library(nor1mix)
library(MASS)
library(PerformanceAnalytics)
library(gamlss)

###################################################
### Implementation du jeux de donnees
###################################################

first.date <- "2010-01-04"
last.date <- "2021-10-31"
freq.data <- 'daily'
type.return<-'log'
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
rte=rt[1:1966] #Ensemble d'estimation de 2010 à 2017 inclu
rtt=rt[1967:N] #Ensemble de test de 2018 à 2021 inclu


###################################################
### Préparation des dates
###################################################

dates_rt = dates[-1]
dates_rte = dates_rt[1:1966]
dates_rte[1966]
dates_rtt = dates_rt[1967:N]
dates_rtt[2914]

dates_rte =as.data.frame(dates_rte) #Ensemble d'estimation de 2010 à 2017 inclu

dates_rtt =as.data.frame(dates_rtt)
dates
length(rte)
length(rtt)
length(rt)

###################################################
### Analyse du chronogramme
###################################################


op<-par(mfrow=c(3,1))
plot(dates,pt,type='l',ylab="indice Shiseido",col=3)
plot(dates[2:length(dates)],dpt,type='l',col=2,ylab="variations de Shiseido")
plot(dates[2:length(dates)],rt,type='l',col=1,ylab="rendement de Shiseido")
par(op)

###################################################
### Distribution des aléas
###################################################

 
###################################################
### QQplot
###################################################

qqnorm(rte)
qqline(rte, col = 2)


###################################################
### Estimation d'une distribution normale
###################################################


fitn<-fit.gaussuv(data=rte)
summary(fitn)
hist(fitn)

#AIC = -10252.03

###################################################
### Estimation student asymétrique
###################################################


fitstu<-fit.tuv(rte)
summary(fitstu)
hist(fitstu)

#AIC = -10641.31

###################################################
### Estimation student symétrique
###################################################

fitstusy<-fit.tuv(rte, symmetric = TRUE)
summary(fitstusy)
hist(fitstusy)

#AIC = -10642.81


###################################################
### Gaussienne inverse asymétrique
###################################################


fitnig<-fit.NIGuv(data=rte,silent=T)
summary(fitnig)
hist(fitnig)
#AIC = -10642.56


###################################################
### Hyperbolique asymétrique
###################################################



fithyp<-fit.hypuv(rte)
summary(fithyp)
hist(fithyp)

#AIC = -10627.47


###################################################
### Hyperbolique généralisé asymétrique
###################################################


fitghypuv<-fit.ghypuv(rte)
summary(fitghypuv)
hist(fitghypuv)


#AIC= -10642.19

###################################################
### Estimateur par noyau de la densité des rendements et distributions estimées
###################################################

plot(density(rte))
lines(fitstu,col=2)
lines(fitstusy,col=3)
lines(fithyp,col=4)
lines(fitghypuv,col=5)
lines(fitnig,col=6)

legend("topleft",legend =c("rte","student","student symetrique","hyp","ghyp","jsu","nig"), col =1:9,lty=rep(1,9))


###################################################
### Distribution  hyperbolique généralisée t asymétrique
###################################################


library(SkewHyperbolic)
ghstfit<-skewhypFit(rte, print = FALSE, plot =FALSE, hessian = TRUE)
summary(ghstfit)

op <- par(mfrow = c(1,2))
plot(ghstfit, which = 1)
plot(ghstfit, which = 3)
par(op)


###################################################
### Modèle APARCH
###################################################


spec7 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="nig")
fit7= ugarchfit(spec = spec7,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit7)

#On fixe avec un delta = 1

spec7bis = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="nig",fixed.pars = list(delta=1))
fit7bis= ugarchfit(spec = spec7bis,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit7bis)

###################################################
### Modèle GJR-GARCH 
###################################################


spec6 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,1)),distribution.model="nig")
fit6= ugarchfit(spec = spec6,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit6)

#distribution dhyp
spec6 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="ghyp")
fit6= ugarchfit(spec = spec6,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit6)

#distribution nig avec alpha=0

spec6 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="nig",fixed.pars=list(alpha1=0))
fit6= ugarchfit(spec = spec6,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit6)

#distribution ghyp avec alpha=0
spec6 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="ghyp",fixed.pars=list(alpha1=0))
fit6= ugarchfit(spec = spec6,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit6)
#distribution std qui ne passe pas le testNyblom
spec6 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="ghyp",fixed.pars=list(alpha1=0))
fit6= ugarchfit(spec = spec6,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit6)

###################################################
### Modèle EGARCH 
###################################################


spec5 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,1)),distribution.model="nig")
fit5 = ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)
#BIC= -5.4431

#Nous voyons ici que notre gamma est significatif et positif (prise en compte de l'effet de levier), il nous reste à avoir notre alpha1 
#significatif car il est déjà négatif.

spec5 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="nig",fixed.pars = list(alpha1=0))
fit5 = ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)

#Test distribution "ghst", le BIC est meilleure, -5.4464
spec_ghst = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="ghst",fixed.pars = list(alpha1=0))
fit_ghst = ugarchfit(spec = spec_ghst, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit_ghst)

#avec JSU, distribution meilleure, -5.4475
spec_jsu = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(1,1)),distribution.model="jsu",fixed.pars = list(alpha1=0))
fit_jsu = ugarchfit(spec = spec_jsu, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit_jsu)

#avec sged, distrib moins bien, -5.4362
spec_sged = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),distribution.model="sged",fixed.pars = list(alpha1=0))
fit_sged = ugarchfit(spec = spec_sged, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit_sged)

#avec ged, distrib encore meilleure -5.4370, pas bon car pearson ne passe pas
spec_ged = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(1,1)),distribution.model="ged",fixed.pars = list(alpha1=0))
fit_ged = ugarchfit(spec = spec_ged, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit_ged)

#avec sstd, tout passe, tout est significatif et le BIC est de -5.4479

spec_sstd = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),distribution.model="sstd",fixed.pars = list(alpha1=0))
fit_sstd = ugarchfit(spec = spec_sstd, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit_sstd)

#avec std tout passe aussi, BIC de -5.4495
spec_std = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(1,1)),distribution.model="c",fixed.pars = list(alpha1=0))
fit_std = ugarchfit(spec = spec_std, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit_std)

#avec norm, pearson ne passe pas donc pas ok 
spec_norm = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),distribution.model="norm",fixed.pars = list(alpha1=0))
fit_norm = ugarchfit(spec = spec_norm, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit_norm)
norm

niegarch=newsimpact(z=NULL, fit5)
plot(niegarch$zx,niegarch$zy, xlab=niegarch$xexpr,ylab=niegarch$yexpr ,type="l", main = "Courbe des impacts des nouvelles dans le EGARCH")

#Test distribution "ghyp" pour voir si le BIC est meilleure 

spec5 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="ghyp",fixed.pars = list(alpha1=0))
fit5 = ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)
#Non le BIC est moins bien 

#Augmente le m à 2 pour elever l'effet taille dans sign bias
spec5 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(2,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="nig",fixed.pars = list(alpha1=0,alpha2=0))
fit5 = ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)

###################################################
### Modèle IGARCH 
###################################################

spec4 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),distribution.model="nig")
fit4 = ugarchfit(spec = spec4, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit4)

spec4 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(2,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="nig")
fit4 = ugarchfit(spec = spec4, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit4)

###################################################
### Modèle ARCH-M
###################################################

spec3 = ugarchspec(mean.model=list(armaOrder=c(1,1),archm=TRUE),distribution.model="nig")
fit3 = ugarchfit(spec = spec3,data = rt,out.sample=length(rtt),solver="hybrid")
fit3

niegarch=newsimpact(z=NULL, fit3)
plot(niegarch$zx,niegarch$zy, xlab=niegarch$xexpr,ylab=niegarch$yexpr ,type="l", main = "Courbe des impacts des nouvelles dans le ARCH-M")

###################################################
### #Prise en compte de la saisonnalité
###################################################

jour=format(dates, format = "%A")
mois=format(dates, format = "%B")
moisrte=mois[1:1966]
janvier=as.integer(moisrte=="janvier")
jourrte=jour[1:1966]#comme rte
lundi=as.integer(jourrte=="lundi")
spec1bis = ugarchspec(mean.model=list(external.regressors=as.matrix(cbind(lundi,janvier))))
fit1bis = ugarchfit(spec = spec1bis, data = rt, out.sample=length(rtt))
show(fit1bis)

###################################################
### NCI courbe 
###################################################

nigarch=newsimpact(z=NULL, fit1)
plot(nigarch$zx,nigarch$zy, xlab=nigarch$xexpr,ylab=nigarch$yexpr ,type="l", main = "Courbe des impacts des nouvelles")


###################################################
### #GARCH 
###################################################

spec2 = ugarchspec(distribution.model="nig")
fit2 = ugarchfit(spec = spec2, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit2)


###################################################
### Calcul de la VaR
###################################################

# VaR paramétrique 


#VaR EGARCH 
VaR95_EGARCH <- as.numeric(quantile(fit5, probs =0.05))
plot(dates[1:1966],VaR95_EGARCH, type='l',col=1,xlab="Dates",main="Figure A")

#STD
VaR95_EGARCH <- as.numeric(quantile(fit_std, probs =0.05))
plot(dates[1:1966],VaR95_EGARCH, type='l',col=1,xlab="Dates",main="Figure A")

#VaR normale
VaR(rte, p=.95, method="gaussian")

#VaR par simulation historique
VaR(rte, p=.95, method="historical")

#VaR Cornish-Fisher (CF)
VaR(rte, p=.95, method="modified")

###################################################
### Backtesting de la VaR
###################################################

#Backtesting de la VaR fenêtre glissante

library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(1,1)),distribution.model="nig")
roll=ugarchroll(spec, data=rt,n.ahead=1,forecast.length=length(rtt),refit.every=10,
                refit.window="moving",solver = "hybrid", cluster=cl,fit.control = list(),calculate.VaR=TRUE,VaR.alpha=0.05,keep.coef = TRUE)
stopCluster(cl)
library(zoo)
valueatrisk<-zoo(roll@forecast$VaR[,1])
3
reelles<-zoo(roll@forecast$VaR[,2])#=rtt
index<-rownames(roll@forecast$VaR)
plot(dates[1967:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
lines(dates[1967:N],valueatrisk,type='l',col="red")
legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))


#Pour STD
library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(1,1)),distribution.model="std")
roll=ugarchroll(spec_std, data=rt,n.ahead=1,forecast.length=length(rtt),refit.every=100,
                refit.window="moving",solver = "hybrid", cluster=cl,fit.control = list(),calculate.VaR=TRUE,VaR.alpha=0.05,keep.coef = TRUE)
stopCluster(cl)
library(zoo)
valueatrisk<-zoo(roll@forecast$VaR[,1])
3
reelles<-zoo(roll@forecast$VaR[,2])#=rtt
index<-rownames(roll@forecast$VaR)
plot(dates[1967:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
lines(dates[1967:N],valueatrisk,type='l',col="red")
legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))



# Backtesting de la VaR avec filtre et distrib nig 

fit = ugarchfit(spec5, data = rte)
spec2 = spec5
setfixed(spec2)<-as.list(coef(fit))
filt = ugarchfilter(spec=spec2, data=rtt)
filt

# location(alpha)+scale(beta) invariance allows to use [mu + sigma*q(p,0,1,skew,shape)]
VaR=fitted(filt)+sigma(filt)*qdist("nig",p=0.05,mu=0,sigma=1,
                                   skew = coef(filt)["skew"],shape=coef(filt)["shape"])


matplot(dates[1967:N],cbind(VaR,rtt),type="l",col=c("red","blue"),xlab="Dates"
        ,ylab="Rendements et VaR",main="VaR avec filtre (apARCH-ghyp)")
legend("topleft",inset=.05,c("VaR","rt"),col=c("red","blue"),lty=c(1,1)
       ,x.intersp=0.5,y.intersp=0.5,cex=0.75)

print(VaRTest(0.05, rtt, VaR))

# Backtesting de la VaR avec filtre distrib STD

fit = ugarchfit(spec_std, data = rte)
spec2 = spec_std
setfixed(spec2)<-as.list(coef(fit))
filt = ugarchfilter(spec=spec2, data=rtt)
filt

# location(alpha)+scale(beta) invariance allows to use [mu + sigma*q(p,0,1,skew,shape)]
VaR=fitted(filt)+sigma(filt)*qdist("std",p=0.05,mu=0,sigma=1,
                                   skew = coef(filt)["skew"],shape=coef(filt)["shape"])


matplot(dates[1967:N],cbind(VaR,rtt),type="l",col=c("red","blue"),xlab="Dates"
        ,ylab="Rendements et VaR",main="VaR avec filtre (apARCH-ghyp)")
legend("topleft",inset=.05,c("VaR","rt"),col=c("red","blue"),lty=c(1,1)
       ,x.intersp=0.5,y.intersp=0.5,cex=0.75)
###################################################
### Violation de la VaR
###################################################


report(roll,type="VaR",VaR.alpha=0.05,conf.level=0.95)


###################################################
### Calcul de l'ES
###################################################

f = function(x)
    + qdist("nig",p=x,mu=0,sigma=1,skew=coef(fit5)["skew"],shape=coef(fit5)["shape"])
ES = fitted(fit5) + sigma(fit5)*integrate(f, 0, 0.05)$value/0.05
print(ESTest(0.05, rtt, ES, VaR, boot = TRUE))

#pour std
f = function(x)
    + qdist("std",p=x,mu=0,sigma=1,skew=coef(fit5)["skew"],shape=coef(fit5)["shape"])
ES = fitted(fit5) + sigma(fit5)*integrate(f, 0, 0.05)$value/0.05
print(ESTest(0.05, rtt, ES, VaR, boot = TRUE))

