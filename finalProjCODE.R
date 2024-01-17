#create two time series variables
TEMP = ts(CaliTemp$TEMP,start = c(1989,1), frequency = 12)
UNCERTAIN = ts(CaliTemp$UNCERTIAN,start = c(1989,1), frequency = 12)
plot(TEMP)
plot(UNCERTAIN)

#time-series regression with time as the independent variable- linear trend
tr1 = dynlm(TEMP~trend(TEMP))
summary(tr1)
ts.plot(TEMP,tr1$fitted.values,col=c('black','red'))
    
# log as dependent variable, time as independent variable
logTEMP = log(TEMP)

tr2 = dynlm(logTEMP~trend(TEMP))
ts.plot(TEMP,tr2$fitted.values,col=c('black','red'))

#decompose the variable
deTEMP = decompose(TEMP, type= 'multi')
# use plot function to generate graph
plot(deTEMP)


#estimate a regression with seasonal dummies as the independent variables 
tr3 = dynlm(TEMP~season(TEMP))
summary(tr3)
#use the CensusX-13 program to seasonally adjust the data. 
library(seasonal)
censaTEMP = seas(TEMP)
# save deseasonalized data using ‘final’ function
saTEMP = final(censaTEMP)
# plot the org series & seasonally adjusted series using plot function
plot(saTEMP)

#APP6
#transformed data- log+1st diff
transTEMP = diff(log(saTEMP))
#ACF
Acf(transTEMP)
#PACF
Pacf(transTEMP)

###propose Arima model 
#model1: 1,0,0
m1= Arima(transTEMP,c(1,0,0))
Box.test(m1$residuals,10,'Lj',2)
Box.test(m1$residuals,11,'Lj',2)

m3= Arima(transTEMP,c(2,0,0))
Box.test(m3$residuals,10,'Lj',3)
Box.test(m3$residuals,11,'Lj',3)

m4= Arima(transTEMP,c(3,0,0))
Box.test(m4$residuals,10,'Lj',4)
Box.test(m4$residuals,11,'Lj',4)

m5= Arima(transTEMP,c(4,0,0))
Box.test(m5$residuals,10,'Lj',5)
Box.test(m5$residuals,11,'Lj',5)

###
#Divide your sample into two time periods for estimation and prediction. 
p = 1
q = 0
mEst = Arima(transTEMP[1:267], c(p,0,q))
mf = Arima(transTEMP[267:297], model = mEst)
#test to see if the average forecast error is zero or not.
t.test(mf$residuals)
#test to see if there's white noise
Box.test(mf$residuals, 10, "Lj")
Box.test(mf$residuals, 11, "Lj")
Box.test(mf$residuals, 12, "Lj")
#est a regression equation(error~forcast) to test if coef is sig
eff = lm(mf$residuals~mf$fitted)
summary(eff)
#joint test
lht(eff, c("(Intercept)", "mf$fitted"))



p = 3
q = 1
mEst = Arima(transTEMP[1:267], c(p,0,q))
mf = Arima(transTEMP[267:297], model = mEst)
#test to see if the average forecast error is zero or not.
t.test(mf$residuals)
#test to see if there's white noise
Box.test(mf$residuals, 10, "Lj")
Box.test(mf$residuals, 11, "Lj")
Box.test(mf$residuals, 12, "Lj")
#est a regression equation(error~forcast) to test if coef is sig
eff = lm(mf$residuals~mf$fitted)
summary(eff)
#joint test
lht(eff, c("(Intercept)", "mf$fitted"))



p = 4
q = 0
mEst = Arima(transTEMP[1:267], c(p,0,q))
mf = Arima(transTEMP[267:297], model = mEst)
#test to see if the average forecast error is zero or not.
t.test(mf$residuals)
#test to see if there's white noise
Box.test(mf$residuals, 10, "Lj")
Box.test(mf$residuals, 11, "Lj")
Box.test(mf$residuals, 12, "Lj")
#est a regression equation(error~forcast) to test if coef is sig
eff = lm(mf$residuals~mf$fitted)
summary(eff)
#joint test
lht(eff, c("(Intercept)", "mf$fitted"))



p = 5
q = 0
mEst = Arima(transTEMP[1:267], c(p,0,q))
mf = Arima(transTEMP[267:297], model = mEst)
#test to see if the average forecast error is zero or not.
t.test(mf$residuals)
#test to see if there's white noise
Box.test(mf$residuals, 10, "Lj")
Box.test(mf$residuals, 11, "Lj")
Box.test(mf$residuals, 12, "Lj")
#est a regression equation(error~forcast) to test if coef is sig
eff = lm(mf$residuals~mf$fitted)
summary(eff)
#joint test
lht(eff, c("(Intercept)", "mf$fitted"))



p = 6
q = 0
mEst = Arima(transTEMP[1:267], c(p,0,q))
mf = Arima(transTEMP[267:297], model = mEst)
#test to see if the average forecast error is zero or not.
t.test(mf$residuals)
#test to see if there's white noise
Box.test(mf$residuals, 10, "Lj")
Box.test(mf$residuals, 11, "Lj")
Box.test(mf$residuals, 12, "Lj")
#est a regression equation(error~forcast) to test if coef is sig
eff = lm(mf$residuals~mf$fitted)
summary(eff)
#joint test
lht(eff, c("(Intercept)", "mf$fitted"))



p = 2
q = 0
mEst = Arima(transTEMP[1:267], c(p,0,q))
mf = Arima(transTEMP[267:297], model = mEst)
#test to see if the average forecast error is zero or not.
t.test(mf$residuals)
#test to see if there's white noise
Box.test(mf$residuals, 10, "Lj")
Box.test(mf$residuals, 11, "Lj")
Box.test(mf$residuals, 12, "Lj")
#est a regression equation(error~forcast) to test if coef is sig
eff = lm(mf$residuals~mf$fitted)
summary(eff)
#joint test
lht(eff, c("(Intercept)", "mf$fitted"))



###one-step ahead forecast
mBest = Arima(transTEMP, c(3,0,0))
f1 = forecast(mBest,h=1,0.95)
f1$mean
mBest$sigma2
fvalue = type.convert(f1$mean,as.is = TRUE)
prob = seq(0,1,0.05)
qfore = qnorm(prob, fvalue, sqrt(mBest$sigma2))
afore = ((1+qfore)^4)-1
show = data.frame(prob,qfore,afore)
View(show)




###alternative choice
#Plot a time-series graph of the log of the variable of interest 
logsaTEMP = log(saTEMP)
plot(logsaTEMP)
#Augmented Dickey-Fuller test 
t1 = ur.df(logsaTEMP,'trend',12, 'AIC')
summary(t1)
#first differnece has a unit root?
difflogsaTEMP = diff(logsaTEMP)
plot(difflogsaTEMP)
t2 = ur.df(difflogsaTEMP,'drift',12,'AIC')
summary(t2)

###deterministic model
t = seq(1:297)
regression = lm(TEMP~t,data = CaliTemp)
new = data.frame(t=seq(297+1,297+8))
predict(regression,new)


###another variable
UNCERTAIN = ts(CaliTemp$UNCERTIAN,start = c(1989,1), frequency = 12)
transUN = diff(log(UNCERTAIN))
#find order of var model that min AIC
data = data.frame(transTEMP,transUN)
VARselect(data,lag.max = 10)
#estimate this var model
VAR(data,10)
v1 = VAR(data,10)
ir = irf(v1, n.ahead = 12)
plot(ir)
#enter return button
#fevd plot
fe = fevd(v1)
plot(fe, col=c('blue','red'))
#causality test
causality(v1,'transTEMP')
causality(v1,'transUN')


