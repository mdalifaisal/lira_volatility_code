#library(readxl)
#library(Rsolnp)
#library(tseries)
#library(quantmod) # to download data
#library(FinTS)
#library(ggplot2)
#library(gridExtra)

#library(changepoint)
#library(rugarch)
#library(WeightedPortTest)
#library(aod)
#library(LongMemoryTS)
#library(fracdiff)

source('p4_libraries.R')

## First match SP500 results of ALLEN (2020)

## prepare sp500 data
sp500 <- getSymbols('^GSPC', from =  '2000-03-1',
                         to = '2020-04-30',auto.assign=FALSE)

sp500 <- na.omit(sp500)
sp500_ret <- quantmod::adjustOHLC(sp500, use.Adjusted = TRUE)
sp500_ret <-  TTR::ROC(Cl(sp500_ret), na.pad = FALSE)
describe(sp500_ret)
## fit GARCH
spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0),include.mean=TRUE),
        variance.model = list(garchOrder = c(1, 1)),
        distribution.model='norm')
fit <- ugarchfit(spec, sp500_ret)
fit


## plot garch and SV volatility
tt <- cbind(sigma(fit), res$summary$sd[,1])
ts.plot(tt, gpars= list(col=c("blue","green")))

## fit GARCH
spec1 <- ugarchspec(mean.model = list(armaOrder = c(0, 0),include.mean=TRUE),
                   variance.model = list(garchOrder = c(1, 1)),
                   distribution.model='norm')
fit1 <- ugarchfit(spec1, lira_ret*100)
fit1
## fit SV

ret1 <- logret(lira$adj_close, demean = TRUE)
res1 <- svsample(ret1, priormu = c(0, 100), priorphi = c(5, 1.5),
                priorsigma = 1, thin = 1)
summary(res1)

        ## plot garch and SV volatility

tt1 <- cbind(sigma(fit1)/100, res1$summary$sd[,1])
ts.plot(tt1, gpars= list(col=c("blue","green")))

# rolling window estimation
sv_roll <- svsample_roll(ret1, forecast_length = 1864,
                         refit_every = 1,
                         refit_window = c("moving"),
                         calculate_quantile = c(0.05),
                         parallel = c("snow"))
View(sv_roll)
