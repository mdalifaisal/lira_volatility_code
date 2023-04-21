# set working directory
mywd <- paste("",
              sep = "")
setwd(mywd)

# load source code

source('p4_source.R')

# load libraries

library(quantmod)
library(utils)
library(TTR)
library(forecast)

library(Rsolnp)


library(ggplot2)
library(quantileVaR)
library(gridExtra)
library(segMGarch)

library(WeightedPortTest) # for GARCH diagnostics




# download data and calculate returns

lira_price <- getSymbols('TRY=X', from =  '2005-01-01',
                            to = '2022-04-30',auto.assign=FALSE)
lira_price <-  na.omit(lira_price)
lira_price <- adjustOHLC(lira_price, use.Adjusted = TRUE)
lira_ret <-  ROC(Cl(lira_price), na.pad = FALSE)*100
lira_data = data.frame(lira_price = c(lira_price), 
                       lira_ret = c(NA,lira_ret))
# save returns and price to csv
#write.csv(lira_data, 'lira_data.csv')
# read from system

lira <- read_xlsx('data1.xlsx', sheet=1)
lira_ret <- lira$log_ret
lira_ret <- na.omit(lira_ret)
lira_ret <- lira_ret*100

# take log_ret from p4_plots


## function to estimate all GARCH models 

x = lira_ret
models <- c('GARCH', 'AVGARCH', 'GJRGARCH', 'TGARCH',
            'NGARCH', 'NAGARCH', 'APARCH', 'ALLGARCH')
distributions <- c("norm", "std", "snorm", "sstd", "jsu")
spec.comp <- list()
for( m in models ) {
        for( d in distributions ) {
                spec.comp[[paste( m, d, sep = "-" )]] <-
                        ugarchspec(mean.model = list(armaOrder = c(0, 0),
                                                     include.mean=TRUE),
                                   variance.model = list(model = 'fGARCH',
                                                         garchOrder = c(1, 1),
                                                         submodel=m),
                                   distribution.model=d)
        }
}
specifications <- names( spec.comp )
base.comp <- list()

for( s in specifications ){
        base.comp[[s]] <- ugarchfit(spec = spec.comp[[s]], data = x)
        if (!is.null(coef(base.comp[[s]]))) {
                message('\tDone')
                
                
        } else {
                message('\tEstimation failed..')
                
        }
        
}
# get AIC, BIC, SBIC and HQIC
result.comp <- list()
for( s in specifications) {
        result.comp[[s]] <- as.data.frame.array(infocriteria(base.comp[[s]]))
}
result.comp
# get LL
LLH <- list()
for( s in specifications) {
        LLH[[s]] <- as.data.frame(base.comp[[s]]@fit$LLH)
}
LLH




models1 <- c('csGARCH', 'eGARCH')
spec.comp1 <- list()
for( m in models1 ) {
        for( d in distributions ) {
                spec.comp1[[paste( m, d, sep = "-" )]] <-
                        ugarchspec(mean.model = list(armaOrder = c(0, 0),
                                                     include.mean=TRUE),
                                   variance.model = list(model = m,
                                                         garchOrder = c(1, 1)),
                                   distribution.model=d)
        }
}
specifications1 <- names( spec.comp1 )
base.comp1 <- list()

for( s in specifications1 ){
        base.comp1[[s]] <- ugarchfit(spec = spec.comp1[[s]], data = x)
        if (!is.null(coef(base.comp1[[s]]))) {
                message('\tDone')
                
                
        } else {
                message('\tEstimation failed..')
                
        }
        
}

result.comp1 <- list()
for( s in specifications1) {
        result.comp1[[s]] <- as.data.frame.array(infocriteria(base.comp1[[s]]))
}
result.comp1

LLH1 <- list()
for( s in specifications1) {
        LLH1[[s]] <- as.data.frame(base.comp1[[s]]@fit$LLH)
}
LLH1


## check for serial correlation and ARCH effects in residuals

weight_box_res <- list()
for (s in specifications) {
        weight_box_res[[s]] <- Weighted.Box.test(
                x = residuals(base.comp[[s]], standardize = TRUE), 
                lag=20, type=c("Ljung-Box"))
}

weight_box_res$`GARCH-norm`

weight_box_res1 <- list()
for (s in specifications1) {
        weight_box_res1[[s]] <- Weighted.Box.test(
                x = residuals(base.comp1[[s]], standardize = TRUE), 
                lag=20, type=c("Ljung-Box"))
}

weight_box_res1



sq_weight_box_res <- list()
for (s in specifications) {
        sq_weight_box_res[[s]] <- Weighted.Box.test(
                x = (residuals(base.comp[[s]], standardize = TRUE))^2, 
                lag=20, type=c("Ljung-Box"))
}

sq_weight_box_res

sq_weight_box_res1 <- list()
for (s in specifications1) {
        sq_weight_box_res1[[s]] <- Weighted.Box.test(
                x = (residuals(base.comp1[[s]], standardize = TRUE))^2, 
                lag=20, type=c("Ljung-Box"))
}

sq_weight_box_res1



weight_arch_res <- list()
for (s in specifications) {
        weight_arch_res[[s]] <- Weighted.LM.test(
                x = base.comp[[s]]@fit$residuals,base.comp[[s]]@fit$sigma^2, 
                lag=20,type = "correlation",
                fitdf=2)
}

weight_arch_res

weight_arch_res1 <- list()
for (s in specifications1) {
        weight_arch_res1[[s]] <- Weighted.LM.test(
                x = base.comp1[[s]]@fit$residuals,base.comp1[[s]]@fit$sigma^2, 
                lag=20,type = "correlation",
                fitdf=2)
}

weight_arch_res1

# HAR model
#function to model realized volatility


# fit HAR type models 
# use package 'highfrequency' but also confirm results with 'HARModel' package
 (daily)

# HAR forecast 
HAR_95 <- HARForecast(rv_15,periods = c(1,5,22), nRoll=1865 , nAhead=1 , 
                      type = "HAR", windowType = "rolling")

tata <- quantile(NGAS_roll, probs=alpha)
tata[1:5]
NGAS_roll@Forecast$Moments[1:5]
-0.009685802
fitdist(distribution = 'sstd' , x = x)$pars
VaR95_td = qdist(distribution = "sstd",
                 mu = roll@forecast$density[,1],
                 sigma = roll@forecast$density[,2],
                 skew = roll@forecast$density[,3],
                 shape = roll@forecast$density[,4],
                 lambda = roll@forecast$density[,5],
                 p=0.05)



head(VaR95_td)

BacktestVaR(x[1002:2865], VaR95_td,alpha = alpha)$AE
str(x[1002:2865])





# HAR-CJ
library(data.table)
price_15 <- intra$price
data <- data.table(DT, price_15)
View(data)
# BNSjumpTest -------------------------------------------------------------
bns <- BNSjumpTest(data[, list(DT, price_15)], IVestimator= "rMinRVar",
                   IQestimator = "rMedRQuar", type= "linear", makeReturns = TRUE,
                   alignBy = 'minutes', alignPeriod = 15)
date_1 <- index(rv_15)
testStats <- as.numeric(sapply(bns, function(x){as.numeric(x[1])}))
for_harcj <- data.table(date_1, rv_15, rbpv_15)
model <- HARmodel(cbind(as.xts(for_harcj[, list(date_1, rv_15, rbpv_15)]),
                        testStats), type = "HARCJ", inputType = "RM")
summary(model)






dat <- intra_ret
x <- HARmodel(dat, periods = c(1,5,22), periodsJ = c(1,5,22),
              periodsQ = c(1), RVest = c("rCov", "rQuar"),
              type="HARQ", inputType = "returns")
summary(x)

# CHAR model
dat <- as.xts(cbind(rv_15, rbpv_15))
x <- HARmodel(dat, periods = c(1,5,22), type="CHAR")
summary(x)


# CHAR-Q model
dat <- as.xts(cbind(rv_15, rbpv_15, rmq))
x <- HARmodel(dat, periods = c(1,5,22), periodsQ = c(1), type="CHARQ")
summary(x)


dat <- as.xts(cbind(rv_15, rbpv_15, rmq))
x <- HARmodel(dat, periods = c(1,5,22), periodsQ = c(1),periodsJ = c(1), type="HARQJ")
summary(x)

## fit GAS models and get AIC BIC np LL

#obtain VaR 95% through GAS
# norm
NGAS_roll <- UniGASRoll(x, GASSpec_norm, RefitEvery = 50, 
                          RefitWindow = c("moving"), Nstart = NULL, 
                          ForecastLength = 1864)
alpha = 0.05
NGAS_95_VaR = quantile(NGAS_roll, probs = alpha)
# snorm
SNGAS_roll <- UniGASRoll(x, GASSpec_snorm, RefitEvery = 50, 
                        RefitWindow = c("moving"), Nstart = NULL, 
                        ForecastLength = 1864)
SNGAS_95_VaR = quantile(SNGAS_roll, probs = alpha)
#std
STDGAS_roll <- UniGASRoll(x, GASSpec_std, RefitEvery = 50, 
                        RefitWindow = c("moving"), Nstart = NULL, 
                        ForecastLength = 1864)
STDGAS_95_VaR = quantile(STDGAS_roll, probs = alpha)
# sstd
SSTDGAS_roll <- UniGASRoll(x, GASSpec_sstd, RefitEvery = 50, 
                          RefitWindow = c("moving"), Nstart = NULL, 
                          ForecastLength = 1864)
SSTDGAS_95_VaR = quantile(SSTDGAS_roll, probs = alpha)
# ast
ASTGAS_roll <- UniGASRoll(x, GASSpec_ast, RefitEvery = 50, 
                          RefitWindow = c("moving"), Nstart = NULL, 
                          ForecastLength = 1864)
ASTGAS_95_VaR = quantile(ASTGAS_roll, probs = alpha)
# ast1
AST1GAS_roll <- UniGASRoll(x, GASSpec_ast1, RefitEvery = 50, 
                          RefitWindow = c("moving"), Nstart = NULL, 
                          ForecastLength = 1864)
AST1GAS_95_VaR = quantile(AST1GAS_roll, probs = alpha)
# ald
ALDGAS_roll <- UniGASRoll(x, GASSpec_ald, RefitEvery = 50, 
                          RefitWindow = c("moving"), Nstart = NULL, 
                          ForecastLength = 1864)
ALDGAS_95_VaR = quantile(ALDGAS_roll, probs = alpha)


BacktestVaR(x[1002:2865], NGAS_95_VaR,alpha = alpha)$AE
BacktestVaR(x[1002:2865], SNGAS_95_VaR,alpha = alpha)$AE
BacktestVaR(x[1002:2865], STDGAS_95_VaR,alpha = alpha)$AE
BacktestVaR(x[1002:2865], SSTDGAS_95_VaR,alpha = alpha)$AE
BacktestVaR(x[1002:2865], ASTGAS_95_VaR,alpha = alpha)$AE
BacktestVaR(x[1002:2865], AST1GAS_95_VaR,alpha = alpha)$AE
BacktestVaR(x[1002:2865], ALDGAS_95_VaR,alpha = alpha)$AE


# get best GARCH(with AIC and BIC)

best_GARCH_AIC
best_GARCH_BIC


# function for estimating GARCH model VaR(95%)
x = log_data$log_ret
models <- c('GARCH')#, 'AVGARCH', 'GJRGARCH', 'TGARCH',
            #'NGARCH', 'NAGARCH', 'APARCH', 'ALLGARCH'
distributions <- c("norm", "std", "snorm", "sstd", "jsu")
gvar.95 <- list()

est_95_VAR <- function(x,
                       models,
                       distributions) {
        gvar.95 <- list()
        for( m in models ) {
                for( d in distributions ) {
                        gvar.95[[paste( m, d, sep = "-" )]] <-
                                ugarchspec(mean.model = list(armaOrder = c(0, 0),
                                                             include.mean=TRUE),
                                           variance.model = list(model = 'fGARCH',
                                                                 garchOrder = c(1, 1),
                                                                 submodel=m),
                                           distribution.model=d)
                }
        }
        specifications <- names( gvar.95 )
        roll.gvar95 <- list()
        for( s in specifications ){
                roll.gvar95[[s]] <- ugarchroll(spec = gvar.95[[s]], 
                                               data = x,
                                               n.start = 1001,
                                               refit.every = 50,
                                               refit.window = 'moving',
                                               VaR.alpha = 0.05)
        }
        gvar95.comp=list()
        for( s in specifications ) {
                gvar95.comp[[s]] <- as.data.frame(roll.gvar95[[s]], 
                                                  which = "VaR")[, 1]
                
        }
        
        return(gvar95.comp)
}


GARCH_95_VaR <- est_95_VAR(x,models,distributions)
View(GARCH_95_VaR)
# AVGARCH
models <- c('AVGARCH')
AVGARCH_95_VaR <- est_95_VAR(x,models,distributions)
View(AVGARCH_95_VaR)
#GJRGARCH
models <- c('GJRGARCH')
GJRGARCH_95_VaR <- est_95_VAR(x,models,distributions)
View(GJRGARCH_95_VaR)
#TGARCH
models <- c('TGARCH')
TGARCH_95_VaR <- est_95_VAR(x,models,distributions)
View(TGARCH_95_VaR)
#NGARCH
models <- c('NGARCH')
NGARCH_95_VaR <- est_95_VAR(x,models,distributions)
View(NGARCH_95_VaR)
#NAGARCH
models <- c('NAGARCH')
NAGARCH_95_VaR <- est_95_VAR(x,models,distributions)
View(NAGARCH_95_VaR)
#APARCH
models <- c('APARCH')
APARCH_95_VaR <- est_95_VAR(x,models,distributions)
View(APARCH_95_VaR)
#ALLGARCH
models <- c('ALLGARCH')
ALLGARCH_95_VaR <- est_95_VAR(x,models,distributions)
View(ALLGARCH_95_VaR)


# To get VaR 95 from CGARCH AND EGARCH models
x = log_data$log_ret
models <- c('csGARCH')
distributions <- c("norm", "std", "snorm", "sstd", "jsu")
gvar1.95 <- list()

est1_95_VAR <- function(x,
                       models,
                       distributions) {
        gvar1.95 <- list()
        for( m in models ) {
                for( d in distributions ) {
                        gvar1.95[[paste( m, d, sep = "-" )]] <-
                                ugarchspec(mean.model = list(armaOrder = c(0, 0),
                                                             include.mean=TRUE),
                                           variance.model = list(model = m,
                                                                 garchOrder = c(1, 1)),
                                           distribution.model=d)
                }
        }
        specifications <- names( gvar1.95 )
        roll1.gvar95 <- list()
        for( s in specifications ){
                roll1.gvar95[[s]] <- ugarchroll(spec = gvar1.95[[s]], 
                                               data = x,
                                               n.start = 1001,
                                               refit.every = 50,
                                               refit.window = 'moving',
                                               VaR.alpha = 0.05, solver='nloptr')
        }
        gvar95.comp1=list()
        for( s in specifications ) {
                gvar95.comp1[[s]] <- as.data.frame(roll1.gvar95[[s]], 
                                                  which = "VaR")[, 1]
                
        }
        
        return(gvar95.comp1)
}


CGARCH_95_VaR <- est1_95_VAR(x,models,distributions)

models <- c('eGARCH')
EGARCH_95_VaR <- est1_95_VAR(x,models,distributions)




# get VaR(95%) through GARCH models
# just for check
spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                   variance.model = list(model = "csGARCH", garchOrder = c(1,1)),
                   distribution = 'sstd')
roll <- ugarchroll(spec, x, n.ahead = 1, n.start = 1001, refit.every = 50,
                   refit.window = c("moving"), VaR.alpha = c(0.05), solver='nloptr')
report(roll, VaR.alpha=0.05)

## GARCH AE
BacktestVaR(x[1002:2865], EGARCH_95_VaR$`eGARCH-norm`,alpha = 0.05)$AE
BacktestVaR(x[1002:2865], EGARCH_95_VaR$`eGARCH-snorm`,alpha = 0.05)$AE
BacktestVaR(x[1002:2865], EGARCH_95_VaR$`eGARCH-std`,alpha = 0.05)$AE
BacktestVaR(x[1002:2865], EGARCH_95_VaR$`eGARCH-sstd`,alpha = 0.05)$AE
BacktestVaR(x[1002:2865], EGARCH_95_VaR$`eGARCH-jsu`,alpha = 0.05)$AE


# TRAFFIC LIGHT TEST
segMGarch::TL(y=x[1002:2865], VaR=GJRGARCH_95_VaR$`GJRGARCH-snorm`,VaR_level = 0.95)
report(roll,VaR.alpha=0.05, type="fpm")


# backtesting VaR
# 95%
# sGARCH
GAS::BacktestVaR(x[1001:2864], GARCH_95_VaR$`GARCH-norm`, 0.95)
BacktestVaR(x[2001:4419], VaRs_95_GARCH_ghyp$eGARCH.ghyp, 0.95)
BacktestVaR(x[2001:4419], VaRs_95_GARCH_ghyp$gjrGARCH.ghyp, 0.95)
BacktestVaR(x[2001:4419], VaRs_95_GARCH_ghyp$csGARCH.ghyp, 0.95)

kupiec(x[2001:4419], VaRs_95_GARCH_ghyp$sGARCH.ghyp, 0.95)
DQtest(x[2001:4419], VaRs_95_GARCH_ghyp$sGARCH.ghyp, 0.95)

