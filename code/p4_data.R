Sys.setenv(TZ = 'GMT')
# set working directory
mywd <- paste("",
              sep = "")
setwd(mywd)

source('p4_libraries.R')
log_data <- read_excel('data1.xlsx', sheet = 1)
my_date <- as.Date(log_data$Date, format = "%m%d%y")
price <- cbind.data.frame(my_date, log_data$adj_close)
colnames(price) <- c('Year', 'Price')
# read data and stats
lira_ret <- log_data$log_ret*100
lira_ret <- na.omit(lira_ret)
stats <- describe(lira_ret)
jb <- jarque.bera.test(lira_ret)
lb <- Box.test(lira_ret, lag = 20, type = c("Ljung-Box"))
lb_sq <- Box.test(lira_ret^2, lag = 20, type = c("Ljung-Box"))
arch_test <- ArchTest(lira_ret)
root_test <- adf.test(lira_ret)
# range data
range_data <- read_excel('data1.xlsx', sheet=2)
range_ret <- range_data$range_ret
range_ret <- na.omit(range_ret)
rr_stats <- describe(range_ret)
rrjb <- jarque.bera.test(range_ret)
rrlb <- Box.test(range_ret, lag = 20, type = c("Ljung-Box"))
rr_stats
rrjb
rrlb

# intra day
# get realized measures HAR
intra <- read_excel('data1.xlsx', sheet = 3)
intra_ret <- intra$intra_ret
intra_ret <- na.omit(intra_ret)
DT <- as.POSIXct(intra$date,format="%m%d%y %h:%m")
intra_ret <- xts(intra_ret, order.by = DT)
rv_15 <- rCov(intra_ret, alignBy = 'minutes', alignPeriod = 15)
rbpv_15 <- rBPCov(intra_ret, alignBy = 'minutes', alignPeriod = 15)
rq_15 <- rQuar(intra_ret, alignBy = 'minutes', alignPeriod = 15)
rms_all <- cbind(rv_15, rbpv_15, rq_15)
rmq <- rMedRQuar(intra_ret, alignBy = 'minutes', alignPeriod = 15)
#write.csv(rv_15, 'rv_15.csv')
# low freq data for MIDAS
tefui <- read_excel('tefui.xlsx')
# data.frame for MIDAS
tefui <- as.data.frame(tefui)
tefui$date <- as.Date(tefui$date)
tefui$month <- as.Date(tefui$month)
tefui$log_ret <- tefui$log_ret*100


# data prep. for MCGARCH

#mc_i <- quantmod::getSymbols('TRY=X', from = '2000-01-01',auto.assign=FALSE)
#mc_i <- na.omit(mc_i)
#mc_i = quantmod::adjustOHLC(mc_i, use.Adjusted = TRUE)
#mc_gi = TTR::ROC(Cl(mc_i), na.pad = FALSE)
#write.csv(mc_gi['/2021-12-30'], 'ms_gi.csv', row.names=index(mc_gi['/2021-12-30']))
# Find the unique days in the intraday sample

#m = unique(format(index(intra_ret), '%Y-%m-%d'))
#t = format(index(mc_gi['2011-01-01/2021-12-30']), '%Y-%m-%d')
#intersect(t,m) # these dates are in both sets
# remove following dates to balance the dataset
#setdiff(m,t) # these dates are not in daily
#setdiff(t,m) # these dates are not in intra



#write.csv(unique(format(index(intra_ret), '%Y-%m-%d')), 'dates.csv')
#format(index(adj_log_ret['2011-01-01/2021-12-30']), '%Y-%m-%d')
#length(f_sigma)
#length(unique())