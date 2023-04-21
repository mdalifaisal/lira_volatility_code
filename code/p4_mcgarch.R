source('p4_libraries.R')
source('p4_data.R')



# define a daily spec
spec_d = ugarchspec(mean.model = list(armaOrder = c(1, 1)), 
                    variance.model = list(model = 'sGARCH', 
                                          garchOrder = c(1, 1)),
                    distribution = 'norm')
# use the ugarchroll method to create a rolling 
# forecast for the period in question:
# load data with adjusted dates
adj_intra <- read_excel('data.xlsx', sheet = 4)
adj_log_data <- read_excel('data.xlsx', sheet = 5)

# prepare daily and intra day returns to be used for estimation
adj_intra_ret <- adj_intra$intra_ret
adj_intra_ret <- na.omit(adj_intra_ret)
adj_DT <- as.POSIXct(adj_intra$date,format="%m%d%y %h:%m", tz= "" )
adj_intra_ret <- xts(adj_intra_ret, order.by = adj_DT, tz = "")


adj_my_date <- as.Date(adj_log_data$Date, format = "%m%d%y", tz= "")
adj_log_ret <- xts(adj_log_data$log_ret_adj, order.by = adj_my_date)


n = length(unique(format(index(adj_intra_ret), '%m-%d-%y')))
roll = ugarchroll(spec_d, data = adj_log_ret['/12-30-2021'], 
                  forecast.length = n, 
                  refit.every = 50, refit.window = 'moving', 
                  moving.size = 1000, calculate.VaR = FALSE, solver='hybrid')
# extract the sigma forecast
df = as.data.frame(roll)
f_sigma = as.xts(df[, 'Sigma', drop = FALSE], tz = "")
# now estimate the intraday model
spec = ugarchspec(mean.model = list(armaOrder = c(1, 0), 
                                    include.mean = TRUE),
                  variance.model = list(model = 'mcsGARCH'), 
                  distribution = 'std')
# DailyVar is the required xts object of the forecast daily variance
fit = ugarchfit(data = adj_intra_ret, spec = spec, 
                DailyVar = f_sigma^2)

'all.equal(unique(format(index(adj_intra_ret), format="%m-%d-%Y")),
                 format(index(f_sigma), format="%m-%d-%Y"))
which(mapply(identical,unlist(unique(format(index(adj_intra_ret),
                                            format="%m-%d-%Y"))),
             unlist(format(index(adj_log_ret), format="%m-%d-%Y")))==FALSE)
str(index(f_sigma))
str(unique(format(index(adj_intra_ret), format="%m-%d-%Y")))

setdiff(unique(format(index(adj_intra_ret), format="%m-%d-%Y")), 
        format(index(adj_log_ret), format="%m-%d-%Y"))
'