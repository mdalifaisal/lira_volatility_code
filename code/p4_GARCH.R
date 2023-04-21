source('p4_libraries.R')
source('p4_data.R')
source('p4_functions.R')

# Estimate GARCH models and save AIC, BIC, HQIC
x = lira_ret
models <- c('GARCH', 'AVGARCH', 'GJRGARCH', 'TGARCH',
            'NGARCH', 'NAGARCH', 'APARCH', 'ALLGARCH')
distributions <- c('norm', 'snorm', 'std', 'sstd', 'jsu')
all_garch_models <- est_all_garch(x, models, distributions)
all_garch_vol <- cbind(all_garch_models$VOL$`GARCH-sstd`,
                       all_garch_models$VOL$`AVGARCH-sstd`,
                       all_garch_models$VOL$`GJRGARCH-sstd`,
                       all_garch_models$VOL$`TGARCH-sstd`,
                       all_garch_models$VOL$`NGARCH-sstd`,
                       all_garch_models$VOL$`NAGARCH-sstd`,
                       all_garch_models$VOL$`APARCH-sstd`,
                       all_garch_models$VOL$`ALLGARCH-sstd`)


## Since fGARCH function does not support EGARCH and CGARCH, these will be 
## estimated separately
models <- c('csGARCH', 'eGARCH')
rem_garch_models <- est_all_garch1(x, models, distributions)

