
library(dplyr)
library(tidyr)
library(ggplot2)
library(fGarch) # for extracting standardized residuals
library(WeightedPortTest) # for garch residual checks



# function for ARCH test
arch_test <- function(x, max_lag = 5) {
        require(FinTS)
        require(tidyverse)
       
        do_single_arch <- function(x, used_lag)  {
                test_out <- FinTS::ArchTest(x, lags = used_lag)
                
                res_out <- tibble(Lag = used_lag,
                                  `LMStatistic` = test_out$statistic, 
                                  `pvalue` = test_out$p.value)
        }
        
        tab_out <- bind_rows(map(1:max_lag,.f = do_single_arch, x = x))
        
        return(tab_out)
}


# function to find best GARCH model automatically

max_lag_ARCH <- 1
max_lag_GARCH <- 1
dist_to_use <- c('norm', 'std', 'snorm', 'sstd', 'ged', 'jsu',
                 'sged', 'nig', 'ghyp') 
models_to_estimate <- c('sGARCH', 'eGARCH', 'gjrGARCH', 'csGARCH')
library(purrr)

graphics.off()
find_best_arch_model <- function(x, 
                                 type_models, 
                                 dist_to_use,
                                 max_lag_ARCH,
                                 max_lag_GARCH) {
        
        require(tidyr)
        
        df_grid <- expand_grid(type_models = type_models,
                               dist_to_use = dist_to_use,
                               arch_lag = 1:max_lag_ARCH,
                               garch_lag = 1:max_lag_GARCH)
        
        
        l_out <- pmap(.l = list(x = rep(list(x), nrow(df_grid)), 
                                type_model = df_grid$type_models,
                                type_dist = df_grid$dist_to_use,
                                lag_arch = df_grid$arch_lag,
                                lag_garch  = df_grid$garch_lag),
                      do_single_garch)
        
        tab_out <- bind_rows(l_out)
        
        # find by AIC
        idx <- which.min(tab_out$AIC)
        best_aic <- tab_out[idx, ]
        
        # find by BIC
        idx <- which.min(tab_out$BIC)
        best_bic <- tab_out[idx, ]
        
        l_out <- list(best_aic = best_aic,
                      best_bic = best_bic,
                      tab_out = tab_out)
        
        return(l_out)
}

do_single_garch <- function(x, 
                            type_model, 
                            type_dist, 
                            lag_arch, 
                            lag_garch) {
        require(rugarch)
        
        
        spec = ugarchspec(variance.model = list(model =  type_model, 
                          garchOrder = c(lag_arch, lag_garch)),
        
                          distribution = type_dist)
        
        message(type_model, '(', lag_arch, ',', lag_garch, ')', 
                ' dist = ', type_dist,
                appendLF = FALSE)
        
        try({
                my_rugarch <- list()
                my_rugarch <- ugarchfit(spec = spec, 
                                        data = x, solver='hybrid')
        })
        
        if (!is.null(coef(my_rugarch))) {
                message('\tDone')
                
                AIC <- rugarch::infocriteria(my_rugarch)[1]
                BIC <- rugarch::infocriteria(my_rugarch)[2]
        } else {
                message('\tEstimation failed..')
                
                AIC <- NA
                BIC <- NA
        }
        
        est_tab <- tibble(lag_arch,
                          lag_garch,
                          AIC =  AIC,
                          BIC = BIC,
                          type_model = type_model,
                          type_dist,
                          model_name = 
                        paste0(
                        type_model, '(', lag_arch, ',', lag_garch, ') ',
                        type_dist) ) 
        
        return(est_tab)
}

out <- find_best_arch_model(x = lira_ret, 
                            type_models = models_to_estimate,
                            dist_to_use = dist_to_use,
                            max_lag_ARCH = max_lag_ARCH,
                            max_lag_GARCH = max_lag_GARCH)

# get table with estimation results
tab_out <- out$tab_out
# pivot table to long format (better for plotting)
df_long <- tidyr::pivot_longer(data = tab_out %>%
                                       select(model_name,
                                              type_model,
                                              type_dist,
                                              AIC, BIC), 
                               cols = c('AIC', 'BIC'))
models_names <- unique(df_long$model_name)
best_models <- c(tab_out$model_name[which.min(tab_out$AIC)],
                 tab_out$model_name[which.min(tab_out$BIC)])

# figure out where is the best model
df_long <- df_long %>%
        mutate(order_model = if_else(model_name %in% best_models, 
                                     'Best Model', 'Not Best Model') ) %>%
        na.omit()

# make table with best models
df_best_models <- df_long %>%
        group_by(name) %>%
        summarise(model_name = model_name[which.min(value)],
                  value = value[which.min(value)],
                  type_model = type_model[which.min(value)])

# plot results
p1 <- ggplot(df_long %>%
                     arrange(type_model), 
             aes(x = reorder(model_name, 
                             order(type_model)),
                 y = value, 
                 color = type_dist,
                 shape = type_model)) + 
        geom_point(size = 3.5, alpha = 0.65) + 
        coord_flip() + 
        theme_bw(base_family = "TT Times New Roman") + 
        facet_wrap(~name, scales = 'free_x') + 
        geom_point(data = df_best_models, 
                   mapping = aes(x = reorder(model_name,
                                             order(type_model)),
                                                        y = value), 
                   color = 'blue', size = 5, shape = 8) +
        labs(title = 'Selecting Garch Models by Fitness Criteria', 
             subtitle = 'The best model is the one with lowest AIC 
             or BIC (with star)',
             x = '',
             y = 'Value of Fitness Criteria',
             shape = 'Type of Dist.',
             color = 'Type of Model') + 
        theme(legend.position = "right")


# function to find best univariate GARCH

best_GARCH_AIC <-  filter(df_long, name == "AIC" &
                          type_model %in% c('sGARCH','eGARCH', 'gjrGARCH',
                                            'csGARCH')) %>%
         group_by(type_model) %>%
                summarise(model_name = model_name[which.min(value)],
                  value = value[which.min(value)],
                  type_model = type_model[which.min(value)])
best_GARCH_BIC <-  filter(df_long, name == "BIC" &
                                  type_model %in% c('sGARCH','eGARCH', 'gjrGARCH',
                                                    'csGARCH')) %>%
        group_by(type_model) %>%
        summarise(model_name = model_name[which.min(value)],
                  value = value[which.min(value)],
                  type_model = type_model[which.min(value)])


spec_g_11 <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                        variance.model = list(model = "fGARCH", garchOrder = c(1,1),
                                              submodel = 'GARCH'),
                        distribution = 'norm')

ugarchfit(spec_g_11, x)
roll1 = ugarchroll(spec_g_11, x, n.start = 4416,
                   refit.every = 1,
                   refit.window = 'moving',
                   VaR.alpha = 0.05)

roll1@forecast$VaR$`alpha(5%)`

x <- na.omit(x)
models <- c( 'sGARCH', "eGARCH", "gjrGARCH", "csGARCH")
distributions <- c("ghyp")

VaRs_95_GARCH_ghyp = data.frame(est_95_VAR(x,models,distributions))
VaRs_95_GARCH_ghyp











combine <- list()
for(s in specifications){
        combine[[s]] <- tibble(AIC=result.comp[[s]]$V1[1], BIC=result.comp[[s]]$V1[2],
                               SBIC=result.comp[[s]]$V1[3], HQIC=result.comp[[s]]$V1[4],
                               LLH = LLH[[s]])        
}
combine
combine$`GARCH-norm`

