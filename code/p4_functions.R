# GARCH FUNCTIONS

source('p4_libraries.R')

# function to estimate and get LL, AIC, BIC, HQIC
est_all_garch <- function(x,
                          models,
                          distributions) {

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
LL <- list()
AIC <- list()
BIC <- list()
HQIC <- list()
VOL <- list()
weight_box_res <- list()
sq_weight_box_res <- list()
weight_arch_res <- list()
        for( s in specifications ){
                base.comp[[s]] <- ugarchfit(spec = spec.comp[[s]], data = x)
                LL[[s]] <- base.comp[[s]]@fit$LLH
                AIC[[s]] <- infocriteria(base.comp[[s]])[1]
                BIC[[s]] <- infocriteria(base.comp[[s]])[2]
                HQIC[[s]] <- infocriteria(base.comp[[s]])[4]
                VOL[[s]] <- base.comp[[s]]@fit$sigma
                weight_box_res[[s]] <- Weighted.Box.test(
                        x = residuals(base.comp[[s]], standardize = TRUE), 
                        lag=20, type=c("Ljung-Box"))[1]
                sq_weight_box_res[[s]] <- Weighted.Box.test(
                        x = (residuals(base.comp[[s]], standardize = TRUE))^2, 
                        lag=20, type=c("Ljung-Box"))[1]
                weight_arch_res[[s]] <- Weighted.LM.test(
                        x = base.comp[[s]]@fit$residuals,base.comp[[s]]@fit$sigma^2, 
                        lag=20,type = "correlation",
                        fitdf=2)[1]
                if (!is.null(coef(base.comp[[s]]))) {
                       
                message('\tDone')
    
                    
         } else {
                message('\tEstimation failed..')
                 LL <- NA   
                 AIC <- NA
                 BIC <- NA
                 HQIC <- NA
                 VOL <- NA
                 weight_box_res <- NA
                 sq_weight_box_res <- NA
                 weight_arch_res <- NA
        }
        
        }
               est <- tibble(LL,AIC,BIC,HQIC,weight_box_res,sq_weight_box_res,
                             weight_arch_res,VOL)
               return(est)
               
} 


## Since fGARCH function does not support EGARCH and CGARCH, these will be 
## estimated separately

est_all_garch1 <- function(x,
                          models,
                          distributions) {
        
        spec.comp <- list()
        for( m in models ) {
                for( d in distributions ) {
                        spec.comp[[paste( m, d, sep = "-" )]] <-
                                ugarchspec(mean.model = list(armaOrder = c(0, 0),
                                                             include.mean=TRUE),
                                           variance.model = list(model = m,
                                                                 garchOrder = c(1, 1)),
                                           distribution.model=d)
                }
        }
        specifications <- names( spec.comp )        
        base.comp <- list()
        LL <- list()
        AIC <- list()
        BIC <- list()
        HQIC <- list()
        VOL <- list()
        weight_box_res <- list()
        sq_weight_box_res <- list()
        weight_arch_res <- list()
        for( s in specifications ){
                base.comp[[s]] <- ugarchfit(spec = spec.comp[[s]], data = x)
                LL[[s]] <- base.comp[[s]]@fit$LLH
                AIC[[s]] <- infocriteria(base.comp[[s]])[1]
                BIC[[s]] <- infocriteria(base.comp[[s]])[2]
                HQIC[[s]] <- infocriteria(base.comp[[s]])[4]
                VOL[[s]] <- base.comp[[s]]@fit$sigma
                weight_box_res[[s]] <- Weighted.Box.test(
                        x = residuals(base.comp[[s]], standardize = TRUE), 
                        lag=20, type=c("Ljung-Box"))[1]
                sq_weight_box_res[[s]] <- Weighted.Box.test(
                        x = (residuals(base.comp[[s]], standardize = TRUE))^2, 
                        lag=20, type=c("Ljung-Box"))[1]
                weight_arch_res[[s]] <- Weighted.LM.test(
                        x = base.comp[[s]]@fit$residuals,base.comp[[s]]@fit$sigma^2, 
                        lag=20,type = "correlation",
                        fitdf=2)[1]
                if (!is.null(coef(base.comp[[s]]))) {
                        
                        message('\tDone')
                        
                        
                } else {
                        message('\tEstimation failed..')
                        LL <- NA   
                        AIC <- NA
                        BIC <- NA
                        HQIC <- NA
                        VOL <- NA
                        weight_box_res <- NA
                        sq_weight_box_res <- NA
                        weight_arch_res <- NA
                }
                
        }
        est <- tibble(LL,AIC,BIC,HQIC,weight_box_res,sq_weight_box_res,
                      weight_arch_res,VOL)
        return(est)
} 

## check for serial correlation and ARCH effects in residuals
