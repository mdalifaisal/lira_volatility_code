source('p4_libraries.R')
source('p4_data.R')



midas <- fit_mfgarch(data = tefui, y = 'log_ret', 
            x ='tefui_sa', low.freq = "month", K = 12,
            var.ratio.freq = 'month', gamma = FALSE)


midas_vol <- cbind(midas$tau, midas$g)
#write.csv(midas_vol, 'midas_vol.csv')
#gjr_midas <- fit_mfgarch(data = tefui, y = 'log_ret', 
#                         x ='tefui_sa', low.freq = "month", K = 12,
#                         var.ratio.freq = 'month', gamma = TRUE)
#gjr_midas
#library(devtools)
#install_github("JasonZhang2333/GarchMidas")
#library(GarchMidas)
#result <- fit_GarchMidas(data = tefui, y = 'log_ret',
#                         x ='tefui_sa', K = 12)
