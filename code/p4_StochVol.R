source('p4_data.R')

## fit SV
ret <- logret(log_data$adj_close, demean = TRUE)
sv <- svsample(ret, priormu = c(0, 100), priorphi = c(5, 1.5),
                priorsigma = 1, thin = 1)
summary(sv)

# fit SVT
svt <- svtsample(ret, priormu = c(0, 100), priorphi = c(5, 1.5),
                 priorsigma = 1, thin = 1)
summary(svt)
# fit SVL
svl <- svlsample(ret, priormu = c(0, 100), priorphi = c(5, 1.5),
                 priorsigma = 1, thin = 1)

summary(svl)
# fit SVTL
svtl <- svtlsample(ret, priormu = c(0, 100), priorphi = c(5, 1.5),
                   priorsigma = 1, thin = 1)
summary(svtl)

#write.csv(sv$summary$sd[,1], 'sv_vol.csv')
#write.csv(svt$summary$sd[,1], 'svt_vol.csv')
#write.csv(svl$summary$sd[,1], 'svl_vol.csv')
#write.csv(svtl$summary$sd[,1], 'svtl_vol.csv')
