source('p4_libraries.R')
source('p4_data.R')
# GAS specs
GASSpec_norm <- UniGASSpec(Dist = "norm", ScalingType = 'Identity',
                           GASPar = list(scale = TRUE, location = TRUE))
GASSpec_snorm <- UniGASSpec(Dist = "snorm", ScalingType = 'Identity',
                            GASPar = list(scale = TRUE, location = TRUE))
GASSpec_std <- UniGASSpec(Dist = "std", ScalingType = 'Identity',
                          GASPar = list(scale = TRUE, location = TRUE))
GASSpec_sstd <- UniGASSpec(Dist = "sstd", ScalingType = 'Identity',
                           GASPar = list(scale = TRUE, location = TRUE))
GASSpec_ast <- UniGASSpec(Dist = "ast", ScalingType = 'Identity',
                          GASPar = list(scale = TRUE, location = TRUE))
GASSpec_ast1 <- UniGASSpec(Dist = "ast1", ScalingType = 'Identity',
                           GASPar = list(scale = TRUE, location = TRUE))
GASSpec_ald <- UniGASSpec(Dist = "ald", ScalingType = 'Identity',
                          GASPar = list(scale = TRUE, location = FALSE))
norm <- UniGASFit(GASSpec_norm, lira_ret)
snorm <- UniGASFit(GASSpec_snorm, lira_ret)
std <- UniGASFit(GASSpec_std, lira_ret)
sstd <- UniGASFit(GASSpec_sstd, lira_ret)
ast <- UniGASFit(GASSpec_ast, lira_ret)
ast1 <- UniGASFit(GASSpec_ast1, lira_ret)
ald <- UniGASFit(GASSpec_ald, lira_ret)

#all_gas_vol <- cbind(norm@Estimates$Moments[,2],
#                    snorm@Estimates$Moments[,2],
#                     std@Estimates$Moments[,2],
#                     sstd@Estimates$Moments[,2],
#                     ast@Estimates$Moments[,2],
#                     ast1@Estimates$Moments[,2],
#                     ald@Estimates$Moments[,2])




