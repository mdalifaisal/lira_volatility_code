source('p4_libraries.R')
source('p4_data.R')
# simple HAR model
HARFit <-  HAREstimate(rv_15, periods = c(1,5,22))
sandwichNeweyWest(HARFit)
summary(HARFit)

# L-HAR model
LHARFit <- HARmodel(rv_15, transform = "log")
summary(LHARFit)

# HAR-J model
HARJFit <- HAREstimate(RM = rv_15, BPV = rbpv_15, 
                       periods = c(1,5,22), periodsJ = c(1),  
                       type = "HARJ" )
sandwichNeweyWest(HARJFit)
summary(HARJFit)

# HAR-Q model
HARQFit <- HAREstimate(RM = rv_15, RQ = rq_15, 
                       periods = c(1,5,22 ), periodsRQ = c(1),  
                       type = "HARQ")
sandwichNeweyWest(HARQFit)
summary(HARQFit)

# HAR-QJ model
HARQJFit <- HAREstimate(RM = rv_15, BPV = rbpv_15, RQ = rq_15, 
                        periods = c(1,5,22), periodsJ = c(1), 
                        periodsRQ = c(1), type = "HARQ-J")
sandwichNeweyWest(HARQJFit)
summary(HARQJFit)

# CHAR
CHARFit <- HAREstimate(rv_15, BPV = rbpv_15, periods = c(1,5,22),
                       type = "CHAR")
sandwichNeweyWest(CHARFit)
summary(CHARFit)

# CHAR-Q
CHARQFit <- HAREstimate(rv_15, BPV = rbpv_15, RQ = rq_15, 
                        periods = c(1,5,22), periodsRQ = c(1), 
                        type = "CHARQ")
sandwichNeweyWest(CHARQFit)
summary(CHARQFit)


#all_har_vol <- cbind(HARFit@model$fitted.values, 
#                     LHARFit$fitted.values,
#                     HARJFit@model$fitted.values,
#                     HARQFit@model$fitted.values,
#                     HARQJFit@model$fitted.values,
#                     CHARFit@model$fitted.values,
#                     CHARQFit@model$fitted.values)
