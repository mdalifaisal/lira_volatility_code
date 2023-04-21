
source('p4_libraries.R')
source('p4_data.R')

# plot
## event-plot

pp <- price %>% 
        ggplot( aes(x=Year, y=Price)) +
        geom_line(color="black", size = 0.5) +
        ggtitle(" ") + 
        ylim(0,17) +
        theme(plot.margin = margin(t = 1.5, unit = "lines")) + 
        annotate(geom="text", x=as.Date("2012-10-10"), y=12.5, 
                 label="European sovereign debt crisis", 
                 color = "#3366cc") +
        annotate(geom="curve", x=as.Date("2011-08-08"),
                 xend = as.Date("2011-08-08"), yend = 2.5,
                 color="#3366cc", y = 12,
                 arrow=arrow(length=unit(0.03,"npc")), size = 1.1,
                 ncp = 1000) + 
        annotate(geom="text", x=as.Date("2013-01-01"), y=9.5, 
                 label="Turkey gets IG rating from Fitch", color = "#000066") +
        annotate(geom="curve", x=as.Date("2013-01-01"),
                 xend = as.Date("2012-11-11"), yend = 2.5,
                 color="#000066", y = 9,
                 arrow=arrow(length=unit(0.03,"npc")), size = 1.1,
                 ncp = 1000) +
        annotate(geom="text", x=as.Date("2015-06-06"), y=7.5, 
                 label="Chinese Stock Crash", color = "#3366cc") +
        annotate(geom="curve", x=as.Date("2015-06-06"),
                 xend = as.Date("2015-06-06"), yend = 3,
                 color="#3366cc", y = 7,
                 arrow=arrow(length=unit(0.05,"npc")), size = 1.1,
                 ncp = 1000, angle = 0) +
        annotate(geom="text", x=as.Date("2016-07-15"), y=0.5, 
                 label="Failed coup de'etat", color = "#000066") +
        annotate(geom="curve", x=as.Date("2016-07-15"),
                 xend = as.Date("2016-07-15"), yend = 2.8,
                 color="#000066", y = 0.7,
                 arrow=arrow(length=unit(0.05,"npc")), size = 1.1,
                 ncp = 1000, angle = 0) +
        annotate(geom="text", x=as.Date("2018-07-07"), y=1.5, 
                 label="Trade Wars", color = "#3366cc") +
        annotate(geom="curve", x=as.Date("2018-07-07"),
                 xend = as.Date("2018-07-07"), yend = 4.2,
                 color="#3366cc", y = 1.8,
                 arrow=arrow(length=unit(0.05,"npc")), size = 1.1,
                 ncp = 1000, angle = 0) +
        annotate(geom="text", x=as.Date("2018-07-07"), y=11, 
                 label="Two credit agencies downgraded \nTurkey's ratings",
                 color = "#000066") +
        annotate(geom="curve", x=as.Date("2018-08-07"),
                 xend = as.Date("2018-07-07"), yend = 5,
                 color="#000066", y = 10,
                 arrow=arrow(length=unit(0.05,"npc")), size = 1.1,
                 ncp = 1000, angle = 60) +
        annotate(geom="text", x=as.Date("2020-10-07"), y=2.5, 
                 label="Y2Y CPI \ngoes above 25%",
                 color = "#000066") +
        annotate(geom="curve", x=as.Date("2020-07-07"),
                 xend = as.Date("2018-10-07"), yend = 4.5,
                 color="#000066", y = 3.7,
                 arrow=arrow(length=unit(0.05,"npc")), size = 1.1,
                 ncp = 1000, angle = 0) +
        annotate(geom="text", x=as.Date("2021-07-07"), y=4.6, 
                 label="Covid 19", color = "#3366cc") +
        annotate(geom="curve", x=as.Date("2021-06-06"),
                 xend = as.Date("2020-12-12"), yend = 7,
                 color="#3366cc", y = 5,
                 arrow=arrow(length=unit(0.05,"npc")), size = 1.1,
                 ncp = 1000, angle = 0) +
        theme_economist() + 
        annotate(geom="text", x=as.Date("2011-06-06"), y=16.5, 
                 label="Global Events", color = "#3366cc") +
        annotate(geom="text", x=as.Date("2012-02-02"), y=15.8, 
                 label="Events specific to Turkey", color = "#000066") + 
        xlab(" ") + ylab(expression(atop(bold('Price against USD'))))
pp
#log_returns plot

log_ret <- cbind.data.frame(my_date, log_data$log_ret)
lr <- log_ret %>%
        ggplot( aes(x=my_date, y=log_data$log_ret)) +
        geom_line(color="black", size = 0.5) +
        ggtitle(" ") + 
        ylim(-0.2,0.2) +
        theme(plot.margin = margin(t = 1.5, unit = "lines")) + 
        theme_economist() + 
        xlab(" ") + ylab(expression(atop(bold(''))))
lr
# range return
range_data <- read_excel('data1.xlsx', sheet = 2)
range_ret <- cbind.data.frame(range_data$Date, range_data$range_ret)
rr <- range_ret %>%
        ggplot( aes(x=range_data$Date,y=range_data$range_ret)) +
        geom_line(color="#000066", size = 0.5) +
        ggtitle(" ") + 
        ylim(0,0.15) +
        theme(plot.margin = margin(t = 1.5, unit = "lines")) + 
        theme_economist() + 
        xlab(" ") + ylab(expression(atop(bold(''))))
rr

## realized volatility
# first calculate realized volatility and then plot
intra <- read_excel('data1.xlsx', sheet = 3)
intra_ret <- intra$intra_ret
#intra_ret <- na.omit(intra_ret)
DT <- as.POSIXct(intra$date,format="%m%d%y %h:%m")
intra_ret <- xts(intra_ret, order.by = DT)
rv_15 <- rCov(intra_ret, alignBy = 'minutes', alignPeriod = 15)


rv <- autoplot(rv_15) + ggtitle("2011-01-03 23:45:00 / 2021-12-30 23:45:00") + 
        theme_economist() + 
        geom_line(color="#3366cc", size = 0.5) +
        xlab(" ") + ylab(expression(atop(bold(''))))
rv

#write.csv(rv_15, 'rv_15.csv')

## MCGARCH
#source('p4_mcgarch.R')
#par(cex.main = 0.85, col.main = 'black')
#acf(abs(as.numeric(intra_ret)), lag.max = 4000, 
#    main = '15-min absolute returns\nTurkish Lira (2011 Jan-2021 Dec)', 
#    cex.lab = 0.8)

#
#ep <- axTicksByTime(fit@model$DiurnalVar[1:25000])
#par(mfrow = c(4, 1), mar = c(2.5, 2.5, 2, 1))
#plot(as.numeric(fit@model$DiurnalVar[1:25000]^0.5), type = 'l', 
#    main = 'Sigma[Diurnal]', col = 'tomato1', xaxt = 'n', 
#     ylab = 'sigma', xlab = ' ')
#axis(1, at = ep, labels = names(ep), tick = TRUE)
#grid()
#plot(as.numeric(fit@model$DailyVar^0.5), type = 'l', 
#     main = 'Sigma[Daily-Forecast]', col = 'tomato2', xaxt = 'n', 
#     ylab = 'sigma', xlab = ' ')
#axis(1, at = ep, labels = names(ep), tick = TRUE)
#grid()
#plot(fit@fit$q, type = 'l', main = 'Sigma[Stochastic]', col = 'tomato3', 
#     xaxt = 'n', ylab = 'sigma', xlab = ' ')
#axis(1, at = ep, labels = names(ep), tick = TRUE)
#grid()
#plot(as.numeric(sigma(fit)), type = 'l', main = 'Sigma[Total]', col = 'tomato4', xaxt = 'n', ylab = 'sigma', xlab = ' ')
#axis(1, at = ep, labels = names(ep), tick = TRUE)
#grid()
## SV graph

sv_volatility <- as.data.frame(cbind(a=sv[["summary"]][["sd"]][,3],
                               b=sv[["summary"]][["sd"]][,4],
                               c=sv[["summary"]][["sd"]][,5]))

svp <- sv_volatility %>%
        ggplot( aes(x=log_data$Date[2:2844])) + 
        geom_line(aes(y=a, color="#3366cc"), size = 0.5) +
        geom_line(aes(y = b), color = "black") + 
        geom_line(aes(y = c), color = "#3366cc") + 
        ggtitle("Posterior Quantiles") + 
        ylim(0,0.10) +
        theme(plot.margin = margin(t = 1.5, unit = "lines")) + 
        theme_economist() + 
        scale_color_manual(name = " ", 
                           values = c("95%" = "#3366cc", 
                                      "50%" = "black",
                                      "5%" = "gray")) + 
        xlab(" ") + ylab(expression(atop(bold(''))))
svp
traceplot(sv$"para"[,3])
densplot(sv$para[,3])
paradensplot(sv)

# SVT graph
svt_volatility <- as.data.frame(cbind(a=svt[["summary"]][["sd"]][,3],
                                     b=svt[["summary"]][["sd"]][,4],
                                     c=svt[["summary"]][["sd"]][,5]))

svtp <- svt_volatility %>%
        ggplot( aes(x=log_data$Date[2:2844])) + 
        geom_line(aes(y=a, color="#3366cc"), size = 0.5) +
        geom_line(aes(y = b), color = "black") + 
        geom_line(aes(y = c), color = "#3366cc") + 
        ggtitle("Posterior Quantiles") + 
        ylim(0,0.10) +
        theme(plot.margin = margin(t = 1.5, unit = "lines")) + 
        theme_economist() + 
        scale_color_manual(name = " ", 
                           values = c("95%" = "#3366cc", 
                                      "50%" = "black",
                                      "5%" = "gray")) + 
        xlab(" ") + ylab(expression(atop(bold(''))))
svtp
traceplot(svt$para[,4])
densplot(svt$para[,4])
paradensplot(svt)
paratraceplot(svt)



# SVL graph
svl_volatility <- as.data.frame(cbind(a=svl[["summary"]][["sd"]][,3],
                                      b=svl[["summary"]][["sd"]][,4],
                                      c=svl[["summary"]][["sd"]][,5]))

svlp <- svl_volatility %>%
        ggplot( aes(x=log_data$Date[2:2844])) + 
        geom_line(aes(y=a, color="#3366cc"), size = 0.5) +
        geom_line(aes(y = b), color = "black") + 
        geom_line(aes(y = c), color = "#3366cc") + 
        ggtitle("Posterior Quantiles") + 
        ylim(0,0.10) +
        theme(plot.margin = margin(t = 1.5, unit = "lines")) + 
        theme_economist() + 
        scale_color_manual(name = " ", 
                           values = c("95%" = "#3366cc", 
                                      "50%" = "black",
                                      "5%" = "gray")) + 
        xlab(" ") + ylab(expression(atop(bold(''))))
svlp
traceplot(svl$para[,5])
densplot(svl$para[,5])
paradensplot(svl)
paratraceplot(svl)





# SVTL graph
svtl_volatility <- as.data.frame(cbind(a=svtl[["summary"]][["sd"]][,3],
                                      b=svtl[["summary"]][["sd"]][,4],
                                      c=svtl[["summary"]][["sd"]][,5]))

svtlp <- svtl_volatility %>%
        ggplot( aes(x=log_data$Date[2:2844])) + 
        geom_line(aes(y=a, color="#3366cc"), size = 0.5) +
        geom_line(aes(y = b), color = "black") + 
        geom_line(aes(y = c), color = "#3366cc") + 
        ggtitle("Posterior Quantiles") + 
        ylim(0,0.10) +
        theme(plot.margin = margin(t = 1.5, unit = "lines")) + 
        theme_economist() + 
        scale_color_manual(name = " ", 
                           values = c("95%" = "#3366cc", 
                                      "50%" = "black",
                                      "5%" = "gray")) + 
        xlab(" ") + ylab(expression(atop(bold(''))))
svtlp
traceplot(svtl$para[,5])
densplot(svtl$para[,5])
paradensplot(svtl)
paratraceplot(svtl)

# proxies plot
prox <- read_excel('svr_proxies.xlsx')
#q <- prox%>% gather(prox,key="parameter", value = "value")
p <- prox %>% 
        ggplot(aes(x = Date, color = Proxy))+
        geom_line(aes(y = value)) +
        geom_line(aes(y = value)) + 
        geom_line(aes(y = value))+
        scale_color_manual(values = c("#3366cc","grey","black"),
                           labels = c(expression(italic("MASR")),
                                      expression(italic("RR")),expression(italic("RV"))))+
        
        ggtitle(" ") + 
        theme_economist() +
        theme(axis.ticks.x = element_blank(),
              strip.text.y = element_blank(),
              strip.text = element_text(face = 'italic'))+
        facet_grid(Proxy ~ ., scales = "free_y")+
        guides(color = guide_legend(override.aes = list(size=1)))+
        xlab(" ") + ylab(expression(atop(bold(''))))
p        

