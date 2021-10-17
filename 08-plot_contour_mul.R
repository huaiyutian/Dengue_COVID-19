# Specifies a unique column name for the manyglm model

colnames(non_Residential_cb) = paste0("non_Residential_cb.", colnames(non_Residential_cb))

colnames(si_cb) = paste0("si_cb.", colnames(si_cb))
colnames(c1_cb) = paste0("c1_cb.", colnames(c1_cb))
colnames(c2_cb) = paste0("c2_cb.", colnames(c2_cb))
colnames(c3_cb) = paste0("c3_cb.", colnames(c3_cb))
colnames(c4_cb) = paste0("c4_cb.", colnames(c4_cb))
colnames(c5_cb) = paste0("c5_cb.", colnames(c5_cb))
colnames(c6_cb) = paste0("c6_cb.", colnames(c6_cb))
colnames(c7_cb) = paste0("c7_cb.", colnames(c7_cb))
colnames(c8_cb) = paste0("c8_cb.", colnames(c8_cb))

colnames(Residential_cb) = paste0("Residential_cb.", colnames(Residential_cb))
colnames(Workplace_cb) = paste0("Workplace_cb.", colnames(Workplace_cb))
colnames(Transit_cb) = paste0("Transit_cb.", colnames(Transit_cb))
colnames(Park_cb) = paste0("Park_cb.", colnames(Park_cb))
colnames(Grocery_cb) = paste0("Grocery_cb.", colnames(Grocery_cb))
colnames(Retail_cb) = paste0("Retail_cb.", colnames(Retail_cb))

library(RColorBrewer)
################################################################################
# FIG 3
################################c1##############################################
# Coefficient and covariance
coef_c1 <- model_c1$coefficients
vcov_c1 <- summary_c1$cov.unscaled

# find position of the terms associated with c1 crossbasis
ind <- c(2,3,4,5)

# set value
cen_c1 <- 0
min_c1 <- min(data_2020$c1, na.rm = FALSE)
max_c1 <- max(data_2020$c1, na.rm = FALSE)

# Forecast overall accumulation
c1_cp <- crosspred(c1_cb, coef = coef_c1[ind], vcov=vcov_c1[ind,ind],
                   model.link = "log", bylag = 0.1, cen = cen_c1)
c1_low_cp <- crosspred(c1_cb, coef = coef_c1[ind], vcov=vcov_c1[ind,ind],
                       model.link = "log", bylag = 0.1, at=seq(min_c1,cen_c1,by=0.1),cen=cen_c1) 
c1_high_cp <- crosspred(c1_cb, coef = coef_c1[ind], vcov=vcov_c1[ind,ind],
                        model.link = "log", bylag = 0.1,at=seq(cen_c1,max_c1,by=0.1),cen=cen_c1) 

# Plot
pdf("02.fig/Fig_3/Fig_3A-c1-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(c1_low_cp,"overall",col=2,xlim=c(0,100),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(c1_high_cp,"overall",ci="area",col=4,lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "School closing",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$c1,xlim=c(0,100),ylim=c(0,400),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:2*70,cex.axis=1.2)
mtext("Frequency",side =4,at=70, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_3/Fig_3B-c1-contour.pdf", width=5, height=5)

nlag = 3
y <- c1_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(c1_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("School closing")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_3/Fig_3C-c1_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- c1_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- c1_cp$matRRfit
rr.lci <- c1_cp$matRRlow
rr.uci <- c1_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 50)
mx2 <- which(round(vars, 2) == 90)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 1.5), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:3*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Closing school = ",vars[mn]),
                  paste0("Closing school = ", vars[mx]),
                  paste0("Closing school = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

################################################################################
################################non_Residential#########################################
coef_non_Residential <- model_non_Residential$coefficients
vcov_non_Residential <- summary_non_Residential$cov.unscaled

# set value
cen_non_Residential <- 100
min_non_Residential <- min(data_2020$non_Residential, na.rm = FALSE)
max_non_Residential <- max(data_2020$non_Residential, na.rm = FALSE)


# Forecast overall accumulation
non_Residential_cp <- crosspred(non_Residential_cb, coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],
                                model.link = "log", bylag = 0.1, cen = cen_non_Residential)
non_Residential_low_cp <- crosspred(non_Residential_cb, coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],
                                    model.link = "log", bylag = 0.1, at=seq(min_non_Residential,cen_non_Residential,by=0.1),cen=cen_non_Residential) 
non_Residential_high_cp <- crosspred(non_Residential_cb, coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],
                                     model.link = "log", bylag = 0.1,at=seq(cen_non_Residential,max_non_Residential,by=0.1),cen=cen_non_Residential) 

# Plot
pdf("02.fig/Fig_3/Fig_3A-non_Residential-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(non_Residential_low_cp,"overall",col=4,xlim=c(20,110),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(non_Residential_high_cp,"overall",ci="area",col=2,lwd=2.5)
axis(1,at=2:11*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Change (%) from baseline in non-residential mobility",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$non_Residential,xlim=c(20,110),ylim=c(0,200),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:3*25,cex.axis=1.2)
mtext("Frequency",side =4,at=37.5, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_3/Fig_3B-non_Residential-contour.pdf", width=5, height=5)

nlag = 3
y <- non_Residential_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(non_Residential_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Change (%) from baseline in non-residential mobility")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})

dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_3/Fig_3C-non_Residential_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- non_Residential_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- non_Residential_cp$matRRfit
rr.lci <- non_Residential_cp$matRRlow
rr.uci <- non_Residential_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 30)
mx <- which(round(vars, 2) == 80)
mx2 <- which(round(vars, 2) == 104)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 1.5), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:3*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Mobility in non-residential = ",vars[mn]),
                  paste0("Mobility in non-residential = ", vars[mx]),
                  paste0("Mobility in non-residential = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

################################################################################
#FIG S12
################################si##############################################
# Coefficient and covariance
coef_si <- model_si$coefficients
vcov_si <- summary_si$cov.unscaled

# set value
cen_si <- 0
min_si <- min(data_2020$si, na.rm = FALSE)
max_si <- max(data_2020$si, na.rm = FALSE)

# Forecast overall accumulation
si_cp <- crosspred(si_cb, coef = coef_si[ind], vcov=vcov_si[ind,ind],
                   model.link = "log", bylag = 0.1, cen = cen_si)
si_low_cp <- crosspred(si_cb, coef = coef_si[ind], vcov=vcov_si[ind,ind],
                       model.link = "log", bylag = 0.1, at=seq(min_si,cen_si,by=0.1),cen=cen_si) 
si_high_cp <- crosspred(si_cb, coef = coef_si[ind], vcov=vcov_si[ind,ind],
                        model.link = "log", bylag = 0.1,at=seq(cen_si,max_si,by=0.1),cen=cen_si) 

# Plot
pdf("02.fig/Fig_S12/Fig_S12A-si-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(si_low_cp,"overall",col=2,xlim=c(0,100),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(si_high_cp,"overall",ci="area",col=4,lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Stringency index",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$si,xlim=c(0,100),ylim=c(0,400),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:2*70,cex.axis=1.2)
mtext("Frequency",side =4,at=70, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_S12/Fig_S12A-si-counter.pdf", width=5, height=5)

nlag = 3
y <- si_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(si_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Stringency index")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_S12/Fig_S12C-si_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- si_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- si_cp$matRRfit
rr.lci <- si_cp$matRRlow
rr.uci <- si_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 50)
mx2 <- which(round(vars, 2) == 90)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 2.0), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:4*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Stringency index = ",vars[mn]),
                  paste0("Stringency index = ", vars[mx]),
                  paste0("Stringency index = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)
dev.off()

################################c2##############################################
# Coefficient and covariance
coef_c2 <- model_c2$coefficients
vcov_c2 <- summary_c2$cov.unscaled

# set value
cen_c2 <- 0
min_c2 <- min(data_2020$c2, na.rm = FALSE)
max_c2 <- max(data_2020$c2, na.rm = FALSE)

# Forecast overall accumulation
c2_cp <- crosspred(c2_cb, coef = coef_c2[ind], vcov=vcov_c2[ind,ind],
                   model.link = "log", bylag = 0.1, cen = cen_c2)
c2_low_cp <- crosspred(c2_cb, coef = coef_c2[ind], vcov=vcov_c2[ind,ind],
                       model.link = "log", bylag = 0.1, at=seq(min_c2,cen_c2,by=0.1),cen=cen_c2) 
c2_high_cp <- crosspred(c2_cb, coef = coef_c2[ind], vcov=vcov_c2[ind,ind],
                        model.link = "log", bylag = 0.1,at=seq(cen_c2,max_c2,by=0.1),cen=cen_c2) 

# Plot
pdf("02.fig/Fig_S12/Fig_S12A-c2-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(c2_low_cp,"overall",col=2,xlim=c(0,100),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(c2_high_cp,"overall",ci="area",col=4,lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Workplace closing",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$c2,xlim=c(0,100),ylim=c(0,400),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:2*70,cex.axis=1.2)
mtext("Frequency",side =4,at=70, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_S12/Fig_S12B-c2-contour.pdf", width=5, height=5)

nlag = 3
y <- c2_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(c2_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Workplace closing")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_S12/Fig_S12C-c2_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- c2_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- c2_cp$matRRfit
rr.lci <- c2_cp$matRRlow
rr.uci <- c2_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 50)
mx2 <- which(round(vars, 2) == 90)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 1.5), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:3*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Workplace closing = ",vars[mn]),
                  paste0("Workplace closing = ", vars[mx]),
                  paste0("Workplace closing = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

################################c3##############################################
# Coefficient and covariance
coef_c3 <- model_c3$coefficients
vcov_c3 <- summary_c3$cov.unscaled

# set value
cen_c3 <- 0
min_c3 <- min(data_2020$c3, na.rm = FALSE)
max_c3 <- max(data_2020$c3, na.rm = FALSE)

# Forecast overall accumulation
c3_cp <- crosspred(c3_cb, coef = coef_c3[ind], vcov=vcov_c3[ind,ind],
                   model.link = "log", bylag = 0.1, cen = cen_c3)
c3_low_cp <- crosspred(c3_cb, coef = coef_c3[ind], vcov=vcov_c3[ind,ind],
                       model.link = "log", bylag = 0.1, at=seq(min_c3,cen_c3,by=0.1),cen=cen_c3) 
c3_high_cp <- crosspred(c3_cb, coef = coef_c3[ind], vcov=vcov_c3[ind,ind],
                        model.link = "log", bylag = 0.1,at=seq(cen_c3,max_c3,by=0.1),cen=cen_c3) 

# Plot
pdf("02.fig/Fig_S12/Fig_S12A-c3-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(c3_low_cp,"overall",col=2,xlim=c(0,100),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(c3_high_cp,"overall",ci="area",col=4,lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Cancel public events",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$c3,xlim=c(0,100),ylim=c(0,400),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:2*70,cex.axis=1.2)
mtext("Frequency",side =4,at=70, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_S12/Fig_S12B-c3-contour.pdf", width=5, height=5)

nlag = 3
y <- c3_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(c3_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Cancel public events")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_S12/Fig_S12C-c3_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- c3_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- c3_cp$matRRfit
rr.lci <- c3_cp$matRRlow
rr.uci <- c3_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 50)
mx2 <- which(round(vars, 2) == 90)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 1.5), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:3*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Cancel public events = ",vars[mn]),
                  paste0("Cancel public events = ", vars[mx]),
                  paste0("Cancel public events = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

################################c4##############################################
# Coefficient and covariance
coef_c4 <- model_c4$coefficients
vcov_c4 <- summary_c4$cov.unscaled

# set value
cen_c4 <- 0
min_c4 <- min(data_2020$c4, na.rm = FALSE)
max_c4 <- max(data_2020$c4, na.rm = FALSE)

# Forecast overall accumulation
c4_cp <- crosspred(c4_cb, coef = coef_c4[ind], vcov=vcov_c4[ind,ind],
                   model.link = "log", bylag = 0.1, cen = cen_c4)
c4_low_cp <- crosspred(c4_cb, coef = coef_c4[ind], vcov=vcov_c4[ind,ind],
                       model.link = "log", bylag = 0.1, at=seq(min_c4,cen_c4,by=0.1),cen=cen_c4) 
c4_high_cp <- crosspred(c4_cb, coef = coef_c4[ind], vcov=vcov_c4[ind,ind],
                        model.link = "log", bylag = 0.1,at=seq(cen_c4,max_c4,by=0.1),cen=cen_c4) 

# Plot
pdf("02.fig/Fig_S12/Fig_S12A-c4-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(c4_low_cp,"overall",col=2,xlim=c(0,100),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(c4_high_cp,"overall",ci="area",col=4,lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Restrictions on gathering size",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$c4,xlim=c(0,100),ylim=c(0,400),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:2*70,cex.axis=1.2)
mtext("Frequency",side =4,at=70, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_S12/Fig_S12B-c4-contour.pdf", width=5, height=5)

nlag = 3
y <- c4_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(c4_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Restrictions on gathering size")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_S12/Fig_S12C-c4_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- c4_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- c4_cp$matRRfit
rr.lci <- c4_cp$matRRlow
rr.uci <- c4_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 50)
mx2 <- which(round(vars, 2) == 90)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 1.5), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:3*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Restrictions on gathering size = ",vars[mn]),
                  paste0("Restrictions on gathering size = ", vars[mx]),
                  paste0("Restrictions on gathering size = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()
################################c6##############################################
# Coefficient and covariance
coef_c6 <- model_c6$coefficients
vcov_c6 <- summary_c6$cov.unscaled

# set value
cen_c6 <- 0
min_c6 <- min(data_2020$c6, na.rm = FALSE)
max_c6 <- max(data_2020$c6, na.rm = FALSE)

# Forecast overall accumulation
c6_cp <- crosspred(c6_cb, coef = coef_c6[ind], vcov=vcov_c6[ind,ind],
                   model.link = "log", bylag = 0.1, cen = cen_c6)
c6_low_cp <- crosspred(c6_cb, coef = coef_c6[ind], vcov=vcov_c6[ind,ind],
                       model.link = "log", bylag = 0.1, at=seq(min_c6,cen_c6,by=0.1),cen=cen_c6) 
c6_high_cp <- crosspred(c6_cb, coef = coef_c6[ind], vcov=vcov_c6[ind,ind],
                        model.link = "log", bylag = 0.1,at=seq(cen_c6,max_c6,by=0.1),cen=cen_c6) 

# Plot
pdf("02.fig/Fig_S12/Fig_S12A-c6-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(c6_low_cp,"overall",col=2,xlim=c(0,100),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(c6_high_cp,"overall",ci="area",col=4,lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Stay at home requirements",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$c6,xlim=c(0,100),ylim=c(0,400),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:2*70,cex.axis=1.2)
mtext("Frequency",side =4,at=70, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_S12/Fig_S12B-c6-contour.pdf", width=5, height=5)

nlag = 3
y <- c6_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(c6_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Stay at home requirements")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_S12/Fig_S12C-c6_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- c6_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- c6_cp$matRRfit
rr.lci <- c6_cp$matRRlow
rr.uci <- c6_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 50)
mx2 <- which(round(vars, 2) == 90)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 1.5), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:3*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Stay at home requirements = ",vars[mn]),
                  paste0("Stay at home requirements = ", vars[mx]),
                  paste0("Stay at home requirements = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()
################################c7##############################################
# Coefficient and covariance
coef_c7 <- model_c7$coefficients
vcov_c7 <- summary_c7$cov.unscaled

# set value
cen_c7 <- 0
min_c7 <- min(data_2020$c7, na.rm = FALSE)
max_c7 <- max(data_2020$c7, na.rm = FALSE)

# Forecast overall accumulation
c7_cp <- crosspred(c7_cb, coef = coef_c7[ind], vcov=vcov_c7[ind,ind],
                   model.link = "log", bylag = 0.1, cen = cen_c7)
c7_low_cp <- crosspred(c7_cb, coef = coef_c7[ind], vcov=vcov_c7[ind,ind],
                       model.link = "log", bylag = 0.1, at=seq(min_c7,cen_c7,by=0.1),cen=cen_c7) 
c7_high_cp <- crosspred(c7_cb, coef = coef_c7[ind], vcov=vcov_c7[ind,ind],
                        model.link = "log", bylag = 0.1,at=seq(cen_c7,max_c7,by=0.1),cen=cen_c7) 

# Plot
pdf("02.fig/Fig_S12/Fig_S12A-c7-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(c7_low_cp,"overall",col=2,xlim=c(0,100),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(c7_high_cp,"overall",ci="area",col=4,lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Restrictions on internal movement",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$c7,xlim=c(0,100),ylim=c(0,400),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:2*70,cex.axis=1.2)
mtext("Frequency",side =4,at=70, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_S12/Fig_S12B-c7-contour.pdf", width=5, height=5)

nlag = 3
y <- c7_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(c7_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Restrictions on internal movement")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_S12/Fig_S12C-c7_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- c7_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- c7_cp$matRRfit
rr.lci <- c7_cp$matRRlow
rr.uci <- c7_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 50)
mx2 <- which(round(vars, 2) == 90)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 1.5), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:3*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Restrictions on internal movement = ",vars[mn]),
                  paste0("Restrictions on internal movement = ", vars[mx]),
                  paste0("Restrictions on internal movement = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()
################################c8##############################################
# Coefficient and covariance
coef_c8 <- model_c8$coefficients
vcov_c8 <- summary_c8$cov.unscaled

# set value
cen_c8 <- 0
min_c8 <- min(data_2020$c8, na.rm = FALSE)
max_c8 <- max(data_2020$c8, na.rm = FALSE)

# Forecast overall accumulation
c8_cp <- crosspred(c8_cb, coef = coef_c8[ind], vcov=vcov_c8[ind,ind],
                   model.link = "log", bylag = 0.1, cen = cen_c8)
c8_low_cp <- crosspred(c8_cb, coef = coef_c8[ind], vcov=vcov_c8[ind,ind],
                       model.link = "log", bylag = 0.1, at=seq(min_c8,cen_c8,by=0.1),cen=cen_c8) 
c8_high_cp <- crosspred(c8_cb, coef = coef_c8[ind], vcov=vcov_c8[ind,ind],
                        model.link = "log", bylag = 0.1,at=seq(cen_c8,max_c8,by=0.1),cen=cen_c8) 

# Plot
pdf("02.fig/Fig_S12/Fig_S12A-c8-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(c8_low_cp,"overall",col=2,xlim=c(0,100),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(c8_high_cp,"overall",ci="area",col=4,lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Restrictions on international travel",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$c8,xlim=c(0,100),ylim=c(0,400),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:2*70,cex.axis=1.2)
mtext("Frequency",side =4,at=70, line =2.5,cex=1.2,las=3)
dev.off()
################################################################################
pdf("02.fig/Fig_S12/Fig_S12B-c8-contour.pdf", width=5, height=5)

nlag = 3
y <- c8_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(c8_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Restrictions on international travel")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
# lag response for different Tmin scenarios (Main text Fig Fig_3B)
pdf("02.fig/Fig_S12/Fig_S12C-c8_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- c8_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- c8_cp$matRRfit
rr.lci <- c8_cp$matRRlow
rr.uci <- c8_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 50)
mx2 <- which(round(vars, 2) == 90)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 1.5), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:3*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Restrictions on international travel = ",vars[mn]),
                  paste0("Restrictions on international travel = ", vars[mx]),
                  paste0("Restrictions on international travel = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

################################################################################
#FIG S13
################################Workplace#########################################
coef_Workplace <- model_Workplace$coefficients
vcov_Workplace <- summary_Workplace$cov.unscaled

# set value
cen_Workplace <- 100
min_Workplace <- min(data_2020$Workplace, na.rm = FALSE)
max_Workplace <- max(data_2020$Workplace, na.rm = FALSE)


# Forecast overall accumulation
Workplace_cp <- crosspred(Workplace_cb, coef = coef_Workplace[ind], vcov=vcov_Workplace[ind,ind],
                        model.link = "log", bylag = 0.1, cen = cen_Workplace)
Workplace_low_cp <- crosspred(Workplace_cb, coef = coef_Workplace[ind], vcov=vcov_Workplace[ind,ind],
                            model.link = "log", bylag = 0.1, at=seq(min_Workplace,cen_Workplace,by=0.1),cen=cen_Workplace) 
Workplace_high_cp <- crosspred(Workplace_cb, coef = coef_Workplace[ind], vcov=vcov_Workplace[ind,ind],
                             model.link = "log", bylag = 0.1,at=seq(cen_Workplace,max_Workplace,by=0.1),cen=cen_Workplace) 

# Plot
pdf("02.fig/fig_S13/fig_S13A-Workplace-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(Workplace_low_cp,"overall",col=4,xlim=c(10,130),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(Workplace_high_cp,"overall",ci="area",col=2,lwd=2.5)
axis(1,at=1:13*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Change (%) from baseline in workplace mobility",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$Workplace,xlim=c(10,130),ylim=c(0,200),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:3*25,cex.axis=1.2)
mtext("Frequency",side =4,at=37.5, line =2.5,cex=1.2,las=3)
dev.off()

################################################################################
pdf("02.fig/Fig_S13/Fig_S13B-Workplace-contour.pdf", width=5, height=5)

nlag = 3
y <- Workplace_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(Workplace_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Change (%) from baseline in workplace mobility")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
pdf("02.fig/Fig_S13/Fig_S13C-Workplace_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- Workplace_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- Workplace_cp$matRRfit
rr.lci <- Workplace_cp$matRRlow
rr.uci <- Workplace_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) ==30)
mx <- which(round(vars, 2) == 80)
mx2 <- which(round(vars, 2) == 114)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 2.0), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:4*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Mobility in workplace = ",vars[mn]),
                  paste0("Mobility in workplace = ", vars[mx]),
                  paste0("Mobility in workplace = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()
################################Transit#########################################
coef_Transit <- model_Transit$coefficients
vcov_Transit <- summary_Transit$cov.unscaled

# set value
cen_Transit <- 100
min_Transit <- min(data_2020$Transit, na.rm = FALSE)
max_Transit <- max(data_2020$Transit, na.rm = FALSE)


# Forecast overall accumulation
Transit_cp <- crosspred(Transit_cb, coef = coef_Transit[ind], vcov=vcov_Transit[ind,ind],
                        model.link = "log", bylag = 0.1, cen = cen_Transit)
Transit_low_cp <- crosspred(Transit_cb, coef = coef_Transit[ind], vcov=vcov_Transit[ind,ind],
                            model.link = "log", bylag = 0.1, at=seq(min_Transit,cen_Transit,by=0.1),cen=cen_Transit) 
Transit_high_cp <- crosspred(Transit_cb, coef = coef_Transit[ind], vcov=vcov_Transit[ind,ind],
                             model.link = "log", bylag = 0.1,at=seq(cen_Transit,max_Transit,by=0.1),cen=cen_Transit) 

# Plot
pdf("02.fig/fig_S13/fig_S13A-Transit-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(Transit_low_cp,"overall",col=4,xlim=c(10,130),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(Transit_high_cp,"overall",ci="area",col=2,lwd=2.5)
axis(1,at=1:13*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Change (%) from baseline in transit stations mobility",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$Transit,xlim=c(10,130),ylim=c(0,200),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:3*25,cex.axis=1.2)
mtext("Frequency",side =4,at=37.5, line =2.5,cex=1.2,las=3)
dev.off()

################################################################################
pdf("02.fig/Fig_S13/Fig_S13B-Transit-contour.pdf", width=5, height=5)

nlag = 3
y <- Transit_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(Transit_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Change (%) from baseline in transit stations mobility")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
pdf("02.fig/Fig_S13/Fig_S13C-Transit_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- Transit_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- Transit_cp$matRRfit
rr.lci <- Transit_cp$matRRlow
rr.uci <- Transit_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) ==30)
mx <- which(round(vars, 2) == 80)
mx2 <- which(round(vars, 2) == 108)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 2.0), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:4*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Mobility in transit stations = ",vars[mn]),
                  paste0("Mobility in transit stations = ", vars[mx]),
                  paste0("Mobility in transit stations = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()
################################Grocery#########################################
coef_Grocery <- model_Grocery$coefficients
vcov_Grocery <- summary_Grocery$cov.unscaled

# set value
cen_Grocery <- 100
min_Grocery <- min(data_2020$Grocery, na.rm = FALSE)
max_Grocery <- max(data_2020$Grocery, na.rm = FALSE)


# Forecast overall accumulation
Grocery_cp <- crosspred(Grocery_cb, coef = coef_Grocery[ind], vcov=vcov_Grocery[ind,ind],
                        model.link = "log", bylag = 0.1, cen = cen_Grocery)
Grocery_low_cp <- crosspred(Grocery_cb, coef = coef_Grocery[ind], vcov=vcov_Grocery[ind,ind],
                            model.link = "log", bylag = 0.1, at=seq(min_Grocery,cen_Grocery,by=0.1),cen=cen_Grocery) 
Grocery_high_cp <- crosspred(Grocery_cb, coef = coef_Grocery[ind], vcov=vcov_Grocery[ind,ind],
                             model.link = "log", bylag = 0.1,at=seq(cen_Grocery,max_Grocery,by=0.1),cen=cen_Grocery) 

# Plot
pdf("02.fig/fig_S13/fig_S13A-Grocery-association.pdf", width=5, height=5)
par(mar = c(5, 4, 2, 4))
plot(Grocery_low_cp,"overall",col=4,xlim=c(10,130),ylim=c(-1.,2),axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(Grocery_high_cp,"overall",ci="area",col=2,lwd=2.5)
axis(1,at=1:13*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Change (%) from baseline in grocery/pharmacy mobility",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
par(new=T)
hist(data_2020$Grocery,xlim=c(10,130),ylim=c(0,200),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:3*25,cex.axis=1.2)
mtext("Frequency",side =4,at=37.5, line =2.5,cex=1.2,las=3)
dev.off()

################################################################################
pdf("02.fig/Fig_S13/Fig_S13B-Grocery-contour.pdf", width=5, height=5)

nlag = 3
y <- Grocery_cp$predvar
x <- seq(0, nlag, 0.1)
z <- t(Grocery_cp$matRRfit)
max(z)
min(z)

pal <- rev(brewer.pal(11, "RdBu"))
levels <- pretty(c(0.32, 1.68), 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, month", ylab = expression(paste("Change (%) from baseline in grocery/pharmacy mobility")), main = "",key.title=title("RR"),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, c(0:nlag)) 
                 axis(2)})
dev.off()

################################################################################
pdf("02.fig/Fig_S13/Fig_S13C-Grocery_scenario.pdf", width = 5, height = 5)

# get exposures values
vars <- Grocery_cp$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- Grocery_cp$matRRfit
rr.lci <- Grocery_cp$matRRlow
rr.uci <- Grocery_cp$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
mn <- which(round(vars, 2) ==30)
mx <- which(round(vars, 2) == 80)
mx2 <- which(round(vars, 2) == 120)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.1)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 2, 
     xlab = "Lag, month", ylab = "Relative risk", main = "", 
     ylim = range(0.5, 2.0), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2,at=c(1:4*0.5))
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))

# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))

abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))

abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Mobility in grocery = ",vars[mn]),
                  paste0("Mobility in grocery = ", vars[mx]),
                  paste0("Mobility in grocery = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()
