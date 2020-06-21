#####################################################################################
### Setting up environment
#####################################################################################

# Load labrary
library("DJL")
source(file = "[Project] dm.net.R")

# Load data
df.raw <- read.csv("[Project] EV_specification.csv", header = T)
df.raw[, 11] <- round(100/df.raw[, 11], 1)

# Set parameters
id.t  <- c(3)
id.x1 <- c(5)
id.z  <- c(6:7)
id.x2 <- c(8:9)
id.y2 <- c(10:11, 13)
rts   <- "vrs"
ori   <- "o"
fy    <- 2020

#########################################################################################################################
### Analysis
#########################################################################################################################

# Cleansed data
# df.eff  <- df.raw[!apply(cbind(apply(is.na(df.raw[, c(5:11)]), 1, any), apply(is.na(df.raw[, c(12:14)]), 1, all)), 1, any), ]
df.eff  <- df.raw[!apply(is.na(df.raw[, c(id.x1, id.z, id.x2, id.y2)]), 1, any), ]

# Table.1 Descriptive statistics of raw data
table.1 <- sapply(df.raw[, c(id.x1, id.z, id.x2, id.y2)], function(x) c(Min  = min(x, na.rm = T),
                                                                        Med  = median(x, na.rm = T),
                                                                        Mean = mean(x, na.rm = T),
                                                                        Max  = max(x, na.rm = T),
                                                                        Std  = sd(x, na.rm = T)))
print(noquote(format(round(t(table.1), 2), big.mark = ",")))

# Table.2 Descriptive statistics of effective data
table.2 <- sapply(df.eff[, c(id.x1, id.z, id.x2, id.y2)], function(x) c(Min  = min(x),
                                                                        Med  = median(x),
                                                                        Mean = mean(x),
                                                                        Max  = max(x),
                                                                        Std  = sd(x)))
print(noquote(format(round(t(table.2), 2), big.mark = ",")))

# Conventional DEA
res.dea <- dm.dea(df.eff[, c(id.x1, id.x2)], df.eff[, id.y2], rts = rts, orientation = ori)
res.1st <- dm.dea(df.eff[, id.x1], df.eff[, id.z], rts = rts, orientation = ori)
res.2nd <- dm.dea(df.eff[, c(id.x2, id.z)], df.eff[, id.y2], rts = rts, orientation = ori)

cbind(df.eff[, c(1:3)], v = res.dea$v, u = res.dea$u)[which(round(res.1st$eff, 6) == 1), ]
cbind(df.eff[, c(1:3)], v = res.dea$v, u = res.dea$u)[which(round(res.2nd$eff, 6) == 1), ]
cbind(df.eff[, c(1:3)], v = res.dea$v, u = res.dea$u)[which(round(res.1st$eff * res.2nd$eff, 6) == 1), ]
cbind(df.eff[, c(1:3)], v = res.dea$v, u = res.dea$u)[which(round(res.dea$eff, 6) == 1), ]

# Network DEA
res.net <- dm.net(df.eff[, id.x1], df.eff[, id.x2], df.eff[, id.y2], df.eff[, id.z])

cbind(df.eff[, c(1:3)], res.net$v1, res.net$v2, res.net$u2, res.net$w1, res.net$w2, res.net$w)[which(round(res.net$eff.sys, 6) == 1), ]
cbind(df.eff[, c(1:3)], res.net$v1, res.net$w1, "w^1_0" = res.net$w[, 1])[which(round(res.net$eff.div[, 1], 6) == 1), ]
cbind(df.eff[, c(1:3)], res.net$v2, res.net$u2, res.net$w2, "w^2_0" = res.net$w[, 2])[which(round(res.net$eff.div[, 2], 6) == 1), ]
cbind(df.eff[, c(1:3)], round(res.net$eff.div[, 1], 6), round(res.net$eff.div[, 2], 6))[which(round(res.net$eff.div[, 1], 6) < round(res.net$eff.div[, 2], 6)), ]
cbind(df.eff[, c(1:3)], round(res.net$eff.div[, 1], 6), round(res.net$eff.div[, 2], 6))[which(round(res.net$eff.div[, 1], 6) > round(res.net$eff.div[, 2], 6)), ]

