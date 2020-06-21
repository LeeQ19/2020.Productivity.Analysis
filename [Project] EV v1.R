#####################################################################################
### Setting up environment
#####################################################################################

# Load labrary
library("DJL")

# Load data
df.raw <- read.csv("EV_evdatabase.csv", header = T)

# Set parameters
id.t  <- c(6)
id.x1 <- c(7:8)
id.z  <- c(9:10)
id.x2 <- c(11)
id.y2 <- c(12:15)
rts   <- "vrs"
ori   <- "i"
fy    <- 2020

#########################################################################################################################
### Analysis
#########################################################################################################################

# Cleansed data
df.eff  <- df.raw
res.roc1 <- roc.dea(df.eff[, id.x1], df.eff[, id.z], floor(df.eff[, id.t]), fy, rts, ori)
res.roc2 <- roc.dea(df.eff[, c(id.x2, id.z)], df.eff[, id.y2], floor(df.eff[, id.t]), fy, rts, ori)

# Table.1 Descriptive statistics
table.1 <- sapply(df.eff[, c(id.x1, id.z, id.x2, id.y2)], function(x) c(Min  = min(x),
                                                                        Med  = median(x),
                                                                        Mean = mean(x),
                                                                        Max  = max(x),
                                                                        Std  = sd(x)))
print(noquote(format(round(t(table.1), 2), big.mark = ",")))
