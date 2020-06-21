dm.net <- function (x1data, x2data, y2data, zdata) {
  x1data <- if (length(dim(x1data)) != 2) {
    array(x1data, c(length(x1data)[1], 1))
  } else {
    as.matrix(x1data)
  }
  x2data <- if (length(dim(x2data)) != 2) {
    array(x2data, c(length(x2data)[1], 1))
  } else {
    as.matrix(x2data)
  }
  y2data <- if (length(dim(y2data)) != 2) {
    array(y2data, c(length(y2data)[1], 1))
  } else {
    as.matrix(y2data)
  }
  zdata <- if (length(dim(zdata)) != 2) {
    array(zdata, c(length(zdata)[1], 1))
  } else {
    as.matrix(zdata)
  }
  
  if (!identical(dim(x1data)[1], dim(x2data)[1], dim(y2data)[1], dim(zdata)[1])) 
    stop("Data must be balanced.")
  
  n  <- dim(x1data)[1]
  m1 <- dim(x1data)[2]
  m2 <- dim(x2data)[2]
  s2 <- dim(y2data)[2]
  h1  <- dim(zdata)[2]
  names <- if (is.null(rownames(x1data))) {
    1:n
  } else {
    rownames(x1data)
  }
  e <- 10^-(ceiling(log(max(c(x1data, x2data, y2data, zdata)), base = 10)) + 6)
  
  results.efficiency.s <- array(NA, dim = c(n, 1), dimnames = list(names, "Eff.Sys"))
  results.efficiency.d <- array(NA, dim = c(n, 2), dimnames = list(names, paste0("Eff.Div.", 1:2)))
  results.v1weight     <- array(NA, dim = c(n, m1), dimnames = list(names, paste0("v^1_", 1:m1)))
  results.v2weight     <- array(NA, dim = c(n, m2), dimnames = list(names, paste0("v^2_", 1:m2)))
  results.u2weight     <- array(NA, dim = c(n, s2), dimnames = list(names, paste0("u^2_", 1:s2)))
  results.w1weight     <- array(NA, dim = c(n, h1), dimnames = list(names, paste0("w^1_", 1:h1)))
  results.w2weight     <- array(NA, dim = c(n, h1), dimnames = list(names, paste0("w^2_", 1:h1)))
  results.w            <- array(NA, dim = c(n, 2), dimnames = list(names, paste0("w^", 1:2, "_0")))
  
  p.v2 <- m1 + 1
  p.u2 <- p.v2 + m2
  p.w1 <- p.u2 + s2
  p.w2 <- p.w1 + h1
  p.w0 <- p.w2 + h1
  p.end <- p.w0 + 2
  for (j in 1:n) {
    lp.dea <- make.lp(0, m1 + m2 + s2 + h1 + h1 + 2) # v1 + v2 + u2 + w1 + w2 + w0
    
    set.objfn(lp.dea, c(x1data[j, ], x2data[j, ], -zdata[j, ], zdata[j, ], rep(1, 2)), indices = c(1:(p.u2 - 1), p.w1:(p.end - 1)))
    
    # Constraint for linearization
    add.constraint(lp.dea, c(y2data[j, ]), indices = c(p.u2:(p.w1 - 1)), "=", 1)
    
    # First - stage
    for (l in 1:n) {
      add.constraint(lp.dea, c(-x1data[l, ], zdata[l, ], -1), indices = c(1:(p.v2 - 1), p.w1:(p.w2 - 1), p.w0), "<=", 0)
    }
    
    # Second - stage
    for (l in 1:n) {
      add.constraint(lp.dea, c(-x2data[l, ], y2data[l, ], -zdata[l, ], -1), indices = c(p.v2:(p.u2 - 1), p.u2:(p.w1 - 1), p.w2:(p.w0 - 1), p.w0 + 1), "<=", 0)
    }
    
    # Set boundary of decision variables
    set.bounds(lp.dea, lower = c(rep(e, p.w0 - 1), rep(-Inf, 2)))
    
    solve.lpExtPtr(lp.dea)
    
    temp.p <- get.variables(lp.dea)
    results.efficiency.s[j]    <- sum(c(x1data[j, ], x2data[j, ], -zdata[j, ], zdata[j, ], rep(1, 2)) * temp.p[c(1:(p.v2 - 1), p.v2:(p.u2 - 1), p.w1:(p.w2 - 1), p.w2:(p.w0 - 1), p.w0:(p.end - 1))])
    results.efficiency.d[j, 1] <- sum(c(x1data[j, ], 1) * temp.p[c(1:(p.v2 - 1), p.w0)]) / sum(c(zdata[j, ]) * temp.p[c(p.w1:(p.w2 - 1))])
    results.efficiency.d[j, 2] <- sum(c(x2data[j, ], zdata[j, ], 1) * temp.p[c(p.v2:(p.u2 - 1), p.w2:(p.w0 - 1), p.w0 + 1)]) / sum(c(y2data[j, ]) * temp.p[c(p.u2:(p.w1 - 1))])
    results.v1weight[j, ] <- temp.p[1:(p.v2 - 1)]
    results.v2weight[j, ] <- temp.p[p.v2:(p.u2 - 1)]
    results.u2weight[j, ] <- temp.p[p.u2:(p.w1 - 1)]
    results.w1weight[j, ] <- temp.p[p.w1:(p.w2 - 1)]
    results.w2weight[j, ] <- temp.p[p.w2:(p.w0 - 1)]
    results.w[j, ]        <- temp.p[p.w0:(p.end - 1)]
  }
  return(list(eff.sys = results.efficiency.s, 
              eff.div = results.efficiency.d, 
              v1 = results.v1weight, 
              v2 = results.v2weight, 
              u2 = results.u2weight, 
              w1 = results.w1weight, 
              w2 = results.w2weight, 
              w  = results.w))
}
