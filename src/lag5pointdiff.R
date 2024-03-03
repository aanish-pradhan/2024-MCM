library(tidyverse)
setwd("~/mcm/")
tennis <- read.csv("Wimbledon_featured_matches.csv")
matches <- tennis |> split(f = tennis$match_id)
pv <- tennis$point_victor - 1
tennis$point_victor <- tennis$point_victor - 1
sv <- tennis$server - 1
tennis$server <- tennis$server - 1

momentum_metric <- function(x, w ,k = 5) { #point diff player 2 - player 1
  scores <- ((x$point_victor-1)*2 - 1)
  N <- nrow(x)
  dif <- integer(N)
  rc <- c(0, x$rally_count)
  for (i in 1:N) {
    ind <- (max(0,i-1-k)):(i-1)
    dif[i] <- (1 + w[10] * rc[i]) * sum(
      w[1]*scores[ind] + w[2]*x$p1_break_pt_won[ind] + w[3] * x$p2_break_pt_won[ind]
      + w[4]*x$p1_break_pt_missed[ind] + w[5] * x$p2_break_pt_missed[ind]
      + w[6] * x$p1_unf_err[ind] + w[7] * x$p2_unf_err[ind]
      + w[8] * x$p1_ace[ind] + w[9] * x$p2_ace[ind]
      + w[11] * x$p1_double_fault[ind] + w[12] * x$p2_double_fault[ind])
  }
  dif
}



wts <- function(w, k, mf) {
  dm <- numeric(k)
  for (i in 1:k) {
    dm[1+k-i] <- w[13]^(i-1)
  }
  mom <- lapply(matches, FUN = mf, w = w, k = k, d = dm) |> unlist(use.names = F)
  huh <- glm(pv ~ mom + sv, family = "binomial") # only mom and biases should be here
  huh$aic
}

w1 <- c(1,-1,1,1,-1,
        1,-1,-1,1,1,
        1, -1)
#best <- optim(w1, wts,k = 10,method = "BFGS", control = list(trace = 1))

#mom <- lapply(matches, momentum_metric, w = best$par) |> unlist(use.names = F)
#huh <- glm(pv ~ mom + sv , family = "binomial")
#summary(huh)

N <- 15
best_pars1 <- list()
best_AIC1 <- numeric(N)
for(j in 1:N) {
  best <- optim(w1, wts,k = j,method = "BFGS", control = list(trace = 1))
  best_pars1[j] <- best$par
  best_AIC1[j] <- best$value
  if (best$convergence != 0) {
    paste("iteration",k,"did not converge properly")
  }
}

# next step: make influence of previous points scale down by some percent. probably optimize wrt to that percent. im tired

m2 <- function(x, w, d ,k = 5) { #point diff player 2 - player 1
  scores <- ((x$point_victor-1)*2 - 1)
  N <- nrow(x)
  dif <- integer(N)
  dif[1] <- 0

  for (i in 2:N) {
    lags <- min(k,i-1)
    ind <- (max(1,i-k)):(i-1)
    dif[i] <- (1 + w[10] * x$rally_count[i-1]) * sum( d[(k-length(ind)+1):k] * (
      w[1]*scores[ind] + w[2]*x$p1_break_pt_won[ind] + w[3] * x$p2_break_pt_won[ind]
      + w[4]*x$p1_break_pt_missed[ind] + w[5] * x$p2_break_pt_missed[ind]
      + w[6] * x$p1_unf_err[ind] + w[7] * x$p2_unf_err[ind]
      + w[8] * x$p1_ace[ind] + w[9] * x$p2_ace[ind]
      + w[11] * x$p1_double_fault[ind] + w[12] * x$p2_double_fault[ind]) )
  }
  dif
}
w2 <- c(1,-1,1,1,-1,
        1,-1,-1,1,1,
        1, -1, .8)
N <- 5
offset <- 9
best_pars2 <- list()
best_AIC2 <- numeric(N)
for(j in 1:N) {
  lag <- j + offset
  cat(paste("lag =",lag,"\n"))
  best <- optim(w2, wts,k = lag, mf = m2, method = "BFGS", control = list(trace = 1))
  best_pars2[[j]] <- best$par
  best_AIC2[j] <- best$value
  if (best$convergence != 0) {
    cat(paste("iteration",lag,"did not converge properly\n"))
  }
}

best_weights <- best_pars2[[3]]
best_acf_correction <- best_weights[[13]]
dm <- numeric(12)
for (i in 1:12) {
  dm[13-i] <- best_weights[13]^(i-1)
}
best_momentum <- lapply(matches, FUN = m2, w = best_weights, k = 12, d = dm) |> unlist(use.names = F)
best_glm <- glm(pv ~ best_momentum + sv, family = "binomial")
summary(best_glm)
qr.reg<-statmod::qresiduals(best_glm)
par(mfrow=c(1,2))
## Residuals vs Fitted
plot(best_glm$fitted, qr.reg, xlab = 'Fitted Values', ylab = 'Randomized Quantile Residuals',
     pch = 16)
abline(h=0, col = 'red')
## QQ plot
qqnorm(qr.reg); abline(0,1, col='red')
