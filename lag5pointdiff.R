library(tidyverse)
setwd("~/mcm/")
tennis <- read.csv("Wimbledon_featured_matches.csv")
matches <- tennis |> split(f = tennis$match_id)
pv <- tennis$point_victor - 1
sv <- tennis$server - 1

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



wts <- function(w, k) {
  mom <- lapply(matches, momentum_metric, w = w, k = k) |> unlist(use.names = F)
  huh <- glm(pv ~ mom + sv, family = "binomial") # only mom and biases should be here
  AIC(huh)
}

w1 <- c(1,-1,1,1,-1,
        1,-1,-1,1,1,
        1, -1)
#best <- optim(w1, wts,k = 10,method = "BFGS", control = list(trace = 1))

#mom <- lapply(matches, momentum_metric, w = best$par) |> unlist(use.names = F)
#huh <- glm(pv ~ mom + sv , family = "binomial")
#summary(huh)

N <- 30
best_pars <- list()
best_AIC <- numeric(N)
for(j in 1:N) {
  best <- optim(w1, wts,k = j,method = "BFGS", control = list(trace = 1))
  best_pars[j] <- best$par
  best_AIC[j] <- best$value
  if (best$convergence != 0) {
    paste("iteration",k,"did not converge properly")
  }
}

# next step: make influence of previous points scale down by some percent. probably optimize wrt to that percent. im tired
