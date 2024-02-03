setwd("~/mcm/")
tennis <- read.csv("Wimbledon_featured_matches.csv")
library(tidyverse)

awbp <- tennis %>% group_by(point_no) %>% summarize(mset = mean(set_no), 
                                                    m_winner = mean(point_victor),
                                                    er = mean(p1_unf_err + p2_unf_err))
awbp %>% ggplot(aes(x = point_no, y = er)) + geom_point()

awbs <- tennis %>% group_by(set_no) %>% summarize(m_winner = mean(point_victor),
                                                  er = mean(p1_unf_err + p2_unf_err))

awbs %>% ggplot(aes(x = set_no, y = er)) + geom_point()
matches <- tennis |> split(f = tennis$match_id)
match1 <- matches[[1]]
library(MSwM)
match1$point_victor <- (match1$point_victor - 1) |> as.integer() |> as.logical()
match1$server <- match1$server - 1 |> as.integer()
match1 <- match1 %>% mutate(err = p1_unf_err + p2_unf_err)
timelm <- glm(point_victor ~ server,data = match1,family = "binomial")
time.HMM <- msmFit(timelm, k=2, p=1, sw=c(T,T,T), family = "binomial",
                   control=list(parallel=FALSE, maxiter = 2000))
summary(time.HMM)
plot(time.HMM)
plotProb(time.HMM)
plotReg(time.HMM)

tennis$point_victor <- (tennis$point_victor - 1) |> as.integer() |> as.logical()
tennis$server <- tennis$server - 1 |> as.integer()
timelm <- glm(point_victor ~ server,data = tennis,family = "binomial")
time.HMM <- msmFit(timelm, k=2, p=1, sw=c(T,T,T), family = "binomial",
                   control=list(parallel=FALSE, maxiter = 500))
summary(time.HMM)
plot(time.HMM)
plotProb(time.HMM)
plotReg(time.HMM)