# Setup ----
N <- 100
p <- 0.2

J <- 4  ## Occasions
y.all <- matrix(NA, N, J)
for(i in 1:N) {
  y.all[i,] <- rbinom(J, 1, p)
}

captured <- rowSums(y.all)>0
(n <- sum(captured))

y <- y.all[captured,]
y[1:3,]

histories <- apply(y, 1, paste, collapse="")
sort(table(histories))

y.tilde <- rowSums(y)
sort(table(y.tilde))

# Model M_t: temporal variation ----
p.t <- c(0.3, 0.5, 0.2, 0.4)

y.all.Mt <- matrix(NA, N, J)
for(i in 1:N) {
  y.all.Mt[i,] <- rbinom(J, 1, p.t) }

captured.Mt <- rowSums(y.all.Mt)>0
n.Mt <- sum(captured.Mt)
y.Mt <- y.all.Mt[captured.Mt,]
y.Mt[1:3,]

colSums(y.Mt)

# Model M_b: behavioral response ----
p.b <- 0.3
c <- 0.5  ## Trap happy

y.all.Mb <- matrix(NA, N, J)
prevcap <- matrix(FALSE, N, J)
for(i in 1:N) {
  y.all.Mb[i,1] <- rbinom(1, 1, p.b)
  for(j in 2:J) {
    prevcap[i,j] <- any(y.all.Mb[i,1:(j-1)]>0)
    prob <- ifelse(prevcap[i,j], c, p.b)
    y.all.Mb[i,j] <- rbinom(1, 1, prob)
  }
}

captured.Mb <- rowSums(y.all.Mb)>0
n.Mb <- sum(captured.Mb)
y.Mb <- y.all.Mb[captured.Mb,]
prevcap[1:3,]

# Model M_h: finite mixture ----
mixture.prob <- 0.6
group <- rbinom(N, 1, mixture.prob)    ## Two groups
p.Mh.mix <- ifelse(group==0, 0.2, 0.7) ## Two-point mixture

y.all.Mh.mix <- matrix(NA, N, J)
for(i in 1:N) {
  y.all.Mh.mix[i,] <- rbinom(J, 1, p.Mh.mix[i])
}

captured.Mh.mix <- rowSums(y.all.Mh.mix)>0
n.Mh.mix <- sum(captured.Mh.mix)
y.Mh.mix <- y.all.Mh.mix[captured.Mh.mix,]


# Joint Likelihood ----
## install.packages("RMark") ## Must install MARK too!!
library(RMark)
y.ch <- data.frame(ch=apply(y, 1, paste, collapse=""))
mark.M0 <- mark(data=y.ch, 
                model="Closed", 
                silent=TRUE,
                model.parameters=list(p=list(formula=~1,
                                             share=TRUE))
                )

mark.M0$results$real
mark.M0$results$derived
