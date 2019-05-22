factorial <- function (n) {
  f <- 1
  if (n > 0) {
    for (i in seq(n, 1)) {
      f <- f*i
    }
  }
  return(f)
}

combinatorial <- function (n, r) {
  return(factorial(n)/(factorial(r)*factorial(n - r)))
}


randomWalk <- function (n, p = 0.5, dim = 1) {
  rwAxis <- c()
  for (i in seq(1, dim)) {
    walk <- seq(1, n)
    if (runif(1) <= p) {
      walk[1] <- 1
    } else {
      walk[1] <- -1
    }
    for(j in seq(2, n)) {
      if (runif(1) <= p) {
        walk[j] <- walk[j - 1] + 1
      } else {
        walk[j] <- walk[j - 1] - 1
      }
    }
    rwAxis <- c(rwAxis, walk)
  }
  return(matrix(rwAxis, ncol = n, nrow = dim, byrow = TRUE))
}

randomBridgeWalk <- function (n) {
  walk <- seq(1, n)
  steps <- sample(c(rep(1, n), rep(-1, n)), size = 2*n, replace = FALSE)
  walk[1] <- steps[1]
  for(i in seq(2, 2*n)) {
    walk[i] <- walk[i-1] + steps[i]
  }

  return(walk)
}

runNRandomWalks <- function (N, n, p = 0.5, dim = 1) {
 walks <- seq(1, N*n*dim)
  for (i in seq(1, N)) {
    walks[seq(n*dim*(i - 1) +  1, n*dim*i)] <- c(randomWalk(n, p, dim))
  }

  return(array(walks, dim = c(n, dim, N)))
}

runNRandomBridgeWalks <- function (N, n) {
  walks <- seq(1, N*2*n)
  for (i in seq(1, N)) {
    walks[seq(2*n*(i - 1) +  1, 2*n*i)] <- randomBridgeWalk(n)
  }

  return(matrix(walks, nrow=N, ncol=2*n, byrow=TRUE))
}

cutOffValues <- function (ns, sims, p) {
  xinf <- c()
  xsup <- c()
  for(n in ns) {
    xsup <- c(xsup, quantile(sims[,n], probs = c(p)[[1]]))
    xinf <- c(xinf, quantile(sims[,n], probs = c(1 - p)[[1]]))
  }

  return(data.frame(xsup, xinf))
}

countLidershipChanges <- function(randomWalk) {
  s <- 0
  for (i in seq(2, length(randomWalk) - 1, 2)) {
    if (randomWalk[i-1] + randomWalk[i + 1] == 0) {
      s <- s + 1
    }
  }
  return(s)
}

lidershipChangesProbability <- function (N) {
  probs <- c()
  for (r in seq(0, N)) {
    probs <- c(probs, combinatorial(N, (N - 1)/2 - r) / 2**(N - 1))
  }
  return(probs)
}

lidershipChanges <- function(randomWalks) {
  s <- c()
  for (i in seq(1, dim(randomWalks)[3])) {
    s <- c(s, countLidershipChanges(randomWalks[,1,i]))
  }
  return(s)
}

last0Probability <- function (N) {
  probs <- c()
  for (k in seq(0, N, 2)) {
    probs <- c(probs, combinatorial(k, k/2) * combinatorial(N - k, (N - k)/2) / 2**N)
  }
  return(probs)
}

findLast0 <- function(randomWalk) {
  for (i in seq(length(randomWalk), 1)) {
    if (randomWalk[i] == 0) {
      return(i)
    }
  }
  return(0)
}

last0Ocurrence <- function(randomWalks) {
  l0s <- c()
  for (i in seq(dim(randomWalks)[3], 1)) {
    l0s <- c(l0s, findLast0(randomWalks[,1,i]))
  }
  return(l0s)
}

gamblersRuinProbability <- function (p, c, d, x) {
  q <- 1 - p
  return ((1 - (q/p)**(x - c)) / (1 - (q/p)**(d - c)))
}

gamblersRuin <- function (x0, c, d, p = 0.5) {
  sn <- x0
  while (sn > c & sn < d) {
    if (runif(1) <= p) {
      sn <- sn + 1
    } else {
      sn <- sn - 1
    }
  }
  return(as.numeric(sn == d))
}

runNgamblersRuin <- function (N, x0, c, d, p = 0.5) {
  ruins <- c()
  for (i in seq(1, N)) {
    ruins <- c(ruins, gamblersRuin(x0, c, d, p))
  }
  return(ruins)
}

#png("changes.png")
randomWalks <- runNRandomWalks(1000, 101)
changes <- lidershipChanges(randomWalks)
changesProps <- c()
for (n in seq(0,101)) {
  changesProps <- c(changesProps, sum(changes == n) / 1000)
}
changesProbs <- lidershipChangesProbability(101)
table1 <- data.frame(changesProps, changesProbs)
write.csv(table1, file = "q1a.csv")

#hist(changes, n=max(changes), 
#  main = "Passeio Aleatório Simples Simétrico",
#  xlab = "Quantidade de Trocas de Liderança",
#  ylab = "Frequência")
#dev.off()
#
#png("lastZeros.png")

randomWalks <- runNRandomWalks(1000, 100)
lastZeros <- last0Ocurrence(randomWalks)
lastZerosProps <- c()
for (n in seq(0,100, 2)) {
  lastZerosProps <- c(lastZerosProps, sum(lastZeros == n) / 1000)
}
lastZerosProbs <- last0Probability(100)
table2 <- data.frame(lastZerosProps, lastZerosProbs)
write.csv(table2, file = "q1b.csv")

#hist(lastZeros, n=max(lastZeros), 
#  main = "Últimas Visitas a Origem em Um P.A.S.S.",
#  xlab = "Última visita",
#  ylab = "Frequência")
#  dev.off()
#
#png("gamblersRuin.png")
wins <- c()
winsProbs <- c()
for (x0 in seq(10, 40, 10)) {
  ruins <- runNgamblersRuin(1000, x0, 0, 50, 18/38)
  wins <- c(wins, sum(ruins)/1000)
  winsProbs <- c(winsProbs, gamblersRuinProbability(18/38, 0, 50, x0))
}
table3 <- data.frame(wins, winsProbs)
write.csv(table3, file = "q2a.csv")

#plot(x = seq(10,40,10), y = wins/1000, 
#  main = "Ruina do Jogador",
#  xlab = "x0",
#  ylab = "Probabilidades (p)")
#dev.off()
#
#png("symmetricGamblersRuin.png")
winsProps <- c()
winsProbs <- c()
for (x0 in seq(10, 40, 10)) {
  ruins <- runNgamblersRuin(1000, x0, 0, 50)
  winsProps <- c(winsProps, sum(ruins)/1000)
}
table4 <- data.frame(winsProps)
write.csv(table4, file = "q2b.csv")
#plot(x = seq(10,40,10), y = wins/1000, 
#  main = "Ruina do Jogador - Passeio Simétrico",
#  xlab = "x0",
#  ylab = "Probabilidades (p)")
#dev.off()

randomWalks <- runNRandomWalks(1000, 10000, p = 0.5, dim = 2)
expectedSn <- c()
for (i in seq(1000, 10000, 1000)) {
  meanDist <- mean(sqrt(randomWalks[i,1,]**2 + randomWalks[i,2,]**2))
  expectedSn <- c(expectedSn, meanDist)
}
write.csv(data.frame(expectedSn), file = "q3.csv")
png('q3.png')
plot(x = seq(1000, 10000, 1000), y = expectedSn, 
  main = "Passeio Aleatório Simétrico no Plano",
  xlab = "N (n-ésimo passo)",
  ylab = "Distância média")
dev.off()
