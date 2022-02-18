



x <- c(round(dnorm(0:30, mean = 15, sd = 5) * 1000), rep(0, times = 60))

plot(x, type = 'l')

y <- zoo::rollapply(x, 2, sum)
plot(y, type = 'l')


delay_dist_len <- 60

pr_mild <- 0.8
pr_moderate <- 0.15
pr_severe <- 1 - pr_mild - pr_moderate


mild_delay_dist <- c(rep(0, times = 7), 1)
mild_delay_dist <- c(mild_delay_dist, rep(0, times = delay_dist_len - length(mild_delay_dist) + 1))

moderate_delay_dist <- dgamma(0:delay_dist_len, shape = 3, scale = 2)
moderate_delay_dist <- moderate_delay_dist / sum(moderate_delay_dist)


severe_delay_dist <- dgamma(0:delay_dist_len, shape = 2, scale = 10)


plot(mild_delay_dist, type = 'l')
plot(moderate_delay_dist, type = 'l')
plot(severe_delay_dist, type = 'l')


combined_delay_dist <- mild_delay_dist * pr_mild + moderate_delay_dist * pr_moderate + severe_delay_dist * pr_severe

combined_delay_dist[1:7] <- 0

plot(cumsum(combined_delay_dist), type = 'l')

cdf_delay_dist <- cumsum(combined_delay_dist)

y <- rep(0, times = length(x))

for(i in 1:length(x)) {
  for(j in i:length(x)) {
    if(j - i + 1 > 0 & j - i + 1 < length(cdf_delay_dist))
      y[j] <- y[j] + x[i] * (1 - cdf_delay_dist[j - i + 1])
  }
}


plot(y, type = 'l')
lines(x)





