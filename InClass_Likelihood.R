wiwr_counts = c(2, 6)
dpois(x = wiwr_counts, lambda = 4.5)
sum(log(dpois(x = wiwr_counts, lambda = 4.0)))

require(here)
dat_bird <- read.csv(here("data", "bird.sta.csv"))
dat_habitat <- read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)


summary(dat_all$WIWR)
hist(dat_all$WIWR, breaks = 0:7 - .5, 
     main = "Winter Wren Abundance", 
     xlab = "Number of Winter Wrens")
sum(log(dpois(x = dat_all$WIWR, lambda = 1.5)))


sum(log(dbinom(x = wiwr_counts, size = 6, prob = 0.95)))
