library(here)
here()
dat_catrate<- read.csv(here("data", "catrate.csv"))
dat_delomys<- read.csv(here("data", "delomys.csv"))
dat_rope<- read.csv(here("data", "rope.csv"))

head(dat_catrate)
head(dat_delomys)
head(dat_rope)

hist(dat_catrate$cat.rate)

plot(dat_delomys$body_mass, dat_delomys$body_length, 
     xlab = "Body Mass" ,ylab = "Body Length", 
     main = "Mercy's Morphological Data Plot")

