require(plyr)
require(palmerpenguins)

sse_mean = function(x)
  {
  sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x)))
}

sse_mean(penguins$bill_depth_mm)
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

two_group_resample = function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  
  return(difference_in_means)
}

two_group_resample(penguins$flipper_length_mm, 68, 152)
table(penguins$species)

set.seed(87)
n = 2000
difference_in_means = c()
for (i in 1:n)
{
  difference_in_means = c(
    difference_in_means,
    two_group_resample(penguins$flipper_length_mm, 68, 152)
  )
}
hist(difference_in_means, xlab = "Mean Differences")

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

sum(abs(difference_in_means) >= diff_observed)


dat_pen$body_mass_g
boxplot(body_mass_g ~ species, data = dat_pen)
mass_t_test = t.test(dat_pen$body_mass_g ~ dat_pen$species)
mass_t_test
mass_diff = round(diff(mass_t_test$estimate), digits = 3)
mass_diff
