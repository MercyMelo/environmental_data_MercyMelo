bird <- read.csv(here("data", "bird.sub.csv"), header = TRUE, sep = ",")
hab <- read.csv(here("data", "hab.sub.csv"), header = TRUE, sep = ",")

birdhab <- merge(bird, hab, by = c("basin", "sub"))

plot(birdhab$ls, birdhab$BRCR)

fit_1 <- lm(BRCR ~ ls, data = birdhab)
summary(fit_1)

abline(fit_1)

linear <- function(x, y_int, slope)
  {print(y_int + slope*x)}

linear_simulator <- function(x, y_int, slope, st_dev)
{paste(y_int + slope * x + rnorm(n = x, mean = 0, sd = st_dev))}

n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)
fit_1_coefs

fit_1_summary = summary(fit_1)
str(fit_1_summary)
fit_1_summary$sigma

int_obs <- 0.099103949
slope_obs <- 0.005840474
sd_obs <- fit_1_summary$sigma

plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_sim = lm(y_sim ~ birdhab$ls)
summary(fit_sim)


n_sims = 1000
p_vals = numeric(n_sims)
alpha = 0.05
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < alpha) / n_sims


linear_sim_fit = function(x, y, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}


alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    power       = effect_size_powers,
    effect_size = effect_sizes_1)


plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = coef(fit_1)[2], lty = 2, col = 'red')


alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

for(j in 1:length(sample_sizes))
{
  x_vals = seq(0, 100, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    power       = sample_size_powers,
    sample_size = sample_sizes)



alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)

sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )
sim_n_effect_size


image(
  sim_n_effect_size$power,
  xlab = "Effect size",
  ylab = "Sample Size",
  axes = FALSE)

# add x-axis labels
axis(
  1, 
  at = c(0, 0.5, 1), 
  labels = c(-.01, 0.0, .01))

# add y=axis labels
axis(
  2, 
  at = c(0, 1), 
  labels = c(sample_sizes[1], tail(sample_sizes, 1)))










require(here)

int_obs <- 0.099103949
slope_obs <- 0.005840474
sd_obs <- 0.1412668


alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

n_sds = 20
pop_sds = seq(from = 0, to = 1, length.out = n_sds)

pop_sd_power = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sd_j,
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_power[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_power)

head(sim_output_dispersion)
plot(sim_output_dispersion$sd, sim_output_dispersion$power)
# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(power ~ pop_sds, data= sim_output_dispersion, 
     type = 'l', xlab = 'Power', ylab = 'Standard Deviation')
abline(v = nrow(birdhab), lty = 2, col = 'red')

# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = sd_obs, lty = 2, col = 'red')


alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

n_sds = 20
pop_sds = seq(from = 0, to = 1, length.out = n_sds)

pop_sd_power = numeric(n_sds)

sample_sizes = seq(10, 50)
sim_output_2 = matrix(nrow = length(pop_sds), 
                      ncol = length(sample_sizes))


for(k in 1:length(pop_sds))
{  
  pop_sd_j = pop_sds[k]

for(j in 1:length(sample_sizes))
{
   x_vals = seq(0, 100, length.out = sample_sizes[j])
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sd_j,
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sim_output_2[k,j] = sum(p_vals < alpha) / n_sims
}
}

sim_n_dispersion = 
  list(
    power = sim_output_2,
    sd = pop_sds,
    sample_size = sample_sizes)
head(sim_n_dispersion)

image(
  sim_n_dispersion$power,
  xlab = "Standard Deviation",
  ylab = "Sample Size",
  axes = FALSE)

# add x-axis labels
axis(
  1, 
  at = c(0, 0.5, 1), 
  labels = c(-.01, 0.0, .01))

# add y=axis labels
axis(
  2, 
  at = c(0, 1), 
  labels = c(sample_sizes[1], tail(sample_sizes, 1)))


contour(
  x = sim_n_dispersion$sd,
  y = sim_n_dispersion$sample_size,
  z = sim_n_dispersion$power,
  xlab = "Standard Deviation",
  ylab = "Sample Size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  method = "simple")
  #method = "edge")


persp(
  x = sim_n_dispersion$sd,
  y = sim_n_dispersion$sample_size,
  z = sim_n_dispersion$power,
  xlab = "sd", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

library(rgl)

persp3d( x = sim_n_dispersion$sd,
         y = sim_n_dispersion$sample_size,
         z = sim_n_dispersion$power,
         xlab = "sd", ylab = "n", zlab = "power",
         col = 'darkolivegreen',
         theta = 30, phi = 30, expand = .75,
         ticktype = 'detailed')

require(htmlwidgets)
saveWidget(rglwidget(),
  file =here("data", "n_sd_power_sim_plot2.html"),
  selfcontained = TRUE)

