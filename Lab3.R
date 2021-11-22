library(psych)
pairs.panels(iris)
dat_bird<- read.csv(here("data", "bird.sta.csv"))
dat_habitat<- read.csv(here("data", "hab.sta.csv"))

head(dat_bird)
head(dat_habitat)
#Columns in common: basin, sub, sta

dat_all <- merge(dat_bird, dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

Waxwings <- dat_all$CEWA
Waxwings > 0
cewa_present_absent <- as.numeric(Waxwings > 0)

plot(x = dat_all$elev, y = cewa_present_absent)

#Making functions for logistic curves
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

#Plotting a logistic curve on CEWA presence/absence data
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

#Assignment work
colnames(dat_all)
terrain_col <- c(87:89,98)
pairs.panels(terrain)
pairs.panels(dat_all[,terrain_col])

SongSparrow <- dat_all$SOSP
SongSparrow > 0
sosp_present_absent <- as.numeric(SongSparrow > 0)
plot(dat_all$ba.tot, sosp_present_absent, 
     xlab = "Basal Area", 
     ylab = "Song Sparrow Presence/Absence",
     main = "Song Sparrow Presence and Basal Area")
curve(logistic_midpoint_slope(x, midpoint = 40, slope = -0.6), add = TRUE)

WoodDuck <- dat_all$WODU
WoodDuck > 0
wodu_present_absent <- as.numeric(WoodDuck > 0)
plot(dat_all$ba.tot, wodu_present_absent,
     xlab = "Basal Area",
     ylab = "Wood Duck Presence/Absence",
     main = "Wood Duck Presence and Basal Area")
curve(logistic_midpoint_slope(x, midpoint = 32, slope = -0.8), add = TRUE)

GrayJay<- dat_all$GRJA
sum(GrayJay)

GJ<- as.numeric(GrayJay > 0)
sum(GJ)
