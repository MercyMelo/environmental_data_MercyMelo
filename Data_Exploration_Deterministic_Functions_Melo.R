library(here)
here()
dat_habitat<- read.csv(here("data", "hab.sta.csv"))
str(dat_habitat)

elev_hist <-hist(dat_habitat$elev, xlab = "Elevation", ylab = "Frequency", main = "Histogram of Elevation") 
slope_hist <-hist(dat_habitat$slope, xlab = "Slope", ylab = "Frequency", main = "Histogram of Slope")
aspect_hist <-hist(dat_habitat$aspect, xlab = "Aspect", ylab = "Frequency", main = "Histogram of Aspect") 

par(mfrow = c(1, 3))


#Training the line_point_slope function
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

mean(dat_habitat$aspect)

plot(dat_habitat$elev, dat_habitat$ba.tot, main = "Basal Area and Elevation", 
     xlab = "Elevation", ylab = "Total Basal Area", cex = 0.7) +
curve(line_point_slope(x, x1 = 400.52, y1 = 22.43, slope = -0.025), add = TRUE, col = "green")

plot(dat_habitat$slope, dat_habitat$ba.tot, main = "Basal Area and Slope", 
     xlab = "Slope", ylab = "Total Basal Area", cex = 0.7) +
  curve(line_point_slope(x, x1 = 50.27, y1 = 22.43, slope = 0.12), add = TRUE, col = "blue")

plot(dat_habitat$aspect, dat_habitat$ba.tot, main = "Basal Area and Aspect", 
     xlab = "Aspect", ylab = "Total Basal Area", cex = 0.7) +
  curve(line_point_slope(x, x1 = 182.49, y1 = 22.43, slope = 0.05), add = TRUE, col = "red")

par(mfrow = c(2,3))

