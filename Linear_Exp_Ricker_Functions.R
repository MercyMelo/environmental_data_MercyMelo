#Import dispersal dataset
require(here)
disp <- read.csv(here("data", "dispersal.csv"))
      
#Define linear function
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

#Define exponential function
exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}

#Use exponential function to plot curve using set a and b values
curve(
  exp_fun(x, 1.9, 0.1),
  from = 0, to = 10, add = FALSE,
  col = "black", lty = "solid",
  xlim = c(-2,10), ylim = c(0,2),
  main = "Exponential Function",
  ylab = "f(x)", xlab = "x")

curve(
  exp_fun(x, 1.9, 0.5),
  from = 0, to = 10, add = TRUE,
  col = "black", lty = "dotted",
  xlim = c(-2,10), ylim = c(0,2),
  main = "Exponential Function",
  ylab = "f(x)", xlab = "x")

curve(
  exp_fun(x, 0.8, 0.1),
  from = 0, to = 10, add = TRUE,
  col = "red", lty = "solid",
  xlim = c(-2,10), ylim = c(0,2),
  main = "Exponential Function",
  ylab = "f(x)", xlab = "x")

curve(
  exp_fun(x, 1.2, 0.4),
  from = 0, to = 10, add = TRUE,
  col = "red", lty = "dotted",
  xlim = c(-2,10), ylim = c(0,2),
  main = "Exponential Function",
  ylab = "f(x)", xlab = "x")

dev.off()

#Define Ricker function     
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

#Use Ricker function to plot curve using set a and b values
curve(
  ricker_fun(x, 20, 0.1), 
  from = 0, to = 50, add = FALSE,
  col = "black", lty = "solid",
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

curve(
  ricker_fun(x, 20, 0.4), 
  from = 0, to = 50, add = TRUE,
  col = "black", lty = "dotted")

curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 50, add = TRUE,
  col = "black", lty = "dotted")

curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 50, add = FALSE,
  col = "red", lty = "solid")

curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 50, add = TRUE,
  col = "red", lty = "dotted")

curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 50, add = TRUE,
  col = "red", lty = "dotted")

#Using salamander data to explore functions
plot(disp$dist.class, disp$disp.rate.ftb,
    main = "Salamander Dispersal
     for First Time Breeders", 
     ylab = "Dispersal Rate",
     xlab = "Distance Class")

#Adding linear models to the plot
curve(
  line_point_slope(x, 300, 0.65, -0.001),
  add = TRUE)

#Adding exponential models to the plot
curve(
  exp_fun(x, 40, 0.01),
  from = 100, to = 1600, add = TRUE,
  col = "black", lty = "solid")

#Adding Ricker models to the plot
curve(
  ricker_fun(x, 0.01, 1/240), 
  from = 100, to = 1500, add = TRUE,
  col = "black", lty = "solid")

#Calculating y_pred values based on different models
#For linear model
y_pred_lin = line_point_slope(disp$dist.class, 300, 0.65, -0.001)
plot(disp$dist.class, y_pred_lin)
resids_linear <- disp$disp.rate.ftb - y_pred_lin
hist(resids_linear, main = "Residuals of Linear Model",
     xlab = "Residuals", ylab = "Frequency")

#For exponential model
y_pred_exp = exp_fun(disp$dist.class, 40, 0.01)
plot(disp$dist.class, y_pred_exp)
resids_exp <- disp$disp.rate.ftb - y_pred_exp
hist(resids_exp, main = "Residuals of Exponential Model",
     xlab = "Residuals", ylab = "Frequency")

#For Ricker model
y_pred_rick = ricker_fun(disp$dist.class, 0.01, 1/240)
plot(disp$dist.class, y_pred_rick)
resids_ricker <- disp$disp.rate.ftb - y_pred_rick
hist(resids_ricker, main = "Residuals of Ricker Model",
     xlab = "Residuals", ylab = "Frequency")

#Adding all residuals into a dataframe
resids_data_frame <- data.frame(resids_linear, resids_exp, resids_ricker)
head(resids_data_frame)

#Displaying histograms of residuals in single figure
par(mfrow = c(3, 1))
hist(resids_linear, main = "Residuals of Linear Model",
     xlab = "Residuals", ylab = "Frequency")
hist(resids_exp, main = "Residuals of Exponential Model",
     xlab = "Residuals", ylab = "Frequency")
hist(resids_ricker, main = "Residuals of Ricker Model",
     xlab = "Residuals", ylab = "Frequency")
