install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)

penguins = data.frame(penguins)
mean(penguins$body_mass_g, na.rm=TRUE)
head(penguins)

boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)
dev.off()

coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
coplot(body_mass_g ~ bill_depth_mm | bill_length_mm, data = penguins)

require(here)
png(filename = here("coplot1.png"), width = 800, height = 600)
coplot(body_mass_g ~ bill_depth_mm | bill_length_mm, data = penguins)
dev.off()

png(filename = here("coplot2.png"), width = 800, height = 600)
coplot(body_mass_g ~ flipper_length_mm | species, data = penguins)
dev.off()

png(filename = here("coplot3.png"), width = 800, height = 600)
boxplot(flipper_length_mm ~ species, data = penguins)
dev.off()
