nycflights <- nycflights %>% mutate(avg_speed = distance/air_time*60)


hist()
rug() # draws the observances under the histogram

library(datasets)
data("cars")
with(cars, plot(speed,dist))
plot(cars$speed, cars$dist)


# The lattice system is most useful for conditioning types of plots which display how y changes with x across
# levels of z. The variable z might be a categorical variable of your data. This system is also good for putting
# many plots on a screen at once.

# using  data(state) 
xyplot(Life.Exp ~ Income | region, data=state, layout=c(4,1))
# Life.Exp ~ | Income | region  # plot life expectancy as it depends on income for each region.



library(ggplot2)
data(mpg)
qplot(displ, hwy, data=mpg)

# http://www.biostat.jhsph.edu/~rpeng/leanpub/rprog/chicago_data.zip
pengChicagoFile='/Users/m071478/HomeDocuments/R/coursera/data/chicago' # rds file
chicago=readRDS(pengChicagoFile)
dim(chicago)
str(chicago)
library(dplyr)
head(select(chicago, city:dptp))
chic.f = filter(chicago, pm25tmean2>30)
head(arrange(chicago, date))
tail(arrange(chicago, date))
head(arrange(chicago, desc(date))) # descending order
chicago=rename(chicago, pm25 = pm25tmean2, dewpoint=dptp)     
names(chicago)

chicago=mutate(chicago, pm25detrend=pm25-mean(pm25,na.rm = TRUE))
head(chicago)
chicago=mutate(chicago, tempcat=factor(1*(tmpd>80), labels=c("cold", "hot")) )
hotcold=group_by(chicago, tempcat)

chicago=mutate(chicago, year= as.POSIXlt(date)$year +1900)



# week 1 lesson 2 ------------
# The par() function is used to specify global graphics parameters that affect all plots in an R session. 
# (Use dev.off or plot.new to reset to the defaults.)

names(par())
par()$pin # Plot dimensions in inches

library(datasets)
hist(airquality$Ozone)
with(airquality, plot(Wind, Ozone, type="n"))
with(airquality, plot(Wind, Ozone), points(Wind, Ozone, col="red"))

airquality = transform(airquality, Month = factor(Month)) # make "Month" a factor for the boxplot function
boxplot(Ozone ~ Month, airquality, xlab="Month", ylab= "Ozone (ppb)",col.axis='blue', col.lab='red')


with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))


 with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
 ## Fit a simple linear regression model
 model <- lm(Ozone ~ Wind, airquality)
 ## Draw regression line on plot
 abline(model, lwd = 2)
   
# Panel plot with two plots
par(mfrow = c(1, 2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})
   
# Panel plot with three plots
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
     plot(Wind, Ozone, main = "Ozone and Wind")
     plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
     plot(Temp, Ozone, main = "Ozone and Temperature")
     mtext("Ozone and Weather in New York City", outer = TRUE)
   })
  
example(points)

set.seed(19)
 x <- rnorm(30)
 y <- rnorm(30)
 plot(x, y, col = rep(1:3, each = 10), pch = 19)
 legend("bottomright", legend = paste("Group", 1:3), col = 1:3, pch = 19, bty = "n")
 
 x=rnorm(100)
 y=rnorm(100)
 g=gl(2,50) #Generate factors by specifying the pattern of their levels.
 g=gl(2,50, labels=c("male", "female")) 
 str(g)
 plot(x,y)
 plot(x,y,type="n")
 points(x[g=="male"], y[g=="male"], col='green')
 points(x[g=="female"], y[g=="female"], col='blue')
 
 
 # Week 1 lesson 3 ---------------------------
 # graphic devices
 
 # ?Devices # to see what graphics devices are available on your system.
 # file devices include 
 # Vector formats are good for line drawings and plots with solid colors using a modest
 # | number of points, while bitmap formats are good for plots with a large number of points, natural scenes or
 # | web-based plots.
 
 # vector formats: i.e. best for line or scatter plots, resize well, portable, not good for a large number of points
 #  pdf useful for line-type graphics and papers. It resizes well, is usually portable, but it is not efficient if a plot has many objects/points.
 #  svg XML-based, scalable vector graphics. This supports animation and interactivity and is potentially useful for web-based plots.
 # bitmap formats:  represents images as a series of pixels, good for lots of points. do not resize well!
 #  png: good for solid colors, lossless compression
 # (Portable Network Graphics) which is good for line drawings or images with solid colors. It uses lossless compression (like the old GIF format), and most web browsers can read this format natively.
       # is good for plots with many points, but it does not resize well.
 # jpeg: are good for photographs or natural scenes. They use lossy compression, so they're good for plots with many points. Files in jpeg format don't resize well, but they can be read by almost any computer
         #  and any web browser. They're not great for line drawings.
 #  tiff
 #  bmp: windows, used for icons
 dev.copy2pdf # specifically copy a plot to a PDF file
 dev.copy(png, file = "name of file.png")
 
 
 # exercise 1 -------------------
 install.packages("swirl")
 packageVersion("swirl")
 library(swirl)
 rm(list=ls()) 
 install_from_swirl("Exploratory Data Analysis")
 swirl()
 