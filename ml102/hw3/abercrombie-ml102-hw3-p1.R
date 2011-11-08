# Machine Learning 102 - Unsupervised - Hacker Dojo
# http://machinelearning102.pbworks.com/w/page/32890352/FrontPage
#
# Homework #3, Gaussian mixtue model, EM algorithm
# Dave Abercrombie, Noveember 1 2011
#
# http://machinelearning102.pbworks.com/w/file/fetch/47711752/Homework3.txt
#
# Problem 1 of 1
#

# Step 1. Generate synthetic datasets


# Step 1a Create a function that generates random points
# around a center cluster. 
#
point.generator.f <- function(
  label.string,
  center.vector,
  sd.vector,
  point.count
) {
  data.frame(
    label=label.string,
    x=rnorm(point.count, sd=sd.vector[1]) + center.vector[1],
    y=rnorm(point.count, sd=sd.vector[2]) + center.vector[2]
  )
}

# Step 1b: generate several clusters, resembling Tan's Basic Cluster Analysis 2004, slide 81'
red.df   <- point.generator.f(label.string="red",   center.vector=c( 4, 0), sd.vector=c(2, 1),      point.count=500)
blue.df  <- point.generator.f(label.string="blue",  center.vector=c(-1, 0), sd.vector=c(0.25, 0.5), point.count=100)
green.df <- point.generator.f(label.string="green", center.vector=c(-4, 0), sd.vector=c(1, 2),      point.count=300)
black.df <- point.generator.f(label.string="black", center.vector=c( 5, 2), sd.vector=c(0.5, 0.25), point.count=100)
gold.df  <- point.generator.f(label.string="gold",  center.vector=c( 6, 1), sd.vector=c(0.5, 0.25), point.count=100)
cyan.df  <- point.generator.f(label.string="cyan",  center.vector=c( 5,-1), sd.vector=c(0.5, 0.25), point.count=100)


# Step 1c: set plot limits and combine plots with par()
xlim.v <- c(-10,10)
ylim.v <- c(-5,5)

plot(   red.df[,-1], col="red",   xlim=xlim.v, ylim=ylim.v); par(new=T)
plot(  blue.df[,-1], col="blue",  xlim=xlim.v, ylim=ylim.v); par(new=T)
plot( green.df[,-1], col="green", xlim=xlim.v, ylim=ylim.v); par(new=T)
plot( black.df[,-1], col="black", xlim=xlim.v, ylim=ylim.v); par(new=T)
plot(  gold.df[,-1], col="gold",  xlim=xlim.v, ylim=ylim.v); par(new=T)
plot(  cyan.df[,-1], col="cyan",  xlim=xlim.v, ylim=ylim.v); par(new=F)

# Step 1d: combine into one data frame
syndata.labeled.df <- rbind(
  red.df,
  blue.df,
  green.df,
  black.df,
  gold.df,
  cyan.df
)

str(syndata.labeled.df)