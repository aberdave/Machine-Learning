# Machine Learning 102 - Unsupervised - Hacker Dojo
# http://machinelearning102.pbworks.com/w/page/32890352/FrontPage
#
# Homework #4, Detecting Anomalies
# Dave Abercrombie, November 16 2011
#
# Part 1: generate data with anomalies.
#
# 1. Data entry of anomalous data and dimensions
# 2. Check data entry, transform it, and derive convenient variables
# 3. Define function to generate a row of typical, non-anomalous data
# 4. Create data frame of typical data, with slots to hold anomalies
# 5. Write the anomaly data sets into the data frame.
# 6. Export to CSV

#######################################################
# 1. Data entry of anomalous data and dimensions

ls()
setwd("/Users/dabercrombie/Documents/aberdave-repos/Machine-Learning/ml102/hw4")

normal.point.count <- 980
total.column.count <- 11

# Hardcode some anomalous data. Here with low
# and constant p1 through p10, but with very
# high r1
#
# See R Cookbook recipe "5.14 Initializing a Matrix"
# for details about this technique
#
anomaly.1.v <- c(
  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 200,
  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 300,
  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 100,
  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 200,
  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 300,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 200,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 300,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 300,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 100,
  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 300
)

anomaly.2.v <- c(
   5,  5, 10, 10, 20, 40, 80,  160, 320, 640, 100,
   5,  5, 10, 20, 30, 50, 90,  170, 330, 650,  90,
   5, 10, 20, 30, 40, 60, 100, 180, 340, 660,  80,
  10, 20, 30, 40, 50, 70, 110, 190, 350, 670,  70,
  20, 30, 40, 50, 60, 80, 120, 200, 360, 680,  60,
   5,  5, 10, 10, 20, 40, 80,  160, 320, 640, 100,
   5,  5, 10, 20, 30, 50, 90,  170, 330, 650,  90,
   5, 10, 20, 30, 40, 60, 100, 180, 340, 660,  80,
  10, 20, 30, 40, 50, 70, 110, 190, 350, 670,  70,
  20, 30, 40, 50, 60, 80, 120, 200, 360, 680,  60
)

####################################################################
# 2. Check data entry, transform it, and derive convenient variables


# count the rows of the anomaly data sets
anomaly.1.point.count <- length(anomaly.1.v) / total.column.count
anomaly.2.point.count <- length(anomaly.2.v) / total.column.count

# convert to matrices to help with later assignment to data frame
anomaly.1.m <- matrix(
  data=anomaly.1.v,
  nrow=anomaly.1.point.count,
  ncol=total.column.count,
  byrow=TRUE
)
anomaly.2.m <- matrix(
  data=anomaly.2.v,
  nrow=anomaly.2.point.count,
  ncol=total.column.count,
  byrow=TRUE
)

#

# Display and check the total count and fraction of data that are anomolous, 
# looking for about 1% ????
total.point.count <- normal.point.count + anomaly.1.point.count + anomaly.2.point.count
total.point.count
anomaly.fraction <- (anomaly.1.point.count + anomaly.2.point.count)/total.point.count
anomaly.fraction


####################################################################
# 3. Define function to generate a row of typical, non-anomalous data

row.generator.f <- function(rnorm.mean.arg) {

  # constants for the all but the last column
  columns.of.rnorm <- 10
  sd.of.rnorm <- 20
  minimum.increment <- 5

  # last column constants
  last.column.mean <- 20
  last.column.sd <- 10
  
  # generate a normally distributed vetor centered 
  # around the function's' argument rnorm.mean.arg
  x <- rnorm(
      n=columns.of.rnorm, 
      mean=rnorm.mean.arg, 
      sd=sd.of.rnorm
  )
  
  # round vector components to nearest increment by
  # dividing by the increment, rounding, then multiplying 
  # by the increment to recover original scale
  x <- x / minimum.increment
  x <- round(x)
  x <- x * minimum.increment
  
  # create one more column
  y <- rnorm(
      n=1, 
      mean=last.column.mean, 
      sd=last.column.sd
  )
  # round, and use zero if less than zero
  y <- round(y)
  if (y < 0) {
    y <- 0
  }
  
  # append this last column to the vector (row)
  x <- c(x, y)
  
  # return x
  x
}

####################################################################
# 4. Create data frame of typical data, with slots to hold anomalies

# pre-allocate memory to avoid costs
# of row-by-row growth of data frame.
# See R Cookbook recipe "5.21 Preallocating a Data Frame"
#
synthetic.data <- data.frame(
  p1=integer(total.point.count),
  p2=integer(total.point.count),
  p3=integer(total.point.count),
  p4=integer(total.point.count),
  p5=integer(total.point.count),
  p6=integer(total.point.count),
  p7=integer(total.point.count),
  p8=integer(total.point.count),
  p9=integer(total.point.count),
  p10=integer(total.point.count),
  r1=integer(total.point.count)
)



# Assign each row of synthetic data to the 
# pre-allocated data frame, using asignment
# rather that rbind(). The R Cookbook recipe
# fails to mention this detail, by the way.
#
for (i in 1:normal.point.count) {
  # Each row (i.e., point) has a mean value
  # that is itself randomly selected from a 
  # normal distribution. Here we generate
  # a random value to be used as this mean.
  #
  tmp.mean <- rnorm(
    n=1,
    mean=100,
    sd=20
  )
  # Generate a row and assign it to data frame
  synthetic.data[i,] <- row.generator.f(rnorm.mean.arg=tmp.mean)
}


####################################################################
# 5. Write the anomaly data sets into the data frame.

# With R's indexing starting at 1, the "start" here
# means the last row before the start of the anomaly

# anomaly 1 is right after typical data
anomaly.1.start <- normal.point.count
for (i in 1:anomaly.1.point.count) {
  synthetic.data[i+anomaly.1.start,] <- anomaly.1.m[i,]
}
anomaly.2.start <- normal.point.count + anomaly.1.point.count
for (i in 1:anomaly.2.point.count) {
  synthetic.data[i+anomaly.2.start,] <- anomaly.2.m[i,]
}

####################################################################
# 6. Export to CSV

write.csv(
    x=synthetic.data,
    row.names=FALSE,
    file="synthetic.data.csv"
)
