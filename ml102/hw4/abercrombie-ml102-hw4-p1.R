# Machine Learning 102 - Unsupervised - Hacker Dojo
# http://machinelearning102.pbworks.com/w/page/32890352/FrontPage
#
# Homework #4, Detecting Anomalies
# Dave Abercrombie, October 24 2011
#


normal.point.count <- 80
anomoly.1.point.count <- 10
anomoly.2.point.count <- 10
total.column.count <- 11
#
total.point.count <- normal.point.count + anomoly.1.point.count + anomoly.2.point.count

# Display and check the fraction of data that are anomolous, 
# looking for about 1%
anomoly.fraction <- (anomoly.1.point.count + anomoly.2.point.count)/total.point.count
anomoly.fraction

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
anomaly.1.m <- matrix(
  data=anomaly.1.v,
  nrow=10,
  ncol=11,
  byrow=TRUE
)


# Check dimensions of anomaly.1.m
# Does R have an "asert()"
#
if (nrow(anomaly.1.m) != anomoly.1.point.count) {
  print("Error: anomoly.1.point.count is wrong")
}
if(ncol(anomaly.1.m) != total.column.count) {
  print("Error: anomoly.1 column count is wrong")
}



# Write the anomoly data set 1 into the data frame.
# With R's indexing starting at 1, the "start" here
# means the last row before the start of the anomoly
#
anomoly.1.start <- normal.point.count
for (i in 1:anomoly.1.point.count) {
  synthetic.data[i+anomoly.1.start,] <- anomaly.1.m[i,]
}

synthetic.data
