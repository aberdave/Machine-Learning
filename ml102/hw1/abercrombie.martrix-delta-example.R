# Machine Learning 102 - Unsupervised - Hacker Dojo
# http://machinelearning102.pbworks.com/w/page/32890352/FrontPage
#
# Example time series delta calulations
# Dave Abercrombie, October 24 2011
#
# http://machinelearning102.pbworks.com/w/file/37958115/ML102Homework02.pdf
# http://archive.ics.uci.edu/ml/datasets/Synthetic+Control+Chart+Time+Series
#

# 5.14 Initializing a Matrix
m1 <- matrix(
  c( 
    1, 2, 4, 7,
    3, 6, 8, 9,
    12, 16, 20, 21
  ),
  3,4, byrow=TRUE
)
dim(m1)
m1
ncol(m1)

# 5.17 Selecting One Row or Column from a Matrix
# 5.29 Combining Two Data Frames

# Itereate the first column of data frame by subtracting the 
# matrix first column from it's second. I use drop=FALSE to
# preserve the column-like nature of the subtraction.
#
control.chart.deltas.df <- data.frame(
  m1[,2,drop=FALSE] - m1[,1,drop=FALSE]
)

# iterate through the remaining matrix columns,
# appending fresh delta column to existing data frame
#
for ( i in 3:ncol(m1) ) {
  # calculate the next column of deltas
  # by subtracting column 1-2 from column i
  new.column <- data.frame(
    m1[,i,drop=FALSE] - m1[,i-1,drop=FALSE]
  )
  # combine the new column with the existing
  # data frame, column-wise
  control.chart.deltas.df <-cbind(
    control.chart.deltas.df,
    new.column
  )
}


# 5.25 Changing the Names of Data Frame Columns
# get original data frame column names
#
colnames(control.chart.deltas.df) <- c("Delta1", "Delta2", "Delta3")

# Interactive visual review ro convince yourself that
# the delta calculations are correct.
#
m1
control.chart.deltas.df

